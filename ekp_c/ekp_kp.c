/*
 * ekp.c - Knuth-Plass line breaking algorithm
 *
 * The heart of the system. O(n²) worst case, but with pruning and
 * parallel candidate evaluation, typically O(n·m) where m is avg line length.
 *
 * Key optimizations:
 * - Prefix sums for O(1) range queries
 * - Early termination when line too long
 * - Parallel demerits computation for large paragraphs
 */

#include "ekp_module.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>

/* Global state */
ekp_state_t *ekp_global = NULL;

/* Fitness classes */
#define FITNESS_TIGHT  0
#define FITNESS_DECENT 1
#define FITNESS_LOOSE  2
#define FITNESS_VERY_LOOSE 3

/* Badness computation */
static inline double compute_badness(int32_t adjustment, int32_t flexibility)
{
    if (adjustment == 0)
        return 0.0;
    if (flexibility <= 0)
        return EKP_INFINITY;

    double ratio = (double)adjustment / flexibility;
    double badness = 100.0 * fabs(ratio * ratio * ratio);
    return badness > 10000.0 ? EKP_INFINITY : badness;
}

/* Fitness classification */
static inline uint8_t compute_fitness(int32_t adjustment, int32_t flexibility)
{
    if (flexibility <= 0)
        return FITNESS_DECENT;

    double ratio = (double)adjustment / flexibility;
    if (ratio < -0.5)
        return FITNESS_TIGHT;
    if (ratio < 0.5)
        return FITNESS_DECENT;
    if (ratio < 1.0)
        return FITNESS_LOOSE;
    return FITNESS_VERY_LOOSE;
}

/* Full demerits computation */
static inline double compute_demerits(double badness, int32_t penalty,
                                       uint8_t prev_fitness, uint8_t curr_fitness,
                                       bool end_hyphen, int prev_hyphen_count,
                                       int line_penalty, int fitness_penalty)
{
    /* Base: (line_penalty + badness)² */
    double base = (line_penalty + badness);
    base = base * base;

    /* Add break penalty squared */
    base += (double)penalty * penalty;

    /* Fitness incompatibility */
    int delta = abs((int)prev_fitness - (int)curr_fitness);
    if (delta > 1)
        base += fitness_penalty;

    /* Consecutive hyphen penalty (quadratic growth) */
    if (end_hyphen) {
        int count = prev_hyphen_count + 1;
        base += 100.0 * count * count;
    }

    return base;
}

/*
 * Check if position is a hyphenation break
 * Uses binary search for O(log n) lookup (positions are sorted)
 */
static inline bool is_hyphen_break(ekp_paragraph_t *p, size_t pos)
{
    if (p->hyphen_count == 0)
        return false;
    
    /* Binary search in sorted hyphen_positions */
    size_t lo = 0;
    size_t hi = p->hyphen_count - 1;
    
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if ((size_t)p->hyphen_positions[mid] < pos)
            lo = mid + 1;
        else
            hi = mid;
    }
    
    return (size_t)p->hyphen_positions[lo] == pos;
}

/*
 * Parallel work item for demerits computation
 */
typedef struct {
    ekp_paragraph_t *para;
    int32_t line_width;
    size_t start;
    size_t end;

    /* Output arrays (pre-allocated) */
    double *demerits;
    int32_t *backptrs;
    int32_t *rest_pixels;
    uint8_t *fitness;
    int32_t *hyphen_counts;
    int32_t *line_counts;

    /* Shared read-only input */
    const double *prev_demerits;
    const uint8_t *prev_fitness;
    const int32_t *prev_hyphen_counts;
    const int32_t *prev_line_counts;

    /* Parameters */
    int line_penalty;
    int hyphen_penalty;
    int fitness_penalty;
    double last_line_ratio;
} dp_work_t;

/*
 * Unified DP input structure for shared core algorithm
 * This allows both ekp_paragraph_t-based and array-based inputs
 * to use the same DP core logic.
 */
typedef struct {
    /* Prefix sum arrays */
    const int32_t *ideal_prefix;
    const int32_t *min_prefix;
    const int32_t *max_prefix;
    
    /* Glue arrays (nullable) */
    const int32_t *glue_ideals;
    const int32_t *glue_shrinks;
    const int32_t *glue_stretches;
    
    /* Hyphen info */
    const int32_t *hyphen_positions;
    size_t hyphen_count;
    int32_t hyphen_width;
    
    /* Dimensions */
    size_t n;  /* box count */
    int32_t line_width;
    
    /* K-P parameters */
    int line_penalty;
    int hyphen_penalty;
    int fitness_penalty;
    double last_line_ratio;
} dp_input_t;

/*
 * Shared hyphen position check for dp_input_t
 */
static inline bool dp_is_hyphen(const dp_input_t *in, size_t pos)
{
    if (!in->hyphen_positions || in->hyphen_count == 0)
        return false;
    
    /* Binary search in sorted positions */
    size_t lo = 0;
    size_t hi = in->hyphen_count - 1;
    
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if ((size_t)in->hyphen_positions[mid] < pos)
            lo = mid + 1;
        else
            hi = mid;
    }
    
    return (size_t)in->hyphen_positions[lo] == pos;
}

/*
 * Core DP algorithm - shared by both entry points
 * Processes position i, trying all end positions k.
 * Updates output arrays when better solutions found.
 */
static void dp_process_position(
    const dp_input_t *in,
    size_t i,
    /* Input state at position i */
    double prev_dem,
    uint8_t prev_fit,
    int prev_hyph,
    int prev_lines,
    /* Output arrays */
    double *demerits,
    int32_t *backptrs,
    int32_t *rest_pixels,
    uint8_t *fitness,
    int32_t *hyphen_counts,
    int32_t *line_counts)
{
    size_t n = in->n;
    int32_t line_width = in->line_width;
    
    /* Get leading glue for line starting at i */
    int32_t lead_ideal = (in->glue_ideals && i < n) ? in->glue_ideals[i] : 0;
    int32_t lead_shrink = (in->glue_shrinks && i < n) ? in->glue_shrinks[i] : 0;
    int32_t lead_stretch = (in->glue_stretches && i < n) ? in->glue_stretches[i] : 0;
    
    /* Try extending to each position k > i */
    for (size_t k = i + 1; k <= n; k++) {
        bool is_last = (k == n);
        bool end_hyphen = dp_is_hyphen(in, k - 1);
        
        /* Line metrics from i to k (excluding leading glue) */
        int32_t ideal = in->ideal_prefix[k] - in->ideal_prefix[i] - lead_ideal;
        int32_t min_w = in->min_prefix[k] - in->min_prefix[i] -
                       (lead_ideal - lead_shrink);
        int32_t max_w = in->max_prefix[k] - in->max_prefix[i] -
                       (lead_ideal + lead_stretch);
        
        /* Add hyphen width if needed */
        if (end_hyphen) {
            ideal += in->hyphen_width;
            min_w += in->hyphen_width;
            max_w += in->hyphen_width;
        }
        
        /* Too long? Also handle is_last && ideal > line_width. */
        if (min_w > line_width || (is_last && ideal > line_width)) {
            /* Force break if nothing else found */
            if (k > i + 1 && demerits[k - 1] >= EKP_INFINITY) {
                int32_t prev_ideal = in->ideal_prefix[k - 1] - in->ideal_prefix[i] - lead_ideal;
                int32_t rest = line_width - prev_ideal;
                demerits[k - 1] = prev_dem + 10000.0 + (double)rest * rest;
                backptrs[k - 1] = i;
                rest_pixels[k - 1] = rest;
                fitness[k - 1] = FITNESS_VERY_LOOSE;
                hyphen_counts[k - 1] = 0;
                line_counts[k - 1] = prev_lines + 1;
            }
            break;  /* No point trying longer lines */
        }
        
        /* Valid break? */
        bool valid = (min_w <= line_width && max_w >= line_width) ||
                    (is_last && ideal <= line_width);
        
        if (!valid)
            continue;
        
        /* Compute demerits */
        int32_t adjustment = line_width - ideal;
        int32_t flexibility = (adjustment > 0) ?
            (max_w - ideal) : (ideal - min_w);
        
        /* Single-box line: use minimum flexibility of 1 */
        bool is_single_box = (k == i + 1);
        if (is_single_box && flexibility <= 0)
            flexibility = 1;
        
        double badness;
        uint8_t fit;
        double dem;
        
        /* Single-box line: use fixed flexibility=1, fitness=decent
         * This must come BEFORE is_last check to match Elisp behavior
         * where single-box lines use consistent calculation */
        if (is_single_box) {
            badness = compute_badness(adjustment, 1);
            fit = FITNESS_DECENT;
            
            int penalty = end_hyphen ? in->hyphen_penalty : 0;
            dem = prev_dem + compute_demerits(badness, penalty,
                                              prev_fit, fit,
                                              end_hyphen, prev_hyph,
                                              in->line_penalty,
                                              in->fitness_penalty);
        } else if (is_last) {
            /* Last line: minimal penalty if reasonably filled */
            double fill_ratio = (double)ideal / line_width;
            if (fill_ratio < in->last_line_ratio) {
                badness = 50.0 * (1.0 - fill_ratio);
            } else {
                badness = 0.0;
            }
            fit = FITNESS_DECENT;
            dem = prev_dem + (in->line_penalty + badness) *
                             (in->line_penalty + badness);
        } else {
            badness = compute_badness(adjustment, flexibility);
            fit = compute_fitness(adjustment, flexibility);
            
            int penalty = end_hyphen ? in->hyphen_penalty : 0;
            dem = prev_dem + compute_demerits(badness, penalty,
                                              prev_fit, fit,
                                              end_hyphen, prev_hyph,
                                              in->line_penalty,
                                              in->fitness_penalty);
        }
        
        /* Update if better */
        if (dem < demerits[k]) {
            demerits[k] = dem;
            backptrs[k] = i;
            rest_pixels[k] = adjustment;
            fitness[k] = fit;
            hyphen_counts[k] = end_hyphen ? prev_hyph + 1 : 0;
            line_counts[k] = prev_lines + 1;
        }
    }
}

/*
 * Process a range of candidate breakpoints (for parallel execution)
 * Now uses shared dp_process_position() core.
 */
static void process_dp_range(void *arg)
{
    dp_work_t *work = (dp_work_t *)arg;
    ekp_paragraph_t *p = work->para;
    size_t n = p->box_count;
    
    /* Build temporary glue arrays from paragraph structure */
    int32_t *glue_ideals = malloc(n * sizeof(int32_t));
    int32_t *glue_shrinks = malloc(n * sizeof(int32_t));
    int32_t *glue_stretches = malloc(n * sizeof(int32_t));
    
    if (!glue_ideals || !glue_shrinks || !glue_stretches) {
        free(glue_ideals); free(glue_shrinks); free(glue_stretches);
        return;
    }
    
    for (size_t i = 0; i < n; i++) {
        glue_ideals[i] = p->glues[i].ideal;
        glue_shrinks[i] = p->glues[i].shrink;
        glue_stretches[i] = p->glues[i].stretch;
    }
    
    /* Create unified input structure */
    dp_input_t in = {
        .ideal_prefix = p->ideal_prefix,
        .min_prefix = p->min_prefix,
        .max_prefix = p->max_prefix,
        .glue_ideals = glue_ideals,
        .glue_shrinks = glue_shrinks,
        .glue_stretches = glue_stretches,
        .hyphen_positions = p->hyphen_positions,
        .hyphen_count = p->hyphen_count,
        .hyphen_width = p->hyphen_width,
        .n = n,
        .line_width = work->line_width,
        .line_penalty = work->line_penalty,
        .hyphen_penalty = work->hyphen_penalty,
        .fitness_penalty = work->fitness_penalty,
        .last_line_ratio = work->last_line_ratio
    };
    
    /* Process each position in range */
    for (size_t i = work->start; i < work->end; i++) {
        if (work->prev_demerits[i] >= EKP_INFINITY)
            continue;
        
        dp_process_position(&in, i,
                           work->prev_demerits[i],
                           work->prev_fitness[i],
                           work->prev_hyphen_counts[i],
                           work->prev_line_counts[i],
                           work->demerits,
                           work->backptrs,
                           work->rest_pixels,
                           work->fitness,
                           work->hyphen_counts,
                           work->line_counts);
    }
    
    free(glue_ideals);
    free(glue_shrinks);
    free(glue_stretches);
}

/*
 * Main line breaking function
 */
ekp_result_t *ekp_break_lines(ekp_paragraph_t *p, int32_t line_width)
{
    if (!p || p->box_count == 0 || line_width <= 0)
        return NULL;

    size_t n = p->box_count;

    /* Allocate DP arrays */
    double *demerits = malloc((n + 1) * sizeof(double));
    int32_t *backptrs = malloc((n + 1) * sizeof(int32_t));
    int32_t *rest_pixels = malloc((n + 1) * sizeof(int32_t));
    uint8_t *fitness = malloc((n + 1) * sizeof(uint8_t));
    int32_t *hyphen_counts = malloc((n + 1) * sizeof(int32_t));
    int32_t *line_counts = malloc((n + 1) * sizeof(int32_t));

    if (!demerits || !backptrs || !rest_pixels ||
        !fitness || !hyphen_counts || !line_counts) {
        free(demerits);
        free(backptrs);
        free(rest_pixels);
        free(fitness);
        free(hyphen_counts);
        free(line_counts);
        return NULL;
    }

    /* Initialize */
    for (size_t i = 0; i <= n; i++) {
        demerits[i] = EKP_INFINITY;
        backptrs[i] = -1;
        rest_pixels[i] = 0;
        fitness[i] = FITNESS_DECENT;
        hyphen_counts[i] = 0;
        line_counts[i] = 0;
    }
    demerits[0] = 0.0;

    /* Get parameters */
    int line_penalty = ekp_global ? ekp_global->line_penalty : 10;
    int hyphen_penalty = ekp_global ? ekp_global->hyphen_penalty : 50;
    int fitness_penalty = ekp_global ? ekp_global->fitness_penalty : 100;
    double last_ratio = ekp_global ? ekp_global->last_line_ratio : 0.5;

    /*
     * Single-threaded DP: simple and correct.
     *
     * Note: Previous "parallel" implementation had data races - multiple
     * threads writing to shared demerits[] array without synchronization.
     * DP has inherent sequential dependencies (demerits[k] depends on all
     * demerits[i] where i < k), making intra-paragraph parallelism complex.
     *
     * For real parallelism, use ekp_break_batch() to process multiple
     * paragraphs concurrently - that's the correct granularity.
     */
    dp_work_t work = {
        .para = p,
        .line_width = line_width,
        .start = 0,
        .end = n,
        .demerits = demerits,
        .backptrs = backptrs,
        .rest_pixels = rest_pixels,
        .fitness = fitness,
        .hyphen_counts = hyphen_counts,
        .line_counts = line_counts,
        .prev_demerits = demerits,
        .prev_fitness = fitness,
        .prev_hyphen_counts = hyphen_counts,
        .prev_line_counts = line_counts,
        .line_penalty = line_penalty,
        .hyphen_penalty = hyphen_penalty,
        .fitness_penalty = fitness_penalty,
        .last_line_ratio = last_ratio,
    };

    /* Iterative DP: O(n²) worst case, typically O(n·m) with early termination */
    for (size_t i = 0; i < n; i++) {
        if (demerits[i] >= EKP_INFINITY)
            continue;

        work.start = i;
        work.end = i + 1;
        process_dp_range(&work);
    }

    /* Trace back optimal path */
    ekp_result_t *result = calloc(1, sizeof(*result));
    if (!result) {
        free(demerits);
        free(backptrs);
        free(rest_pixels);
        free(fitness);
        free(hyphen_counts);
        free(line_counts);
        return NULL;
    }

    /* Count breaks */
    size_t break_count = 0;
    int32_t idx = n;
    while (idx > 0) {
        break_count++;
        idx = backptrs[idx];
        if (idx < 0)
            break;
    }

    result->breaks = malloc(break_count * sizeof(int32_t));
    result->rest_pixels = malloc(break_count * sizeof(int32_t));
    if (!result->breaks || !result->rest_pixels) {
        ekp_result_destroy(result);
        free(demerits);
        free(backptrs);
        free(rest_pixels);
        free(fitness);
        free(hyphen_counts);
        free(line_counts);
        return NULL;
    }

    result->break_count = break_count;
    result->total_cost = demerits[n];

    /* Fill in reverse order */
    idx = n;
    for (size_t i = break_count; i > 0; i--) {
        result->breaks[i - 1] = idx;
        result->rest_pixels[i - 1] = rest_pixels[idx];
        idx = backptrs[idx];
    }

    free(demerits);
    free(backptrs);
    free(rest_pixels);
    free(fitness);
    free(hyphen_counts);
    free(line_counts);

    return result;
}

void ekp_result_destroy(ekp_result_t *r)
{
    if (!r)
        return;
    free(r->breaks);
    free(r->rest_pixels);
    free(r);
}

/*
 * Pure DP with pre-computed prefix arrays (for Elisp integration)
 *
 * This is the preferred API when Elisp has already computed everything.
 * Elisp does: tokenization, width measurement, glue computation, prefix sums.
 * C module only does: O(n²) DP computation.
 *
 * All font-dependent calculations happen in Elisp. C module is pure algorithm.
 *
 * Note: This function now uses dp_process_position() for the core DP logic,
 * sharing the same algorithm with process_dp_range(). Any bug fix only needs
 * to be made once in dp_process_position().
 */

ekp_result_t *ekp_break_with_prefixes(
    const int32_t *ideal_prefix,
    const int32_t *min_prefix,
    const int32_t *max_prefix,
    const int32_t *glue_ideals,
    const int32_t *glue_shrinks,
    const int32_t *glue_stretches,
    size_t n,
    const int32_t *hyphen_positions,
    size_t hyphen_count,
    int32_t hyphen_width,
    int32_t line_width)
{
    if (!ideal_prefix || !min_prefix || !max_prefix || n == 0 || line_width <= 0)
        return NULL;

    /* Allocate DP arrays */
    double *demerits = malloc((n + 1) * sizeof(double));
    int32_t *backptrs = malloc((n + 1) * sizeof(int32_t));
    int32_t *rest_pixels = malloc((n + 1) * sizeof(int32_t));
    uint8_t *fitness = malloc((n + 1) * sizeof(uint8_t));
    int32_t *hyph_counts = malloc((n + 1) * sizeof(int32_t));
    int32_t *line_counts = malloc((n + 1) * sizeof(int32_t));

    if (!demerits || !backptrs || !rest_pixels || !fitness || !hyph_counts || !line_counts) {
        free(demerits); free(backptrs); free(rest_pixels);
        free(fitness); free(hyph_counts); free(line_counts);
        return NULL;
    }

    /* Initialize */
    for (size_t i = 0; i <= n; i++) {
        demerits[i] = EKP_INFINITY;
        backptrs[i] = -1;
        rest_pixels[i] = 0;
        fitness[i] = FITNESS_DECENT;
        hyph_counts[i] = 0;
        line_counts[i] = 0;
    }
    demerits[0] = 0.0;

    /* Get K-P parameters */
    int lp = ekp_global ? ekp_global->line_penalty : 10;
    int hp = ekp_global ? ekp_global->hyphen_penalty : 50;
    int fp = ekp_global ? ekp_global->fitness_penalty : 100;
    double last_ratio = ekp_global ? ekp_global->last_line_ratio : 0.5;

    /* Create unified input structure */
    dp_input_t in = {
        .ideal_prefix = ideal_prefix,
        .min_prefix = min_prefix,
        .max_prefix = max_prefix,
        .glue_ideals = glue_ideals,
        .glue_shrinks = glue_shrinks,
        .glue_stretches = glue_stretches,
        .hyphen_positions = hyphen_positions,
        .hyphen_count = hyphen_count,
        .hyphen_width = hyphen_width,
        .n = n,
        .line_width = line_width,
        .line_penalty = lp,
        .hyphen_penalty = hp,
        .fitness_penalty = fp,
        .last_line_ratio = last_ratio
    };

    /* DP: for each valid start, try all ends */
    for (size_t i = 0; i < n; i++) {
        if (demerits[i] >= EKP_INFINITY)
            continue;
        
        dp_process_position(&in, i,
                           demerits[i],
                           fitness[i],
                           hyph_counts[i],
                           line_counts[i],
                           demerits,
                           backptrs,
                           rest_pixels,
                           fitness,
                           hyph_counts,
                           line_counts);
    }

    /* If no valid path found to end, return NULL to fallback to Elisp */
    if (demerits[n] >= EKP_INFINITY) {
        free(demerits); free(backptrs); free(rest_pixels);
        free(fitness); free(hyph_counts); free(line_counts);
        return NULL;
    }

    /* Build result */
    ekp_result_t *result = calloc(1, sizeof(*result));
    if (!result) {
        free(demerits); free(backptrs); free(rest_pixels);
        free(fitness); free(hyph_counts); free(line_counts);
        return NULL;
    }

    /* Count breaks */
    size_t break_count = 0;
    int32_t idx = n;
    while (idx > 0) {
        break_count++;
        idx = backptrs[idx];
        if (idx < 0) break;
    }

    result->breaks = malloc(break_count * sizeof(int32_t));
    result->rest_pixels = malloc(break_count * sizeof(int32_t));
    if (!result->breaks || !result->rest_pixels) {
        ekp_result_destroy(result);
        free(demerits); free(backptrs); free(rest_pixels);
        free(fitness); free(hyph_counts); free(line_counts);
        return NULL;
    }

    result->break_count = break_count;
    result->total_cost = demerits[n];

    /* Fill in reverse order */
    idx = n;
    for (size_t i = break_count; i > 0; i--) {
        result->breaks[i - 1] = idx;
        result->rest_pixels[i - 1] = rest_pixels[idx];
        idx = backptrs[idx];
    }

    free(demerits); free(backptrs); free(rest_pixels);
    free(fitness); free(hyph_counts); free(line_counts);

    return result;
}

/*
 * Work item for batch processing
 */
typedef struct {
    ekp_batch_input_t *input;
    ekp_result_t *result;
} batch_work_t;

static void batch_worker(void *arg)
{
    batch_work_t *work = (batch_work_t *)arg;
    ekp_batch_input_t *in = work->input;

    work->result = ekp_break_with_prefixes(
        in->ideal_prefix, in->min_prefix, in->max_prefix,
        in->glue_ideals, in->glue_shrinks, in->glue_stretches,
        in->n,
        in->hyphen_positions, in->hyphen_count,
        in->hyphen_width, in->line_width);
}

/*
 * Batch line breaking: process multiple paragraphs in parallel
 *
 * This is the correct parallelization - each paragraph is completely
 * independent, so we get linear speedup with zero synchronization overhead.
 */
ekp_result_t **ekp_break_batch(ekp_batch_input_t *inputs, size_t count)
{
    if (!inputs || count == 0)
        return NULL;

    ekp_result_t **results = calloc(count, sizeof(ekp_result_t *));
    if (!results)
        return NULL;

    /* Single paragraph: no point using threads */
    if (count == 1 || !ekp_global || !ekp_global->pool) {
        for (size_t i = 0; i < count; i++) {
            ekp_batch_input_t *in = &inputs[i];
            results[i] = ekp_break_with_prefixes(
                in->ideal_prefix, in->min_prefix, in->max_prefix,
                in->glue_ideals, in->glue_shrinks, in->glue_stretches,
                in->n,
                in->hyphen_positions, in->hyphen_count,
                in->hyphen_width, in->line_width);
        }
        return results;
    }

    /* Multiple paragraphs: parallel processing */
    batch_work_t *works = malloc(count * sizeof(batch_work_t));
    if (!works) {
        /* Fallback to sequential */
        for (size_t i = 0; i < count; i++) {
            ekp_batch_input_t *in = &inputs[i];
            results[i] = ekp_break_with_prefixes(
                in->ideal_prefix, in->min_prefix, in->max_prefix,
                in->glue_ideals, in->glue_shrinks, in->glue_stretches,
                in->n,
                in->hyphen_positions, in->hyphen_count,
                in->hyphen_width, in->line_width);
        }
        return results;
    }

    /* Submit all work items */
    for (size_t i = 0; i < count; i++) {
        works[i].input = &inputs[i];
        works[i].result = NULL;
        ekp_pool_submit(ekp_global->pool, batch_worker, &works[i]);
    }

    /* Wait for all to complete */
    ekp_pool_wait(ekp_global->pool);

    /* Collect results */
    for (size_t i = 0; i < count; i++) {
        results[i] = works[i].result;
    }

    free(works);
    return results;
}

/*
 * Initialization and cleanup
 */
int ekp_init(void)
{
    if (ekp_global)
        return 0;

    ekp_global = calloc(1, sizeof(*ekp_global));
    if (!ekp_global)
        return -1;

    /* Default spacing */
    ekp_global->spacing.lws_ideal = 7;
    ekp_global->spacing.lws_stretch = 3;
    ekp_global->spacing.lws_shrink = 2;
    ekp_global->spacing.mws_ideal = 5;
    ekp_global->spacing.mws_stretch = 2;
    ekp_global->spacing.mws_shrink = 1;
    ekp_global->spacing.cws_ideal = 0;
    ekp_global->spacing.cws_stretch = 2;
    ekp_global->spacing.cws_shrink = 0;

    /* Default K-P parameters */
    ekp_global->line_penalty = 10;
    ekp_global->hyphen_penalty = 50;
    ekp_global->fitness_penalty = 100;
    ekp_global->last_line_ratio = 0.5;

    /* Create thread pool */
    ekp_global->pool = ekp_pool_create(EKP_THREAD_POOL_SIZE);
    if (!ekp_global->pool) {
        free(ekp_global);
        ekp_global = NULL;
        return -1;
    }

    pthread_mutex_init(&ekp_global->cache_lock, NULL);

    return 0;
}

void ekp_cleanup(void)
{
    if (!ekp_global)
        return;

    /* Destroy hyphenators */
    for (size_t i = 0; i < ekp_global->hyphenator_count; i++) {
        ekp_hyphen_destroy(ekp_global->hyphenators[i]);
    }

    /* Destroy paragraph cache */
    if (ekp_global->para_cache) {
        for (size_t i = 0; i < ekp_global->para_cache_size; i++) {
            ekp_para_destroy(ekp_global->para_cache[i]);
        }
        free(ekp_global->para_cache);
    }

    pthread_mutex_destroy(&ekp_global->cache_lock);
    ekp_pool_destroy(ekp_global->pool);
    free(ekp_global);
    ekp_global = NULL;
}
