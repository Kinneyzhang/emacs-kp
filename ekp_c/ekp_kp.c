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
 */
static inline bool is_hyphen_break(ekp_paragraph_t *p, size_t pos)
{
    for (size_t i = 0; i < p->hyphen_count; i++) {
        if ((size_t)p->hyphen_positions[i] == pos)
            return true;
    }
    return false;
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
 * Process a range of candidate breakpoints (for parallel execution)
 */
static void process_dp_range(void *arg)
{
    dp_work_t *work = (dp_work_t *)arg;
    ekp_paragraph_t *p = work->para;
    int32_t line_width = work->line_width;
    size_t n = p->box_count;

    for (size_t i = work->start; i < work->end; i++) {
        if (work->prev_demerits[i] >= EKP_INFINITY)
            continue;

        double prev_dem = work->prev_demerits[i];
        uint8_t prev_fit = work->prev_fitness[i];
        int prev_hyph = work->prev_hyphen_counts[i];
        int prev_lines = work->prev_line_counts[i];

        /* Get leading glue for line starting at i */
        int32_t leading_glue_ideal = (i < n) ? p->glues[i].ideal : 0;
        int32_t leading_glue_stretch = (i < n) ? p->glues[i].stretch : 0;
        int32_t leading_glue_shrink = (i < n) ? p->glues[i].shrink : 0;

        /* Try extending to each position k > i */
        for (size_t k = i + 1; k <= n; k++) {
            bool is_last = (k == n);
            bool end_hyphen = (k > 0) && is_hyphen_break(p, k - 1);

            /* Line metrics from i to k (excluding leading glue) */
            int32_t ideal = p->ideal_prefix[k] - p->ideal_prefix[i] - leading_glue_ideal;
            int32_t min_w = p->min_prefix[k] - p->min_prefix[i] -
                           (leading_glue_ideal - leading_glue_shrink);
            int32_t max_w = p->max_prefix[k] - p->max_prefix[i] -
                           (leading_glue_ideal + leading_glue_stretch);

            /* Add hyphen width if needed */
            if (end_hyphen) {
                ideal += p->hyphen_width;
                min_w += p->hyphen_width;
                max_w += p->hyphen_width;
            }

            /* Too long? */
            if (min_w > line_width) {
                /* Force break if nothing else found */
                if (k > 1 && work->demerits[k - 1] >= EKP_INFINITY) {
                    int32_t rest = line_width - (p->ideal_prefix[k - 1] -
                                                 p->ideal_prefix[i] - leading_glue_ideal);
                    work->demerits[k - 1] = prev_dem + 10000.0 + rest * rest;
                    work->backptrs[k - 1] = i;
                    work->rest_pixels[k - 1] = rest;
                    work->fitness[k - 1] = FITNESS_VERY_LOOSE;
                    work->hyphen_counts[k - 1] = 0;
                    work->line_counts[k - 1] = prev_lines + 1;
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

            double badness;
            uint8_t fit;
            double dem;

            if (is_last) {
                /* Last line: minimal penalty if reasonably filled */
                double fill_ratio = (double)ideal / line_width;
                if (fill_ratio < work->last_line_ratio) {
                    badness = 50.0 * (1.0 - fill_ratio);
                } else {
                    badness = 0.0;
                }
                fit = FITNESS_DECENT;
                dem = prev_dem + (work->line_penalty + badness) *
                                 (work->line_penalty + badness);
            } else {
                badness = compute_badness(adjustment, flexibility);
                fit = compute_fitness(adjustment, flexibility);

                int penalty = end_hyphen ? work->hyphen_penalty : 0;
                dem = prev_dem + compute_demerits(badness, penalty,
                                                  prev_fit, fit,
                                                  end_hyphen, prev_hyph,
                                                  work->line_penalty,
                                                  work->fitness_penalty);
            }

            /* Update if better */
            if (dem < work->demerits[k]) {
                work->demerits[k] = dem;
                work->backptrs[k] = i;
                work->rest_pixels[k] = adjustment;
                work->fitness[k] = fit;
                work->hyphen_counts[k] = end_hyphen ? prev_hyph + 1 : 0;
                work->line_counts[k] = prev_lines + 1;
            }
        }
    }
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

    /* For small paragraphs, single-threaded */
    if (n < 100 || !ekp_global || !ekp_global->pool) {
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

        /* Simple iterative DP */
        for (size_t i = 0; i < n; i++) {
            if (demerits[i] >= EKP_INFINITY)
                continue;

            work.start = i;
            work.end = i + 1;
            process_dp_range(&work);
        }
    } else {
        /* Parallel processing for large paragraphs */
        /* Split work across threads (wavefront approach) */
        size_t chunk_size = n / EKP_THREAD_POOL_SIZE;
        if (chunk_size < 10)
            chunk_size = 10;

        dp_work_t *works = malloc(EKP_THREAD_POOL_SIZE * sizeof(dp_work_t));
        if (!works) {
            /* Fall back to single-threaded */
            for (size_t i = 0; i < n; i++) {
                if (demerits[i] >= EKP_INFINITY)
                    continue;

                dp_work_t work = {
                    .para = p,
                    .line_width = line_width,
                    .start = i,
                    .end = i + 1,
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
                process_dp_range(&work);
            }
        } else {
            /* Wavefront: process in chunks */
            for (size_t wave = 0; wave < n; wave += chunk_size) {
                size_t wave_end = wave + chunk_size;
                if (wave_end > n)
                    wave_end = n;

                size_t work_count = 0;
                for (size_t i = wave; i < wave_end; i++) {
                    if (demerits[i] >= EKP_INFINITY)
                        continue;

                    works[work_count] = (dp_work_t){
                        .para = p,
                        .line_width = line_width,
                        .start = i,
                        .end = i + 1,
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
                    ekp_pool_submit(ekp_global->pool, process_dp_range,
                                    &works[work_count]);
                    work_count++;
                }

                ekp_pool_wait(ekp_global->pool);
            }
            free(works);
        }
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
 */

static inline bool is_hyphen_pos(const int32_t *positions, size_t count, int32_t pos)
{
    for (size_t i = 0; i < count; i++) {
        if (positions[i] == pos)
            return true;
        if (positions[i] > pos)
            return false;
    }
    return false;
}

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

    /* DP: for each valid start, try all ends */
    for (size_t i = 0; i < n; i++) {
        if (demerits[i] >= EKP_INFINITY)
            continue;

        /* Leading glue for line starting at i */
        int32_t lead_ideal = glue_ideals ? glue_ideals[i] : 0;
        int32_t lead_shrink = glue_shrinks ? glue_shrinks[i] : 0;
        int32_t lead_stretch = glue_stretches ? glue_stretches[i] : 0;

        for (size_t k = i + 1; k <= n; k++) {
            bool is_last = (k == n);
            bool end_hyph = hyphen_positions && is_hyphen_pos(hyphen_positions, hyphen_count, k - 1);

            /* Line width from i to k (exclude leading glue) */
            int32_t ideal = ideal_prefix[k] - ideal_prefix[i] - lead_ideal;
            int32_t min_w = min_prefix[k] - min_prefix[i] - (lead_ideal - lead_shrink);
            int32_t max_w = max_prefix[k] - max_prefix[i] - (lead_ideal + lead_stretch);

            if (end_hyph) {
                ideal += hyphen_width;
                min_w += hyphen_width;
                max_w += hyphen_width;
            }

            /* Too long? Force break at k-1 if no valid break found yet */
            if (min_w > line_width) {
                if (k > i + 1 && demerits[k - 1] >= EKP_INFINITY) {
                    /* Force break at previous position with high penalty */
                    int32_t prev_ideal = ideal_prefix[k - 1] - ideal_prefix[i] - lead_ideal;
                    int32_t rest = line_width - prev_ideal;
                    double forced_dem = demerits[i] + 10000.0 + (double)rest * rest;
                    demerits[k - 1] = forced_dem;
                    backptrs[k - 1] = i;
                    rest_pixels[k - 1] = rest;
                    fitness[k - 1] = FITNESS_VERY_LOOSE;
                    hyph_counts[k - 1] = 0;
                    line_counts[k - 1] = line_counts[i] + 1;
                }
                break;
            }

            /* Valid break? */
            bool valid = (min_w <= line_width && max_w >= line_width) ||
                        (is_last && ideal <= line_width);

            if (!valid)
                continue;

            /* Compute demerits */
            int32_t adj = line_width - ideal;
            int32_t flex = (adj > 0) ? (max_w - ideal) : (ideal - min_w);

            double bad;
            uint8_t fit;
            double dem;

            if (is_last) {
                double fill = (double)ideal / line_width;
                bad = (fill < last_ratio) ? 50.0 * (1.0 - fill) : 0.0;
                fit = FITNESS_DECENT;
                dem = demerits[i] + (lp + bad) * (lp + bad);
            } else {
                bad = compute_badness(adj, flex);
                fit = compute_fitness(adj, flex);
                int pen = end_hyph ? hp : 0;
                dem = demerits[i] + compute_demerits(bad, pen, fitness[i], fit,
                                                      end_hyph, hyph_counts[i], lp, fp);
            }

            if (dem < demerits[k]) {
                demerits[k] = dem;
                backptrs[k] = i;
                rest_pixels[k] = adj;
                fitness[k] = fit;
                hyph_counts[k] = end_hyph ? hyph_counts[i] + 1 : 0;
                line_counts[k] = line_counts[i] + 1;
            }
        }
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
