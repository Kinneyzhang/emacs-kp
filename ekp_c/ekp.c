/*
 * ekp.c - Emacs Knuth-Plass module entry point
 *
 * This is the main entry file for the Emacs dynamic module.
 * Naming follows Emacs module convention: module name = file name.
 * (require 'ekp-c) loads ekp.so, entry point in ekp.c
 *
 * Bridges C implementation to Emacs Lisp. Keep the interface minimal:
 * pixel measurement callback from Emacs, everything else in C.
 */

#include "ekp_module.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Required for Emacs modules */
int plugin_is_GPL_compatible;

/* Cached Emacs environment for callbacks */
static emacs_env *current_env = NULL;
static emacs_value measure_func = NULL;

/*
 * Pixel measurement callback that calls back into Emacs
 */
static int32_t emacs_measure_string(const char *text, size_t len)
{
    if (!current_env || !measure_func)
        return len * 7;  /* fallback: ~7 pixels per char */

    emacs_value str = current_env->make_string(current_env, text, len);
    emacs_value result = current_env->funcall(current_env, measure_func, 1, &str);

    if (current_env->non_local_exit_check(current_env) != emacs_funcall_exit_return)
        return len * 7;

    return (int32_t)current_env->extract_integer(current_env, result);
}

/*
 * ekp-c-init: Initialize the module
 */
static emacs_value Fekp_c_init(emacs_env *env, ptrdiff_t nargs,
                                emacs_value *args, void *data)
{
    (void)nargs; (void)args; (void)data;

    if (ekp_init() != 0) {
        emacs_value signal = env->intern(env, "error");
        emacs_value msg = env->make_string(env, "Failed to initialize ekp-c", 26);
        env->non_local_exit_signal(env, signal, msg);
        return env->intern(env, "nil");
    }

    return env->intern(env, "t");
}

/*
 * ekp-c-cleanup: Cleanup resources
 */
static emacs_value Fekp_c_cleanup(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value *args, void *data)
{
    (void)nargs; (void)args; (void)data;
    ekp_cleanup();
    return env->intern(env, "t");
}

/*
 * ekp-c-load-hyphenator: Load hyphenation dictionary
 */
static emacs_value Fekp_c_load_hyphenator(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value *args, void *data)
{
    (void)nargs; (void)data;

    if (!ekp_global) {
        emacs_value signal = env->intern(env, "error");
        emacs_value msg = env->make_string(env, "ekp-c not initialized", 21);
        env->non_local_exit_signal(env, signal, msg);
        return env->intern(env, "nil");
    }

    /* Get dictionary path */
    ptrdiff_t size = 0;
    env->copy_string_contents(env, args[0], NULL, &size);
    char *path = malloc(size);
    if (!path)
        return env->intern(env, "nil");

    env->copy_string_contents(env, args[0], path, &size);

    /* Load hyphenator */
    ekp_hyphenator_t *h = ekp_hyphen_create(path);
    free(path);

    if (!h)
        return env->intern(env, "nil");

    /* Store in global state */
    if (ekp_global->hyphenator_count < 32) {
        ekp_global->hyphenators[ekp_global->hyphenator_count++] = h;
        return env->make_integer(env, ekp_global->hyphenator_count - 1);
    }

    ekp_hyphen_destroy(h);
    return env->intern(env, "nil");
}

/*
 * ekp-c-set-spacing: Set spacing parameters
 */
static emacs_value Fekp_c_set_spacing(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value *args, void *data)
{
    (void)data;

    if (!ekp_global || nargs < 9) {
        return env->intern(env, "nil");
    }

    ekp_global->spacing.lws_ideal = env->extract_integer(env, args[0]);
    ekp_global->spacing.lws_stretch = env->extract_integer(env, args[1]);
    ekp_global->spacing.lws_shrink = env->extract_integer(env, args[2]);
    ekp_global->spacing.mws_ideal = env->extract_integer(env, args[3]);
    ekp_global->spacing.mws_stretch = env->extract_integer(env, args[4]);
    ekp_global->spacing.mws_shrink = env->extract_integer(env, args[5]);
    ekp_global->spacing.cws_ideal = env->extract_integer(env, args[6]);
    ekp_global->spacing.cws_stretch = env->extract_integer(env, args[7]);
    ekp_global->spacing.cws_shrink = env->extract_integer(env, args[8]);

    return env->intern(env, "t");
}

/*
 * ekp-c-set-penalties: Set K-P parameters
 */
static emacs_value Fekp_c_set_penalties(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value *args, void *data)
{
    (void)data;

    if (!ekp_global || nargs < 4)
        return env->intern(env, "nil");

    ekp_global->line_penalty = env->extract_integer(env, args[0]);
    ekp_global->hyphen_penalty = env->extract_integer(env, args[1]);
    ekp_global->fitness_penalty = env->extract_integer(env, args[2]);
    ekp_global->last_line_ratio = env->extract_float(env, args[3]);

    return env->intern(env, "t");
}

/*
 * ekp-c-hyphenate: Get hyphenation positions for a word
 */
static emacs_value Fekp_c_hyphenate(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value *args, void *data)
{
    (void)data;

    if (!ekp_global || nargs < 2)
        return env->intern(env, "nil");

    intmax_t h_idx = env->extract_integer(env, args[0]);
    if (h_idx < 0 || (size_t)h_idx >= ekp_global->hyphenator_count)
        return env->intern(env, "nil");

    ekp_hyphenator_t *h = ekp_global->hyphenators[h_idx];

    /* Get word */
    ptrdiff_t size = 0;
    env->copy_string_contents(env, args[1], NULL, &size);
    char *word = malloc(size);
    if (!word)
        return env->intern(env, "nil");

    env->copy_string_contents(env, args[1], word, &size);

    /* Hyphenate */
    int8_t positions[EKP_MAX_WORD_LEN];
    int count = ekp_hyphen_word(h, word, size - 1, positions, EKP_MAX_WORD_LEN);
    free(word);

    /* Build result list */
    emacs_value result = env->intern(env, "nil");
    emacs_value cons_sym = env->intern(env, "cons");

    for (int i = count - 1; i >= 0; i--) {
        emacs_value pos = env->make_integer(env, positions[i]);
        emacs_value args2[2] = {pos, result};
        result = env->funcall(env, cons_sym, 2, args2);
    }

    return result;
}

/*
 * ekp-c-break-lines: Core line breaking function
 *
 * Args: (string hyphenator-index line-width measure-func)
 * Returns: (breaks . total-cost) where breaks is a list
 */
static emacs_value Fekp_c_break_lines(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value *args, void *data)
{
    (void)data;

    if (!ekp_global || nargs < 4)
        return env->intern(env, "nil");

    /* Get string */
    ptrdiff_t size = 0;
    env->copy_string_contents(env, args[0], NULL, &size);
    char *text = malloc(size);
    if (!text)
        return env->intern(env, "nil");

    env->copy_string_contents(env, args[0], text, &size);
    size_t text_len = size - 1;

    /* Get hyphenator */
    intmax_t h_idx = env->extract_integer(env, args[1]);
    ekp_hyphenator_t *h = NULL;
    if (h_idx >= 0 && (size_t)h_idx < ekp_global->hyphenator_count)
        h = ekp_global->hyphenators[h_idx];

    /* Get line width */
    int32_t line_width = env->extract_integer(env, args[2]);

    /* Get measure function */
    current_env = env;
    measure_func = args[3];

    /* Create paragraph */
    ekp_paragraph_t *para = ekp_para_create(text, text_len, h, emacs_measure_string);
    free(text);

    if (!para) {
        current_env = NULL;
        measure_func = NULL;
        return env->intern(env, "nil");
    }

    /* Break lines */
    ekp_result_t *result = ekp_break_lines(para, line_width);

    current_env = NULL;
    measure_func = NULL;

    if (!result) {
        ekp_para_destroy(para);
        return env->intern(env, "nil");
    }

    /* Build result: ((breaks...) . cost) */
    emacs_value breaks_list = env->intern(env, "nil");
    emacs_value cons_sym = env->intern(env, "cons");

    for (size_t i = result->break_count; i > 0; i--) {
        emacs_value brk = env->make_integer(env, result->breaks[i - 1]);
        emacs_value args2[2] = {brk, breaks_list};
        breaks_list = env->funcall(env, cons_sym, 2, args2);
    }

    emacs_value cost = env->make_float(env, result->total_cost);
    emacs_value args2[2] = {breaks_list, cost};
    emacs_value final = env->funcall(env, cons_sym, 2, args2);

    ekp_result_destroy(result);
    ekp_para_destroy(para);

    return final;
}

/*
 * ekp-c-version: Return module version
 */
static emacs_value Fekp_c_version(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value *args, void *data)
{
    (void)nargs; (void)args; (void)data;

    char version[32];
    snprintf(version, sizeof(version), "%d.%d",
             EKP_VERSION_MAJOR, EKP_VERSION_MINOR);

    return env->make_string(env, version, strlen(version));
}

/*
 * ekp-c-thread-count: Return number of worker threads
 */
static emacs_value Fekp_c_thread_count(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value *args, void *data)
{
    (void)nargs; (void)args; (void)data;
    return env->make_integer(env, EKP_THREAD_POOL_SIZE);
}

/*
 * ekp-c-break-with-arrays: Pure DP with Elisp-provided prefix arrays
 *
 * Args: (ideal-prefix min-prefix max-prefix glue-ideals glue-shrinks glue-stretches
 *        hyphen-positions hyphen-width line-width)
 *
 * All 6 arrays must have consistent sizes:
 *   - ideal/min/max-prefix: (n+1) elements
 *   - glue-ideals/shrinks/stretches: n elements
 *
 * Returns: (breaks . total-cost) where breaks is a list of box indices.
 *
 * This is the preferred API: Elisp computes all font-dependent values,
 * C module only does pure O(n²) DP computation.
 */
static emacs_value Fekp_c_break_with_arrays(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value *args, void *data)
{
    (void)data;

    if (!ekp_global || nargs < 9)
        return env->intern(env, "nil");

    /* Get prefix array sizes (n+1 elements) */
    ptrdiff_t prefix_len = env->vec_size(env, args[0]);
    if (prefix_len <= 1)
        return env->intern(env, "nil");

    size_t n = prefix_len - 1;  /* number of boxes */

    /* Allocate arrays */
    int32_t *ideal_prefix = malloc(prefix_len * sizeof(int32_t));
    int32_t *min_prefix = malloc(prefix_len * sizeof(int32_t));
    int32_t *max_prefix = malloc(prefix_len * sizeof(int32_t));
    int32_t *glue_ideals = malloc(n * sizeof(int32_t));
    int32_t *glue_shrinks = malloc(n * sizeof(int32_t));
    int32_t *glue_stretches = malloc(n * sizeof(int32_t));

    if (!ideal_prefix || !min_prefix || !max_prefix ||
        !glue_ideals || !glue_shrinks || !glue_stretches) {
        free(ideal_prefix); free(min_prefix); free(max_prefix);
        free(glue_ideals); free(glue_shrinks); free(glue_stretches);
        return env->intern(env, "nil");
    }

    /* Extract prefix arrays */
    for (ptrdiff_t i = 0; i < prefix_len; i++) {
        ideal_prefix[i] = env->extract_integer(env, env->vec_get(env, args[0], i));
        min_prefix[i] = env->extract_integer(env, env->vec_get(env, args[1], i));
        max_prefix[i] = env->extract_integer(env, env->vec_get(env, args[2], i));
    }

    /* Extract glue arrays */
    for (size_t i = 0; i < n; i++) {
        glue_ideals[i] = env->extract_integer(env, env->vec_get(env, args[3], i));
        glue_shrinks[i] = env->extract_integer(env, env->vec_get(env, args[4], i));
        glue_stretches[i] = env->extract_integer(env, env->vec_get(env, args[5], i));
    }

    /* Get hyphen positions vector */
    ptrdiff_t hyph_count = env->vec_size(env, args[6]);
    int32_t *hyph_pos = NULL;
    if (hyph_count > 0) {
        hyph_pos = malloc(hyph_count * sizeof(int32_t));
        if (hyph_pos) {
            for (ptrdiff_t i = 0; i < hyph_count; i++) {
                hyph_pos[i] = env->extract_integer(env, env->vec_get(env, args[6], i));
            }
        }
    }

    int32_t hyph_width = env->extract_integer(env, args[7]);
    int32_t line_width = env->extract_integer(env, args[8]);

    /* Call the pure DP function */
    ekp_result_t *result = ekp_break_with_prefixes(
        ideal_prefix, min_prefix, max_prefix,
        glue_ideals, glue_shrinks, glue_stretches,
        n,
        hyph_pos, hyph_count > 0 ? (size_t)hyph_count : 0,
        hyph_width, line_width);

    free(ideal_prefix); free(min_prefix); free(max_prefix);
    free(glue_ideals); free(glue_shrinks); free(glue_stretches);
    free(hyph_pos);

    if (!result)
        return env->intern(env, "nil");

    /* Build result: ((breaks...) . cost) */
    emacs_value breaks_list = env->intern(env, "nil");
    emacs_value cons_sym = env->intern(env, "cons");

    for (size_t i = result->break_count; i > 0; i--) {
        emacs_value brk = env->make_integer(env, result->breaks[i - 1]);
        emacs_value args2[2] = {brk, breaks_list};
        breaks_list = env->funcall(env, cons_sym, 2, args2);
    }

    emacs_value cost = env->make_float(env, result->total_cost);
    emacs_value args2[2] = {breaks_list, cost};
    emacs_value final = env->funcall(env, cons_sym, 2, args2);

    ekp_result_destroy(result);

    return final;
}

/*
 * Helper to extract paragraph data from Elisp vectors
 */
static bool extract_paragraph_data(
    emacs_env *env, emacs_value *args,
    int32_t **ideal_prefix, int32_t **min_prefix, int32_t **max_prefix,
    int32_t **glue_ideals, int32_t **glue_shrinks, int32_t **glue_stretches,
    int32_t **hyph_pos, size_t *n, ptrdiff_t *hyph_count,
    int32_t *hyph_width, int32_t *line_width)
{
    ptrdiff_t prefix_len = env->vec_size(env, args[0]);
    if (prefix_len <= 1)
        return false;

    *n = prefix_len - 1;

    *ideal_prefix = malloc(prefix_len * sizeof(int32_t));
    *min_prefix = malloc(prefix_len * sizeof(int32_t));
    *max_prefix = malloc(prefix_len * sizeof(int32_t));
    *glue_ideals = malloc(*n * sizeof(int32_t));
    *glue_shrinks = malloc(*n * sizeof(int32_t));
    *glue_stretches = malloc(*n * sizeof(int32_t));

    if (!*ideal_prefix || !*min_prefix || !*max_prefix ||
        !*glue_ideals || !*glue_shrinks || !*glue_stretches) {
        free(*ideal_prefix); free(*min_prefix); free(*max_prefix);
        free(*glue_ideals); free(*glue_shrinks); free(*glue_stretches);
        return false;
    }

    for (ptrdiff_t i = 0; i < prefix_len; i++) {
        (*ideal_prefix)[i] = env->extract_integer(env, env->vec_get(env, args[0], i));
        (*min_prefix)[i] = env->extract_integer(env, env->vec_get(env, args[1], i));
        (*max_prefix)[i] = env->extract_integer(env, env->vec_get(env, args[2], i));
    }

    for (size_t i = 0; i < *n; i++) {
        (*glue_ideals)[i] = env->extract_integer(env, env->vec_get(env, args[3], i));
        (*glue_shrinks)[i] = env->extract_integer(env, env->vec_get(env, args[4], i));
        (*glue_stretches)[i] = env->extract_integer(env, env->vec_get(env, args[5], i));
    }

    *hyph_count = env->vec_size(env, args[6]);
    *hyph_pos = NULL;
    if (*hyph_count > 0) {
        *hyph_pos = malloc(*hyph_count * sizeof(int32_t));
        if (*hyph_pos) {
            for (ptrdiff_t i = 0; i < *hyph_count; i++) {
                (*hyph_pos)[i] = env->extract_integer(env, env->vec_get(env, args[6], i));
            }
        }
    }

    *hyph_width = env->extract_integer(env, args[7]);
    *line_width = env->extract_integer(env, args[8]);

    return true;
}

/*
 * ekp-c-break-batch: Process multiple paragraphs in parallel
 *
 * Args: vector of (ideal-prefix min-prefix max-prefix glue-ideals glue-shrinks
 *                  glue-stretches hyphen-positions hyphen-width line-width)
 *
 * Each element is a vector of 9 elements (same as ekp-c-break-with-arrays args).
 * Returns vector of (breaks . total-cost) for each paragraph.
 *
 * This is the high-performance API for processing multi-paragraph text.
 */
static emacs_value Fekp_c_break_batch(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value *args, void *data)
{
    (void)data;

    if (!ekp_global || nargs < 1)
        return env->intern(env, "nil");

    ptrdiff_t para_count = env->vec_size(env, args[0]);
    if (para_count <= 0)
        return env->intern(env, "nil");

    /* Allocate batch inputs and temporary storage */
    ekp_batch_input_t *inputs = calloc(para_count, sizeof(ekp_batch_input_t));
    int32_t **all_ideal = calloc(para_count, sizeof(int32_t *));
    int32_t **all_min = calloc(para_count, sizeof(int32_t *));
    int32_t **all_max = calloc(para_count, sizeof(int32_t *));
    int32_t **all_glue_i = calloc(para_count, sizeof(int32_t *));
    int32_t **all_glue_sh = calloc(para_count, sizeof(int32_t *));
    int32_t **all_glue_st = calloc(para_count, sizeof(int32_t *));
    int32_t **all_hyph = calloc(para_count, sizeof(int32_t *));

    if (!inputs || !all_ideal || !all_min || !all_max ||
        !all_glue_i || !all_glue_sh || !all_glue_st || !all_hyph) {
        free(inputs); free(all_ideal); free(all_min); free(all_max);
        free(all_glue_i); free(all_glue_sh); free(all_glue_st); free(all_hyph);
        return env->intern(env, "nil");
    }

    /* Extract all paragraph data */
    for (ptrdiff_t p = 0; p < para_count; p++) {
        emacs_value para_vec = env->vec_get(env, args[0], p);

        /* Extract 9 arguments from this paragraph's vector */
        emacs_value para_args[9];
        for (int i = 0; i < 9; i++) {
            para_args[i] = env->vec_get(env, para_vec, i);
        }

        size_t n;
        ptrdiff_t hyph_count;
        int32_t hyph_width, line_width;

        if (!extract_paragraph_data(env, para_args,
                                     &all_ideal[p], &all_min[p], &all_max[p],
                                     &all_glue_i[p], &all_glue_sh[p], &all_glue_st[p],
                                     &all_hyph[p], &n, &hyph_count,
                                     &hyph_width, &line_width)) {
            /* Cleanup on failure */
            for (ptrdiff_t j = 0; j < p; j++) {
                free(all_ideal[j]); free(all_min[j]); free(all_max[j]);
                free(all_glue_i[j]); free(all_glue_sh[j]); free(all_glue_st[j]);
                free(all_hyph[j]);
            }
            free(inputs); free(all_ideal); free(all_min); free(all_max);
            free(all_glue_i); free(all_glue_sh); free(all_glue_st); free(all_hyph);
            return env->intern(env, "nil");
        }

        inputs[p].ideal_prefix = all_ideal[p];
        inputs[p].min_prefix = all_min[p];
        inputs[p].max_prefix = all_max[p];
        inputs[p].glue_ideals = all_glue_i[p];
        inputs[p].glue_shrinks = all_glue_sh[p];
        inputs[p].glue_stretches = all_glue_st[p];
        inputs[p].n = n;
        inputs[p].hyphen_positions = all_hyph[p];
        inputs[p].hyphen_count = hyph_count > 0 ? (size_t)hyph_count : 0;
        inputs[p].hyphen_width = hyph_width;
        inputs[p].line_width = line_width;
    }

    /* Process all paragraphs in parallel */
    ekp_result_t **results = ekp_break_batch(inputs, para_count);

    /* Cleanup input arrays */
    for (ptrdiff_t p = 0; p < para_count; p++) {
        free(all_ideal[p]); free(all_min[p]); free(all_max[p]);
        free(all_glue_i[p]); free(all_glue_sh[p]); free(all_glue_st[p]);
        free(all_hyph[p]);
    }
    free(inputs); free(all_ideal); free(all_min); free(all_max);
    free(all_glue_i); free(all_glue_sh); free(all_glue_st); free(all_hyph);

    if (!results)
        return env->intern(env, "nil");

    /* Build result vector */
    emacs_value result_vec = env->funcall(env, env->intern(env, "make-vector"),
                                           2, (emacs_value[]){
                                               env->make_integer(env, para_count),
                                               env->intern(env, "nil")
                                           });
    emacs_value cons_sym = env->intern(env, "cons");

    for (ptrdiff_t p = 0; p < para_count; p++) {
        ekp_result_t *r = results[p];
        emacs_value entry;

        if (r) {
            /* Build (breaks . cost) */
            emacs_value breaks_list = env->intern(env, "nil");
            for (size_t i = r->break_count; i > 0; i--) {
                emacs_value brk = env->make_integer(env, r->breaks[i - 1]);
                emacs_value args2[2] = {brk, breaks_list};
                breaks_list = env->funcall(env, cons_sym, 2, args2);
            }

            emacs_value cost = env->make_float(env, r->total_cost);
            emacs_value args2[2] = {breaks_list, cost};
            entry = env->funcall(env, cons_sym, 2, args2);

            ekp_result_destroy(r);
        } else {
            entry = env->intern(env, "nil");
        }

        env->vec_set(env, result_vec, p, entry);
    }

    free(results);
    return result_vec;
}

/*
 * Helper to define functions
 */
static void defun(emacs_env *env, const char *name,
                  ptrdiff_t min_arity, ptrdiff_t max_arity,
                  emacs_value (*func)(emacs_env *, ptrdiff_t, emacs_value *, void *),
                  const char *doc)
{
    emacs_value fn = env->make_function(env, min_arity, max_arity, func, doc, NULL);
    emacs_value sym = env->intern(env, name);
    emacs_value args[2] = {sym, fn};
    env->funcall(env, env->intern(env, "fset"), 2, args);
}

/*
 * Module entry point - required by Emacs dynamic module spec
 */
int emacs_module_init(struct emacs_runtime *runtime)
{
    if (runtime->size < sizeof(*runtime))
        return 1;

    emacs_env *env = runtime->get_environment(runtime);
    if (env->size < sizeof(*env))
        return 2;

    /* Define functions */
    defun(env, "ekp-c-init", 0, 0, Fekp_c_init,
          "Initialize EKP C module with thread pool.");

    defun(env, "ekp-c-cleanup", 0, 0, Fekp_c_cleanup,
          "Cleanup EKP C module resources.");

    defun(env, "ekp-c-load-hyphenator", 1, 1, Fekp_c_load_hyphenator,
          "Load hyphenation dictionary from PATH.\n\
Returns hyphenator index or nil on failure.\n\n(fn PATH)");

    defun(env, "ekp-c-set-spacing", 9, 9, Fekp_c_set_spacing,
          "Set spacing parameters (in pixels).\n\n\
Arguments are: LWS-IDEAL LWS-STRETCH LWS-SHRINK\n\
               MWS-IDEAL MWS-STRETCH MWS-SHRINK\n\
               CWS-IDEAL CWS-STRETCH CWS-SHRINK\n\n\
LWS = Latin Word Space, MWS = Mixed, CWS = CJK.\n\n\
(fn LWS-I LWS-+ LWS-- MWS-I MWS-+ MWS-- CWS-I CWS-+ CWS--)");

    defun(env, "ekp-c-set-penalties", 4, 4, Fekp_c_set_penalties,
          "Set Knuth-Plass algorithm penalties.\n\n\
LINE-PENALTY: base penalty per line break (default 10)\n\
HYPHEN-PENALTY: penalty for hyphenated breaks (default 50)\n\
FITNESS-PENALTY: penalty for adjacent line tightness mismatch (default 100)\n\
LAST-LINE-RATIO: minimum fill ratio for last line (default 0.5)\n\n\
(fn LINE-PENALTY HYPHEN-PENALTY FITNESS-PENALTY LAST-LINE-RATIO)");

    defun(env, "ekp-c-hyphenate", 2, 2, Fekp_c_hyphenate,
          "Get hyphenation positions for WORD using HYPHENATOR-INDEX.\n\
Returns list of positions where word can be hyphenated.\n\n(fn HYPHENATOR-INDEX WORD)");

    defun(env, "ekp-c-break-lines", 4, 4, Fekp_c_break_lines,
          "Break STRING into lines of LINE-WIDTH pixels.\n\n\
Uses Knuth-Plass optimal line breaking with hyphenation.\n\
HYPHENATOR-INDEX: index from `ekp-c-load-hyphenator', or -1 for none\n\
MEASURE-FUNC: function that takes a string and returns pixel width\n\n\
Returns (BREAKS . TOTAL-COST) where BREAKS is list of break positions.\n\n\
(fn STRING HYPHENATOR-INDEX LINE-WIDTH MEASURE-FUNC)");

    defun(env, "ekp-c-break-with-arrays", 9, 9, Fekp_c_break_with_arrays,
          "Break lines using Elisp's pre-computed prefix arrays (preferred API).\n\n\
IDEAL-PREFIX: vector of ideal width prefix sums (n+1 elements)\n\
MIN-PREFIX: vector of min width prefix sums (n+1 elements)\n\
MAX-PREFIX: vector of max width prefix sums (n+1 elements)\n\
GLUE-IDEALS: vector of glue ideal widths (n elements)\n\
GLUE-SHRINKS: vector of glue shrink amounts (n elements)\n\
GLUE-STRETCHES: vector of glue stretch amounts (n elements)\n\
HYPHEN-POS: vector of hyphenable box indices (sorted)\n\
HYPHEN-WIDTH: pixel width of hyphen character\n\
LINE-WIDTH: target line width in pixels\n\n\
Returns (BREAKS . TOTAL-COST) where BREAKS is list of box indices.\n\
This API ensures C uses Elisp's font-dependent measurements.\n\n\
(fn IDEAL-PREFIX MIN-PREFIX MAX-PREFIX GLUE-IDEALS GLUE-SHRINKS GLUE-STRETCHES HYPHEN-POS HYPHEN-WIDTH LINE-WIDTH)");

    defun(env, "ekp-c-version", 0, 0, Fekp_c_version,
          "Return EKP C module version string.");

    defun(env, "ekp-c-thread-count", 0, 0, Fekp_c_thread_count,
          "Return number of worker threads in the thread pool.");

    defun(env, "ekp-c-break-batch", 1, 1, Fekp_c_break_batch,
          "Break multiple paragraphs in parallel.\n\n\
PARAGRAPHS: vector of paragraph data, each element is a vector of 9 items:\n\
  [ideal-prefix min-prefix max-prefix glue-ideals glue-shrinks\n\
   glue-stretches hyphen-positions hyphen-width line-width]\n\n\
Returns vector of (BREAKS . COST) for each paragraph.\n\
This is the high-performance API for multi-paragraph processing.\n\n\
(fn PARAGRAPHS)");

    /* Provide feature */
    emacs_value provide_args[1] = {env->intern(env, "ekp-c")};
    env->funcall(env, env->intern(env, "provide"), 1, provide_args);

    return 0;
}
