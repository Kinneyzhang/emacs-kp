/*
 * ekp.c - Emacs Knuth-Plass module entry point
 *
 * Copyright (C) 2024-2026 Kinney Zhang
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * This file is part of emacs-kp, which is free software: you can
 * redistribute it and/or modify it under the terms of the GNU General
 * Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 * It is distributed WITHOUT ANY WARRANTY; see the GNU General Public
 * License (COPYING) for details.
 *
 * This is the main entry file for the Emacs dynamic module.
 * Naming follows Emacs module convention: module name = file name.
 * (require 'ekp-c) loads ekp.so, entry point in ekp.c
 *
 * Bridges C implementation to Emacs Lisp. Keep the interface minimal:
 * pixel measurement callback from Emacs, everything else in C.
 */

#include "ekp_module.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Required for Emacs modules */
int plugin_is_GPL_compatible;

/* Public array values are signed 32-bit pixels. The DP uses wider
 * intermediates; rejecting an out-of-range API value is safer than
 * silently changing it. */
static inline int32_t clamp32(intmax_t v)
{
    if (v > INT32_MAX) return INT32_MAX;
    if (v < INT32_MIN) return INT32_MIN;
    return (int32_t)v;
}

static bool lisp_predicate(emacs_env *env, const char *name, emacs_value value)
{
    emacs_value result = env->funcall(
        env, env->intern(env, name), 1, (emacs_value[]){value});
    return env->is_not_nil(env, result);
}

static bool i32_value_p(emacs_env *env, emacs_value value)
{
    intmax_t integer = env->extract_integer(env, value);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        return false;
    }
    return integer >= INT32_MIN && integer <= INT32_MAX;
}

static bool finite_number_p(emacs_env *env, emacs_value value)
{
    if (lisp_predicate(env, "floatp", value))
        return isfinite(env->extract_float(env, value));
    return i32_value_p(env, value);
}

static emacs_value signal_invalid_input(emacs_env *env, const char *message)
{
    emacs_value text = env->make_string(env, message, strlen(message));
    emacs_value data = env->funcall(
        env, env->intern(env, "list"), 1, (emacs_value[]){text});
    env->non_local_exit_signal(
        env, env->intern(env, "ekp-c-invalid-input"), data);
    return env->intern(env, "nil");
}

static bool i32_vector_p(emacs_env *env, emacs_value vector)
{
    ptrdiff_t length = env->vec_size(env, vector);
    for (ptrdiff_t i = 0; i < length; i++) {
        if (!i32_value_p(env, env->vec_get(env, vector, i)))
            return false;
    }
    return true;
}

static ptrdiff_t paragraph_field_length(int field, ptrdiff_t prefix_len)
{
    switch (field) {
    case 0: case 1: case 2: case 9: case 10: case 12:
        return prefix_len;
    case 3: case 4: case 5:
        return prefix_len - 1;
    default:
        return -1;
    }
}

static const char *validate_paragraph_shapes(emacs_env *env, emacs_value *args)
{
    static const int vectors[] = {0, 1, 2, 3, 4, 5, 6, 9, 10, 11, 12};

    for (size_t i = 0; i < sizeof(vectors) / sizeof(vectors[0]); i++) {
        if (!lisp_predicate(env, "vectorp", args[vectors[i]]))
            return "EKP C paragraph array fields must be vectors";
    }

    ptrdiff_t prefix_len = env->vec_size(env, args[0]);
    if (prefix_len <= 1 || prefix_len > INT32_MAX)
        return "EKP C prefix vectors must contain 2..INT32_MAX elements";

    for (int field = 0; field < 15; field++) {
        ptrdiff_t expected = paragraph_field_length(field, prefix_len);
        if (expected >= 0 && env->vec_size(env, args[field]) != expected)
            return "EKP C paragraph vector lengths are inconsistent";
    }
    return NULL;
}

static const char *validate_paragraph_values(emacs_env *env, emacs_value *args)
{
    static const int vectors[] = {0, 1, 2, 3, 4, 5, 6, 9, 10, 11, 12};
    static const int scalars[] = {7, 8, 13, 14};

    for (size_t i = 0; i < sizeof(vectors) / sizeof(vectors[0]); i++) {
        if (!i32_vector_p(env, args[vectors[i]]))
            return "EKP C paragraph vectors require signed 32-bit integers";
    }
    for (size_t i = 0; i < sizeof(scalars) / sizeof(scalars[0]); i++) {
        if (!i32_value_p(env, args[scalars[i]]))
            return "EKP C paragraph scalars require signed 32-bit integers";
    }

    if (env->extract_integer(env, args[8]) <= 0)
        return "EKP C line width must be positive";
    if (env->extract_integer(env, args[7]) < 0 ||
        env->extract_integer(env, args[13]) < 0)
        return "EKP C hyphen width and protrusion must be nonnegative";
    return NULL;
}

static const char *validate_paragraph(emacs_env *env, emacs_value *args)
{
    const char *error = validate_paragraph_shapes(env, args);
    return error ? error : validate_paragraph_values(env, args);
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
 * ekp-c-set-penalties: Set K-P parameters
 */
typedef struct {
    int32_t line;
    int32_t hyphen;
    int32_t fitness;
    double last_ratio;
    int32_t consecutive;
    double last_short;
    int32_t extra_stretch;
    int32_t emergency_stretch;
} penalty_config_t;

static const char *parse_penalties(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value *args, penalty_config_t *out)
{
    if (!i32_value_p(env, args[0]) || !i32_value_p(env, args[1]) ||
        !i32_value_p(env, args[2]) || !finite_number_p(env, args[3]) ||
        (nargs > 4 && !i32_value_p(env, args[4])) ||
        (nargs > 5 && !finite_number_p(env, args[5])) ||
        (nargs > 6 && !i32_value_p(env, args[6])) ||
        (nargs > 7 && !i32_value_p(env, args[7])))
        return "EKP C penalties require finite signed 32-bit numbers";

    out->line = clamp32(env->extract_integer(env, args[0]));
    out->hyphen = clamp32(env->extract_integer(env, args[1]));
    out->fitness = clamp32(env->extract_integer(env, args[2]));
    out->last_ratio = lisp_predicate(env, "floatp", args[3])
        ? env->extract_float(env, args[3])
        : (double)env->extract_integer(env, args[3]);
    if (nargs > 4)
        out->consecutive = clamp32(env->extract_integer(env, args[4]));
    if (nargs > 5)
        out->last_short = lisp_predicate(env, "floatp", args[5])
            ? env->extract_float(env, args[5])
            : (double)env->extract_integer(env, args[5]);
    out->extra_stretch = nargs > 6
        ? clamp32(env->extract_integer(env, args[6])) : 0;
    out->emergency_stretch = nargs > 7
        ? clamp32(env->extract_integer(env, args[7])) : 0;

    if (out->last_ratio < 0.0 || out->last_ratio > 1.0 ||
        out->last_short < 0.0 || out->extra_stretch < 0 ||
        out->emergency_stretch < 0)
        return "EKP C ratios and stretch values are outside valid ranges";
    return NULL;
}

static emacs_value Fekp_c_set_penalties(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value *args, void *data)
{
    (void)data;

    if (!ekp_global || nargs < 4)
        return env->intern(env, "nil");

    penalty_config_t config = {
        .consecutive = ekp_global->consec_hyphen_penalty,
        .last_short = ekp_global->last_line_short_penalty
    };
    const char *error = parse_penalties(env, nargs, args, &config);
    if (error)
        return signal_invalid_input(env, error);

    ekp_global->line_penalty = config.line;
    ekp_global->hyphen_penalty = config.hyphen;
    ekp_global->fitness_penalty = config.fitness;
    ekp_global->last_line_ratio = config.last_ratio;
    ekp_global->consec_hyphen_penalty = config.consecutive;
    ekp_global->last_line_short_penalty = config.last_short;
    ekp_global->extra_stretch = config.extra_stretch;
    ekp_global->emergency_stretch = config.emergency_stretch;

    return env->intern(env, "t");
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
    /* Pool is created lazily; report its actual size once it exists,
     * else the size it will get. */
    if (ekp_global && ekp_global->pool)
        return env->make_integer(env, (intmax_t)ekp_global->pool->thread_count);
    return env->make_integer(env, (intmax_t)ekp_pool_default_threads());
}

/*
 * ekp-c-break-with-arrays: Pure DP with Elisp-provided prefix arrays
 *
 * Args: (ideal-prefix min-prefix max-prefix glue-ideals glue-shrinks
 *        glue-stretches hyphen-positions hyphen-width line-width
 *        lead-spaces trail-spaces forbidden-positions tail-protrudes
 *        hyphen-protrude first-line-width)
 *
 * Array sizes must be consistent:
 *   - ideal/min/max-prefix, lead/trail-spaces, tail-protrudes: n+1
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

    if (!ekp_global || nargs < 15)
        return env->intern(env, "nil");

    const char *validation_error = validate_paragraph(env, args);
    if (validation_error)
        return signal_invalid_input(env, validation_error);

    /* Get prefix array sizes (n+1 elements) */
    ptrdiff_t prefix_len = env->vec_size(env, args[0]);

    size_t n = prefix_len - 1;  /* number of boxes */

    /* Allocate arrays */
    int32_t *ideal_prefix = malloc(prefix_len * sizeof(int32_t));
    int32_t *min_prefix = malloc(prefix_len * sizeof(int32_t));
    int32_t *max_prefix = malloc(prefix_len * sizeof(int32_t));
    int32_t *glue_ideals = malloc(n * sizeof(int32_t));
    int32_t *glue_shrinks = malloc(n * sizeof(int32_t));
    int32_t *glue_stretches = malloc(n * sizeof(int32_t));
    int32_t *lead_spaces = malloc(prefix_len * sizeof(int32_t));
    int32_t *trail_spaces = malloc(prefix_len * sizeof(int32_t));

    if (!ideal_prefix || !min_prefix || !max_prefix ||
        !glue_ideals || !glue_shrinks || !glue_stretches ||
        !lead_spaces || !trail_spaces) {
        free(ideal_prefix); free(min_prefix); free(max_prefix);
        free(glue_ideals); free(glue_shrinks); free(glue_stretches);
        free(lead_spaces); free(trail_spaces);
        return env->intern(env, "nil");
    }

    /* Extract prefix arrays */
    for (ptrdiff_t i = 0; i < prefix_len; i++) {
        ideal_prefix[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[0], i)));
        min_prefix[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[1], i)));
        max_prefix[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[2], i)));
        lead_spaces[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[9], i)));
        trail_spaces[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[10], i)));
    }

    /* Extract glue arrays */
    for (size_t i = 0; i < n; i++) {
        glue_ideals[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[3], i)));
        glue_shrinks[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[4], i)));
        glue_stretches[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[5], i)));
    }

    /* Get hyphen positions vector */
    ptrdiff_t hyph_count = env->vec_size(env, args[6]);
    int32_t *hyph_pos = NULL;
    if (hyph_count > 0) {
        hyph_pos = malloc(hyph_count * sizeof(int32_t));
        if (hyph_pos) {
            for (ptrdiff_t i = 0; i < hyph_count; i++) {
                hyph_pos[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[6], i)));
            }
        }
    }

    int32_t hyph_width = clamp32(env->extract_integer(env, args[7]));
    int32_t line_width = clamp32(env->extract_integer(env, args[8]));

    /* Right-edge protrusion: per-gap array (n+1) and hyphen scalar */
    int32_t *tail_pro = malloc(prefix_len * sizeof(int32_t));
    if (tail_pro) {
        for (ptrdiff_t i = 0; i < prefix_len; i++) {
            tail_pro[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[12], i)));
        }
    }
    int32_t hyphen_protrude = clamp32(env->extract_integer(env, args[13]));
    int32_t first_line_width = clamp32(env->extract_integer(env, args[14]));

    /* Forbidden break positions (sorted gap indices, may be empty) */
    ptrdiff_t forb_count = env->vec_size(env, args[11]);
    int32_t *forb_pos = NULL;
    if (forb_count > 0) {
        forb_pos = malloc(forb_count * sizeof(int32_t));
        if (forb_pos) {
            for (ptrdiff_t i = 0; i < forb_count; i++) {
                forb_pos[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[11], i)));
            }
        }
    }

    /* One consolidated gate: any partial allocation above (silent
     * "no kinsoku / no hyphenation" degradation) or a pending Lisp
     * signal from a bad element type must fail the whole call — the
     * Elisp engine is the correct fallback, not a subtly different
     * layout. */
    if ((hyph_count > 0 && !hyph_pos) ||
        (forb_count > 0 && !forb_pos) ||
        !tail_pro ||
        env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        free(ideal_prefix); free(min_prefix); free(max_prefix);
        free(glue_ideals); free(glue_shrinks); free(glue_stretches);
        free(lead_spaces); free(trail_spaces);
        free(hyph_pos); free(forb_pos); free(tail_pro);
        return env->intern(env, "nil");
    }

    /* Call the pure DP function */
    ekp_result_t *result = ekp_break_with_prefixes(
        ideal_prefix, min_prefix, max_prefix,
        glue_ideals, glue_shrinks, glue_stretches,
        n,
        hyph_pos, hyph_count > 0 ? (size_t)hyph_count : 0,
        hyph_width, line_width,
        lead_spaces, trail_spaces,
        forb_pos, (forb_pos && forb_count > 0) ? (size_t)forb_count : 0,
        tail_pro, hyphen_protrude, first_line_width);

    free(ideal_prefix); free(min_prefix); free(max_prefix);
    free(glue_ideals); free(glue_shrinks); free(glue_stretches);
    free(lead_spaces); free(trail_spaces);
    free(hyph_pos);
    free(forb_pos);
    free(tail_pro);

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
    int32_t *hyph_width, int32_t *line_width,
    int32_t **lead_spaces, int32_t **trail_spaces,
    int32_t **forb_pos, ptrdiff_t *forb_count,
    int32_t **tail_pro, int32_t *hyphen_protrude,
    int32_t *first_line_width)
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
    *lead_spaces = malloc(prefix_len * sizeof(int32_t));
    *trail_spaces = malloc(prefix_len * sizeof(int32_t));

    if (!*ideal_prefix || !*min_prefix || !*max_prefix ||
        !*glue_ideals || !*glue_shrinks || !*glue_stretches ||
        !*lead_spaces || !*trail_spaces) {
        free(*ideal_prefix); free(*min_prefix); free(*max_prefix);
        free(*glue_ideals); free(*glue_shrinks); free(*glue_stretches);
        free(*lead_spaces); free(*trail_spaces);
        return false;
    }

    for (ptrdiff_t i = 0; i < prefix_len; i++) {
        (*ideal_prefix)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[0], i)));
        (*min_prefix)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[1], i)));
        (*max_prefix)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[2], i)));
        (*lead_spaces)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[9], i)));
        (*trail_spaces)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[10], i)));
    }

    for (size_t i = 0; i < *n; i++) {
        (*glue_ideals)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[3], i)));
        (*glue_shrinks)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[4], i)));
        (*glue_stretches)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[5], i)));
    }

    *hyph_count = env->vec_size(env, args[6]);
    *hyph_pos = NULL;
    if (*hyph_count > 0) {
        *hyph_pos = malloc(*hyph_count * sizeof(int32_t));
        if (*hyph_pos) {
            for (ptrdiff_t i = 0; i < *hyph_count; i++) {
                (*hyph_pos)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[6], i)));
            }
        }
    }

    *hyph_width = clamp32(env->extract_integer(env, args[7]));
    *line_width = clamp32(env->extract_integer(env, args[8]));

    *forb_count = env->vec_size(env, args[11]);
    *forb_pos = NULL;
    if (*forb_count > 0) {
        *forb_pos = malloc(*forb_count * sizeof(int32_t));
        if (*forb_pos) {
            for (ptrdiff_t i = 0; i < *forb_count; i++) {
                (*forb_pos)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[11], i)));
            }
        }
    }

    *tail_pro = malloc(prefix_len * sizeof(int32_t));
    if (*tail_pro) {
        for (ptrdiff_t i = 0; i < prefix_len; i++) {
            (*tail_pro)[i] = clamp32(env->extract_integer(env, env->vec_get(env, args[12], i)));
        }
    }
    *hyphen_protrude = clamp32(env->extract_integer(env, args[13]));
    *first_line_width = clamp32(env->extract_integer(env, args[14]));

    if ((*hyph_count > 0 && !*hyph_pos) ||
        (*forb_count > 0 && !*forb_pos) ||
        !*tail_pro ||
        env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        free(*ideal_prefix); free(*min_prefix); free(*max_prefix);
        free(*glue_ideals); free(*glue_shrinks); free(*glue_stretches);
        free(*lead_spaces); free(*trail_spaces);
        free(*hyph_pos); free(*forb_pos); free(*tail_pro);
        *ideal_prefix = *min_prefix = *max_prefix = NULL;
        *glue_ideals = *glue_shrinks = *glue_stretches = NULL;
        *lead_spaces = *trail_spaces = NULL;
        *hyph_pos = *forb_pos = *tail_pro = NULL;
        return false;
    }

    return true;
}

/*
 * ekp-c-break-batch: Process multiple paragraphs in parallel
 *
 * Args: vector of (ideal-prefix min-prefix max-prefix glue-ideals glue-shrinks
 *                  glue-stretches hyphen-positions hyphen-width line-width
 *                  lead-spaces trail-spaces forbidden-positions tail-protrudes
 *                  hyphen-protrude first-line-width)
 *
 * Each element is a vector of 15 elements (same as
 * ekp-c-break-with-arrays args).
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

    if (!lisp_predicate(env, "vectorp", args[0]))
        return signal_invalid_input(
            env, "EKP C batch input must be a vector");

    ptrdiff_t para_count = env->vec_size(env, args[0]);
    if (para_count <= 0)
        return env->intern(env, "nil");

    /* Validate the complete batch before allocating or extracting any
     * paragraph, so malformed input cannot leave a partial batch. */
    for (ptrdiff_t p = 0; p < para_count; p++) {
        emacs_value para_vec = env->vec_get(env, args[0], p);
        if (!lisp_predicate(env, "vectorp", para_vec) ||
            env->vec_size(env, para_vec) != 15)
            return signal_invalid_input(
                env, "Each EKP C batch paragraph must contain 15 fields");

        emacs_value para_args[15];
        for (int i = 0; i < 15; i++)
            para_args[i] = env->vec_get(env, para_vec, i);
        const char *validation_error = validate_paragraph(env, para_args);
        if (validation_error)
            return signal_invalid_input(env, validation_error);
    }

    /* Allocate batch inputs and temporary storage */
    ekp_batch_input_t *inputs = calloc(para_count, sizeof(ekp_batch_input_t));
    int32_t **all_ideal = calloc(para_count, sizeof(int32_t *));
    int32_t **all_min = calloc(para_count, sizeof(int32_t *));
    int32_t **all_max = calloc(para_count, sizeof(int32_t *));
    int32_t **all_glue_i = calloc(para_count, sizeof(int32_t *));
    int32_t **all_glue_sh = calloc(para_count, sizeof(int32_t *));
    int32_t **all_glue_st = calloc(para_count, sizeof(int32_t *));
    int32_t **all_hyph = calloc(para_count, sizeof(int32_t *));
    int32_t **all_lead = calloc(para_count, sizeof(int32_t *));
    int32_t **all_trail = calloc(para_count, sizeof(int32_t *));
    int32_t **all_forb = calloc(para_count, sizeof(int32_t *));
    int32_t **all_pro = calloc(para_count, sizeof(int32_t *));

    if (!inputs || !all_ideal || !all_min || !all_max ||
        !all_glue_i || !all_glue_sh || !all_glue_st || !all_hyph ||
        !all_lead || !all_trail || !all_forb || !all_pro) {
        free(inputs); free(all_ideal); free(all_min); free(all_max);
        free(all_glue_i); free(all_glue_sh); free(all_glue_st); free(all_hyph);
        free(all_lead); free(all_trail); free(all_forb); free(all_pro);
        return env->intern(env, "nil");
    }

    /* Extract all paragraph data */
    for (ptrdiff_t p = 0; p < para_count; p++) {
        emacs_value para_vec = env->vec_get(env, args[0], p);

        /* Extract 15 arguments from this paragraph's vector */
        emacs_value para_args[15];
        for (int i = 0; i < 15; i++) {
            para_args[i] = env->vec_get(env, para_vec, i);
        }

        size_t n;
        ptrdiff_t hyph_count, forb_count;
        int32_t hyph_width, line_width, hyphen_protrude, first_line_width;

        if (!extract_paragraph_data(env, para_args,
                                     &all_ideal[p], &all_min[p], &all_max[p],
                                     &all_glue_i[p], &all_glue_sh[p], &all_glue_st[p],
                                     &all_hyph[p], &n, &hyph_count,
                                     &hyph_width, &line_width,
                                     &all_lead[p], &all_trail[p],
                                     &all_forb[p], &forb_count,
                                     &all_pro[p], &hyphen_protrude,
                                     &first_line_width)) {
            /* Cleanup on failure */
            for (ptrdiff_t j = 0; j < p; j++) {
                free(all_ideal[j]); free(all_min[j]); free(all_max[j]);
                free(all_glue_i[j]); free(all_glue_sh[j]); free(all_glue_st[j]);
                free(all_hyph[j]); free(all_lead[j]); free(all_trail[j]);
                free(all_forb[j]); free(all_pro[j]);
            }
            free(inputs); free(all_ideal); free(all_min); free(all_max);
            free(all_glue_i); free(all_glue_sh); free(all_glue_st); free(all_hyph);
            free(all_lead); free(all_trail); free(all_forb); free(all_pro);
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
        inputs[p].lead_spaces = all_lead[p];
        inputs[p].trail_spaces = all_trail[p];
        inputs[p].forbidden_positions = all_forb[p];
        inputs[p].forbidden_count =
            (all_forb[p] && forb_count > 0) ? (size_t)forb_count : 0;
        inputs[p].tail_protrudes = all_pro[p];
        inputs[p].hyphen_protrude = hyphen_protrude;
        inputs[p].first_line_width = first_line_width;
    }

    /* Process all paragraphs in parallel */
    ekp_result_t **results = ekp_break_batch(inputs, para_count);

    /* Cleanup input arrays */
    for (ptrdiff_t p = 0; p < para_count; p++) {
        free(all_ideal[p]); free(all_min[p]); free(all_max[p]);
        free(all_glue_i[p]); free(all_glue_sh[p]); free(all_glue_st[p]);
        free(all_hyph[p]); free(all_lead[p]); free(all_trail[p]);
        free(all_forb[p]); free(all_pro[p]);
    }
    free(inputs); free(all_ideal); free(all_min); free(all_max);
    free(all_glue_i); free(all_glue_sh); free(all_glue_st); free(all_hyph);
    free(all_lead); free(all_trail); free(all_forb); free(all_pro);

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
    if ((size_t)runtime->size < sizeof(*runtime))
        return 1;

    emacs_env *env = runtime->get_environment(runtime);
    if ((size_t)env->size < sizeof(*env))
        return 2;

    emacs_value error_name = env->intern(env, "ekp-c-invalid-input");
    emacs_value error_message = env->make_string(
        env, "Invalid EKP C module input", 26);
    env->funcall(env, env->intern(env, "define-error"), 2,
                 (emacs_value[]){error_name, error_message});

    /* Define functions */
    defun(env, "ekp-c-init", 0, 0, Fekp_c_init,
          "Initialize EKP C module with thread pool.");

    defun(env, "ekp-c-cleanup", 0, 0, Fekp_c_cleanup,
          "Cleanup EKP C module resources.");

    defun(env, "ekp-c-set-penalties", 4, 8, Fekp_c_set_penalties,
          "Set Knuth-Plass algorithm penalties.\n\n\
LINE-PENALTY: base penalty per line break (default 10)\n\
HYPHEN-PENALTY: penalty for hyphenated breaks (default 50)\n\
FITNESS-PENALTY: penalty for adjacent line tightness mismatch (default 100)\n\
LAST-LINE-RATIO: minimum fill ratio for last line (default 0.5)\n\
CONSEC-HYPHEN-PENALTY: multiplier for consecutive hyphen runs (default 100)\n\
LAST-LINE-SHORT-PENALTY: multiplier for short last lines (default 50.0)\n\
EXTRA-STRETCH: per-line non-justify flexibility in pixels (default 0)\n\
EMERGENCY-STRETCH: fixed final-pass emergency stretch in pixels (default 0)\n\n\
(fn LINE-PENALTY HYPHEN-PENALTY FITNESS-PENALTY LAST-LINE-RATIO \
&optional CONSEC-HYPHEN-PENALTY LAST-LINE-SHORT-PENALTY EXTRA-STRETCH \
EMERGENCY-STRETCH)");

    defun(env, "ekp-c-break-with-arrays", 15, 15, Fekp_c_break_with_arrays,
          "Break lines using Elisp's pre-computed prefix arrays (preferred API).\n\n\
IDEAL-PREFIX: vector of ideal width prefix sums (n+1 elements)\n\
MIN-PREFIX: vector of min width prefix sums (n+1 elements)\n\
MAX-PREFIX: vector of max width prefix sums (n+1 elements)\n\
GLUE-IDEALS: vector of glue ideal widths (n elements)\n\
GLUE-SHRINKS: vector of glue shrink amounts (n elements)\n\
GLUE-STRETCHES: vector of glue stretch amounts (n elements)\n\
HYPHEN-POS: vector of hyphenable box indices (sorted)\n\
HYPHEN-WIDTH: pixel width of hyphen character\n\
LINE-WIDTH: target line width in pixels\n\
LEAD-SPACES: vector (n+1) of space-box run widths starting at box i\n\
TRAIL-SPACES: vector (n+1) of space-box run widths ending at box k-1\n\
FORBIDDEN-POS: vector of gap indices where breaking is forbidden (sorted)\n\
TAIL-PROTRUDES: vector (n+1) of right-edge protrusion pixels per gap\n\
HYPHEN-PROTRUDE: protrusion pixels for the soft hyphen\n\
FIRST-LINE-WIDTH: width of line 0 (first-line indent); <=0 = LINE-WIDTH\n\n\
Returns (BREAKS . TOTAL-COST) where BREAKS is list of box indices.\n\
This API ensures C uses Elisp's font-dependent measurements.\n\n\
(fn IDEAL-PREFIX MIN-PREFIX MAX-PREFIX GLUE-IDEALS GLUE-SHRINKS GLUE-STRETCHES \
HYPHEN-POS HYPHEN-WIDTH LINE-WIDTH LEAD-SPACES TRAIL-SPACES FORBIDDEN-POS \
TAIL-PROTRUDES HYPHEN-PROTRUDE FIRST-LINE-WIDTH)");

    defun(env, "ekp-c-version", 0, 0, Fekp_c_version,
          "Return EKP C module version string.");

    defun(env, "ekp-c-thread-count", 0, 0, Fekp_c_thread_count,
          "Return number of worker threads in the thread pool.");

    defun(env, "ekp-c-break-batch", 1, 1, Fekp_c_break_batch,
          "Break multiple paragraphs in parallel.\n\n\
PARAGRAPHS: vector of paragraph data, each element a vector of the\n\
same 15 items `ekp-c-break-with-arrays' takes, in the same order.\n\n\
Returns vector of (BREAKS . COST) for each paragraph.\n\
This is the high-performance API for multi-paragraph processing.\n\n\
(fn PARAGRAPHS)");

    /* Provide feature */
    emacs_value provide_args[1] = {env->intern(env, "ekp-c")};
    env->funcall(env, env->intern(env, "provide"), 1, provide_args);

    return 0;
}
