/*
 * ekp_module.h - Emacs Knuth-Plass dynamic module
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
 * Core data structures and API declarations.
 * Design principle: Data structures are the code. Get them right,
 * and the rest writes itself. -- Linus Torvalds
 */

#ifndef EKP_MODULE_H
#define EKP_MODULE_H

#include <emacs-module.h>
#include <stdint.h>
#include <stdbool.h>
#include <pthread.h>
#include <math.h>

/* Version */
#define EKP_VERSION_MAJOR 1
#define EKP_VERSION_MINOR 6

/* Limits */
#define EKP_THREAD_POOL_MAX 64

/* Infinity for impossible breaks */
#define EKP_INFINITY HUGE_VAL  /* unreachable sentinel */

/*
 * Line break result
 */
typedef struct {
    int32_t *breaks;        /* break positions */
    size_t break_count;
    int64_t *rest_pixels;   /* remaining space per line */
    double total_cost;
} ekp_result_t;

/*
 * Thread pool for parallel computation
 */
typedef struct {
    pthread_t threads[EKP_THREAD_POOL_MAX];
    size_t thread_count;
    pthread_mutex_t queue_lock;
    pthread_cond_t queue_cond;
    pthread_cond_t done_cond;

    struct {
        void (*func)(void *);
        void *arg;
    } *queue;
    size_t queue_size;
    size_t queue_head;
    size_t queue_tail;
    size_t active_count;

    bool shutdown;
} ekp_thread_pool_t;

/*
 * Global state
 */
typedef struct {
    ekp_thread_pool_t *pool;  /* created lazily on first batch */

    /* K-P parameters */
    int line_penalty;
    int hyphen_penalty;
    int fitness_penalty;
    double last_line_ratio;
    int consec_hyphen_penalty;      /* multiplier for consecutive hyphens */
    double last_line_short_penalty; /* multiplier for short last lines */
    int32_t extra_stretch;          /* per-line flexibility for non-justify
                                     * alignment (0 = justify) */
    int32_t emergency_stretch;      /* fixed final-pass emergency stretch */
} ekp_state_t;

/* Global state instance */
extern ekp_state_t *ekp_global;

/*
 * API: Hyphenation
 */

/*
 * API: Paragraph processing
 */

/*
 * API: Line breaking (the main algorithm)
 */
void ekp_result_destroy(ekp_result_t *r);

/*
 * API: Pure DP with pre-computed prefix arrays (for Elisp integration)
 *
 * This is the preferred API when Elisp has already computed everything.
 * Elisp does: tokenization, width measurement, glue computation, prefix sums.
 * C module only does: O(n²) DP computation.
 *
 * ideal_prefix, min_prefix, max_prefix: prefix sum arrays (n+1 elements)
 * glue_ideals, glue_stretches, glue_shrinks: per-box glue values (n elements)
 * hyphen_positions: sorted array of hyphenable box indices
 * hyphen_count: length of hyphen_positions
 * hyphen_width: pixel width of hyphen character
 * line_width: target line width in pixels
 * lead_spaces: (n+1 elements, nullable) width of the space-box run
 *              starting at box i; entry 0 must be 0 (indentation kept)
 * trail_spaces: (n+1 elements, nullable) width of the space-box run
 *               ending at box k-1
 * forbidden_positions: sorted array of gap indices where a line may
 *               NOT end (kinsoku, no-break spans); nullable
 * forbidden_count: length of forbidden_positions
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
    int32_t line_width,
    const int32_t *lead_spaces,
    const int32_t *trail_spaces,
    const int32_t *forbidden_positions,
    size_t forbidden_count,
    const int32_t *tail_protrudes,
    int32_t hyphen_protrude,
    int32_t first_line_width);

/*
 * Batch input for parallel processing
 */
typedef struct {
    const int32_t *ideal_prefix;
    const int32_t *min_prefix;
    const int32_t *max_prefix;
    const int32_t *glue_ideals;
    const int32_t *glue_shrinks;
    const int32_t *glue_stretches;
    size_t n;
    const int32_t *hyphen_positions;
    size_t hyphen_count;
    int32_t hyphen_width;
    int32_t line_width;
    const int32_t *lead_spaces;   /* nullable, n+1 elements */
    const int32_t *trail_spaces;  /* nullable, n+1 elements */
    const int32_t *forbidden_positions;  /* nullable, sorted gap indices */
    size_t forbidden_count;
    const int32_t *tail_protrudes;  /* nullable, n+1 elements */
    int32_t hyphen_protrude;
    int32_t first_line_width;  /* width of line 0; <=0 = line_width */
} ekp_batch_input_t;

/*
 * API: Batch line breaking (parallel across paragraphs)
 *
 * Processes multiple paragraphs concurrently using the thread pool.
 * This is the correct parallelization granularity - paragraphs are
 * independent, so no synchronization overhead.
 *
 * Returns array of results (caller must free each result and the array).
 */
ekp_result_t **ekp_break_batch(
    ekp_batch_input_t *inputs,
    size_t count);

/*
 * API: Thread pool
 */
size_t ekp_pool_default_threads(void);
ekp_thread_pool_t *ekp_pool_create(size_t num_threads);
void ekp_pool_destroy(ekp_thread_pool_t *pool);
void ekp_pool_submit(ekp_thread_pool_t *pool, void (*func)(void *), void *arg);
void ekp_pool_wait(ekp_thread_pool_t *pool);

/*
 * API: Initialization
 */
int ekp_init(void);
void ekp_cleanup(void);

/*
 * Emacs module interface
 */
int emacs_module_init(struct emacs_runtime *runtime);

#endif /* EKP_MODULE_H */
