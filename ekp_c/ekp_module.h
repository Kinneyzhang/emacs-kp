/*
 * ekp_module.h - Emacs Knuth-Plass dynamic module
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

/* Version */
#define EKP_VERSION_MAJOR 1
#define EKP_VERSION_MINOR 0

/* Limits */
#define EKP_MAX_PATTERN_LEN 64
#define EKP_MAX_WORD_LEN 256
#define EKP_CACHE_SIZE 4096
#define EKP_THREAD_POOL_SIZE 8

/* Infinity for impossible breaks */
#define EKP_INFINITY 1e10

/*
 * Box: indivisible content with fixed width
 * Keep it small - we'll have thousands of these
 */
typedef struct {
    const char *text;       /* UTF-8 string, NOT owned */
    int32_t text_len;       /* byte length */
    int32_t pixel_width;    /* rendered width in pixels */
    uint8_t box_type;       /* 0=latin, 1=cjk, 2=cjk_punct, 3=space */
    uint8_t start_type;     /* first char type */
    uint8_t end_type;       /* last char type */
} ekp_box_t;

/*
 * Glue: flexible space between boxes
 * The heart of Knuth-Plass: ideal ± stretch/shrink
 */
typedef struct {
    int16_t ideal;          /* natural width */
    int16_t stretch;        /* max stretch */
    int16_t shrink;         /* max shrink */
    uint8_t type;           /* 0=none, 1=lws, 2=mws, 3=cws */
} ekp_glue_t;

/*
 * Breakpoint candidate for DP
 */
typedef struct {
    int32_t index;          /* box index */
    int32_t prev;           /* previous breakpoint index */
    double demerits;        /* accumulated demerits */
    int32_t line_count;     /* lines so far */
    uint8_t fitness;        /* 0-3: tight to very-loose */
    uint8_t hyphen_count;   /* consecutive hyphens */
    bool is_hyphen;         /* ends with hyphen? */
} ekp_breakpoint_t;

/*
 * Hyphenation pattern (Liang's algorithm)
 * Compact representation: letters + priority values
 */
typedef struct {
    char letters[EKP_MAX_PATTERN_LEN];
    uint8_t values[EKP_MAX_PATTERN_LEN + 1];
    uint8_t len;
    uint8_t offset;         /* where values start */
} ekp_pattern_t;

/*
 * Hyphenator: compiled patterns + cache
 * Thread-safe with read-write lock
 */
typedef struct {
    ekp_pattern_t *patterns;
    size_t pattern_count;
    size_t max_pattern_len;

    /* Hash table for O(1) pattern lookup */
    uint32_t *hash_table;
    size_t hash_size;

    /* Word cache (LRU) */
    struct {
        uint64_t hash;
        char word[EKP_MAX_WORD_LEN];
        int8_t positions[EKP_MAX_WORD_LEN];
        int pos_count;
    } cache[EKP_CACHE_SIZE];
    size_t cache_head;

    pthread_rwlock_t lock;

    /* Margin constraints */
    int left_min;
    int right_min;
} ekp_hyphenator_t;

/*
 * Paragraph: preprocessed text ready for line breaking
 * All arrays are parallel: boxes[i] has glues[i], widths[i], etc.
 */
typedef struct {
    ekp_box_t *boxes;
    ekp_glue_t *glues;
    size_t box_count;

    /* Prefix sums for O(1) range queries */
    int32_t *ideal_prefix;
    int32_t *min_prefix;
    int32_t *max_prefix;

    /* Hyphenation data */
    int32_t *hyphen_positions;
    size_t hyphen_count;
    int32_t hyphen_width;

    /* Original string (owned) */
    char *text;
    size_t text_len;

    /* Hash for cache lookup */
    uint64_t hash;
} ekp_paragraph_t;

/*
 * Line break result
 */
typedef struct {
    int32_t *breaks;        /* break positions */
    size_t break_count;
    int32_t *rest_pixels;   /* remaining space per line */
    double total_cost;
} ekp_result_t;

/*
 * Global spacing parameters
 */
typedef struct {
    int16_t lws_ideal, lws_stretch, lws_shrink;
    int16_t mws_ideal, mws_stretch, mws_shrink;
    int16_t cws_ideal, cws_stretch, cws_shrink;
} ekp_spacing_t;

/*
 * Thread pool for parallel computation
 */
typedef struct {
    pthread_t threads[EKP_THREAD_POOL_SIZE];
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
    ekp_hyphenator_t *hyphenators[32];  /* by language */
    size_t hyphenator_count;

    ekp_paragraph_t **para_cache;
    size_t para_cache_size;
    pthread_mutex_t cache_lock;

    ekp_spacing_t spacing;
    ekp_thread_pool_t *pool;

    /* K-P parameters */
    int line_penalty;
    int hyphen_penalty;
    int fitness_penalty;
    double last_line_ratio;
} ekp_state_t;

/* Global state instance */
extern ekp_state_t *ekp_global;

/*
 * API: Hyphenation
 */
ekp_hyphenator_t *ekp_hyphen_create(const char *dict_path);
void ekp_hyphen_destroy(ekp_hyphenator_t *h);
int ekp_hyphen_word(ekp_hyphenator_t *h, const char *word, size_t len,
                    int8_t *positions, size_t max_pos);

/*
 * API: Paragraph processing
 */
ekp_paragraph_t *ekp_para_create(const char *text, size_t len,
                                  ekp_hyphenator_t *h,
                                  int32_t (*measure_fn)(const char *, size_t));
void ekp_para_destroy(ekp_paragraph_t *p);

/*
 * API: Line breaking (the main algorithm)
 */
ekp_result_t *ekp_break_lines(ekp_paragraph_t *p, int32_t line_width);
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
    int32_t line_width);

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
