/*
 * ekp_thread_pool.c - Work-stealing thread pool
 *
 * Simple but effective: fixed thread count, lock-free queue would be
 * overkill for our batch sizes. Keep it simple, stupid.
 */

#include "ekp_module.h"
#include <stdlib.h>
#include <string.h>

#define QUEUE_CAPACITY 1024

static void *worker_thread(void *arg)
{
    ekp_thread_pool_t *pool = (ekp_thread_pool_t *)arg;

    while (1) {
        pthread_mutex_lock(&pool->queue_lock);

        /* Wait for work */
        while (pool->queue_head == pool->queue_tail && !pool->shutdown) {
            pthread_cond_wait(&pool->queue_cond, &pool->queue_lock);
        }

        if (pool->shutdown && pool->queue_head == pool->queue_tail) {
            pthread_mutex_unlock(&pool->queue_lock);
            break;
        }

        /* Dequeue work */
        void (*func)(void *) = pool->queue[pool->queue_head].func;
        void *work_arg = pool->queue[pool->queue_head].arg;
        pool->queue_head = (pool->queue_head + 1) % pool->queue_size;
        pool->active_count++;

        pthread_mutex_unlock(&pool->queue_lock);

        /* Execute */
        if (func)
            func(work_arg);

        /* Mark done */
        pthread_mutex_lock(&pool->queue_lock);
        pool->active_count--;
        if (pool->active_count == 0 && pool->queue_head == pool->queue_tail) {
            pthread_cond_signal(&pool->done_cond);
        }
        pthread_mutex_unlock(&pool->queue_lock);
    }

    return NULL;
}

ekp_thread_pool_t *ekp_pool_create(size_t num_threads)
{
    if (num_threads == 0)
        num_threads = EKP_THREAD_POOL_SIZE;
    if (num_threads > EKP_THREAD_POOL_SIZE)
        num_threads = EKP_THREAD_POOL_SIZE;

    ekp_thread_pool_t *pool = calloc(1, sizeof(*pool));
    if (!pool)
        return NULL;

    pool->queue_size = QUEUE_CAPACITY;
    pool->queue = calloc(pool->queue_size, sizeof(pool->queue[0]));
    if (!pool->queue) {
        free(pool);
        return NULL;
    }

    pthread_mutex_init(&pool->queue_lock, NULL);
    pthread_cond_init(&pool->queue_cond, NULL);
    pthread_cond_init(&pool->done_cond, NULL);

    /* Start worker threads */
    for (size_t i = 0; i < num_threads; i++) {
        if (pthread_create(&pool->threads[i], NULL, worker_thread, pool) != 0) {
            /* Cleanup on failure */
            pool->shutdown = true;
            pthread_cond_broadcast(&pool->queue_cond);
            for (size_t j = 0; j < i; j++) {
                pthread_join(pool->threads[j], NULL);
            }
            pthread_mutex_destroy(&pool->queue_lock);
            pthread_cond_destroy(&pool->queue_cond);
            pthread_cond_destroy(&pool->done_cond);
            free(pool->queue);
            free(pool);
            return NULL;
        }
    }

    pool->thread_count = num_threads;
    return pool;
}

void ekp_pool_destroy(ekp_thread_pool_t *pool)
{
    if (!pool)
        return;

    pthread_mutex_lock(&pool->queue_lock);
    pool->shutdown = true;
    pthread_cond_broadcast(&pool->queue_cond);
    pthread_mutex_unlock(&pool->queue_lock);

    for (size_t i = 0; i < pool->thread_count; i++) {
        pthread_join(pool->threads[i], NULL);
    }

    pthread_mutex_destroy(&pool->queue_lock);
    pthread_cond_destroy(&pool->queue_cond);
    pthread_cond_destroy(&pool->done_cond);
    free(pool->queue);
    free(pool);
}

void ekp_pool_submit(ekp_thread_pool_t *pool, void (*func)(void *), void *arg)
{
    if (!pool || !func)
        return;

    pthread_mutex_lock(&pool->queue_lock);

    size_t next_tail = (pool->queue_tail + 1) % pool->queue_size;

    /* Queue full - drop task (shouldn't happen with proper sizing) */
    if (next_tail == pool->queue_head) {
        pthread_mutex_unlock(&pool->queue_lock);
        return;
    }

    pool->queue[pool->queue_tail].func = func;
    pool->queue[pool->queue_tail].arg = arg;
    pool->queue_tail = next_tail;

    pthread_cond_signal(&pool->queue_cond);
    pthread_mutex_unlock(&pool->queue_lock);
}

void ekp_pool_wait(ekp_thread_pool_t *pool)
{
    if (!pool)
        return;

    pthread_mutex_lock(&pool->queue_lock);
    while (pool->active_count > 0 || pool->queue_head != pool->queue_tail) {
        pthread_cond_wait(&pool->done_cond, &pool->queue_lock);
    }
    pthread_mutex_unlock(&pool->queue_lock);
}
