/*
 * ekp_hyphen.c - Liang hyphenation algorithm implementation
 *
 * Fast, thread-safe hyphenation with pattern caching.
 * Uses FNV-1a hash for O(1) pattern lookup.
 */

#include "ekp_module.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* FNV-1a hash constants */
#define FNV_OFFSET 14695981039346656037ULL
#define FNV_PRIME  1099511628211ULL

static uint64_t fnv1a_hash(const char *data, size_t len)
{
    uint64_t hash = FNV_OFFSET;
    for (size_t i = 0; i < len; i++) {
        hash ^= (uint8_t)data[i];
        hash *= FNV_PRIME;
    }
    return hash;
}

/*
 * Parse a pattern like "hy3ph" into letters and values.
 * Returns true on success.
 */
static bool parse_pattern(const char *pat, ekp_pattern_t *out)
{
    size_t pat_len = strlen(pat);
    if (pat_len == 0 || pat_len >= EKP_MAX_PATTERN_LEN * 2)
        return false;

    size_t pos = 0;
    size_t letter_idx = 0;
    size_t value_idx = 0;

    memset(out->values, 0, sizeof(out->values));
    memset(out->letters, 0, sizeof(out->letters));

    while (pos < pat_len) {
        /* Read optional digit */
        uint8_t digit = 0;
        if (isdigit((unsigned char)pat[pos])) {
            digit = pat[pos] - '0';
            pos++;
        }
        out->values[value_idx++] = digit;

        /* Read letter if present */
        if (pos < pat_len && !isdigit((unsigned char)pat[pos])) {
            out->letters[letter_idx++] = pat[pos];
            pos++;
        }
    }

    out->len = letter_idx;

    /* Find non-zero range */
    size_t start = 0, end = value_idx;
    while (start < end && out->values[start] == 0) start++;
    while (end > start && out->values[end - 1] == 0) end--;

    out->offset = start;

    /* Shift values to start */
    if (start > 0) {
        memmove(out->values, out->values + start, end - start);
        memset(out->values + (end - start), 0, start);
    }

    return letter_idx > 0;
}

/*
 * Load patterns from .dic file
 */
ekp_hyphenator_t *ekp_hyphen_create(const char *dict_path)
{
    FILE *fp = fopen(dict_path, "r");
    if (!fp)
        return NULL;

    ekp_hyphenator_t *h = calloc(1, sizeof(*h));
    if (!h) {
        fclose(fp);
        return NULL;
    }

    pthread_rwlock_init(&h->lock, NULL);
    h->left_min = 2;
    h->right_min = 2;

    /* First pass: count patterns */
    char line[256];
    size_t count = 0;

    fgets(line, sizeof(line), fp);  /* skip encoding line */

    while (fgets(line, sizeof(line), fp)) {
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n')
            line[--len] = '\0';

        /* Skip empty, comments, HYPHENMIN, patterns with / */
        if (len == 0 || line[0] == '%' || line[0] == '#')
            continue;
        if (strstr(line, "HYPHENMIN") || strchr(line, '/'))
            continue;

        count++;
    }

    /* Allocate patterns */
    h->patterns = calloc(count, sizeof(ekp_pattern_t));
    h->hash_size = count * 2;  /* load factor 0.5 */
    h->hash_table = calloc(h->hash_size, sizeof(uint32_t));

    if (!h->patterns || !h->hash_table) {
        ekp_hyphen_destroy(h);
        fclose(fp);
        return NULL;
    }

    /* Second pass: parse patterns */
    rewind(fp);
    fgets(line, sizeof(line), fp);  /* skip encoding line */

    size_t idx = 0;
    while (fgets(line, sizeof(line), fp)) {
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n')
            line[--len] = '\0';

        if (len == 0 || line[0] == '%' || line[0] == '#')
            continue;
        if (strstr(line, "HYPHENMIN") || strchr(line, '/'))
            continue;

        /* Handle ^^XX hex escapes */
        char decoded[256];
        char *dst = decoded;
        const char *src = line;
        while (*src) {
            if (src[0] == '^' && src[1] == '^' &&
                isxdigit((unsigned char)src[2]) &&
                isxdigit((unsigned char)src[3])) {
                char hex[3] = {src[2], src[3], 0};
                *dst++ = (char)strtol(hex, NULL, 16);
                src += 4;
            } else {
                *dst++ = *src++;
            }
        }
        *dst = '\0';

        if (parse_pattern(decoded, &h->patterns[idx])) {
            /* Insert into hash table */
            uint64_t hash = fnv1a_hash(h->patterns[idx].letters,
                                       h->patterns[idx].len);
            size_t slot = hash % h->hash_size;

            while (h->hash_table[slot] != 0) {
                slot = (slot + 1) % h->hash_size;
            }
            h->hash_table[slot] = idx + 1;  /* 1-indexed */

            if (h->patterns[idx].len > h->max_pattern_len)
                h->max_pattern_len = h->patterns[idx].len;

            idx++;
        }
    }

    h->pattern_count = idx;
    fclose(fp);
    return h;
}

void ekp_hyphen_destroy(ekp_hyphenator_t *h)
{
    if (!h) return;
    pthread_rwlock_destroy(&h->lock);
    free(h->patterns);
    free(h->hash_table);
    free(h);
}

/*
 * Find pattern by letters (hash table lookup)
 */
static ekp_pattern_t *find_pattern(ekp_hyphenator_t *h,
                                    const char *letters, size_t len)
{
    if (len == 0 || len > h->max_pattern_len)
        return NULL;

    uint64_t hash = fnv1a_hash(letters, len);
    size_t slot = hash % h->hash_size;

    for (size_t i = 0; i < h->hash_size; i++) {
        uint32_t idx = h->hash_table[slot];
        if (idx == 0)
            return NULL;

        ekp_pattern_t *p = &h->patterns[idx - 1];
        if (p->len == len && memcmp(p->letters, letters, len) == 0)
            return p;

        slot = (slot + 1) % h->hash_size;
    }
    return NULL;
}

/*
 * Compute hyphenation positions for a word
 * Thread-safe (read lock)
 */
int ekp_hyphen_word(ekp_hyphenator_t *h, const char *word, size_t len,
                    int8_t *positions, size_t max_pos)
{
    if (!h || !word || len == 0 || len > EKP_MAX_WORD_LEN - 2)
        return 0;

    /* Check cache first */
    uint64_t word_hash = fnv1a_hash(word, len);
    size_t cache_slot = word_hash % EKP_CACHE_SIZE;

    pthread_rwlock_rdlock(&h->lock);

    if (h->cache[cache_slot].hash == word_hash &&
        strncmp(h->cache[cache_slot].word, word, len) == 0) {
        int count = h->cache[cache_slot].pos_count;
        if (count <= (int)max_pos) {
            memcpy(positions, h->cache[cache_slot].positions,
                   count * sizeof(int8_t));
        }
        pthread_rwlock_unlock(&h->lock);
        return count;
    }

    pthread_rwlock_unlock(&h->lock);

    /* Compute hyphenation */
    char padded[EKP_MAX_WORD_LEN + 2];
    padded[0] = '.';
    for (size_t i = 0; i < len; i++)
        padded[i + 1] = tolower((unsigned char)word[i]);
    padded[len + 1] = '.';
    size_t padded_len = len + 2;

    uint8_t prio[EKP_MAX_WORD_LEN + 3];
    memset(prio, 0, sizeof(prio));

    /* Apply matching patterns */
    pthread_rwlock_rdlock(&h->lock);

    for (size_t i = 0; i < padded_len - 1; i++) {
        for (size_t j = i + 1; j <= padded_len && j <= i + h->max_pattern_len; j++) {
            ekp_pattern_t *pat = find_pattern(h, padded + i, j - i);
            if (pat) {
                size_t val_len = pat->len + 1 - pat->offset;
                for (size_t k = 0; k < val_len && k < sizeof(pat->values); k++) {
                    size_t pos = i + pat->offset + k;
                    if (pos < sizeof(prio) && pat->values[k] > prio[pos])
                        prio[pos] = pat->values[k];
                }
            }
        }
    }

    pthread_rwlock_unlock(&h->lock);

    /* Collect odd positions (subtract 1 for padding offset) */
    int8_t result[EKP_MAX_WORD_LEN];
    int count = 0;

    for (size_t i = 1; i < padded_len && count < EKP_MAX_WORD_LEN; i++) {
        if (prio[i] & 1) {  /* odd = break allowed */
            int pos = (int)i - 1;  /* adjust for leading '.' */
            /* Apply margin constraints */
            if (pos >= h->left_min && pos <= (int)len - h->right_min) {
                result[count++] = pos;
            }
        }
    }

    /* Update cache */
    pthread_rwlock_wrlock(&h->lock);

    h->cache[cache_slot].hash = word_hash;
    strncpy(h->cache[cache_slot].word, word, len);
    h->cache[cache_slot].word[len] = '\0';
    memcpy(h->cache[cache_slot].positions, result, count * sizeof(int8_t));
    h->cache[cache_slot].pos_count = count;

    pthread_rwlock_unlock(&h->lock);

    /* Copy to output */
    int out_count = count < (int)max_pos ? count : (int)max_pos;
    memcpy(positions, result, out_count * sizeof(int8_t));
    return out_count;
}
