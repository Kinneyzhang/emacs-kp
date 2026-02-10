/*
 * ekp_paragraph.c - Text preprocessing and box/glue construction
 *
 * The boring part that makes everything else fast.
 * Get the data layout right, and the algorithm sings.
 */

#include "ekp_module.h"
#include <stdlib.h>
#include <string.h>

/* Box types */
#define BOX_LATIN     0
#define BOX_CJK       1
#define BOX_CJK_PUNCT 2
#define BOX_SPACE     3

/* Glue types */
#define GLUE_NONE 0
#define GLUE_LWS  1  /* Latin word space */
#define GLUE_MWS  2  /* Mixed (Latin-CJK) */
#define GLUE_CWS  3  /* CJK character space */

/* UTF-8 helpers */
static inline uint32_t utf8_decode(const char *s, int *len)
{
    unsigned char c = s[0];
    *len = 1;

    if ((c & 0x80) == 0)
        return c;

    if ((c & 0xE0) == 0xC0) {
        *len = 2;
        return ((c & 0x1F) << 6) | (s[1] & 0x3F);
    }

    if ((c & 0xF0) == 0xE0) {
        *len = 3;
        return ((c & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F);
    }

    if ((c & 0xF8) == 0xF0) {
        *len = 4;
        return ((c & 0x07) << 18) | ((s[1] & 0x3F) << 12) |
               ((s[2] & 0x3F) << 6) | (s[3] & 0x3F);
    }

    return c;
}

/* Character classification */
static inline bool is_cjk(uint32_t cp)
{
    /* CJK Unified Ideographs and related blocks */
    return (cp >= 0x4E00 && cp <= 0x9FFF) ||   /* CJK Unified */
           (cp >= 0x3400 && cp <= 0x4DBF) ||   /* CJK Ext A */
           (cp >= 0x20000 && cp <= 0x2A6DF) || /* CJK Ext B */
           (cp >= 0x2A700 && cp <= 0x2B73F) || /* CJK Ext C */
           (cp >= 0x2B740 && cp <= 0x2B81F) || /* CJK Ext D */
           (cp >= 0xF900 && cp <= 0xFAFF) ||   /* CJK Compat */
           (cp >= 0x3000 && cp <= 0x303F) ||   /* CJK Symbols */
           (cp >= 0x3040 && cp <= 0x309F) ||   /* Hiragana */
           (cp >= 0x30A0 && cp <= 0x30FF) ||   /* Katakana */
           (cp >= 0xAC00 && cp <= 0xD7AF);     /* Hangul */
}

static inline bool is_cjk_punct(uint32_t cp)
{
    return (cp >= 0x3000 && cp <= 0x303F) ||   /* CJK Symbols */
           (cp >= 0xFF00 && cp <= 0xFF60) ||   /* Fullwidth Forms */
           cp == 0x201C || cp == 0x201D ||     /* " " */
           cp == 0x2018 || cp == 0x2019;       /* ' ' */
}

static inline bool is_whitespace(uint32_t cp)
{
    return cp == ' ' || cp == '\t' || cp == '\n' || cp == '\r' ||
           cp == 0x00A0 || cp == 0x3000;  /* NBSP, ideographic space */
}

/*
 * Determine box type from codepoint
 */
static uint8_t classify_char(uint32_t cp)
{
    if (is_whitespace(cp))
        return BOX_SPACE;
    if (is_cjk_punct(cp))
        return BOX_CJK_PUNCT;
    if (is_cjk(cp))
        return BOX_CJK;
    return BOX_LATIN;
}

/*
 * Determine glue type between two boxes
 */
static uint8_t glue_between(uint8_t prev_end, uint8_t curr_start)
{
    if (prev_end == BOX_SPACE || curr_start == BOX_SPACE)
        return GLUE_NONE;

    bool prev_latin = (prev_end == BOX_LATIN);
    bool curr_latin = (curr_start == BOX_LATIN);

    if (prev_latin && curr_latin)
        return GLUE_LWS;
    if (!prev_latin && !curr_latin)
        return GLUE_CWS;
    return GLUE_MWS;
}

/*
 * Split text into boxes with hyphenation
 */
ekp_paragraph_t *ekp_para_create(const char *text, size_t len,
                                  ekp_hyphenator_t *h,
                                  int32_t (*measure_fn)(const char *, size_t))
{
    if (!text || len == 0)
        return NULL;

    ekp_paragraph_t *p = calloc(1, sizeof(*p));
    if (!p)
        return NULL;

    /* Copy text */
    p->text = malloc(len + 1);
    if (!p->text) {
        free(p);
        return NULL;
    }
    memcpy(p->text, text, len);
    p->text[len] = '\0';
    p->text_len = len;

    /* Compute hash for caching */
    uint64_t hash = 14695981039346656037ULL;
    for (size_t i = 0; i < len; i++) {
        hash ^= (uint8_t)text[i];
        hash *= 1099511628211ULL;
    }
    p->hash = hash;

    /* First pass: count boxes (rough estimate) */
    size_t max_boxes = len + 1;

    /* Temporary arrays for first pass */
    size_t *box_starts = malloc(max_boxes * sizeof(size_t));
    size_t *box_lens = malloc(max_boxes * sizeof(size_t));
    uint8_t *box_types = malloc(max_boxes * sizeof(uint8_t));
    if (!box_starts || !box_lens || !box_types) {
        free(box_starts);
        free(box_lens);
        free(box_types);
        ekp_para_destroy(p);
        return NULL;
    }

    /* Tokenize into boxes */
    size_t box_count = 0;
    size_t pos = 0;
    size_t word_start = 0;
    bool in_latin_word = false;

    while (pos < len) {
        int char_len;
        uint32_t cp = utf8_decode(text + pos, &char_len);
        uint8_t type = classify_char(cp);

        if (in_latin_word) {
            if (type != BOX_LATIN) {
                /* End Latin word */
                box_starts[box_count] = word_start;
                box_lens[box_count] = pos - word_start;
                box_types[box_count] = BOX_LATIN;
                box_count++;
                in_latin_word = false;
            }
        }

        if (type == BOX_LATIN) {
            if (!in_latin_word) {
                word_start = pos;
                in_latin_word = true;
            }
        } else {
            /* Non-Latin: each character is its own box */
            box_starts[box_count] = pos;
            box_lens[box_count] = char_len;
            box_types[box_count] = type;
            box_count++;
        }

        pos += char_len;
    }

    /* Flush final Latin word */
    if (in_latin_word) {
        box_starts[box_count] = word_start;
        box_lens[box_count] = pos - word_start;
        box_types[box_count] = BOX_LATIN;
        box_count++;
    }

    /* Hyphenation: expand Latin words */
    size_t *hyphen_pos = malloc(max_boxes * sizeof(size_t));
    size_t hyphen_count = 0;

    /* Estimate expanded size */
    size_t expanded_boxes = box_count * 2;
    ekp_box_t *boxes = calloc(expanded_boxes, sizeof(ekp_box_t));
    if (!boxes || !hyphen_pos) {
        free(box_starts);
        free(box_lens);
        free(box_types);
        free(hyphen_pos);
        free(boxes);
        ekp_para_destroy(p);
        return NULL;
    }

    size_t final_count = 0;

    for (size_t i = 0; i < box_count; i++) {
        const char *box_text = text + box_starts[i];
        size_t box_len = box_lens[i];
        uint8_t type = box_types[i];

        if (type == BOX_LATIN && h && box_len > 4) {
            /* Try hyphenation */
            int8_t positions[EKP_MAX_WORD_LEN];
            int pos_count = ekp_hyphen_word(h, box_text, box_len,
                                            positions, EKP_MAX_WORD_LEN);

            if (pos_count > 0) {
                /* Split at hyphenation points */
                size_t prev_split = 0;
                for (int j = 0; j < pos_count; j++) {
                    size_t split = positions[j];
                    if (split <= prev_split || split >= box_len)
                        continue;

                    boxes[final_count].text = box_text + prev_split;
                    boxes[final_count].text_len = split - prev_split;
                    boxes[final_count].box_type = BOX_LATIN;
                    boxes[final_count].start_type = BOX_LATIN;
                    boxes[final_count].end_type = BOX_LATIN;
                    boxes[final_count].pixel_width =
                        measure_fn ? measure_fn(boxes[final_count].text,
                                                boxes[final_count].text_len) : 0;

                    hyphen_pos[hyphen_count++] = final_count;
                    final_count++;
                    prev_split = split;
                }

                /* Final segment */
                if (prev_split < box_len) {
                    boxes[final_count].text = box_text + prev_split;
                    boxes[final_count].text_len = box_len - prev_split;
                    boxes[final_count].box_type = BOX_LATIN;
                    boxes[final_count].start_type = BOX_LATIN;
                    boxes[final_count].end_type = BOX_LATIN;
                    boxes[final_count].pixel_width =
                        measure_fn ? measure_fn(boxes[final_count].text,
                                                boxes[final_count].text_len) : 0;
                    final_count++;
                }
                continue;
            }
        }

        /* No hyphenation */
        boxes[final_count].text = box_text;
        boxes[final_count].text_len = box_len;
        boxes[final_count].box_type = type;
        boxes[final_count].start_type = type;
        boxes[final_count].end_type = type;
        boxes[final_count].pixel_width =
            measure_fn ? measure_fn(box_text, box_len) : 0;
        final_count++;
    }

    free(box_starts);
    free(box_lens);
    free(box_types);

    /* Build final arrays */
    p->boxes = boxes;
    p->box_count = final_count;

    /* Hyphenation positions */
    p->hyphen_positions = malloc(hyphen_count * sizeof(int32_t));
    if (p->hyphen_positions) {
        for (size_t i = 0; i < hyphen_count; i++) {
            p->hyphen_positions[i] = hyphen_pos[i];
        }
        p->hyphen_count = hyphen_count;
    }
    free(hyphen_pos);

    /* Hyphen width */
    p->hyphen_width = measure_fn ? measure_fn("-", 1) : 5;

    /* Build glues */
    p->glues = calloc(final_count, sizeof(ekp_glue_t));
    if (!p->glues) {
        ekp_para_destroy(p);
        return NULL;
    }

    if (!ekp_global) {
        ekp_para_destroy(p);
        return NULL;
    }

    ekp_spacing_t *sp = &ekp_global->spacing;

    for (size_t i = 0; i < final_count; i++) {
        /* Check if after hyphenation point */
        bool after_hyphen = false;
        for (size_t j = 0; j < p->hyphen_count; j++) {
            if ((size_t)(p->hyphen_positions[j] + 1) == i) {
                after_hyphen = true;
                break;
            }
        }

        if (after_hyphen || i == 0) {
            p->glues[i].type = GLUE_NONE;
            continue;
        }

        uint8_t prev_end = boxes[i - 1].end_type;
        uint8_t curr_start = boxes[i].start_type;
        uint8_t gtype = glue_between(prev_end, curr_start);

        p->glues[i].type = gtype;
        switch (gtype) {
        case GLUE_LWS:
            p->glues[i].ideal = sp->lws_ideal;
            p->glues[i].stretch = sp->lws_stretch;
            p->glues[i].shrink = sp->lws_shrink;
            break;
        case GLUE_MWS:
            p->glues[i].ideal = sp->mws_ideal;
            p->glues[i].stretch = sp->mws_stretch;
            p->glues[i].shrink = sp->mws_shrink;
            break;
        case GLUE_CWS:
            p->glues[i].ideal = sp->cws_ideal;
            p->glues[i].stretch = sp->cws_stretch;
            p->glues[i].shrink = sp->cws_shrink;
            break;
        default:
            break;
        }
    }

    /* Build prefix sums for O(1) range queries */
    p->ideal_prefix = calloc(final_count + 1, sizeof(int32_t));
    p->min_prefix = calloc(final_count + 1, sizeof(int32_t));
    p->max_prefix = calloc(final_count + 1, sizeof(int32_t));

    if (!p->ideal_prefix || !p->min_prefix || !p->max_prefix) {
        ekp_para_destroy(p);
        return NULL;
    }

    for (size_t i = 0; i < final_count; i++) {
        int32_t box_w = boxes[i].pixel_width;
        int32_t glue_ideal = p->glues[i].ideal;
        int32_t glue_stretch = p->glues[i].stretch;
        int32_t glue_shrink = p->glues[i].shrink;

        p->ideal_prefix[i + 1] = p->ideal_prefix[i] + box_w + glue_ideal;
        p->min_prefix[i + 1] = p->min_prefix[i] + box_w + (glue_ideal - glue_shrink);
        p->max_prefix[i + 1] = p->max_prefix[i] + box_w + (glue_ideal + glue_stretch);
    }

    return p;
}

void ekp_para_destroy(ekp_paragraph_t *p)
{
    if (!p)
        return;
    free(p->text);
    free(p->boxes);
    free(p->glues);
    free(p->hyphen_positions);
    free(p->ideal_prefix);
    free(p->min_prefix);
    free(p->max_prefix);
    free(p);
}
