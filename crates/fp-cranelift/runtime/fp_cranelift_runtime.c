#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

typedef enum {
    FP_VAL_I64 = 0,
    FP_VAL_U64 = 1,
    FP_VAL_F64 = 2,
    FP_VAL_BOOL = 3,
    FP_VAL_CHAR = 4,
    FP_VAL_PTR = 5
} FpValueTag;

typedef struct {
    unsigned char tag;
    unsigned char pad[7];
    union {
        long long i64;
        unsigned long long u64;
        double f64;
        unsigned long long ptr;
    } data;
} FpValue;

static void append_str(char **buf, size_t *len, size_t *cap, const char *text) {
    size_t text_len = strlen(text);
    if (*len + text_len + 1 > *cap) {
        size_t new_cap = (*cap == 0) ? 64 : *cap;
        while (*len + text_len + 1 > new_cap) {
            new_cap *= 2;
        }
        char *next = (char *)realloc(*buf, new_cap);
        if (!next) {
            return;
        }
        *buf = next;
        *cap = new_cap;
    }
    memcpy(*buf + *len, text, text_len);
    *len += text_len;
    (*buf)[*len] = '\0';
}

static void append_formatted(char **buf, size_t *len, size_t *cap, const char *spec, FpValue val) {
    char scratch[256];
    int written = 0;
    switch (val.tag) {
        case FP_VAL_I64:
            written = snprintf(scratch, sizeof(scratch), spec, val.data.i64);
            break;
        case FP_VAL_U64:
            written = snprintf(scratch, sizeof(scratch), spec, val.data.u64);
            break;
        case FP_VAL_F64:
            written = snprintf(scratch, sizeof(scratch), spec, val.data.f64);
            break;
        case FP_VAL_BOOL:
            written = snprintf(scratch, sizeof(scratch), spec, (int)val.data.i64);
            break;
        case FP_VAL_CHAR:
            written = snprintf(scratch, sizeof(scratch), spec, (int)val.data.i64);
            break;
        case FP_VAL_PTR:
            written = snprintf(scratch, sizeof(scratch), spec, (const char *)(uintptr_t)val.data.ptr);
            break;
        default:
            written = snprintf(scratch, sizeof(scratch), "%s", "<unknown>");
            break;
    }
    if (written <= 0) {
        return;
    }
    if ((size_t)written < sizeof(scratch)) {
        append_str(buf, len, cap, scratch);
        return;
    }
    char *dyn = (char *)malloc((size_t)written + 1);
    if (!dyn) {
        return;
    }
    switch (val.tag) {
        case FP_VAL_I64:
            snprintf(dyn, (size_t)written + 1, spec, val.data.i64);
            break;
        case FP_VAL_U64:
            snprintf(dyn, (size_t)written + 1, spec, val.data.u64);
            break;
        case FP_VAL_F64:
            snprintf(dyn, (size_t)written + 1, spec, val.data.f64);
            break;
        case FP_VAL_BOOL:
            snprintf(dyn, (size_t)written + 1, spec, (int)val.data.i64);
            break;
        case FP_VAL_CHAR:
            snprintf(dyn, (size_t)written + 1, spec, (int)val.data.i64);
            break;
        case FP_VAL_PTR:
            snprintf(dyn, (size_t)written + 1, spec, (const char *)(uintptr_t)val.data.ptr);
            break;
        default:
            snprintf(dyn, (size_t)written + 1, "%s", "<unknown>");
            break;
    }
    append_str(buf, len, cap, dyn);
    free(dyn);
}

static char *format_with_args(const char *fmt, const FpValue *args, size_t len, int append_newline) {
    char *out = NULL;
    size_t out_len = 0;
    size_t out_cap = 0;
    size_t arg_idx = 0;
    const char *cursor = fmt;

    while (*cursor) {
        if (*cursor != '%') {
            char literal[2] = { *cursor, '\0' };
            append_str(&out, &out_len, &out_cap, literal);
            cursor++;
            continue;
        }

        if (cursor[1] == '%') {
            append_str(&out, &out_len, &out_cap, "%");
            cursor += 2;
            continue;
        }

        const char *start = cursor;
        cursor++;
        while (*cursor && !((*cursor >= 'a' && *cursor <= 'z') || (*cursor >= 'A' && *cursor <= 'Z'))) {
            cursor++;
        }
        if (!*cursor) {
            append_str(&out, &out_len, &out_cap, "%");
            break;
        }
        cursor++;
        size_t spec_len = (size_t)(cursor - start);
        char *spec = (char *)malloc(spec_len + 1);
        if (!spec) {
            break;
        }
        memcpy(spec, start, spec_len);
        spec[spec_len] = '\0';
        FpValue val = {0};
        if (arg_idx < len) {
            val = args[arg_idx];
        }
        append_formatted(&out, &out_len, &out_cap, spec, val);
        arg_idx++;
        free(spec);
    }

    if (append_newline) {
        append_str(&out, &out_len, &out_cap, "\n");
    }

    if (!out) {
        out = (char *)malloc(1);
        if (out) {
            out[0] = '\0';
        }
    }
    return out;
}

char *fp_cranelift_format(const char *fmt, const FpValue *args, size_t len) {
    return format_with_args(fmt, args, len, 0);
}

void fp_cranelift_print(const char *fmt, const FpValue *args, size_t len, unsigned char newline) {
    char *text = format_with_args(fmt, args, len, newline ? 1 : 0);
    if (!text) {
        return;
    }
    fputs(text, stdout);
    fflush(stdout);
    free(text);
}

double fp_cranelift_time_now(void) {
    time_t now = time(NULL);
    return (double)now;
}
