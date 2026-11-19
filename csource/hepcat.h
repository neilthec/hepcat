#ifndef HEPCAT_REDUCE_H
#define HEPCAT_REDUCE_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct hepcat_amp hepcat_amp;
typedef struct {
    const int *legs;
    size_t leg_count;
    const char *mass;
} hepcat_shell;

/* FullForm syntax. Legs start at 1. Use NULL, 0 for no internal shells. */
hepcat_amp *hepcat_create(const char *expression, size_t particle_count,
                          const char *const *masses, const hepcat_shell *shells,
                          size_t shell_count);
void hepcat_destroy(hepcat_amp *amp);
int reduce(hepcat_amp *amp); /* in place; 1 on success, 0 on error */
char *hepcat_format(const hepcat_amp *amp);
/* NULL and empty error when there is no discarded remainder. */
char *hepcat_format_remainder(const hepcat_amp *amp);
void hepcat_free_string(char *text);
/* The last error on this thread, valid until the next operation. */
const char *hepcat_error(void);

#ifdef __cplusplus
}
#endif
#endif
