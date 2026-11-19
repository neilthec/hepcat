#include "hepcat.h"
#include <string.h>

int main(void) {
    const char *masses[] = {"0","0","0","0"};
    const int legs[] = {1,2};
    const hepcat_shell shell = {legs,2,"2"};
    hepcat_amp *amp = hepcat_create("MomProd[1,2]",4,masses,&shell,1);
    if (!amp || !reduce(amp)) return 1;
    char *value = hepcat_format(amp);
    char *remainder = hepcat_format_remainder(amp);
    int passed = value && strcmp(value,"2")==0 && remainder && strstr(remainder,"MomProd");
    hepcat_free_string(value);
    hepcat_free_string(remainder);
    hepcat_destroy(amp);
    return passed ? 0 : 1;
}
