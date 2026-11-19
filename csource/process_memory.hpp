#pragma once
#if defined(__APPLE__) || defined(__linux__)
#include <sys/resource.h>
#endif

// Process high-water RSS includes startup and parsing. A rise between two
// readings places a new process high-water mark within that interval.
inline long long highWaterRSS() {
#if defined(__APPLE__) || defined(__linux__)
    rusage usage{};
    if (getrusage(RUSAGE_SELF, &usage) == 0) {
#ifdef __APPLE__
        return usage.ru_maxrss;
#else
        return usage.ru_maxrss * 1024LL;
#endif
    }
#endif
    return -1;
}
