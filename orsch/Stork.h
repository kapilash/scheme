#pragma once
#ifdef _WIN32
#ifdef STORK_DLL
#ifdef STORK_LIBRARY_EXPORT
#define STORK_EXPORT __declspec(dllexport)
#else
#define STORK_EXPORT __declspec(dllimport)
#endif
#else
#define STORK_EXPORT
#endif
#else
#define STORK_EXPORT
#endif

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <curl/curl.h>
#include <glog/logging.h>
#include <chrono>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <uv.h>


namespace Stork{
    const std::string LibraryDirEnvVar {"STORKLIBDIR"};
    struct Configuration {
        std::string library {"stork.scm"};
        std::string libraryDir;
        const uint64_t initialSegmentSize{32768};
        const uint64_t tokenLength {1024};
        const uint64_t maxHandles {64};
        const uint64_t hashThreshold{5};
        const uint64_t callTrace{100};
        const uint64_t defaultLimitKn {12392};
        const int64_t intSegLimit{1000000000000000000};
    };
    typedef std::ptrdiff_t Cell;
}
