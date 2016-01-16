# BKTGA [![travis-ci.org][badge.travis]][travis] [![Build status][badge.appveyor]][appveyor] [![codecov.io][badge.codecov]][codecov]

A c++14 header only TGA file parsing and conversion library.

## Overview

A minimal, but fully TGA 2.0 standard compliant parser.

## Examples

```cpp
#include <iostream>
#include "bktga.hpp"

int main() {
    constexpr char filename[] = "./test/8-bit-rle.tga";

    // Check if filename represents what looks like a valid TGA file.
    // std::pair<optional, string_view>
    auto const result = bktga::check_file(filename);
    if (!result.first) {
        std::cerr << "Error: " << result.second.to_string() << std::endl;
        return 1;
    }

    // bktga::tga_descriptor
    auto const& tga = *result.first;

    // Convert the seemingly valid TGA to a flat buffer of ABGR data.
    // std::vector<uint32_t>
    auto converted = bktga::decode(tga, filename);

    return 0;
}

```

## Compiler Support
Other compiler + standard library version combinations might work, but the following have been tested.

+ :o: MSVC 2015 Update 1 (_MSC_FULLVER 190023506)
+ :o: GCC 4.9
+ :x: GCC 4.8 (no digit separators; lack of standard library features in libstdc++)
+ :o: Clang 3.6
+ :heavy_exclamation_mark: Clang 3.5 (with GCC 4.9, or with libc++)
+ :o: Xcode 6.4

<!-- Links -->
[travis]: https://travis-ci.org/bkentel/bktga?branch=master
[codecov]: https://codecov.io/github/bkentel/bktga?branch=master
[appveyor]: https://ci.appveyor.com/project/bkentel/bktga/branch/master
[badge.travis]: https://travis-ci.org/bkentel/bktga.svg?branch=master
[badge.codecov]: https://codecov.io/github/bkentel/bktga/coverage.svg?branch=master
[badge.appveyor]: https://ci.appveyor.com/api/projects/status/k0bsdcmy98scppjm/branch/master?svg=true