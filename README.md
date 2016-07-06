# BKTGA [![travis-ci.org][badge.travis]][travis] [![Build status][badge.appveyor]][appveyor] [![codecov.io][badge.codecov]][codecov]

A c++14 header only TGA file parsing and conversion library.

## Overview

A minimal, but fully TGA 2.0 standard compliant parser.

## Examples

```cpp
#include <bktga/tga.hpp>
#include <vector>
#include <iostream>

int main() {
    namespace tga = ::bktga;

    std::vector<std::string> const files {
        "./test/tc-rgb16a1-128x128-rle.tga"
      , "./tga.hpp"
      , "./non-existant-file.none"
    };

    for (auto const& file : files) {
        auto result = tga::detect(tga::read_from_file, file);
        if (!result) {
            std::cerr << "Cannot open \"" << file << "\" as TGA: " << result.error() << '\n';
            continue;
        }

        auto const data = tga::decode(result);
        std::cout << "Okay" << std::endl;
        // use data...
    }

    return 0;
}
```
```
Okay
Cannot open "./tga.hpp" as TGA: unknown reserved image type
Cannot open "./non-existant-file.none" as TGA: The system cannot find the file specified.
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