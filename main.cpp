#include "tga.hpp"

int main() {
    std::ifstream in {R"(D:\Users\Brandon\Documents\GitHub\old\tez\data\dungeon.tga)", std::ios::binary};

    auto const hdr = bktga::header {in};
    auto const img = bktga::image {hdr, in};
    auto const result = img.transform();

    return 0;
}