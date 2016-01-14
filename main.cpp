//#include "tga.hpp"

//int main() {
//    constexpr char filename[] = R"(D:\Users\Brandon\Downloads\256 Colors (RLE).tga)";
//
//    auto file_result = bktga::check_file(filename);
//
//    if (file_result.first) {
//        auto const result = bktga::decode(*file_result.first, std::ifstream {filename, std::ios::binary});
//    } else {
//
//    }
//
//
//    //std::ofstream out{R"(./out.raw)", std::ios::binary};
//    //out.write(reinterpret_cast<char const*>(result.data()),
//    //          result.size() * sizeof(uint32_t));
//
//    return 0;
//}
