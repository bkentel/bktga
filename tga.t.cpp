#include "tga.hpp"

#if defined(_MSC_VER)
#   pragma warning(push)
#   pragma warning(disable : 4571)
#   pragma warning(disable : 4625)
#   pragma warning(disable : 4626)
#   pragma warning(disable : 5026)
#   pragma warning(disable : 5027)
#elif defined(__clang__)
#   pragma clang diagnostic push
#   pragma clang diagnostic ignored "-Wexit-time-destructors"
#   pragma clang diagnostic ignored "-Wmissing-braces"
#elif defined __GNUC__
#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wparentheses"
#endif

#if defined(_MSC_VER) && _MSC_FULL_VER >= 190023506
#   define CATCH_CONFIG_CPP11_NULLPTR
#   define CATCH_CONFIG_CPP11_NOEXCEPT
#   define CATCH_CONFIG_CPP11_GENERATED_METHODS
#   define CATCH_CONFIG_CPP11_IS_ENUM
#   define CATCH_CONFIG_CPP11_TUPLE
#   define CATCH_CONFIG_CPP11_LONG_LONG
#   define CATCH_CONFIG_CPP11_OVERRIDE
#   define CATCH_CONFIG_CPP11_UNIQUE_PTR
#   define CATCH_CONFIG_CPP11_OR_GREATER
#   define CATCH_CONFIG_VARIADIC_MACROS
#endif
#include <Catch/catch.hpp>

#include <algorithm>
#include <array>
#include <chrono>

namespace detail = bktga::detail;

namespace {
char const test_file_8bit_rle[] = "./test/8-bit-rle.tga";
char const test_file_4_color[]  = "./test/4-color.tga";

std::array<const char*, 2> const test_files {
    test_file_8bit_rle
  , test_file_4_color
};

template <typename Container>
bktga::unique_file fill_temp_file(Container const& c) noexcept {
    using std::begin;
    using std::end;
    using std::data;
    using std::size;

    auto temp_file = bktga::unique_file {std::tmpfile(), fclose};
    REQUIRE(temp_file);

    using value_t = std::decay_t<decltype(*begin(c))>;
    static_assert(std::is_pod<value_t> {}, "");

    fwrite(data(c), sizeof(value_t), size(c), temp_file.get());
    fseek(temp_file.get(), 0, SEEK_SET);

    return temp_file;
}

}

TEST_CASE("round_up_bits_to_bytes", "[utility]") {
    using bktga::detail::round_up_bits_to_bytes;

    REQUIRE(round_up_bits_to_bytes(-1) == 0);
    REQUIRE(round_up_bits_to_bytes( 0) == 0);
    REQUIRE(round_up_bits_to_bytes( 1) == 1);
    REQUIRE(round_up_bits_to_bytes( 8) == 1);
    REQUIRE(round_up_bits_to_bytes( 9) == 2);
    REQUIRE(round_up_bits_to_bytes(16) == 2);
    REQUIRE(round_up_bits_to_bytes(17) == 3);
    REQUIRE(round_up_bits_to_bytes(24) == 3);
    REQUIRE(round_up_bits_to_bytes(25) == 4);
    REQUIRE(round_up_bits_to_bytes(32) == 4);
}

TEST_CASE("buffer", "[utility]") {
    using bktga::detail::buffer;

    auto const check = [](auto const size) {
        buffer const b {size};
        REQUIRE(b.size()  == static_cast<ptrdiff_t>(size));
        REQUIRE(b.begin() == b.data());
        REQUIRE(b.end()   == (b.data() + size));
    };

    check(100);
    check(size_t {100});
    check(ptrdiff_t {100});

    REQUIRE(buffer { 0}.data() == nullptr);
    REQUIRE(buffer {-1}.data() == nullptr);
}

TEST_CASE("min_0", "[utility]") {
    using bktga::detail::min_0;

    REQUIRE(min_0(-1, -1) == 0);
    REQUIRE(min_0(-1,  1) == 0);
    REQUIRE(min_0( 0,  1) == 0);
    REQUIRE(min_0( 1,  0) == 0);
    REQUIRE(min_0( 1,  1) == 1);
    REQUIRE(min_0( 1,  2) == 1);
    REQUIRE(min_0( 2,  1) == 1);

    REQUIRE(min_0(-1, -1, -1) == 0);
    REQUIRE(min_0(-1,  1,  2) == 0);
}

TEST_CASE("little_endian_to_host", "[io]") {
    using bktga::detail::little_endian_to_host;

    auto const check = [](auto const n) noexcept {
        alignas (uint64_t) constexpr char const data[] {
            0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x7F
        };

        auto const m = little_endian_to_host(
            *reinterpret_cast<decltype(n) const*>(data));

        REQUIRE(m == n);
    };

    check(char     {0x11});
    check(int8_t   {0x11});
    check(int16_t  {0x2211});
    check(int32_t  {0x44332211});
    check(int64_t  {0x7F77665544332211});
    check(uint8_t  {0x11});
    check(uint16_t {0x2211});
    check(uint32_t {0x44332211});
    check(uint64_t {0x7F77665544332211});
}

TEST_CASE("to_rgba", "[io]") {
    using bktga::detail::to_rgba;

    REQUIRE(to_rgba< 8>(0xDDCCBBAA) == 0xFFAAAAAA);

    //with bit-15 set
    REQUIRE(to_rgba<15>(0xDDCCBBAA) == 0xFF73EF52);
    REQUIRE(to_rgba<16>(0xDDCCBBAA) == 0xFF73EF52);

    //with bit-15 cleared
    REQUIRE(to_rgba<15>(0xDDCC3BAA) == 0xFF73EF52);
    REQUIRE(to_rgba<16>(0xDDCC3BAA) == 0x0073EF52);

    REQUIRE(to_rgba<24>(0xDDCCBBAA) == 0xFFAABBCC);
    REQUIRE(to_rgba<32>(0xDDCCBBAA) == 0xDDAABBCC);
}

TEST_CASE("data source - read", "[io]") {
    constexpr uint8_t data[] {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88
    };

    auto const check_source = [&](auto& source) {
        SECTION("size") {
            auto const size = static_cast<ptrdiff_t>(std::size(data));
            REQUIRE(source.size() == size);
        }

        auto const check = [&source](ptrdiff_t const n, auto const& expected) {
            std::decay_t<decltype(*std::begin(expected))> out;
            for (size_t i = 0; i < std::size(expected); ++i) {
                detail::read(source, n, out);
                REQUIRE(out == expected[i]);
            }
        };

        SECTION("read 1 byte at a time") {
            constexpr uint8_t expected[] {
                0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88
              , 0x0 // past the end
            };

            check(1, expected);
        }

        SECTION("read 2 bytes at a time") {
            constexpr uint16_t expected[] {
                0x2211, 0x4433, 0x6655, 0x8877
              , 0x0 // past the end
            };

            check(2, expected);
        }

        SECTION("read 3 bytes at a time") {
            constexpr uint32_t expected[] {
                0x332211, 0x665544
              , 0x8877 // past the end
            };

            check(3, expected);
        }

        SECTION("read 4 bytes at a time") {
            constexpr uint32_t expected[] {
                0x44332211, 0x88776655
              , 0x0 // past the end
            };

            check(4, expected);
        }

        SECTION("read 5 bytes at a time") {
            constexpr uint64_t expected[] {
                0x5544332211, 0x887766
              , 0x0 // past the end
            };

            check(5, expected);
        }
    };

    SECTION("file_source") {
        auto source = bktga::detail::file_source {fill_temp_file(data)};
        check_source(source);
    }

    SECTION("memory_source") {
        auto source = bktga::detail::memory_source {data};
        check_source(source);
    }
}

TEST_CASE("data source - seek", "[io]") {
    constexpr uint8_t data[] {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88
    };

    auto const check = [&](auto& source) {
        uint8_t out;

        // past begin
        source.seek(-1);
        detail::read(source, 1, out);
        REQUIRE(out == data[0]);

        // past end
        source.seek(static_cast<ptrdiff_t>(std::size(data) + 5));
        detail::read(source, 1, out);
        REQUIRE(out == uint8_t {0});

        // at begin
        source.seek(0);
        detail::read(source, 1, out);
        REQUIRE(out == data[0]);

        // at end
        source.seek(static_cast<ptrdiff_t>(std::size(data) - 1));
        detail::read(source, 1, out);
        REQUIRE(out == data[std::size(data) - 1]);
    };

    SECTION("file_source") {
        auto source = bktga::detail::file_source {fill_temp_file(data)};
        check(source);
    }

    SECTION("memory_source") {
        auto source = bktga::detail::memory_source {data};
        check(source);
    }
}

TEST_CASE("detect", "[api]") {
    using bktga::detect;

    constexpr auto header_size = bktga::tga_header_size;

    constexpr uint8_t carray[header_size] {
        0x00, 0x01, 0x09, 0x00, 0x00, 0x00, 0x01, 0x18, 0x00
      , 0x00, 0x00, 0x00, 0xF4, 0x02, 0x00, 0x02, 0x08, 0x00
    };

    auto const result = detect(carray);

    auto const check = [](auto const& result) {
        auto const& tga = result.tga;

        REQUIRE(tga.id_length   == 0);
        REQUIRE(tga.cmap_type   == bktga::tga_color_map_type::present);
        REQUIRE(tga.img_type    == bktga::tga_image_type::rle_color_mapped);
        REQUIRE(tga.cmap_start  == 0);
        REQUIRE(tga.cmap_len    == 256);
        REQUIRE(tga.cmap_depth  == 24);
        REQUIRE(tga.x_offset    == 0);
        REQUIRE(tga.y_offset    == 0);
        REQUIRE(tga.width       == 756);
        REQUIRE(tga.height      == 512);
        REQUIRE(tga.pixel_depth == 8);

        REQUIRE(tga.image_desc.attribute_bits() == 0);
        REQUIRE(tga.image_desc.interleave()     == bktga::tga_interleave::none);
        REQUIRE(tga.image_desc.origin()         == bktga::tga_origin::lo_left);
    };

    SECTION("memory") {
        std::array<uint8_t, header_size> array;
        std::copy_n(carray, header_size, array.data());

        std::vector<uint8_t> const vector {begin(array), end(array)};

        // from c-array
        check(detect(carray));

        // from std::array
        check(detect(array));

        // from std::vector
        check(detect(vector));

        // from pointer pair
        check(detect(std::begin(carray), std::end(carray)));

        // from pointer and size
        check(detect(carray, header_size));
    }

    SECTION("file name") {
        check(detect(bktga::read_from_file, test_files[0]));
    }

    SECTION("file handle") {
        check(detect(fill_temp_file(carray)));
    }
}

//TEST_CASE("fields", "[io]") {
//    namespace detail = bktga::detail;
//    using detail::static_field_t;
//    using detail::variable_field_t;
//    using detail::read_field;
//
//    constexpr std::array<uint8_t, 18> mem_data {
//        0x00, 0x01, 0x09, 0x00, 0x00, 0x00, 0x01, 0x18, 0x00
//      , 0x00, 0x00, 0x00, 0xF4, 0x02, 0x00, 0x02, 0x08, 0x00
//    };
//
//    detail::memory_source src_mem  {mem_data};
//    detail::file_source   src_file {"8-bit-rle.tga"};
//
//    SECTION("static fields") {
//        constexpr auto field_8a_t  = static_field_t<char>          {};
//        constexpr auto field_8b_t  = static_field_t<signed char>   {};
//        constexpr auto field_8c_t  = static_field_t<unsigned char> {};
//        constexpr auto field_16a_t = static_field_t<int16_t,  1>   {};
//        constexpr auto field_16b_t = static_field_t<uint16_t, 1>   {};
//        constexpr auto field_32a_t = static_field_t<int32_t,  4>   {};
//        constexpr auto field_32b_t = static_field_t<uint32_t, 4>   {};
//        constexpr auto field_64a_t = static_field_t<int64_t,  0>   {};
//        constexpr auto field_64b_t = static_field_t<uint64_t, 0>   {};
//
//        constexpr auto field_array_t = static_field_t<decltype(mem_data)> {};
//
//        auto const check = [&](auto& src) {
//            REQUIRE(read_field(field_8a_t,  src) == 0x00);
//            REQUIRE(read_field(field_8b_t,  src) == 0x00);
//            REQUIRE(read_field(field_8c_t,  src) == 0x00);
//            REQUIRE(read_field(field_16a_t, src) == 0x09'01);
//            REQUIRE(read_field(field_16b_t, src) == 0x09'01);
//            REQUIRE(read_field(field_32a_t, src) == 0x18'01'00'00);
//            REQUIRE(read_field(field_32b_t, src) == 0x18'01'00'00);
//            REQUIRE(read_field(field_64a_t, src) == 0x18'01'00'00'00'09'01'00ll);
//            REQUIRE(read_field(field_64b_t, src) == 0x18'01'00'00'00'09'01'00ull);
//
//            auto const a = read_field(field_array_t, src);
//            REQUIRE(std::equal(begin(mem_data), end(mem_data), begin(a), end(a)));
//        };
//
//        check(src_mem);
//        check(src_file);
//    }
//
//    SECTION("variable fields") {
//        auto const check = [&](auto& src) {
//            using std::begin;
//            using std::end;
//
//            auto const a = read_field(variable_field_t {18, 0}, src);
//            REQUIRE(std::equal(begin(mem_data), end(mem_data), begin(a), end(a)));
//        };
//
//        check(src_mem);
//        check(src_file);
//    }
//}

//TEST_CASE("memory_source from std::array", "[read]") {
//    using size = std::integral_constant<ptrdiff_t, 10>;
//
//    auto const data = [] {
//        std::array<char, size::value> out;
//        std::iota(begin(out), end(out), 1);
//        return out;
//    }();
//
//    SECTION("check size") {
//        for (ptrdiff_t i = 0; i < size::value; ++i) {
//            memory_source s {data, i};
//            REQUIRE(s.size() == (size::value - i));
//        }
//    }
//
//    memory_source src {data};
//
//    SECTION("read 1 byte") {
//        for (ptrdiff_t i = 0; i < size::value; ++i) {
//            char c {};
//            src.read(1, &c, 1);
//            REQUIRE(c == (i + 1));
//        }
//    }
//
//    SECTION("over-read source") {
//        std::array<char, size::value> out;
//        src.read(size::value + 1, out.data(), out.size());
//        REQUIRE(std::equal(begin(out), end(out), begin(data), end(data)));
//    }
//
//    SECTION("over-read dest") {
//        std::array<char, size::value - 1> out;
//        src.read(size::value, out.data(), out.size());
//        REQUIRE(std::equal(begin(out), end(out), begin(data)));
//    }
//
//    SECTION("under-read") {
//        std::array<char, size::value + 3> out;
//        src.read(out.size(), out.data(), out.size());
//        REQUIRE(std::equal(begin(data), end(data), begin(out)));
//
//        auto const it = std::find_if_not(begin(out) + size::value, end(out)
//            , [](auto const n) { return n == 0; });
//
//        REQUIRE(it == end(out));
//    }
//}
//
//TEST_CASE("memory_source from std::vector", "[read]") {
//    using size = std::integral_constant<ptrdiff_t, 10>;
//
//    auto const data = [] {
//        std::vector<char> out;
//        out.reserve(size::value);
//        std::generate_n(std::back_inserter(out), size::value, [i = char {1}]() mutable {
//            return i++;
//        });
//
//        return out;
//    }();
//
//    for (ptrdiff_t i = 0; i < size::value; ++i) {
//        memory_source s {data, i};
//        REQUIRE(s.size() == (size::value - i));
//    }
//}
//
//TEST_CASE("file_source", "[read]") {
//    file_source src {"foo.txt"};
//
//    std::array<char, 5> out;
//    src.read(5, out.data(), out.size());
//
//}


#if defined(_MSC_VER)
#   pragma warning(pop)
#elif defined(__clang__)
#   pragma clang diagnostic pop
#elif defined __GNUC__
#   pragma GCC diagnostic pop
#endif
