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

#if _MSC_FULL_VER >= 190023506
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

namespace detail = bktga::detail;

TEST_CASE("detect", "[tga]") {
    constexpr uint8_t header_data[] {
        0x00, 0x01, 0x09, 0x00, 0x00, 0x00, 0x01, 0x18, 0x00
      , 0x00, 0x00, 0x00, 0xF4, 0x02, 0x00, 0x02, 0x08, 0x00
    };

    auto const check = [](auto const& result) {
        REQUIRE(result);
        auto const& tga = result.tga;

        REQUIRE(result.status == bktga::status::success);

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

    SECTION("c-array") {
        check(bktga::detect(header_data));
    }

    SECTION("std::array") {
        std::array<uint8_t, bktga::tga_header_size> array;
        std::copy(std::begin(header_data), std::end(header_data), begin(array));
        check(bktga::detect(array));
    }
}

TEST_CASE("fields", "[io]") {
    namespace detail = bktga::detail;
    using detail::static_field_t;
    using detail::variable_field_t;
    using detail::read_field;

    constexpr std::array<uint8_t, 18> mem_data {
        0x00, 0x01, 0x09, 0x00, 0x00, 0x00, 0x01, 0x18, 0x00
      , 0x00, 0x00, 0x00, 0xF4, 0x02, 0x00, 0x02, 0x08, 0x00
    };

    detail::memory_source src_mem  {mem_data};
    detail::file_source   src_file {"8-bit-rle.tga"};

    SECTION("static fields") {
        constexpr auto field_8a_t  = static_field_t<char>          {};
        constexpr auto field_8b_t  = static_field_t<signed char>   {};
        constexpr auto field_8c_t  = static_field_t<unsigned char> {};
        constexpr auto field_16a_t = static_field_t<int16_t,  1>   {};
        constexpr auto field_16b_t = static_field_t<uint16_t, 1>   {};
        constexpr auto field_32a_t = static_field_t<int32_t,  4>   {};
        constexpr auto field_32b_t = static_field_t<uint32_t, 4>   {};
        constexpr auto field_64a_t = static_field_t<int64_t,  0>   {};
        constexpr auto field_64b_t = static_field_t<uint64_t, 0>   {};

        constexpr auto field_array_t = static_field_t<decltype(mem_data)> {};

        auto const check = [&](auto& src) {
            REQUIRE(read_field(field_8a_t,  src) == 0x00);
            REQUIRE(read_field(field_8b_t,  src) == 0x00);
            REQUIRE(read_field(field_8c_t,  src) == 0x00);
            REQUIRE(read_field(field_16a_t, src) == 0x09'01);
            REQUIRE(read_field(field_16b_t, src) == 0x09'01);
            REQUIRE(read_field(field_32a_t, src) == 0x18'01'00'00);
            REQUIRE(read_field(field_32b_t, src) == 0x18'01'00'00);
            REQUIRE(read_field(field_64a_t, src) == 0x18'01'00'00'00'09'01'00ll);
            REQUIRE(read_field(field_64b_t, src) == 0x18'01'00'00'00'09'01'00ull);

            auto const a = read_field(field_array_t, src);
            REQUIRE(std::equal(begin(mem_data), end(mem_data), begin(a), end(a)));
        };

        check(src_mem);
        check(src_file);
    }

    SECTION("variable fields") {
        auto const check = [&](auto& src) {
            using std::begin;
            using std::end;

            auto const a = read_field(variable_field_t {18, 0}, src);
            REQUIRE(std::equal(begin(mem_data), end(mem_data), begin(a), end(a)));
        };

        check(src_mem);
        check(src_file);
    }
}

TEST_CASE("api", "[api]") {
    auto tga = bktga::detect(bktga::read_from_file, "8-bit-rle.tga");
    if (!tga) {
        REQUIRE(false);
    }

    auto const decoded = bktga::decode(tga);

    auto const handle = std::unique_ptr<FILE, decltype(&fclose)> {
        fopen("out.raw", "wb"), &fclose
    };

    REQUIRE(handle);
    fwrite(decoded.data(), sizeof(uint32_t), decoded.size(), handle.get());
}

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
