#include "tga.hpp"

#if defined(_MSC_VER)
#   pragma warning(push)
#   pragma warning(disable : 4571)
#   pragma warning(disable : 4625)
#   pragma warning(disable : 4626)
#   pragma warning(disable : 4820)
#   pragma warning(disable : 5026)
#   pragma warning(disable : 5027)
#elif defined(__clang__)
#   pragma clang diagnostic push
#   pragma clang diagnostic ignored "-Wexit-time-destructors"
#   pragma clang diagnostic ignored "-Wmissing-braces"
#endif
#define CATCH_CPP11_OR_GREATER
#define CATCH_CONFIG_CPP11_NOEXCEPT
#define CATCH_CONFIG_MAIN
#include <Catch/catch.hpp>

#include <string>
#include <sstream>
#include <iterator>

TEST_CASE("static", "[bktga]") {
    namespace detail = bktga::detail;

    static_assert(std::is_same<void,     detail::uint_t<0>>::value, "");
    static_assert(std::is_same<uint8_t,  detail::uint_t<1>>::value, "");
    static_assert(std::is_same<uint16_t, detail::uint_t<2>>::value, "");
    static_assert(std::is_same<uint32_t, detail::uint_t<3>>::value, "");
    static_assert(std::is_same<uint32_t, detail::uint_t<4>>::value, "");
    static_assert(std::is_same<uint64_t, detail::uint_t<5>>::value, "");
    static_assert(std::is_same<uint64_t, detail::uint_t<6>>::value, "");
    static_assert(std::is_same<uint64_t, detail::uint_t<7>>::value, "");
    static_assert(std::is_same<uint64_t, detail::uint_t<8>>::value, "");
    static_assert(std::is_same<void,     detail::uint_t<9>>::value, "");

    static_assert(detail::round_up_bits_to_bytes(0) == 0, "");
    static_assert(detail::round_up_bits_to_bytes(1) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(2) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(3) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(4) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(5) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(6) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(7) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(8) == 1, "");
    static_assert(detail::round_up_bits_to_bytes(9) == 2, "");

    static_assert(detail::round_up_bits_to_bytes(-1) == 0, "");

    constexpr auto imax = std::numeric_limits<intmax_t>::max();
    constexpr auto umax = std::numeric_limits<uintmax_t>::max();

    static_assert(detail::round_up_bits_to_bytes(imax)
               == detail::round_up_bits_to_bytes(static_cast<uintmax_t>(imax)), "");

    static_assert(detail::round_up_bits_to_bytes(std::numeric_limits<uintmax_t>::max()) == 0, "");
}

namespace {
template <typename T, typename U>
bool test_int_types(U const value, std::array<uint32_t, 5> const& expected) {
    using bktga::detail::to_rgba;

    auto const n = static_cast<T>(
        static_cast<std::make_unsigned_t<U>>(value)
      & static_cast<std::make_unsigned_t<T>>(~0));

    return (to_rgba< 8>(n) == expected[0])
        && (to_rgba<15>(n) == expected[1])
        && (to_rgba<16>(n) == expected[2])
        && (to_rgba<24>(n) == expected[3])
        && (to_rgba<32>(n) == expected[4]);
}

template <typename U, typename... Ts>
bool test_int_types(std::tuple<Ts...>, U const value, std::array<uint32_t, 5> const& expected) {
    bool const results[] = {true, test_int_types<Ts>(value, expected)...};
    return std::all_of(std::begin(results), std::end(results), [](auto a) { return !!a;} );
}

} //namespace

TEST_CASE("detail::to_rgba", "[detail]") {
    using t1 = std::tuple<
        char, wchar_t
      , int8_t,  int16_t,  int32_t,  int64_t
      , uint8_t, uint16_t, uint32_t, uint64_t>;

    using array_t = std::array<uint32_t, 5> const;

    REQUIRE(test_int_types(t1 {}, 0, array_t {
        0xFF000000
      , 0xFF000000
      , 0x00000000
      , 0xFF000000
      , 0x00000000}));

    REQUIRE(test_int_types(t1 {}, 0xFF, array_t {
        0xFFFFFFFF
      , 0xFF0039FF
      , 0x000039FF
      , 0xFF0000FF
      , 0x000000FF}));

    REQUIRE(test_int_types(t1 {}, 0x1F, array_t {
        0xFF1F1F1F
      , 0xFF0000FF
      , 0x000000FF
      , 0xFF00001F
      , 0x0000001F}));

    using t2 = std::tuple<
        wchar_t
      , int16_t,  int32_t,  int64_t
      , uint16_t, uint32_t, uint64_t>;

    REQUIRE(test_int_types(t2 {}, 0xFFFF, array_t {
        0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFF00FFFF
      , 0x0000FFFF}));

    REQUIRE(test_int_types(t2 {}, 0x7FFF, array_t {
        0xFFFFFFFF
      , 0xFFFFFFFF
      , 0x00FFFFFF
      , 0xFF007FFF
      , 0x00007FFF}));

    REQUIRE(test_int_types(t2 {}, 0x0421, array_t {
        0xFF212121
      , 0xFF080808
      , 0x00080808
      , 0xFF000421
      , 0x00000421}));

    REQUIRE(test_int_types(t2 {}, 0x8421, array_t {
        0xFF212121
      , 0xFF080808
      , 0xFF080808
      , 0xFF008421
      , 0x00008421}));

    using t4 = std::conditional_t<sizeof(wchar_t) >= 4
      , std::tuple<wchar_t, int32_t,  int64_t, uint32_t, uint64_t>
      , std::tuple<int32_t, int64_t, uint32_t, uint64_t>>;

    REQUIRE(test_int_types(t4 {}, 0xFFFFFFFF, array_t {
        0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF}));

    REQUIRE(test_int_types(t4 {}, 0x00FFFFFF, array_t {
        0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF
      , 0x00FFFFFF}));

    REQUIRE(test_int_types(t4 {}, 0x7FFFFFFF, array_t {
        0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF
      , 0xFFFFFFFF
      , 0x7FFFFFFF}));
}

namespace {

template <typename T, typename It>
struct read_n_bytes_checker {
    It const beg;
    It const end;

    template <size_t N>
    bool apply(T const value, char const next) const {
        auto const r = bktga::detail::read_n_bytes<N>(beg, end);
        return std::get<0>(r) == value && *std::get<1>(r) == next;
    }
};

template <size_t N>
std::istringstream make_istream(char const (&s)[N]) {
    return std::istringstream {
        std::string {
            std::begin(s), std::end(s)}
      , std::ios::binary};
}

template <size_t N>
std::istringstream make_istream(std::array<char, N> const& s) {
    return std::istringstream {
        std::string {
            begin(s), end(s)}
      , std::ios::binary};
}

} //namespace

TEST_CASE("detail::read_n_bytes", "[detail]") {
    constexpr char data[] =
        "\x11\x22\x33\x44\x55\x66\x77\x88\x99\xAA\xBB\xCC\xDD\xEE\xFF";

    auto in = make_istream(data);

    using iterator_t = std::istreambuf_iterator<char>;
    iterator_t const beg {in};
    iterator_t const end {};

    SECTION("read_n_bytes uint8_t") {
        auto const check = read_n_bytes_checker<uint8_t, iterator_t> {beg, end};

        //REQUIRE(check.apply<0>(0x00u, data[0]));
        REQUIRE(check.apply<1>(0x11u, data[1]));
    }

    SECTION("read_n_bytes uint16_t") {
        auto const check = read_n_bytes_checker<uint16_t, iterator_t> {beg, end};

        //REQUIRE(check.apply<0>(0x0000u, data[0]));
        REQUIRE(check.apply<1>(0x0011u, data[1]));
        REQUIRE(check.apply<2>(0x3322u, data[3]));
    }

    SECTION("read_n_bytes uint32_t") {
        auto const check = read_n_bytes_checker<uint32_t, iterator_t> {beg, end};

        //REQUIRE(check.apply<0>(0x00000000u, data[0]));
        REQUIRE(check.apply<1>(0x00000011u, data[1]));
        REQUIRE(check.apply<2>(0x00003322u, data[3]));
        REQUIRE(check.apply<3>(0x00665544u, data[6]));
        REQUIRE(check.apply<4>(0xAA998877u, data[10]));
    }
}

namespace {

struct read_n_bits_checker {
    std::istringstream in;

    template <size_t N, typename T>
    bool apply(T const value, char const next) {
        in.seekg(0, std::ios::beg);

        auto const beg = std::istreambuf_iterator<char> {in};
        auto const end = std::istreambuf_iterator<char> {};

        auto const r = bktga::detail::read_n_bits<N>(beg, end);
        auto const t0 = std::get<0>(r);

        static_assert(std::is_same<T, std::remove_cv_t<decltype(t0)>>::value, "");

        return t0 == value && *std::get<1>(r) == next;
    }
};

template <typename T0>
uint8_t make_int(T0 const b0) noexcept {
    return static_cast<uint8_t>(b0);
}

template <typename T0, typename T1>
uint16_t make_int(T0 const b0, T1 const b1) noexcept {
    return static_cast<uint16_t>(
        static_cast<uint8_t>(b0)
      | static_cast<uint8_t>(b1) << 8);
}

template <typename T0, typename T1, typename T2, typename T3 = T0>
uint32_t make_int(T0 const b0, T1 const b1, T2 const b2, T3 const b3 = 0) noexcept {
    return static_cast<uint32_t>(
        static_cast<uint8_t>(b0)
      | static_cast<uint8_t>(b1) << 8
      | static_cast<uint8_t>(b2) << 16
      | static_cast<uint8_t>(b3) << 24);
}

} //namespace

TEST_CASE("detail::read_n_bits", "[detail]") {
    std::array<char, 5> const data {
        static_cast<char>(0b0101'0101u)
      , static_cast<char>(0b1010'1010u)
      , static_cast<char>(0b1100'1100u)
      , static_cast<char>(0b0011'0011u)
      , static_cast<char>(0b1111'1111u)
    };

    read_n_bits_checker check {make_istream(data)};

    REQUIRE(check.apply< 1>(make_int(0b0000'0001), data[1]));
    REQUIRE(check.apply< 2>(make_int(0b0000'0001), data[1]));
    REQUIRE(check.apply< 3>(make_int(0b0000'0101), data[1]));
    REQUIRE(check.apply< 4>(make_int(0b0000'0101), data[1]));
    REQUIRE(check.apply< 5>(make_int(0b0001'0101), data[1]));
    REQUIRE(check.apply< 6>(make_int(0b0001'0101), data[1]));
    REQUIRE(check.apply< 7>(make_int(0b0101'0101), data[1]));
    REQUIRE(check.apply< 8>(make_int(0b0101'0101), data[1]));

    auto const n16 = [b0 = data[0]](auto const b1) {
        return make_int(b0, b1);
    };

    REQUIRE(check.apply< 9>(n16(0b0000'0000), data[2]));
    REQUIRE(check.apply<10>(n16(0b0000'0010), data[2]));
    REQUIRE(check.apply<11>(n16(0b0000'0010), data[2]));
    REQUIRE(check.apply<12>(n16(0b0000'1010), data[2]));
    REQUIRE(check.apply<13>(n16(0b0000'1010), data[2]));
    REQUIRE(check.apply<14>(n16(0b0010'1010), data[2]));
    REQUIRE(check.apply<15>(n16(0b0010'1010), data[2]));
    REQUIRE(check.apply<16>(n16(0b1010'1010), data[2]));

    auto const n24 = [b0 = data[0], b1 = data[1]](auto const b2) {
        return make_int(b0, b1, b2);
    };

    REQUIRE(check.apply<17>(n24(0b0000'0000), data[3]));
    REQUIRE(check.apply<18>(n24(0b0000'0000), data[3]));
    REQUIRE(check.apply<19>(n24(0b0000'0100), data[3]));
    REQUIRE(check.apply<20>(n24(0b0000'1100), data[3]));
    REQUIRE(check.apply<21>(n24(0b0000'1100), data[3]));
    REQUIRE(check.apply<22>(n24(0b0000'1100), data[3]));
    REQUIRE(check.apply<23>(n24(0b0100'1100), data[3]));
    REQUIRE(check.apply<24>(n24(0b1100'1100), data[3]));

    auto const n32 = [b0 = data[0], b1 = data[1], b2 = data[2]](auto const b3) {
        return make_int(b0, b1, b2, b3);
    };

    REQUIRE(check.apply<25>(n32(0b0000'0001), data[4]));
    REQUIRE(check.apply<26>(n32(0b0000'0011), data[4]));
    REQUIRE(check.apply<27>(n32(0b0000'0011), data[4]));
    REQUIRE(check.apply<28>(n32(0b0000'0011), data[4]));
    REQUIRE(check.apply<29>(n32(0b0001'0011), data[4]));
    REQUIRE(check.apply<30>(n32(0b0011'0011), data[4]));
    REQUIRE(check.apply<31>(n32(0b0011'0011), data[4]));
    REQUIRE(check.apply<32>(n32(0b0011'0011), data[4]));
}

TEST_CASE("detail::flip", "[detail]") {
    constexpr std::array<int, 16> original {
        0, 2, 2, 2
      , 1, 0, 2, 2
      , 1, 1, 0, 2
      , 1, 1, 1, 0
    };

    auto data = original;

    SECTION("horizontal") {
        bktga::detail::flip_horizontal(data.data(), 4, 4);

        constexpr std::array<int, 16> result {
            2, 2, 2, 0
          , 2, 2, 0, 1
          , 2, 0, 1, 1
          , 0, 1, 1, 1
        };

        REQUIRE(std::equal(begin(data), end(data), begin(result)));

        bktga::detail::flip_horizontal(data.data(), 4, 4);
        REQUIRE(std::equal(begin(data), end(data), begin(original)));
    }

    SECTION("vertical") {
        bktga::detail::flip_vertical(data.data(), 4, 4);

        constexpr std::array<int, 16> result {
            1, 1, 1, 0
          , 1, 1, 0, 2
          , 1, 0, 2, 2
          , 0, 2, 2, 2
        };

        REQUIRE(std::equal(begin(data), end(data), begin(result)));

        bktga::detail::flip_vertical(data.data(), 4, 4);
        REQUIRE(std::equal(begin(data), end(data), begin(original)));
    }
}

TEST_CASE("id and color map", "[bktga]") {
    constexpr char data[] {
        "\x05"     //id length - 5
        "\x01"     //color map
        "\x01"     //true color
        "\x10\x00" //color map start  - 16
        "\x04\x00" //color map length - 4
        "\x10"     //color map depth  - 16
        "\x00\x00" //x offset
        "\x00\x00" //y offset
        "\x0F\x01" //width
        "\xF0\x10" //height
        "\x18"     //pixel depth - 24
        "\x00"     //image descriptor

        // id
        "abcd\0"

        // color map
        "\x10\x00" "\x20\x00" "\x30\x00" "\x40\x00"

        // footer
        "\x00\x00\x00\x00"
        "\x00\x00\x00\x00"
        "TRUEVISION-XFILE."
    };

    auto in     = make_istream(data);
    auto result = bktga::check_file(in);

    auto const& tga = *result.first;

    constexpr auto expected_id_len    = 5;
    constexpr auto expected_cmap_size = 4 * 2;
    constexpr auto expected_data_size = 0x010F * 0x10F0 * 3;
    constexpr char expected_id[]      = "abcd";

    auto const make_range = [](auto const beg, auto const size) {
        return bktga::tga_descriptor::range_t {
            static_cast<ptrdiff_t>(beg), static_cast<ptrdiff_t>(beg + size)
        };
    };

    auto const id_range    = make_range(bktga::header_size,      expected_id_len);
    auto const cmap_range  = make_range(std::get<1>(id_range),   expected_cmap_size);
    auto const idata_range = make_range(std::get<1>(cmap_range), expected_data_size);

    REQUIRE(tga.id_range()         == id_range);
    REQUIRE(tga.color_map_range()  == cmap_range);
    REQUIRE(tga.image_data_range() == idata_range);

    REQUIRE(std::equal(std::begin(tga.id), std::end(tga.id), std::begin(expected_id)));

    bktga::tga_color_map const cmap {tga, in};
    REQUIRE(cmap.size() == 4);

    std::array<uint32_t, 4> const expected_cmap {
        bktga::detail::to_rgba<16>(0x10)
      , bktga::detail::to_rgba<16>(0x20)
      , bktga::detail::to_rgba<16>(0x30)
      , bktga::detail::to_rgba<16>(0x40)
    };

    REQUIRE(std::equal(std::begin(cmap), std::end(cmap), std::begin(expected_cmap)));
}

TEST_CASE("check_file", "[bktga]") {
    SECTION("version 1") {
        constexpr char data[64] {
            "\x00"     //id length
            "\x01"     //color map
            "\x02"     //true color
            "\x00\x00" //color map start
            "\x00\x00" //color map length
            "\x00"     //color map depth
            "\x00\x00" //x offset
            "\x00\x00" //y offset
            "\x0F\x01" //width
            "\xF0\x10" //height
            "\x18"     //pixel depth
            "\x00"     //image descriptor
        };

        auto const result = bktga::check_file(make_istream(data));
        REQUIRE(!!result.first);
        REQUIRE(result.second.empty());

        auto const& tga = *result.first;

        REQUIRE(tga.id_length   == uint8_t {0});
        REQUIRE(tga.cmap_type   == bktga::tga_color_map_type::present);
        REQUIRE(tga.img_type    == bktga::tga_image_type::true_color);
        REQUIRE(tga.cmap_start  == uint16_t {0});
        REQUIRE(tga.cmap_len    == uint16_t {0});
        REQUIRE(tga.cmap_depth  == uint8_t {0});
        REQUIRE(tga.x_offset    == uint16_t {0});
        REQUIRE(tga.y_offset    == uint16_t {0});
        REQUIRE(tga.width       == uint16_t {0x010F});
        REQUIRE(tga.height      == uint16_t {0x10F0});
        REQUIRE(tga.pixel_depth == uint8_t {0x18});
        REQUIRE(tga.image_desc  == bktga::tga_image_descriptor {0});
        REQUIRE(tga.ext_offset  == uint8_t {0});
        REQUIRE(tga.dev_offset  == uint8_t {0});

        REQUIRE(tga.version == bktga::tga_version::v1);
    }

    SECTION("version 2") {
        constexpr char data[] {
            "\x00"     //id length
            "\x01"     //color map
            "\x02"     //true color
            "\x00\x00" //color map start
            "\x00\x00" //color map length
            "\x00"     //color map depth
            "\x00\x00" //x offset
            "\x00\x00" //y offset
            "\x0F\x01" //width
            "\xF0\x10" //height
            "\x18"     //pixel depth
            "\x00"     //image descriptor

            // footer
            "\x00\x00\x00\x00"
            "\x00\x00\x00\x00"
            "TRUEVISION-XFILE."
        };

        auto const result = bktga::check_file(make_istream(data));
        REQUIRE(!!result.first);
        REQUIRE(result.second.empty());

        auto const& tga = *result.first;

        REQUIRE(tga.id_length   == uint8_t {0});
        REQUIRE(tga.cmap_type   == bktga::tga_color_map_type::present);
        REQUIRE(tga.img_type    == bktga::tga_image_type::true_color);
        REQUIRE(tga.cmap_start  == uint16_t {0});
        REQUIRE(tga.cmap_len    == uint16_t {0});
        REQUIRE(tga.cmap_depth  == uint8_t {0});
        REQUIRE(tga.x_offset    == uint16_t {0});
        REQUIRE(tga.y_offset    == uint16_t {0});
        REQUIRE(tga.width       == uint16_t {0x010F});
        REQUIRE(tga.height      == uint16_t {0x10F0});
        REQUIRE(tga.pixel_depth == uint8_t {0x18});
        REQUIRE(tga.image_desc  == bktga::tga_image_descriptor {0});
        REQUIRE(tga.ext_offset  == uint8_t {0});
        REQUIRE(tga.dev_offset  == uint8_t {0});

        REQUIRE(tga.version == bktga::tga_version::v2);
    }
}

TEST_CASE("write raw", "[output]") {
    constexpr char filename[] = R"(D:\Users\Brandon\Downloads\256 Colors (RLE).tga)";

    auto result = bktga::check_file(filename);
    REQUIRE(result.first);

    auto const& tga = *result.first;

    auto const decoded = bktga::decode(
        tga, std::ifstream {filename, std::ios::binary});

    std::ofstream out{R"(./out.raw)", std::ios::binary};
    out.write(reinterpret_cast<char const*>(decoded.data()),
              decoded.size() * sizeof(uint32_t));
}

#if defined(_MSC_VER)
#   pragma warning(pop)
#elif defined(__clang__)
#   pragma clang diagnostic pop
#endif
