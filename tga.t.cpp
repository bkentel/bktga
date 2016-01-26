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
#include <numeric>

namespace detail = bktga::detail;

namespace {

template <typename Container>
bktga::unique_file fill_temp_file(Container const& c) noexcept {
    using std::begin;
    using std::end;
    using bktga::detail::size;
    using bktga::detail::data;

    auto temp_file = bktga::unique_file {std::tmpfile(), fclose};
    REQUIRE(temp_file);

    using value_t = std::decay_t<decltype(*begin(c))>;
    static_assert(std::is_pod<value_t> {}, "");

    fwrite(data(c), sizeof(value_t), size(c), temp_file.get());
    fseek(temp_file.get(), 0, SEEK_SET);

    return temp_file;
}

template <typename Container>
void write_raw(Container const& c, bktga::string_view const filename) noexcept {
    using std::begin;
    using std::end;
    using bktga::detail::size;
    using bktga::detail::data;

    std::string fname = filename.to_string();
    fname += ".raw";

    auto file = bktga::unique_file {std::fopen(fname.c_str(), "wb"), fclose};
    REQUIRE(file);

    using value_t = std::decay_t<decltype(*begin(c))>;
    static_assert(std::is_pod<value_t> {}, "");
    fwrite(data(c), sizeof(value_t), size(c), file.get());
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

        REQUIRE(reinterpret_cast<std::uintptr_t>(b.begin())
             == reinterpret_cast<std::uintptr_t>(b.data()));

        REQUIRE(reinterpret_cast<std::uintptr_t>(b.end())
             == reinterpret_cast<std::uintptr_t>(b.data() + size));
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
    REQUIRE(to_rgba<15>(0xDDCCBBAA) == 0xFF52EF73);
    REQUIRE(to_rgba<16>(0xDDCCBBAA) == 0xFF52EF73);

    //with bit-15 cleared
    REQUIRE(to_rgba<15>(0xDDCC3BAA) == 0xFF52EF73);
    REQUIRE(to_rgba<16>(0xDDCC3BAA) == 0x0052EF73);

    REQUIRE(to_rgba<24>(0xDDCCBBAA) == 0xFFAABBCC);
    REQUIRE(to_rgba<32>(0xDDCCBBAA) == 0xDDAABBCC);
}

TEST_CASE("data source - read", "[io]") {
    constexpr uint8_t data[] {
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88
    };

    auto const check_source = [&](auto& source) {
        SECTION("size") {
            auto const size = static_cast<ptrdiff_t>(bktga::detail::size(data));
            REQUIRE(source.size() == size);
        }

        auto const check = [&source](ptrdiff_t const n, auto const& expected) {
            std::decay_t<decltype(*std::begin(expected))> out;
            for (size_t i = 0; i < bktga::detail::size(expected); ++i) {
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
        source.seek(static_cast<ptrdiff_t>(bktga::detail::size(data) + 5));
        detail::read(source, 1, out);
        REQUIRE(out == uint8_t {0});

        // at begin
        source.seek(0);
        detail::read(source, 1, out);
        REQUIRE(out == data[0]);

        // at end
        source.seek(static_cast<ptrdiff_t>(bktga::detail::size(data) - 1));
        detail::read(source, 1, out);
        REQUIRE(out == data[bktga::detail::size(data) - 1]);
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

TEST_CASE("detect - bad", "[api]") {
    uint8_t carray[bktga::tga_header_size] {
        0x00, 0x01, 0x09, 0x00, 0x00, 0x00, 0x01, 0x18, 0x00
      , 0x00, 0x00, 0x00, 0xF4, 0x02, 0x00, 0x02, 0x08, 0x00
    };

    auto const check = [&] {
        auto const result = bktga::detect(carray);
        REQUIRE_FALSE(result);
        REQUIRE_FALSE(result.tga.is_valid());
        REQUIRE_FALSE(result.tga.diagnostic.empty());
    };

    using field = bktga::tga_descriptor::field;

    SECTION("bad color map type") {
        carray[field::cmap_type::begin] = 0;   // none
        check();
        carray[field::cmap_type::begin] = 127; // reserved
        check();
        carray[field::cmap_type::begin] = 255; // custom
        check();
    }

    SECTION("bad image type") {
        carray[field::img_type::begin] = 127; // reserved
        check();
        carray[field::img_type::begin] = 255; // custom
        check();
    }

    SECTION("bad color map depth") {
        carray[field::cmap_depth::begin] = 1;
        check();
        carray[field::cmap_depth::begin] = 33;
        check();
    }

    SECTION("bad pixel depth") {
        carray[field::pixel_depth::begin] = 1;
        check();
        carray[field::pixel_depth::begin] = 33;
        check();
    }

    SECTION("bad attribute") {
        auto const set_attribute_bits = [&](int const bits) noexcept {
            auto& value = carray[field::image_desc::begin];
            value = static_cast<uint8_t>(value | (bits & 0b1111));
        };

        carray[field::cmap_depth::begin] = 8;
        set_attribute_bits(1);
        check();

        carray[field::cmap_depth::begin] = 15;
        set_attribute_bits(1);
        check();

        carray[field::cmap_depth::begin] = 16;
        set_attribute_bits(2);
        check();

        carray[field::cmap_depth::begin] = 24;
        set_attribute_bits(1);
        check();

        carray[field::cmap_depth::begin] = 32;
        set_attribute_bits(1);
        check();
    }
}


TEST_CASE("detect", "[api]") {
    using bktga::detect;

    constexpr auto header_size = bktga::tga_header_size;

    constexpr uint8_t carray[header_size] {
        0x00, 0x01, 0x09, 0x00, 0x00, 0x00, 0x01, 0x18, 0x00
      , 0x00, 0x00, 0x00, 0xF4, 0x02, 0x00, 0x02, 0x08, 0x00
    };

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
        check(detect(bktga::read_from_file, "./test/cm-8-rgb24a0-756x512-rle.tga"));
    }

    SECTION("file handle") {
        check(detect(fill_temp_file(carray)));
    }
}

namespace {

template <typename It1, typename It2>
bool create_dev_area_tga(
    bktga::string_view const fname
  , It1 const first_record, It1 const last_record
  , It2 const first_data,   It2 const last_data
) {
    auto records = std::vector<bktga::tga_developer_area::record_t>(
        first_record, last_record);

    uint8_t const header[] = {
        0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      , 0x01, 0x00, 0x01, 0x00, 0x18, 0x00
      , 0xFF, 0xFF, 0xFF
    };

    bktga::unique_file file {
        fopen(fname.to_string().c_str(), "wb"), &fclose};

    if (!file) {
        return false;
    }

    // start of the developer area
    auto const offset = fwrite(header, 1, sizeof(decltype(header)), file.get());
    auto const header_size = 2 + records.size() * (2 + 4 + 4);
    auto const data_size = std::accumulate(
        first_data, last_data, size_t {},
        [&, it = begin(records)](size_t const sum, std::string const& s) mutable {
            auto& r = *it++;
            r.offset = static_cast<uint32_t>(sum + header_size + offset);
            r.size   = static_cast<uint32_t>(s.size() + 1);
            return sum + r.size;
        });

    auto const write = [&](auto const n) {
        fwrite(&n, 1, sizeof(n), file.get());
    };

    write(static_cast<uint16_t>(records.size()));

    for (auto const& r : records) {
        write(r.tag);
        write(r.offset);
        write(r.size);
    }

    std::for_each(first_data, last_data, [&](auto const& r) {
        fwrite(r.data(), 1, r.size() + 1, file.get());
    });

    write(static_cast<uint32_t>(0));
    write(static_cast<uint32_t>(offset));

    fwrite(bktga::tga_signature, 1, sizeof(decltype(bktga::tga_signature)), file.get());

    return true;
}

} // namespace

TEST_CASE("developer area", "[footer]") {
    std::vector<uint16_t> const tags {
        1, 2, 3, 4
    };

    std::vector<std::string> const data {
        "record 1 data 1"
      , "record 2 data 22"
      , "record 3 data 333"
      , "record 4 data 4444"
    };

    bktga::string_view const filename {"./test/dev.tga"};

    REQUIRE(create_dev_area_tga(filename
      , begin(tags), end(tags), begin(data), end(data)));

    auto result = bktga::detect(bktga::read_from_file, filename);
    REQUIRE(result);

    auto const dev = bktga::tga_developer_area {result.source, result.tga.dev_offset};
    REQUIRE(dev.size() == 4);

    size_t i = 0;
    for (auto const& r : dev) {
        REQUIRE(r.tag  == tags[i]);
        REQUIRE(r.size == (data[i].size() + 1));

        auto const buffer = dev.get_data(result.source, r);
        REQUIRE(std::equal(begin(data[i]), end(data[i]), std::begin(buffer)));

        ++i;
    }
}

#if defined(_MSC_VER)
#   pragma warning(pop)
#elif defined(__clang__)
#   pragma clang diagnostic pop
#elif defined __GNUC__
#   pragma GCC diagnostic pop
#endif
