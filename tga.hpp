#pragma once

//===----------------------------------------------------------------------===//
//                              Configuration
//===----------------------------------------------------------------------===//
#if defined(_MSC_VER)
#   pragma warning(push)
#   pragma warning(disable : 4623) // implicitly deleted default constructor
#   pragma warning(disable : 4625) // implicitly deleted copy constructor
#
#   if !defined(_SCL_SECURE_NO_WARNINGS)
#       define _SCL_SECURE_NO_WARNINGS
#       define BKTGA_SCL_SECURE_NO_WARNINGS
#   endif
#
#   if !defined(_CRT_SECURE_NO_WARNINGS)
#       define _CRT_SECURE_NO_WARNINGS
#       define BKTGA_CRT_SECURE_NO_WARNINGS
#   endif
#endif

#include <boost/predef/other/endian.h>

// BKTGA_STRING_VIEW      The implementation to use for string_view; defaults to boost::string_ref
// BKTGA_ASSERT_IMPL      The function to use for standard assertions.
// BKTGA_ASSERT_SAFE_IMPL The function to use for "safe mode" only assertions.
// BKTGA_ASSERT_OPT_IMPL  The function to use for -always- assertions.

#if !defined(BKTGA_STRING_VIEW)
#   include <boost/utility/string_ref.hpp>
#endif

#include <algorithm>                         // for forward
#include <array>                             // for array
#include <iterator>                          // for back_inserter
#include <memory>                            // for unique_ptr
#include <type_traits>                       // for conditional_t, etc
#include <vector>                            // for vector
#include <initializer_list>                  // for std::initializer_list
#include <system_error>                      // for system_error, etc

#include <cerrno>                            // for errno
#include <cstdint>                           // for uint32_t, uint8_t, etc
#include <cstdio>                            // for fclose, FILE, fseek, etc

#if !defined(BKTGA_ASSERT_IMPL)
#   include <cassert>
#   define BKTGA_ASSERT_IMPL assert
#endif

#if !defined(BKTGA_ASSERT_SAFE_IMPL)
#   include <cassert>
#   define BKTGA_ASSERT_SAFE_IMPL assert
#endif

#if !defined(BKTGA_ASSERT_OPT_IMPL)
#   include <cstdlib>
#   define BKTGA_ASSERT_OPT_IMPL(x) (void)(!!(x) || (std::abort(), 0))
#endif

#define BKTGA_ASSERT(x)      BKTGA_ASSERT_IMPL(x)
#define BKTGA_ASSERT_SAFE(x) BKTGA_ASSERT_SAFE_IMPL(x)
#define BKTGA_ASSERT_OPT(x)  BKTGA_ASSERT_OPT_IMPL(x)

#if defined(__clang__)
#   define BKTGA_FALLTHROUGH [[clang::fallthrough]];
#elif defined(__GNUC__)
#   define BKTGA_FALLTHROUGH
#else
#   define BKTGA_FALLTHROUGH
#endif

namespace bktga {
//===----------------------------------------------------------------------===//
//                              Type Aliases
//===----------------------------------------------------------------------===//
#if !defined(BKTGA_STRING_VIEW)
using string_view = ::boost::string_ref;
#endif

//===----------------------------------------------------------------------===//
//                              Utility
//===----------------------------------------------------------------------===//
namespace detail {
/// An unsigned type of at least N bits for 0 < N < 9; otherwise void.
template <size_t N>
using uint_t = std::conditional_t<N < 1, void,
               std::conditional_t<N < 2, uint8_t,
               std::conditional_t<N < 3, uint16_t,
               std::conditional_t<N < 5, uint32_t,
               std::conditional_t<N < 9, uint64_t, void>>>>>;

template <typename T>
inline constexpr auto enum_value(T const n) noexcept {
    static_assert(std::is_enum<T> {}, "");
    return static_cast<std::underlying_type_t<T>>(n);
}

template <typename T>
using is_byte_type = std::integral_constant<bool
  , std::is_integral<T>::value && sizeof(T) == 1>;

template <typename T>
struct is_array : std::false_type {};

template <typename T, size_t N>
struct is_array<std::array<T, N>> : std::true_type {};

/// Return the number of bytes (rounded up) required for @p n bits where
/// @p n >= 0; otherwise 0.
template <typename T>
inline constexpr ptrdiff_t round_up_bits_to_bytes(T const n) noexcept {
    static_assert(std::is_integral<T>::value, "");
    return (n >= 0)
      ? (n / 8) + ((n % 8) ? 1 : 0)
      : 0;
}

/// A wrapper around a dynamically allocated buffer.
class buffer {
public:
    template <typename T
      , typename = std::enable_if_t<std::is_integral<T>::value>>
    explicit buffer(T const size)
      : buffer {size, std::is_signed<T> {}}
    {
    }

    ptrdiff_t   size()  const noexcept { return size_; }
    char const* data()  const noexcept { return data_.get(); }
    char*       data()        noexcept { return data_.get(); }
    char const* begin() const noexcept { return data(); }
    char const* end()   const noexcept { return data() + size_; }
private:
    //unsigned
    template <typename T>
    explicit buffer(T const size, std::false_type)
      : size_ {static_cast<ptrdiff_t>(size)}
      , data_ {size ? new char[size] : nullptr}
    {
    }

    //signed
    template <typename T>
    explicit buffer(T const size, std::true_type)
      : buffer {static_cast<size_t>(size > 0 ? size : 0), std::false_type {}}
    {
    }

    ptrdiff_t                size_;
    std::unique_ptr<char []> data_;
};

/// Minimum value clamped to a floor of 0.
template <typename T>
inline constexpr T min_0(T const a, T const b) noexcept {
    return (a < b)
             ? (a < 0 ? 0 : a)
             : (b < 0 ? 0 : b);
}

/// @see min_0(a, b)
template <typename T>
inline constexpr T min_0(T const a, T const b, T const c) noexcept {
    return min_0(min_0(a, b), c);
}

/// c++17 std::data()
template <typename Container>
inline constexpr auto data(Container&& c) noexcept {
    return c.data();
}

template <typename T, size_t Size>
inline constexpr T* data(T (&array)[Size]) noexcept {
    return array;
}

template <typename T>
inline constexpr T const* data(std::initializer_list<T> ilist) noexcept {
    return ilist.begin();
}

/// c++17 std::size()
template <typename T, size_t Size>
inline constexpr size_t size(T (&)[Size]) noexcept {
    return Size;
}

template <typename Container>
inline constexpr auto size(Container&& c) noexcept -> decltype(c.size()) {
    return c.size();
}


} // namespace bktga::detail

//===----------------------------------------------------------------------===//
//                              I/O Functions
//===----------------------------------------------------------------------===//
using unique_file = std::unique_ptr<FILE, decltype(&fclose)>;

namespace detail {
/// Convert a little endian integer to a host integer of at least the same size.
#if defined(BOOST_ENDIAN_LITTLE_BYTE_AVAILABLE)
template <typename T>
inline constexpr T little_endian_to_host(T const n) noexcept {
    static_assert(std::is_integral<T>::value, "");
    return n;
}

template <ptrdiff_t Bpp>
uint32_t to_rgba(uint32_t n) noexcept;

template <> inline constexpr uint32_t to_rgba<32>(uint32_t const n) noexcept {
    // n      -> 0xAARRGGBB
    // result -> 0xAABBGGRR

    return (((n & 0xFF'00'00'00) >> 24) << 24)  // alpha
         | (((n & 0x00'FF'00'00) >> 16) <<  0)  // red
         | (((n & 0x00'00'FF'00) >>  8) <<  8)  // green
         | (((n & 0x00'00'00'FF) >>  0) << 16); // blue
}

template <> inline constexpr uint32_t to_rgba<24>(uint32_t const n) noexcept {
    // n      -> 0xXXRRGGBB
    // result -> 0x00BBGGRR

    return to_rgba<32>(n) | (0xFFu << 24);
}

inline uint32_t constexpr to_rgba_16_mask(uint32_t const n, int const i) noexcept {
    return (n >> i * 5) & 0b11111;
}

inline uint32_t constexpr to_rgba_16_expand(uint32_t const n, int const i) noexcept {
    return ((n << 3) | (n >> 2)) << i * 8;
}

template <> inline constexpr uint32_t to_rgba<16>(uint32_t const n) noexcept {
    // n      -> A BBBBB GGGGG RRRRR
    // result -> 0xAABBGGRR

    return to_rgba_16_expand(to_rgba_16_mask(n, 2), 0) // red
         | to_rgba_16_expand(to_rgba_16_mask(n, 1), 1) // green
         | to_rgba_16_expand(to_rgba_16_mask(n, 0), 2) // blue
         | (((n & 0x8000) << 9) * 0xFF);               // alpha
}

template <> inline constexpr uint32_t to_rgba<15>(uint32_t const n) noexcept {
    // n      -> X BBBBB GGGGG RRRRR
    // result -> 0x00BBGGRR

    return to_rgba<16>(n | 0x8000);
}

template <> inline constexpr uint32_t to_rgba<8>(uint32_t const n) noexcept {
    // n      -> 0xXXXXXXNN
    // result -> 0x00NNNNNN

    return ((n & 0xFFu) <<  0)  // red
         | ((n & 0xFFu) <<  8)  // green
         | ((n & 0xFFu) << 16)  // blue
         | ((    0xFFu) << 24); // alpha
}

#else
#   error not implemented
#endif

/// filesystem data source
class file_source {
public:
    explicit file_source(string_view const fname)
      : file_source {do_open_(fname.to_string().c_str())}
    {
    }

    explicit file_source(unique_file file) noexcept
      : handle_ {std::move(file)}
    {
    }

    ptrdiff_t size() const noexcept {
        FILE* const f = *this;
        auto const old = std::ftell(f);
        std::fseek(f, 0, SEEK_END);
        auto const size = std::ftell(f) - old;
        std::fseek(f, old, SEEK_SET);
        return size;
    }

    void read(
        ptrdiff_t const n        ///< number of bytes to read
      , char*     const out      ///< output buffer
      , ptrdiff_t const out_size ///< size of the output buffer
    ) noexcept {
        auto const read_size = min_0(n, out_size);
        auto const result = static_cast<ptrdiff_t>(std::fread(
            out, sizeof(char), static_cast<size_t>(read_size), *this));

        auto const fill_size = std::max(ptrdiff_t {}, out_size - result);
        std::fill_n(out + result, fill_size, char {});
    }

    void seek(ptrdiff_t const offset) noexcept {
        std::fseek(*this, static_cast<long>(offset), SEEK_SET);
    }
private:
    static unique_file do_open_(char const* const fname) {
        auto const handle = fopen(fname, "rb");
        if (!handle) {
            throw std::system_error(errno, std::system_category());
        }

        return {handle, std::fclose};
    }

    operator FILE*() const noexcept { return handle_.get(); }

    unique_file handle_;
};

/// in-memory data source
class memory_source {
public:
    template <typename Byte, typename Size>
    memory_source(Byte const* const data, Size const size) noexcept
      : first_ {reinterpret_cast<char const*>(data)}
      , last_  {first_ + size}
      , pos_   {first_}
    {
        static_assert(std::is_integral<Size>::value, "");
        static_assert(std::is_integral<Byte>::value, "");
        static_assert(sizeof(Byte) == 1, "");
    }

    template <typename Container>
    explicit memory_source(Container const& in) noexcept
      : memory_source {detail::data(in), detail::size(in)}
    {
    }

    ptrdiff_t size() const noexcept {
        return last_ - first_;
    }

    void read(
        ptrdiff_t const n        ///< number of bytes to read
      , char*     const out      ///< output buffer
      , ptrdiff_t const out_size ///< size of the output buffer
    ) noexcept {
        auto const read_size = min_0(last_ - pos_, n, out_size);
        std::copy_n(pos_, read_size, out);
        pos_ += read_size;

        auto const fill_size = std::max(ptrdiff_t {}, out_size - read_size);
        std::fill_n(out + read_size, fill_size, char {});
    }

    void seek(ptrdiff_t const offset) noexcept {
        pos_ = (offset < 0)       ? first_
             : (offset >= size()) ? last_
             : first_ + offset;
    }
private:
    char const* first_;
    char const* last_;
    char const* pos_;
};

template <typename Source, typename T>
inline void read(
    Source&         src ///< input
  , ptrdiff_t const n   ///< number of bytes to read
  , T&              out ///< output
) noexcept {
    static_assert(std::is_pod<T> {}, "");
    src.read(n, reinterpret_cast<char*>(&out), sizeof(T));
}

template <typename Source, typename T>
void read_at(
    Source&         src    ///< input
  , ptrdiff_t const n      ///< number of bytes to read
  , ptrdiff_t const offset ///< offset from start
  , T&              out    ///< output
) noexcept {
    static_assert(std::is_pod<T> {}, "");
    src.seek(offset);
    src.read(n, reinterpret_cast<char*>(&out), sizeof(T));
}

/// read at least Bits bits from in at the current position.
template <ptrdiff_t Bits, typename Source>
inline auto read_bits(Source& in) {
    constexpr ptrdiff_t bytes = round_up_bits_to_bytes(Bits);

    uint_t<bytes> out {};
    read(in, bytes, out);
    return out;
}

/// read exactly Bytes bytes from in starting at offset.
template <ptrdiff_t Bytes, typename Source>
inline auto read_bytes_at(Source& in, ptrdiff_t const offset) {
    uint_t<Bytes> out {};
    read_at(in, Bytes, offset, out);
    return out;
}

/// statically sized and located data field
template <typename T, ptrdiff_t Offset = 0>
struct static_field_t {
    using type = std::decay_t<T>;

    static constexpr ptrdiff_t begin = Offset;
    static constexpr ptrdiff_t end   = Offset + sizeof(T);
    static constexpr ptrdiff_t size  = sizeof(T);
};

/// variably sized and located data field
struct variable_field_t {
    ptrdiff_t size()  const noexcept { return size_; }
    ptrdiff_t begin() const noexcept { return offset_; }
    ptrdiff_t end()   const noexcept { return size_ + offset_; }

    ptrdiff_t size_;
    ptrdiff_t offset_;
};

struct read_field_scalar_tag    {}; ///< @see read_field
struct read_field_array_tag     {}; ///< @see read_field
struct read_field_construct_tag {}; ///< @see read_field

/// Read a variable field defined by field from src.
template <typename Source>
inline detail::buffer read_field(
    variable_field_t const field
  , Source&                src
  , ptrdiff_t        const offset = 0
) {
    detail::buffer out {field.size()};
    src.seek(field.begin() + offset);
    src.read(field.size(), out.data(), out.size());
    return out;
}

template <typename Field, typename Source, typename T = typename Field::type>
inline T read_field(read_field_array_tag, Source& src, ptrdiff_t const offset = 0) {
    T out;
    read_at(src, Field::size, Field::begin + offset, out);
    return out;
}

template <typename Field, typename Source, typename T = typename Field::type>
inline T read_field(read_field_scalar_tag, Source& src, ptrdiff_t const offset = 0) {
    return static_cast<T>(little_endian_to_host(
        read_bytes_at<Field::size>(src, Field::begin + offset)));
}

template <typename Field, typename Source, typename T = typename Field::type>
inline T read_field(read_field_construct_tag, Source& src, ptrdiff_t const offset = 0) {
    return T {little_endian_to_host(
        read_bytes_at<sizeof(T)>(src, Field::begin + offset))};
}

/// Read a static field defined by field from src.
template <typename Field, typename Source, typename T = typename Field::type>
inline T read_field(Field, Source& src, ptrdiff_t const offset = 0) {
    using tag =
        std::conditional_t<
            std::is_scalar<T>::value
          , read_field_scalar_tag
        , std::conditional_t<
            is_array<T>::value
          , read_field_array_tag
          , read_field_construct_tag>>;

    return read_field<Field>(tag {}, src, offset);
}

} // namespace bktga::detail

//===----------------------------------------------------------------------===//
//                              TGA Types
//===----------------------------------------------------------------------===//
constexpr ptrdiff_t tga_header_size           = 18;
constexpr ptrdiff_t tga_footer_size           = 26;
constexpr ptrdiff_t tga_footer_signature_size = 18;
constexpr ptrdiff_t tga_extension_area_size   = 495;

constexpr char const tga_signature[] {"TRUEVISION-XFILE."};

/// Color map type.
enum class tga_color_map_type : uint8_t {
    absent           = 0   //!< no pallet present
  , present          = 1   //!< pallet present

  , unknown_reserved = 127 //!< values <= 127 are reserved
  , unknown_custom         //!< 128 <= values <= 255 indicate custom types
};

/// Image type.
enum class tga_image_type : uint8_t {
    none             = 0   //!< no image data
  , color_mapped     = 1   //!< palletized image data
  , true_color       = 2   //!< non-palletized image data
  , grayscale        = 3   //!< 8-bit grayscale data
  , rle_color_mapped = 9   //!< RLE compressed palletized image data
  , rle_true_color   = 10  //!< RLE compressed non-palletized image data
  , rle_grayscale    = 11  //!< RLE compressed 8-bit grayscale data

  , unknown_reserved = 127 //!< values <= 127 are reserved
  , unknown_custom         //!< 128 <= values <= 255 indicate custom types
};

/// Image origin.
enum class tga_origin : uint8_t {
    lo_left  //!< Pixel data starts in the lower left corner
  , lo_right //!< Pixel data starts in the lower right corner
  , up_left  //!< Pixel data starts in the upper left corner
  , up_right //!< Pixel data starts in the upper right corner
};

/// Image interleave type.
/// @note this isn't supported by most TGA readers / writers.
enum class tga_interleave : uint8_t {
    none
  , even_odd
  , four_way
  , reserved
};

/// Image version.
enum class tga_version {
    invalid, v1, v2
};

/// Image descriptor.
struct tga_image_descriptor {
    tga_image_descriptor() = default;
    explicit tga_image_descriptor(uint8_t const n) noexcept
      : value {n}
    {
    }

    uint8_t attribute_bits() const noexcept {
        return static_cast<uint8_t>(value & 0b00001111);
    }

    tga_origin origin() const noexcept {
        return static_cast<bktga::tga_origin>((value & 0b00110000) >> 4);
    }

    tga_interleave interleave() const noexcept {
        return static_cast<bktga::tga_interleave>((value & uint8_t {0b11000000}) >> 6);
    }

    bool operator==(tga_image_descriptor rhs) const noexcept {
        return value == rhs.value;
    }

    uint8_t value = 0;
};

class tga_descriptor {
public:
    struct field {
        using header_t    = std::array<char, tga_header_size>;
        using footer_t    = std::array<char, tga_footer_size>;
        using signature_t = std::array<char, tga_footer_signature_size>;

        template <typename T, ptrdiff_t Offset>
        using sf = detail::static_field_t<T, Offset>;

        //header fields
        using id_length   = sf<uint8_t,              0>;
        using cmap_type   = sf<tga_color_map_type,   id_length::end>;
        using img_type    = sf<tga_image_type,       cmap_type::end>;
        using cmap_start  = sf<uint16_t,             img_type::end>;
        using cmap_len    = sf<uint16_t,             cmap_start::end>;
        using cmap_depth  = sf<uint8_t,              cmap_len::end>;
        using x_offset    = sf<uint16_t,             cmap_depth::end>;
        using y_offset    = sf<uint16_t,             x_offset::end>;
        using width       = sf<uint16_t,             y_offset::end>;
        using height      = sf<uint16_t,             width::end>;
        using pixel_depth = sf<uint8_t,              height::end>;
        using image_desc  = sf<tga_image_descriptor, pixel_depth::end>;

        //footer fields
        using ext_offset  = sf<uint32_t,             0>;
        using dev_offset  = sf<uint32_t,             ext_offset::end>;
        using signature   = sf<signature_t,          dev_offset::end>;
    };

    tga_descriptor() = default;

    template <typename Source>
    explicit tga_descriptor(Source& src)
      : tga_descriptor {src, src.size()}
    {
    }

    bool is_valid() const noexcept {
        return diagnostic.empty()
            && (version == tga_version::v1 || version == tga_version::v2);
    }

    bool is_color_mapped() const noexcept {
        return img_type == tga_image_type::color_mapped
            || img_type == tga_image_type::rle_color_mapped;
    }

    bool is_compressed() const noexcept {
        return img_type == tga_image_type::rle_color_mapped
            || img_type == tga_image_type::rle_grayscale
            || img_type == tga_image_type::rle_true_color;
    }

    ptrdiff_t pixel_count() const noexcept {
        return width * height;
    }

    ptrdiff_t color_map_size() const noexcept {
        return cmap_len * detail::round_up_bits_to_bytes(cmap_depth);
    }

    ptrdiff_t id_offset() const noexcept {
        return tga_header_size;
    }

    ptrdiff_t color_map_offset() const noexcept {
        return id_offset() + id_length;
    }

    ptrdiff_t image_data_offset() const noexcept {
        return color_map_offset() + color_map_size();
    }

    ////////////////////////////////////////////////////////////////////////////
    //                   TGA header / footer values.
    ////////////////////////////////////////////////////////////////////////////
    field::id_length::type   id_length   {};
    field::cmap_type::type   cmap_type   {tga_color_map_type::absent};
    field::img_type::type    img_type    {tga_image_type::none};
    field::cmap_start::type  cmap_start  {};
    field::cmap_len::type    cmap_len    {};
    field::cmap_depth::type  cmap_depth  {};
    field::x_offset::type    x_offset    {};
    field::y_offset::type    y_offset    {};
    field::width::type       width       {};
    field::height::type      height      {};
    field::pixel_depth::type pixel_depth {};
    field::image_desc::type  image_desc  {};
    field::ext_offset::type  ext_offset  {};
    field::dev_offset::type  dev_offset  {};
    field::signature::type   signature   {{}}; // to make older clang and gcc happy
    ////////////////////////////////////////////////////////////////////////////
    ptrdiff_t                size        {};
    tga_version              version     {tga_version::invalid};
    string_view              diagnostic  {};
private:
    static bool check_footer_signature_(field::signature_t const& sig) noexcept {
        return std::equal(begin(sig), end(sig), tga_signature);
    }

    template <typename Source>
    tga_descriptor(Source& src, ptrdiff_t const fsize)
      : id_length   {detail::read_field(field::id_length   {}, src)}
      , cmap_type   {detail::read_field(field::cmap_type   {}, src)}
      , img_type    {detail::read_field(field::img_type    {}, src)}
      , cmap_start  {detail::read_field(field::cmap_start  {}, src)}
      , cmap_len    {detail::read_field(field::cmap_len    {}, src)}
      , cmap_depth  {detail::read_field(field::cmap_depth  {}, src)}
      , x_offset    {detail::read_field(field::x_offset    {}, src)}
      , y_offset    {detail::read_field(field::y_offset    {}, src)}
      , width       {detail::read_field(field::width       {}, src)}
      , height      {detail::read_field(field::height      {}, src)}
      , pixel_depth {detail::read_field(field::pixel_depth {}, src)}
      , image_desc  {detail::read_field(field::image_desc  {}, src)}
      , ext_offset  {detail::read_field(field::ext_offset  {}, src, fsize - tga_footer_size)}
      , dev_offset  {detail::read_field(field::dev_offset  {}, src, fsize - tga_footer_size)}
      , signature   (detail::read_field(field::signature   {}, src, fsize - tga_footer_size))
      , size        {fsize}
      , version     {tga_version::v1}
      , diagnostic  {check_()}
    {
        if (check_footer_signature_(signature)) {
            version = tga_version::v2;
        } else {
            std::fill(begin(signature), end(signature), char {});
            ext_offset = 0;
            dev_offset = 0;
        }
    }

    string_view check_() const noexcept {
        auto const match_list = [](int const n, std::initializer_list<int> list) noexcept {
            return std::any_of(begin(list), end(list), [n](auto const i) noexcept {
                return i == n;
            });
        };

        auto const check_attribute_bits = [&](int const depth) noexcept -> string_view {
            bool const ok = [&] {
                auto const bits = image_desc.attribute_bits();

                switch (depth) {
                case 8  : return match_list(bits, {0});
                case 15 : return match_list(bits, {0});
                case 16 : return match_list(bits, {0, 1});
                case 24 : return match_list(bits, {0});
                case 32 : return match_list(bits, {8});
                default : break;
                }

                return false;
            }();

            return ok ? string_view {}
                      : string_view {"invalid attribute bit count"};
        };

        auto const check_color_mapped = [&]() noexcept -> string_view {
            if (cmap_type != tga_color_map_type::present) {
                return {"absent or unknown color map type"};
            }

            if (!match_list(pixel_depth, {8, 15, 16, 24, 32})) {
                return {"unexpected pixel (index) depth"};
            }

            if (!match_list(cmap_depth, {8, 15, 16, 24, 32})) {
                return {"unexpected color map depth"};
            }

            return check_attribute_bits(cmap_depth);
        };

        auto const check_true_color = [&]() noexcept -> string_view {
            if (!match_list(pixel_depth, {15, 16, 24, 32})) {
                return {"unexpected pixel (index) depth"};
            }

            return check_attribute_bits(pixel_depth);
        };

        auto const check_grayscale = [&]() noexcept -> string_view {
            if (pixel_depth != 8) {
                return {"unexpected grayscale pixel depth"};
            }

            if (image_desc.attribute_bits()) {
                return {"invalid attribute bit count"};
            }

            return {};
        };

        using im = tga_image_type;
        switch (img_type) {
        case im::none:             return {};
        case im::rle_color_mapped: BKTGA_FALLTHROUGH
        case im::color_mapped:     return check_color_mapped();
        case im::rle_true_color:   BKTGA_FALLTHROUGH
        case im::true_color:       return check_true_color();
        case im::rle_grayscale:    BKTGA_FALLTHROUGH
        case im::grayscale:        return check_grayscale();
        case im::unknown_reserved: BKTGA_FALLTHROUGH
        case im::unknown_custom:   BKTGA_FALLTHROUGH
        default:                   break;
        }

        return (detail::enum_value(img_type)
              < detail::enum_value(im::unknown_custom))
          ? string_view {"unknown reserved image type"}
          : string_view {"unknown custom image type"};
    }
};

/// The (optional) TGA color map.
struct tga_color_map {
    template <typename Source>
    tga_color_map(tga_descriptor const& tga, Source& src)
      : first_ {tga.cmap_start}
    {
        auto const field = detail::variable_field_t {
            tga.is_color_mapped() ? tga.color_map_size() : 0
          , tga.color_map_offset()
        };

        if (field.size() <= 0) {
            return;
        }

        data_.reserve(tga.cmap_len);
        auto const buffer = read_field(field, src);
        auto       data   = detail::memory_source {buffer};

        switch (tga.cmap_depth) {
        case 8  : convert_to_32bpp_< 8>(data, tga.cmap_len); break;
        case 15 : convert_to_32bpp_<15>(data, tga.cmap_len); break;
        case 16 : convert_to_32bpp_<16>(data, tga.cmap_len); break;
        case 24 : convert_to_32bpp_<24>(data, tga.cmap_len); break;
        case 32 : convert_to_32bpp_<32>(data, tga.cmap_len); break;
        default : break;
        }
    }

    uint32_t operator[](ptrdiff_t const i) const noexcept {
        auto const size = static_cast<ptrdiff_t>(data_.size());
        auto const j    = i - first_;
        return (j < size && j >= 0) ? data_[static_cast<size_t>(j)] : 0;
    }
private:
    template <size_t Bits, typename Source>
    void convert_to_32bpp_(Source& src, ptrdiff_t const n) {
        std::generate_n(std::back_inserter(data_), n, [&] {
            return detail::to_rgba<32>(detail::read_bits<Bits>(src));
        });
    }

    ptrdiff_t             first_ {};
    std::vector<uint32_t> data_  {};
};

//===----------------------------------------------------------------------===//
//                              API Types
//===----------------------------------------------------------------------===//
using decode_t = std::vector<uint32_t>;

/// The result of a call to detect.
template <typename Source>
class detect_result_t {
public:
    explicit detect_result_t(Source&& src)
      : detect_result_t {tga_descriptor {src}, std::move(src)}
    {
    }

    explicit operator bool() const noexcept {
        return tga.is_valid();
    }

    tga_descriptor tga;
    Source         source;
private:
    detect_result_t(tga_descriptor&& tga_data, Source&& src)
      : tga    {std::move(tga_data)}
      , source {std::move(src)}
    {
    }

};

struct read_from_file_t {}; ///< @see detect

constexpr read_from_file_t read_from_file {};

//===----------------------------------------------------------------------===//
//                              API Implementation
//===----------------------------------------------------------------------===//
namespace detail {

template <ptrdiff_t Bpp, typename Source, typename ColorMapper, typename OutIt>
inline void decode_rle_compressed(
    ptrdiff_t   const  count
  , Source&            src
  , ColorMapper const& cmap
  , OutIt              out
) {
    std::fill_n(out, count, cmap(read_bits<Bpp>(src)));
}

template <ptrdiff_t Bpp, typename Source, typename ColorMapper, typename OutIt>
inline void decode_rle_raw(
    ptrdiff_t   const  count
  , Source&            src
  , ColorMapper const& cmap
  , OutIt              out
) {
    std::generate_n(out, count, [&] {
        return cmap(read_bits<Bpp>(src));
    });
}

template <ptrdiff_t Bpp, typename Source, typename ColorMapper, typename OutIt>
inline void decode_rle(
    tga_descriptor const& tga
  , Source&               src
  , ColorMapper    const& cmap
  , OutIt                 out
) {
    for (ptrdiff_t i = 0; i < tga.pixel_count(); ) {
        auto const packet = read_bits<8>(src);
        bool const is_raw = !(packet & 0b1000'0000);
        auto const count  =  (packet & 0b0111'1111) + 1;

        if (is_raw) {
            decode_rle_raw<Bpp>(count, src, cmap, out);
        } else {
            decode_rle_compressed<Bpp>(count, src, cmap, out);
        }

        i += count;
    }
}

template <ptrdiff_t Bpp, typename Source, typename ColorMapper, typename OutIt>
inline void decode_direct(
    tga_descriptor const& tga
  , Source&               src
  , ColorMapper    const& cmap
  , OutIt                 out
) {
    std::generate_n(out, tga.pixel_count(), [&] {
        return cmap(read_bits<Bpp>(src));
    });
}

/// Level 3 -- Choose compression type.
template <ptrdiff_t Bpp, typename Source, typename ColorMapper>
inline decode_t decode(
    tga_descriptor const& tga
  , Source&               src
  , ColorMapper    const& cmap
) {
    decode_t result;
    result.reserve(size_t {tga.width} * size_t {tga.height});

    auto const out = back_inserter(result);
    src.seek(tga.image_data_offset());

    if (tga.is_compressed()) {
      decode_rle<Bpp>(tga, src, cmap, out);
    } else {
      decode_direct<Bpp>(tga, src, cmap, out);
    }

    return result;
}

/// Level 2 -- Choose color mapper.
template <ptrdiff_t Bpp, typename Source>
inline decode_t decode(tga_descriptor const& tga, Source& src) {
    using detail::decode;
    return tga.is_color_mapped()
      ? decode<Bpp>(tga, src
          , [m = tga_color_map(tga, src)](auto const c) noexcept -> uint32_t {
                return m[static_cast<ptrdiff_t>(c)];
            })
      : decode<Bpp>(tga, src
          , [](auto const c) noexcept -> uint32_t {
                return to_rgba<Bpp>(c);
            });
}

/// Level 1 -- Choose source pixel depth.
template <typename Source>
inline decode_t decode(tga_descriptor const& tga, Source& src) {
    switch (tga.pixel_depth) {
    case 8  : return decode<8 >(tga, src);
    case 15 : return decode<15>(tga, src);
    case 16 : return decode<16>(tga, src);
    case 24 : return decode<24>(tga, src);
    case 32 : return decode<32>(tga, src);
    default:
        break;
    }

    return {};
}

template <typename Source>
inline detect_result_t<Source> detect(Source&& source) {
    return detect_result_t<Source>{std::forward<Source>(source)};
}

} //namespace bktga::detail

//===----------------------------------------------------------------------===//
//                              API - detect
//===----------------------------------------------------------------------===//
inline auto detect(unique_file file)
    -> detect_result_t<detail::file_source>
{
    return detail::detect(detail::file_source {std::move(file)});
}

inline auto detect(read_from_file_t, string_view const filename)
    -> detect_result_t<detail::file_source>
{
    return detail::detect(detail::file_source {filename});
}

template <typename Byte, typename Size>
inline auto detect(Byte const* const data, Size const size)
    -> detect_result_t<detail::memory_source>
{
    return detail::detect(detail::memory_source {data, size});
}

template <typename Byte>
inline auto detect(Byte const* const beg, Byte const* const end)
    -> detect_result_t<detail::memory_source>
{
    return detect(beg, end - beg);
}

template <typename Container>
inline auto detect(Container const& c)
    -> detect_result_t<detail::memory_source>
{
    return detect(detail::data(c), detail::data(c) + detail::size(c));
}

//===----------------------------------------------------------------------===//
//                              API - decode
//===----------------------------------------------------------------------===//
template <typename Source>
inline decode_t decode(detect_result_t<Source>& in) {
    return in ? detail::decode(in.tga, in.source)
              : decode_t {};
}

template <typename Source>
inline decode_t decode(detect_result_t<Source>&& in) {
    return decode(in);
}

} // namespace bktga

#if defined(_MSC_VER)
#   pragma warning(pop)
#
#   if defined(BKTGA_SCL_SECURE_NO_WARNINGS)
#       undef BKTGA_CL_SECURE_NO_WARNINGS
#       undef _SCL_SECURE_NO_WARNINGS
#   endif
#   if defined(BKTGA_CRT_SECURE_NO_WARNINGS)
#       undef BKTGA_CRT_SECURE_NO_WARNINGS
#       undef _CRT_SECURE_NO_WARNINGS
#   endif
#endif
