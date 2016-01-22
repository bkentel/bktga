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
#       define BKTGA_CL_SECURE_NO_WARNINGS
#   endif
#endif

#include <boost/predef.h>

// BKTGA_STRING_VIEW The implementation to use for string_view; defaults to boost::string_ref
// BKTGA_OPTIONAL    The implementation to use for optional<T>; defaults to boost::optional<T>

#if !defined(BKTGA_STRING_VIEW)
#   include <boost/utility/string_ref.hpp>
#endif

#if !defined(BKTGA_OPTIONAL)
#   include <boost/optional.hpp>
#endif

#include <vector>
#include <array>
#include <memory>
#include <algorithm>
#include <type_traits>

#include <cstddef>
#include <cstdint>

// BKTGA_ASSERT_IMPL      The function to use for standard assertions.
// BKTGA_ASSERT_SAFE_IMPL The function to use for "safe mode" only assertions.
// BKTGA_ASSERT_OPT_IMPL  The function to use for -always- assertions.

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

#if !defined(BKTGA_OPTIONAL)
template <typename T>
using optional = ::boost::optional<T>;
using nullopt_t = ::boost::none_t;
#endif

using decode_t = std::vector<uint32_t>;

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
    explicit buffer(size_t const size)
      : size_ {static_cast<ptrdiff_t>(size)}
      , data_ {new char[size]}
    {
    }

    explicit buffer(ptrdiff_t const size)
      : buffer {static_cast<size_t>(size > 0 ? size : 0)}
    {
    }

    ptrdiff_t   size()  const noexcept { return size_; }
    char const* data()  const noexcept { return data_.get(); }
    char*       data()        noexcept { return data_.get(); }
    char const* begin() const noexcept { return data(); }
    char const* end()   const noexcept { return data() + size_; }
private:
    ptrdiff_t                size_;
    std::unique_ptr<char []> data_;
};

/// Minimum value clamped to a floor of 0.
template <typename T>
inline constexpr T min_0(T const a, T const b) noexcept {
    return (a <= b && a > 0) ? a :
           (          b > 0) ? b : T {0};
}

/// @see min_0(a, b)
template <typename T>
inline constexpr T min_0(T const a, T const b, T const c) noexcept {
    return min_0(min_0(a, b), c);
}

} // namespace bktga::detail

//===----------------------------------------------------------------------===//
//                              I/O Functions
//===----------------------------------------------------------------------===//
namespace detail {
/// Convert a little endian integer to a host integer of at least the same size.
#if defined(BOOST_ENDIAN_LITTLE_BYTE_AVAILABLE)
template <typename T>
inline constexpr T little_endian_to_host(T const n) noexcept {
    static_assert(std::is_integral<T>::value, "");
    return n;
}

template <ptrdiff_t Bpp>
uint32_t to_rgba(uint32_t n) = delete;

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

    return to_rgba<32>(n) & 0x00FFFFFF;
}

inline uint32_t constexpr to_rgba_16_mask(uint32_t const n, int const i) noexcept {
    return (n >> i * 5) & 0b11111;
}

inline uint32_t constexpr to_rgba_16_expand(uint32_t const n, int const i) noexcept {
    return ((n << 3) | (n >> 2)) << i;
}

template <> inline constexpr uint32_t to_rgba<16>(uint32_t const n) noexcept {
    // n      -> A BBBBB GGGGG RRRRR
    // result -> 0xAABBGGRR

    return to_rgba_16_expand(to_rgba_16_mask(n, 0), 0) // red
         | to_rgba_16_expand(to_rgba_16_mask(n, 0), 0) // green
         | to_rgba_16_expand(to_rgba_16_mask(n, 0), 0) // blue
         | (((n & 0x8000) >> 15) * 0xFF);              // alpha
}

template <> inline constexpr uint32_t to_rgba<15>(uint32_t const n) noexcept {
    // n      -> X BBBBB GGGGG RRRRR
    // result -> 0x00BBGGRR

    return to_rgba<16>(n) & 0x7FFF;
}

template <> inline constexpr uint32_t to_rgba<8>(uint32_t const n) noexcept {
    // n      -> 0xXXXXXXNN
    // result -> 0x00NNNNNN

    return ((n & 0xFF) <<  0)  // red
         | ((n & 0xFF) <<  8)  // green
         | ((n & 0xFF) << 16); // blue
}
#else
#   error not implemented
#endif

/// filesystem data source
class file_source {
public:
    file_source(
        char const* const fname
      , ptrdiff_t   const first
      , ptrdiff_t   const last
    )
      : first_  {first}
      , last_   {last}
      , handle_ {do_open_(fname)}
    {
    }

    explicit file_source(string_view const fname)
      : file_source {fname.to_string().c_str(), 0, 0}
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
        ptrdiff_t const n        //!< number of bytes to read
      , char*     const out      //!< output buffer
      , ptrdiff_t const out_size //!< size of the output buffer
    ) noexcept {
        auto const result = static_cast<ptrdiff_t>(
            std::fread(out, 1
              , static_cast<size_t>(min_0(n, out_size)), *this));

        std::fill_n(out + result, min_0(n - result, out_size - result), char {});
    }

    void read_at(
        ptrdiff_t const n        //!< number of bytes to read
      , ptrdiff_t const offset   //!< offset from start
      , char*     const out      //!< output buffer
      , ptrdiff_t const out_size //!< size of the output buffer
    ) noexcept {
        seek(offset);
        read(n, out, out_size);
    }

    void seek(ptrdiff_t const offset) {
        std::fseek(*this, static_cast<long>(offset), SEEK_SET);
    }
private:
    using handle_t = std::unique_ptr<FILE, decltype(&std::fclose)>;

    static handle_t do_open_(char const* const fname) noexcept {
        FILE* handle {};
        if (auto result = fopen_s(&handle, fname, "rb")) {
            return {nullptr, std::fclose};
        }

        return {handle, std::fclose};
    }

    operator FILE*() const noexcept { return handle_.get(); }

    ptrdiff_t first_;
    ptrdiff_t last_;
    handle_t  handle_;
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
      : memory_source {in.data(), in.size()}
    {
    }

    ptrdiff_t size() const noexcept {
        return last_ - first_;
    }

    void read(
        ptrdiff_t const n        //!< number of bytes to read
      , char*     const out      //!< output buffer
      , ptrdiff_t const out_size //!< size of the output buffer
    ) noexcept {
        auto const n0 = min_0(last_ - pos_, n, out_size);
        std::copy_n(pos_, n0, out);
        pos_ += n0;

        auto const n1 = min_0(n - n0, out_size - n0);
        std::fill_n(out + n0, n1, char {});
    }

    void read_at(
        ptrdiff_t const n        //!< number of bytes to read
      , ptrdiff_t const offset   //!< offset from start
      , char*     const out      //!< output buffer
      , ptrdiff_t const out_size //!< size of the output buffer
    ) noexcept {
        seek(offset);
        read(n, out, out_size);
    }

    void seek(ptrdiff_t const offset) {
        pos_ = (offset < 0)       ? first_
             : (offset >= size()) ? last_
             : first_ + offset;
    }
private:
    char const* first_;
    char const* last_;
    char const* pos_;
};

/// read at least Bits bits from in at the current position.
template <ptrdiff_t Bits, typename Source>
inline auto read_bits(Source& in) {
    constexpr ptrdiff_t bytes = round_up_bits_to_bytes(Bits);
    uint_t<bytes> out {};
    in.read(bytes, reinterpret_cast<char*>(&out), sizeof(out));
    return out;
}

/// read exactly Bytes bytes from in starting at offset.
template <ptrdiff_t Bytes, typename Source>
inline auto read_bytes_at(Source& in, ptrdiff_t const offset) {
    uint_t<Bytes> out {};
    in.read_at(Bytes, offset, reinterpret_cast<char*>(&out), sizeof(out));
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
    src.read_at(field.size(), field.begin() + offset, out.data(), out.size());
    return out;
}

template <typename Field, typename Source, typename T = typename Field::type>
inline T read_field(read_field_array_tag, Source& src, ptrdiff_t const offset = 0) {
    T out;
    src.read_at(Field::size, Field::begin + offset
      , reinterpret_cast<char*>(&out), Field::size);

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

/// Color map type.
enum class tga_color_map_type : uint8_t {
    absent           = 0   ///< no pallet present
  , present          = 1   ///< pallet present

  , unknown_reserved = 127 ///< values <= 127 are reserved
  , unknown_custom         ///< 128 <= values <= 255 indicate custom types
};

/// Image type.
enum class tga_image_type : uint8_t {
    none             = 0   ///< no image data
  , color_mapped     = 1   ///< palletized image data
  , true_color       = 2   ///< non-palletized image data
  , grayscale        = 3   ///< 8-bit grayscale data
  , rle_color_mapped = 9   ///< RLE compressed palletized image data
  , rle_true_color   = 10  ///< RLE compressed non-palletized image data
  , rle_grayscale    = 11  ///< RLE compressed 8-bit grayscale data

  , unknown_reserved = 127 ///< values <= 127 are reserved
  , unknown_custom         ///< 128 <= values <= 255 indicate custom types
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
private:
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
public:
    tga_descriptor() = default;

    template <typename Source>
    explicit tga_descriptor(Source& src)
      : tga_descriptor {src, src.size()}
    {
    }

    bool is_valid() const noexcept {
        return version == tga_version::v1 || version == tga_version::v2;
    }

    bool uses_color_map() const noexcept {
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
private:
    template <typename Source>
    tga_descriptor(Source& src, ptrdiff_t const size)
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
      , ext_offset  {detail::read_field(field::ext_offset  {}, src, size - tga_footer_size)}
      , dev_offset  {detail::read_field(field::dev_offset  {}, src, size - tga_footer_size)}
      , signature   {detail::read_field(field::signature   {}, src, size - tga_footer_size)}
      , size        {size}
      , version     {tga_version::v1}
    {
    }
};

/// The (optional) TGA color map.
struct tga_color_map {
    template <typename Source>
    tga_color_map(tga_descriptor const& tga, Source& src)
      : first_ {tga.cmap_start}
    {
        auto const field = detail::variable_field_t {
            tga.uses_color_map() ? tga.color_map_size() : 0
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
        auto const j    = i - first_;
        auto const size = static_cast<ptrdiff_t>(data_.size());
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
/// The result of a call to detect.
template <typename Source>
struct detect_result_t {
    string_view              result;
    Source                   source;
    optional<tga_descriptor> tga;

    explicit operator bool() const noexcept { return !!tga; }
};

struct read_from_file_t {}; ///< @see detect

constexpr read_from_file_t read_from_file;

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
    return tga.uses_color_map()
      ? decode<Bpp>(tga, src
          , [m = tga_color_map(tga, src)](auto const c) noexcept -> uint32_t {
                return m[c];
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
    detect_result_t<Source> result {
        "Success", std::move(source), nullptr_t {}};

    result.tga = tga_descriptor(result.source);
    if (!result.tga->is_valid()) {
        result.tga = nullopt_t {};
    }

    return result;
}

} //namespace bktga::detail

//===----------------------------------------------------------------------===//
//                              API - detect
//===----------------------------------------------------------------------===//
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

template <typename Byte, size_t N>
inline auto detect(std::array<Byte, N> const& data)
    -> detect_result_t<detail::memory_source>
{
    return detect(data.data(), data.data() + N);
}

template <typename Byte, size_t N>
inline auto detect(Byte const (&data)[N])
    -> detect_result_t<detail::memory_source>
{
    return detect(data + 0, data + N);
}

//===----------------------------------------------------------------------===//
//                              API - decode
//===----------------------------------------------------------------------===//
template <typename Source>
inline decode_t decode(detect_result_t<Source>& in) {
    return in ? detail::decode(*in.tga, in.source)
              : decode_t {};
}

} // namespace bktga

#if defined(_MSC_VER)
#   pragma warning(pop)
#
#   if defined(BKTGA_CL_SECURE_NO_WARNINGS)
#       undef BKTGA_CL_SECURE_NO_WARNINGS
#       undef _SCL_SECURE_NO_WARNINGS
#   endif
#endif
