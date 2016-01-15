#pragma once

#if defined(_MSC_VER)
#   define _SCL_SECURE_NO_WARNINGS // TODO undefine
#endif

// BKTGA_STRING_VIEW The implementation to use for string_view; defaults to boost::string_ref
// BKTGA_OPTIONAL    The implementation to use for optional<T>; defaults to boost::optional<T>

#if !defined(BKTGA_STRING_VIEW)
#   include <boost/utility/string_ref.hpp>
#endif

#if !defined(BKTGA_OPTIONAL)
#   include <boost/optional.hpp>
#endif

#include <istream>
#include <fstream>
#include <vector>
#include <array>
#include <memory>
#include <tuple>
#include <utility>
#include <limits>
#include <type_traits>

#include <cstdint>

// BKTGA_ASSERT_IMPL      The function to use for standard assertions.
// BKTGA_ASSERT_SAFE_IMPL The function to use for "safe mode" only assertions.
// BKTGA_ASSERT_OPT_IMPL  The function to use for -always- assertions.
//

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

#if !defined(BKTGA_STRING_VIEW)
using string_view = boost::string_ref;
#endif

#if !defined(BKTGA_OPTIONAL)
template <typename T>
using optional = boost::optional<T>;
#endif

using std::uint8_t;
using std::uint16_t;
using std::uint32_t;

static constexpr size_t header_size    = 18u;
static constexpr size_t footer_size    = 26u;
static constexpr size_t extension_size = 495u;

namespace detail {

static constexpr auto all_exceptions =
    std::ios::badbit | std::ios::eofbit | std::ios::failbit;

//==============================================================================
//! An unsigned type of exactly N bytes, or void.
//==============================================================================
template <size_t N>
using uint_t = std::conditional_t<N < 1, void,
               std::conditional_t<N < 2, std::uint8_t,
               std::conditional_t<N < 3, std::uint16_t,
               std::conditional_t<N < 5, std::uint32_t,
               std::conditional_t<N < 9, std::uint64_t, void>>>>>;

//==============================================================================
//! Return the number of bytes (rounded up) required for @p n bits where
//! @p n >= 0; otherwise 0.
//!
//! @tparam T An integral type.
//! @param[in] n The number of bits.
//! @return The number of bits as an unsigned integer type.
//==============================================================================
template <typename T>
inline constexpr auto round_up_bits_to_bytes(T const n) noexcept {
    static_assert(std::is_integral<T>::value, "");

    using R = std::conditional_t<sizeof(T) <= sizeof(size_t), size_t, uintmax_t>;

    return (n > 0) && (static_cast<uintmax_t>(n)
                        <= std::numeric_limits<uintmax_t>::max() - 7u)
      ? static_cast<R>((static_cast<uintmax_t>(n) + 7u) / 8u)
      : R {0};
}

//==============================================================================
//! Get the value of an enum as its underlying type.
//! @tparam T An enumerator type.
//! @param[in] value The enumerator value.
//==============================================================================
template <typename T>
inline constexpr auto underlying_value(T const value) noexcept {
    static_assert(std::is_enum<T>::value, "");
    return static_cast<std::underlying_type_t<T>>(value);
}

//==============================================================================
//! Synonym for std::integral_constant
//==============================================================================
template <size_t Bits>
using bit_width = std::integral_constant<size_t, Bits>;

//==============================================================================
//! 8-bit to 32-bit (grayscale)
//==============================================================================
inline uint32_t to_rgba(uint32_t const n, bit_width<8>) noexcept {
    auto const c = n & 0xFFu;
    return (c << 0) | (c << 8) | (c << 16) | 0xFF000000u;
}

//==============================================================================
//! 15-bit to 32-bit (alpha 255)
//==============================================================================
inline uint32_t to_rgba(uint32_t const n, bit_width<15>) noexcept {
    auto const get = [n](uint32_t const i) noexcept {
        uint32_t const b5 = (n >> i * 5) & 0b11111u;
        uint32_t const b8 = (b5 << 3) | (b5 >> 2);
        return b8 << i * 8;
    };

    //     red      green    blue     alpha
    return get(0) | get(1) | get(2) | 0xFF000000u;
}

//==============================================================================
//! 16-bit to 32-bit (alpha 0 or 255)
//==============================================================================
inline uint32_t to_rgba(uint32_t const n, bit_width<16>) noexcept {
    auto const a = 0x00FFFFFFu | ((((n >> 15) & 1u) * 0xFFu) << 24);
    return to_rgba(n, bit_width<15> {}) & a;
}

//==============================================================================
//! 24-bit to 32-bit (alpha 255)
//==============================================================================
inline uint32_t to_rgba(uint32_t const n, bit_width<24>) noexcept {
    return n | 0xFF000000u;
}

//==============================================================================
//! 32-bit to 32-bit (noop)
//==============================================================================
inline uint32_t to_rgba(uint32_t const n, bit_width<32>) noexcept {
    return n;
}

//==============================================================================
//! Disable other conversions.
//==============================================================================
template <typename T, typename U>
uint32_t to_rgba(T const n, U) = delete;

//==============================================================================
//! Family of functions to convert from one color depth to 32-bit RGBA.
//! The layout of the bytes will be of the form 0xAABBGGRR.
//==============================================================================
template <size_t Bits, typename T>
inline uint32_t to_rgba(T const n) noexcept {
    static_assert(std::is_integral<T>::value, "");

    uint32_t const m = static_cast<uint32_t>(
        static_cast<std::make_unsigned_t<T>>(n) & ~uint32_t {0});
    return to_rgba(m, bit_width<Bits> {});
}

//==============================================================================
//! Construct an unsigned integer from @p N bytes of data from the range given
//! by [@p it, @p end); the @p N bytes of data are interpreted as being in
//! little endian order.
//!
//! @tparam N A number of bytes to read; 0 <= N <= 8.
//!
//! @return A tuple: (value, next) where is it + N, or end.
//==============================================================================
template <size_t N, typename R = uint_t<N>, typename InputIt>
inline auto read_n_bytes(InputIt it, InputIt const end)
 -> std::tuple<R, InputIt>
{
    static_assert(std::is_unsigned<R>::value, "");
    static_assert(N <= sizeof(R), "");

    using value_t = std::remove_reference_t<decltype(*it)>;
    static_assert(std::is_integral<value_t>::value, "");
    using common_t = std::common_type_t<unsigned, R>;

    common_t result {0};

    for (size_t i = 0; i < N && it != end; ++i, ++it) {
        auto     const uvalue = static_cast<std::make_unsigned_t<value_t>>(*it);
        common_t const byte   = static_cast<uint8_t>(uvalue & 0xFFu);

        result |= byte << i * 8;
    }

    return std::make_tuple(static_cast<R>(result), it);
}

template <typename T, typename InputIt>
inline auto read_n_bytes(InputIt const it, InputIt const end)
 -> std::tuple<T, InputIt>
{
    return read_n_bytes<sizeof(T)>(it, end);
}

struct read_pod_construct_tag {};
struct read_pod_convert_tag   {};
struct read_pod_container_tag {};
struct read_pod_stream_tag    {};

//==============================================================================
//! Read a T which can be cast to from an unsigned value.
//==============================================================================
template <typename T, typename InputIt>
inline T read_pod(read_pod_convert_tag, InputIt const first, InputIt const last) {
    using U = uint_t<sizeof(T)>;
    return static_cast<T>(std::get<0>(read_n_bytes<U>(first, last)));
}

//==============================================================================
//! Read a T which can be list-initialized from an unsigned value.
//==============================================================================
template <typename T, typename InputIt>
inline T read_pod(read_pod_construct_tag, InputIt const first, InputIt const last) {
    using U = uint_t<sizeof(T)>;
    return T {std::get<0>(read_n_bytes<U>(first, last))};
}

//==============================================================================
//! Construct a T from the stream of bytes given by [first, last).
//==============================================================================
template <typename T, typename InputIt>
inline T read_pod(InputIt const first, InputIt const last) {
    using Tag = std::conditional_t<
        std::is_fundamental<T>::value || std::is_enum<T>::value
      , read_pod_convert_tag
      , read_pod_construct_tag>;

    return read_pod<T>(Tag {}, first, last);
}

//==============================================================================
//! Construct a T from the container of bytes given by c.
//==============================================================================
template <typename T, typename Container>
inline T read_pod(read_pod_container_tag, Container&& c) {
    using std::begin;
    using std::end;

    auto const first = begin(c);
    auto const last  = end(c);

    return read_n_byte_int(first, last);
}

//==============================================================================
//! Construct a T from the stream of bytes given by in.
//==============================================================================
template <typename T, typename Stream>
inline T read_pod(read_pod_stream_tag, Stream&& in) {
    std::istreambuf_iterator<char> first {in};
    std::istreambuf_iterator<char> last  {};

    return read_pod<T>(first, last);
}

//==============================================================================
//! Construct a T from the source given by in. In must model a container or a
//! stream.
//==============================================================================
template <typename T, typename Source>
inline T read_pod(Source&& in) {
    using Tag = std::conditional_t<
        std::is_constructible<
            std::istreambuf_iterator<char>, Source>::value
      , read_pod_stream_tag
      , read_pod_container_tag
    >;

    return read_pod<T>(Tag {}, in);
}

//==============================================================================
//! Construct a T from the container given by c starting at the offset given by
//! Offset.
//==============================================================================
template <typename T, ptrdiff_t Offset, typename Container>
inline T read_pod_at(Container&& c) {
    using std::begin;
    using std::end;

    auto const first = std::next(begin(c), Offset);
    auto const last  = end(c);

    return read_pod<T>(first, last);
}

//==============================================================================
//! Construct an unsigned integer by reading N bits (ceil(N / 8) bytes) from the
//! range of bytes given by [it, end).
//!
//! @return A tuple: (value, next) where next is (it + ceil(N / 8)) or end.
//==============================================================================
template <size_t N, typename InputIt>
inline auto read_n_bits(InputIt const it, InputIt const end) {
    static_assert(N > 0 && N <= 32, "");

    constexpr size_t bytes = round_up_bits_to_bytes(N);
    using T = uint_t<bytes>;

    using common_t = std::common_type_t<size_t, T>;

    constexpr common_t size  = sizeof(T) * 8;
    constexpr common_t extra = size - N;
    constexpr common_t shift = extra ? N : 0;
    constexpr common_t mask  = ~(((1u << extra) - 1u) << shift);

    auto const result = read_n_bytes<bytes>(it, end);
    auto const bits   = std::get<0>(result) & mask;

    return std::make_tuple(static_cast<T>(bits), std::get<1>(result));
}

//==============================================================================
//! Fill a temporary buffer with size(range) bytes from @p in.
//==============================================================================
template <typename Range>
inline auto read_to_buffer(std::istream& in, Range const range)
  -> std::unique_ptr<char []>
{
    static_assert(std::tuple_size<Range>::value == 2, "");
    BKTGA_ASSERT_OPT(in.exceptions() == all_exceptions);

    auto const first = std::get<0>(range);
    auto const last  = std::get<1>(range);

    BKTGA_ASSERT(first >= 0 && first <= last);

    auto const size = static_cast<size_t>(last - first);

    std::unique_ptr<char []> result {new char[size]};

    in.seekg(first, std::ios::beg);
    in.read(result.get(), static_cast<std::streamsize>(size));

    return result;
}

//==============================================================================
//! Flip an image horizontally (around the y axis).
//! @param[in,out] data The data to flip in place.
//! @param[in] w The width, in pixels, of the data.
//! @param[in] h The height, in pixels, of the data.
//! @pre data must point to a buffer of at least w*h*sizeof(T) bytes.
//==============================================================================
template <typename T>
inline void flip_horizontal(
    T*     const data
  , size_t const w
  , size_t const h
) noexcept {
    static_assert(std::is_pod<T>::value, "");

    BKTGA_ASSERT_OPT(data);

    for (size_t y = 0; y < h; ++y) {
        auto const row_offset = y * w;
        for (size_t x = 0; x < w / 2; ++x) {
            std::swap(data[row_offset + (        x)]
                    , data[row_offset + (w - 1 - x)]);
        }
    }
}

//==============================================================================
//! Flip an image vertically (around the x axis).
//! @param[in,out] data The data to flip in place.
//! @param[in] w The width, in pixels, of the data.
//! @param[in] h The height, in pixels, of the data.
//! @pre data must point to a buffer of at least w*h*sizeof(T) bytes.
//==============================================================================
template <typename T>
inline void flip_vertical(
    T*     const data
  , size_t const w
  , size_t const h
) {
    static_assert(std::is_pod<T>::value, "");

    BKTGA_ASSERT_OPT(data);

    std::vector<T> line;
    line.resize(w);

    for (size_t y = 0; y < h / 2; ++y) {
        auto const l0 = (        y) * w;
        auto const l1 = (h - 1 - y) * w;

        //swap
        std::copy_n(&data[l0],   w, line.data());
        std::copy_n(&data[l1],   w, &data[l0]);
        std::copy_n(line.data(), w, &data[l1]);
    }
}

} //namespace detail

//==============================================================================
//! Color map type.
//==============================================================================
enum class tga_color_map_type : uint8_t {
    absent           = 0   //!< no pallet present
  , present          = 1   //!< pallet present

  , unknown_reserved = 127 //!< values <= 127 are reserved
  , unknown_custom         //!< 128 <= values <= 255 indicate custom types
};

//==============================================================================
//! Image type.
//==============================================================================
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

//==============================================================================
//!
//==============================================================================
inline constexpr bool is_compressed_rle(tga_image_type const type) noexcept {
    using it = bktga::tga_image_type;

    return type == it::rle_color_mapped
        || type == it::rle_grayscale
        || type == it::rle_true_color;
}

//==============================================================================
//!
//==============================================================================
inline constexpr bool has_color_map(tga_image_type const type) noexcept {
    using it = bktga::tga_image_type;

    return type == it::rle_color_mapped
        || type == it::color_mapped;
}

//==============================================================================
//! Image origin.
//==============================================================================
enum class tga_origin : uint8_t {
    lo_left  //!< Pixel data starts in the lower left corner
  , lo_right //!< Pixel data starts in the lower right corner
  , up_left  //!< Pixel data starts in the upper left corner
  , up_right //!< Pixel data starts in the upper right corner
};

//==============================================================================
//! Image interleave type.
//! @note this isn't supported by most TGA readers / writers.
//==============================================================================
enum class tga_interleave : uint8_t {
    none
  , even_odd
  , four_way
  , reserved
};

//==============================================================================
//! Image version.
//==============================================================================
enum class tga_version {
    invalid, v1, v2
};

//==============================================================================
//! Image descriptor.
//==============================================================================
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

//==============================================================================
//! The header and footer information for a TGA file.
//==============================================================================
struct tga_descriptor {
    using header_t    = std::array<char, header_size>;
    using footer_t    = std::array<char, footer_size>;
    using signature_t = std::array<char, 18>;

    struct field {
        template <typename T, ptrdiff_t Offset>
        struct field_t {
            using type = T;
            static constexpr auto offset = Offset;
        };

        using id_length   = field_t<uint8_t,              0x00>;
        using cmap_type   = field_t<tga_color_map_type,   0x01>;
        using img_type    = field_t<tga_image_type,       0x02>;
        using cmap_start  = field_t<uint16_t,             0x03>;
        using cmap_len    = field_t<uint16_t,             0x05>;
        using cmap_depth  = field_t<uint8_t,              0x07>;
        using x_offset    = field_t<uint16_t,             0x08>;
        using y_offset    = field_t<uint16_t,             0x0A>;
        using width       = field_t<uint16_t,             0x0C>;
        using height      = field_t<uint16_t,             0x0E>;
        using pixel_depth = field_t<uint8_t,              0x10>;
        using image_desc  = field_t<tga_image_descriptor, 0x11>;
        using ext_offset  = field_t<uint32_t,             0x00>;
        using dev_offset  = field_t<uint32_t,             0x04>;
        using signature   = field_t<signature_t,          0x08>;

        template <typename Field, typename From>
        inline static decltype(auto) read(Field, From&& from) {
            return detail::read_pod_at<
                typename Field::type, Field::offset>(from);
        }
    };

    static header_t read_header(std::istream& in) {
        header_t buffer;

        in.seekg(0, std::ios::beg);
        in.read(buffer.data(), header_size);

        return buffer;
    }

    static footer_t read_footer(std::istream& in) {
        footer_t buffer;

        in.seekg(-static_cast<ptrdiff_t>(footer_size), std::ios::end);
        in.read(buffer.data(), footer_size);

        return buffer;
    }

    static signature_t get_signature(footer_t const& footer) {
        signature_t result;

        std::copy(footer.begin() + field::signature::offset
                , footer.end()
                , result.begin());

        return result;
    }

    static size_t get_size(std::istream& in) {
        auto const beg = in.seekg(0, std::ios::beg).tellg();
        auto const end = in.seekg(0, std::ios::end).tellg();

        in.seekg(0, std::ios::beg);

        return static_cast<size_t>(end - beg);
    }

    std::vector<char> get_id(std::istream& in) const {
        auto const range   = id_range();
        auto const id_size = std::get<1>(range) - std::get<0>(range);

        if (id_size <= 0) {
            return {};
        }

        auto const buffer = detail::read_to_buffer(in, range);
        auto const beg    = buffer.get();
        auto const end    = beg + id_size;

        return std::vector<char>(beg, end);
    }

    tga_descriptor() = default;

    tga_descriptor(
        std::istream& in
      , header_t const header
      , footer_t const footer
      , size_t   const total_size
    )
      : id_length   {field::read(field::id_length   {}, header)}
      , cmap_type   {field::read(field::cmap_type   {}, header)}
      , img_type    {field::read(field::img_type    {}, header)}
      , cmap_start  {field::read(field::cmap_start  {}, header)}
      , cmap_len    {field::read(field::cmap_len    {}, header)}
      , cmap_depth  {field::read(field::cmap_depth  {}, header)}
      , x_offset    {field::read(field::x_offset    {}, header)}
      , y_offset    {field::read(field::y_offset    {}, header)}
      , width       {field::read(field::width       {}, header)}
      , height      {field::read(field::height      {}, header)}
      , pixel_depth {field::read(field::pixel_depth {}, header)}
      , image_desc  {field::read(field::image_desc  {}, header)}
      , ext_offset  {field::read(field::ext_offset  {}, footer)}
      , dev_offset  {field::read(field::dev_offset  {}, footer)}
      , signature   (get_signature(footer))  // () instead of {} to make older clang and gcc happy
      , size        {total_size}
      , version     {validate(in)}
      , id          {get_id(in)}
    {
    }

    explicit tga_descriptor(std::istream& in)
      : tga_descriptor {in, read_header(in), read_footer(in), get_size(in)}
    {
    }

    static bool has_footer(signature_t const& signature) noexcept {
        constexpr char footer_signature[] {"TRUEVISION-XFILE."};

        constexpr auto size0 = std::remove_reference_t<decltype(signature)>().size();
        constexpr auto size1 = std::extent<decltype(footer_signature)>::value;

        static_assert(size0 == size1, "");

        return std::char_traits<char>::compare(
            footer_signature, signature.data(), size0) == 0;
    }

    enum class footer_result_t {
        absent  //!< no signature present.
      , present //!< valid footer.
      , invalid //!< signature present, but otherwise invalid.
    };

    std::pair<footer_result_t, string_view> validate_footer(std::istream& in) const {
        if (!has_footer(signature)) {
            return {footer_result_t::absent, {}};
        }

        if (dev_offset) {
            if (dev_offset > size) {
                return {footer_result_t::invalid, "Bad developer section offset."};
            }

            in.seekg(static_cast<std::streamoff>(dev_offset));
            auto const tag_count =
                detail::read_pod<uint16_t>(in);

            auto const expected_size = sizeof(uint16_t)
              + tag_count * (sizeof(uint16_t) + sizeof(uint32_t) + sizeof(uint32_t));

            if (dev_offset + expected_size > size) {
                return {footer_result_t::invalid, "Unexpected developer section size."};
            }
        }

        if (ext_offset) {
            if (ext_offset > size) {
                return {footer_result_t::invalid, "Bad extension section offset."};
            }

            in.seekg(static_cast<std::streamoff>(ext_offset));
            auto const ext_size =
                detail::read_pod<uint16_t>(in);

            if (ext_size < extension_size) {
                return {footer_result_t::invalid, "Unexpected developer section size."};
            }

            if (ext_offset + ext_size > size) {
                return {footer_result_t::invalid, "Bad developer section size."};
            }

            //TODO scanline
            //TODO stamp image
            //TODO color correction
        }

        return {footer_result_t::present, {}};
    }

    template <tga_image_type Value>
    using image_type_t = std::integral_constant<tga_image_type, Value>;

    template <tga_image_type Value>
    bool validate_image_type(image_type_t<Value>) const {
        return false;
    }

    bool validate_image_type(image_type_t<tga_image_type::none>) const {
        if (width || height) {
            return false;
        }

        return true;
    }

    bool validate_image_type(image_type_t<tga_image_type::rle_color_mapped>) const {
        if (pixel_depth != 8) {
            return false;
        }



        return true;
    }

    bool validate_image_type() const {
        using it = tga_image_type;

        switch (img_type) {
        case it::none:             return validate_image_type(image_type_t<it::none> {});
        case it::color_mapped:     return validate_image_type(image_type_t<it::color_mapped> {});
        case it::true_color:       return validate_image_type(image_type_t<it::true_color> {});
        case it::grayscale:        return validate_image_type(image_type_t<it::grayscale> {});
        case it::rle_color_mapped: return validate_image_type(image_type_t<it::rle_color_mapped> {});
        case it::rle_true_color:   return validate_image_type(image_type_t<it::rle_true_color> {});
        case it::rle_grayscale:    return validate_image_type(image_type_t<it::rle_grayscale> {});
        case it::unknown_reserved: return validate_image_type(image_type_t<it::unknown_reserved> {});
        case it::unknown_custom:   return validate_image_type(image_type_t<it::unknown_custom> {});
        }

        return false;
    }

    tga_version validate(std::istream& in) const {
        auto const footer_result_value = validate_footer(in);
        auto const footer_result = std::get<0>(footer_result_value);

        if (footer_result == footer_result_t::invalid) {
            return tga_version::invalid;
        }

        auto const ver = (footer_result == footer_result_t::present)
          ? tga_version::v2
          : tga_version::v1;

        validate_image_type();

        return ver;
    }

    ////////////////////////////////////////////////////////////////////////////
    bool has_color_map() const noexcept {
        return cmap_type == tga_color_map_type::present;
    }

    ////////////////////////////////////////////////////////////////////////////
    using range_t = std::pair<ptrdiff_t, ptrdiff_t>;

    range_t id_range() const noexcept {
        auto const beg = ptrdiff_t {header_size};
        auto const end = ptrdiff_t {beg + id_length};
        return {beg, end};
    }

    range_t color_map_range() const noexcept {
        auto const map_size = static_cast<ptrdiff_t>(
            cmap_len * detail::round_up_bits_to_bytes(cmap_depth));
        auto const beg = ptrdiff_t {std::get<1>(id_range())};
        auto const end = ptrdiff_t {beg + map_size};
        return {beg, end};
    }

    range_t image_data_range() const noexcept {
        BKTGA_ASSERT(version != tga_version::invalid);

        auto const beg = std::get<1>(color_map_range());
        auto const data_end = [&] {
            if (version == tga_version::v1) {
                return static_cast<ptrdiff_t>(size);
            }

            if (ext_offset && dev_offset) {
                return static_cast<ptrdiff_t>(std::min(ext_offset, dev_offset));
            }

            if (!ext_offset && !dev_offset) {
                return static_cast<ptrdiff_t>(size - footer_size);
            }

            return static_cast<ptrdiff_t>(ext_offset ? ext_offset : dev_offset);
        }();

        auto const data_size = static_cast<ptrdiff_t>(width * height
              * detail::round_up_bits_to_bytes(pixel_depth));

        auto const end = is_compressed_rle(img_type)
          ? data_end
          : beg + data_size;

        return {beg, end};
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
    size_t                   size        {};
    tga_version              version     {tga_version::invalid};
    std::vector<char>        id          {};
private:

};

//==============================================================================
//! The (optional) TGA color map.
//==============================================================================
struct tga_color_map {
    tga_color_map(tga_descriptor const& descriptor, std::istream& in)
      : first_ {descriptor.cmap_start}
      , len_   {descriptor.cmap_len}
    {
        // no map present
        if (!descriptor.has_color_map()) {
            return;
        }

        // present, but no entries
        if (!len_) {
            return;
        }

        auto const size =
            detail::round_up_bits_to_bytes(descriptor.cmap_depth) * len_;

        auto const range = descriptor.color_map_range();
        BKTGA_ASSERT_OPT(static_cast<size_t>(range.second - range.first) == size);

        auto const input_buffer = detail::read_to_buffer(in, range);

        auto const first = input_buffer.get();
        auto const last  = first + size;

        data_.reserve(len_);

        switch (descriptor.cmap_depth) {
        case 8u  : convert_color_map_data_< 8>(first, last); break;
        case 15u : convert_color_map_data_<15>(first, last); break;
        case 16u : convert_color_map_data_<16>(first, last); break;
        case 24u : convert_color_map_data_<24>(first, last); break;
        case 32u : convert_color_map_data_<32>(first, last); break;
        default:
            //TODO
            break;
        }

        BKTGA_ASSERT_OPT(data_.size() == len_);
    }

    auto   begin() const noexcept { return std::begin(data_); }
    auto   end()   const noexcept { return std::end(data_); }
    size_t size()  const noexcept { return data_.size(); }

    uint32_t operator[](size_t const i) const noexcept {
        return ((i >= first_) && (i < size()))
          ? data_[i - first_]
          : 0;
    }
private:
    template <size_t Bits, typename InputIt>
    void convert_color_map_data_(InputIt const first, InputIt const last) {
        auto it  = first;
        auto out = std::back_inserter(data_);

        while (it != last) {
            auto const result = detail::read_n_bits<Bits>(it, last);

            *out++ = detail::to_rgba<Bits>(std::get<0>(result));
            it     = std::get<1>(result);
        }
    }

    std::vector<uint32_t> data_  {};
    size_t                first_ {};
    size_t                len_   {};
};

namespace detail {

//==============================================================================
//! Run-length encoded packet info.
//==============================================================================
struct rle_packet {
    template <typename T, typename = std::enable_if_t<
        std::is_integral<T>::value && sizeof(T) == 1>>
    explicit rle_packet(T const n)
      : value {static_cast<uint8_t>(n)}
    {
    }

    bool is_raw() const noexcept {
        return !(value & 0b1000'0000);
    }

    size_t size() const noexcept {
        return 1u + (value & 0b0111'1111);
    }

    uint8_t value;
};

struct rle_compressed_tag {};
struct rle_raw_tag        {};

//==============================================================================
//! Decompress compressed RLE packets.
//! @note If @p it == @p end, then @p out will be filled with @p n zeros.
//==============================================================================
template <size_t Bits, typename InputIt, typename OutputIt, typename ColorMapper>
auto decode_rle(
    rle_compressed_tag
  , ColorMapper const mapper
  , InputIt     const it
  , InputIt     const end
  , OutputIt          out
  , size_t      const n
) {
    auto const result = read_n_bits<Bits>(it, end);
    auto const pixel  = mapper(std::get<0>(result));

    for (size_t i = 0; i < n; ++i) {
        *out++ = pixel;
    }

    return std::make_tuple(std::get<1>(result), out);
}

//==============================================================================
//! Decompress raw RLE packets.
//! @note If, for d = distance(@p it, @p end), d < @p n then @p out will be
//! filled with n - d zeros.
//==============================================================================
template <size_t Bits, typename InputIt, typename OutputIt, typename ColorMapper>
auto decode_rle(
    rle_raw_tag
  , ColorMapper const mapper
  , InputIt           it
  , InputIt     const end
  , OutputIt          out
  , size_t      const n
) {
    for (size_t i = 0; i < n; ++i) {
        auto const result = read_n_bits<Bits>(it, end);
        auto const pixel  = mapper(std::get<0>(result));

        *out++ = pixel;
        it = std::get<1>(result);
    }

    return std::make_tuple(it, out);
}

//==============================================================================
//! Decode the RLE compressed data given by [@p it, @p end).
//==============================================================================
template <size_t Bits, typename InputIt, typename OutputIt, typename ColorMapper>
inline void decode_rle(
    ColorMapper const mapper
  , InputIt           it
  , InputIt     const end
  , OutputIt          out
) {
    while (it != end) {
        auto const packet = rle_packet {*it++};
        auto const n      = packet.size();

        tie(it, out) = packet.is_raw()
          ? decode_rle<Bits>(rle_raw_tag        {}, mapper, it, end, out, n)
          : decode_rle<Bits>(rle_compressed_tag {}, mapper, it, end, out, n);
    }
}

//==============================================================================
//! Decode the uncompressed data given by [@p it, @p end).
//==============================================================================
template <size_t Bits, typename InputIt, typename OutputIt, typename ColorMapper>
inline void decode_direct(
    ColorMapper const mapper
  , InputIt           it
  , InputIt     const end
  , OutputIt          out
) {
    while (it != end) {
        auto const result = read_n_bits<Bits>(it, end);
        *out++            = mapper(std::get<0>(result));
        it                = std::get<1>(result);
    }
}

//==============================================================================
//!
//==============================================================================
inline auto make_color_mapper(
    tga_descriptor const& descriptor
  , std::istream&         in
) noexcept {
    return [cmap = tga_color_map {descriptor, in}](auto const i) noexcept {
        return cmap[i];
    };
}

//==============================================================================
//!
//==============================================================================
inline auto make_color_mapper_dummy() noexcept {
    return [](auto const i) noexcept {
        return i;
    };
}

//==============================================================================
//!
//==============================================================================
template <size_t Bits, typename ColorMapper, typename InputIt>
inline std::vector<uint32_t> decode(
    tga_descriptor const& tga
  , ColorMapper    const  cmap
  , InputIt        const  first
  , InputIt        const  last
) {
    std::vector<uint32_t> result;
    result.reserve(static_cast<size_t>(tga.width * tga.height));

    auto const out = back_inserter(result);

    if (is_compressed_rle(tga.img_type)) {
      decode_rle<Bits>(cmap, first, last, out);
    } else {
      decode_direct<Bits>(cmap, first, last, out);
    }

    return result;
}

//==============================================================================
//!
//==============================================================================
template <typename ColorMapper, typename InputIt>
inline std::vector<uint32_t> decode(
    tga_descriptor const& tga
  , ColorMapper    const  cmap
  , InputIt        const  first
  , InputIt        const  last
) {
    switch (tga.pixel_depth) {
    case 8u  : return decode< 8>(tga, cmap, first, last);
    case 15u : return decode<15>(tga, cmap, first, last);
    case 16u : return decode<16>(tga, cmap, first, last);
    case 24u : return decode<24>(tga, cmap, first, last);
    case 32u : return decode<32>(tga, cmap, first, last);
    default:
        break;
    }

    return {};
}

} //namespace detail

//==============================================================================
//! Decode a TGA image file into a vector of 32-bit unsigned integers in
//! 0xAABBGGRR color format
//!
//! @pre @p tga was constructed from the stream given by @p in.
//! @pre @p tga is valid.
//!
//! @param[in] tga The descriptor for the TGA file to decode.
//! @param[in] in The stream that @p tga was constructed from.
//==============================================================================
inline std::vector<uint32_t> decode(tga_descriptor const& tga, std::istream& in) {
    using namespace detail;

    in.exceptions(detail::all_exceptions);

    auto const bpp = tga.has_color_map() ? tga.cmap_depth : tga.pixel_depth;
    auto const decoded_size =
        tga.width * tga.height * round_up_bits_to_bytes(bpp);

    auto const range = tga.image_data_range();
    auto const read_size = static_cast<size_t>(range.second - range.first);
    BKTGA_ASSERT_OPT(read_size <= decoded_size);

    auto const data = read_to_buffer(in, range);

    auto const first = data.get();
    auto const last  = first + read_size;

    auto result = tga.has_color_map()
      ? decode(tga, make_color_mapper(tga, in), first, last)
      : decode(tga, make_color_mapper_dummy(), first, last);

    auto const origin = tga.image_desc.origin();

    if (origin == tga_origin::lo_left || origin == tga_origin::lo_right) {
        detail::flip_vertical(result.data(), tga.width, tga.height);
    }

    if (origin == tga_origin::up_right || origin == tga_origin::lo_right) {
        detail::flip_horizontal(result.data(), tga.width, tga.height);
    }

    return result;
}

inline std::vector<uint32_t> decode(tga_descriptor const& tga, std::istream&& in) {
    return decode(tga, in);
}

//! @throws: std::ios::failure
inline std::vector<uint32_t> decode(
    tga_descriptor const& tga
  , string_view    const  filename
) {
    std::ifstream in;
    in.exceptions(detail::all_exceptions);
    in.open(filename.to_string().c_str(), std::ios::binary);

    return decode(tga, in);
}

//==============================================================================
//! Determines whether the stream @p in specifies a valid TGA file and
//! returns a valid tga_descriptor and an empty string, otherwise returns an
//! empty optional and a string describing the error.
//==============================================================================
inline std::pair<optional<tga_descriptor>, string_view>
check_file(std::istream& in) {
    in.exceptions(detail::all_exceptions);

    try {
        tga_descriptor tga {in};

        if (tga.version != tga_version::invalid) {
            return {std::move(tga), {}};
        }
    } catch (std::ios::failure const&) {
        return {{}, "IO failure."};
    }

    return {{}, "Invalid file format."};
}

inline std::pair<optional<tga_descriptor>, string_view>
check_file(std::istream&& in) {
    return check_file(in);
}

//==============================================================================
//! Determines whether @p filename is a valid TGA file.
//! @see check_file(std::istream&)
//==============================================================================
inline std::pair<optional<tga_descriptor>, string_view>
check_file(string_view const filename) {
    std::ifstream in;
    in.exceptions(detail::all_exceptions);

    try {
        in.open(filename.to_string().c_str(), std::ios::binary);
    } catch (std::ios::failure const&) {
        return {{}, "IO failure."};
    }

    return check_file(in);
}

} //namespace bktga
