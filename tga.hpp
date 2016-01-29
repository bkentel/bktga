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
#include <string>                            // for string
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

//! ceil(n / 8.0) for n >= 0; otherwise 0.
template <typename T>
inline constexpr ptrdiff_t round_up_bits_to_bytes(T const n) noexcept {
    static_assert(std::is_integral<T>::value, "");
    return (n >= 0)
      ? (n / 8) + ((n % 8) ? 1 : 0)
      : 0;
}

//! A wrapper around a dynamically allocated buffer.
//! Primarily this is used as storage for variable sized fields. A vector or
//! string would (might) incur extra cost for first being zeroed.
class buffer {
public:
    buffer() = default;

    explicit buffer(ptrdiff_t const size)
      : size_ {size}
      , data_ {size > 0 ? new char[static_cast<size_t>(size)] : nullptr}
    {
    }

    ptrdiff_t   size()  const noexcept { return size_; }
    char const* data()  const noexcept { return data_.get(); }
    char*       data()        noexcept { return data_.get(); }
    char const* begin() const noexcept { return data(); }
    char const* end()   const noexcept { return data() + size_; }
private:
    ptrdiff_t                size_ {};
    std::unique_ptr<char []> data_ {};
};

//! equivalent to max(min(a, b), 0)
template <typename T>
inline constexpr T min_0(T const a, T const b) noexcept {
    return (a < b)
             ? (a < 0 ? 0 : a)
             : (b < 0 ? 0 : b);
}

//! equivalent to max(min(a, b, c), 0)
template <typename T>
inline constexpr T min_0(T const a, T const b, T const c) noexcept {
    return min_0(min_0(a, b), c);
}

// c++17 std::data()
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

// c++17 std::size()
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
    file_source() = default;

    //! Throwing version for opening files by name/
    //! @throws std::system_error
    explicit file_source(string_view const fname)
      : file_source {do_open_(fname.to_string().c_str())}
    {
    }

    //! Non-throwing version for already opened files
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

    void read(ptrdiff_t const n, char* const out, ptrdiff_t const out_size) noexcept {
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
            throw std::system_error {errno, std::system_category()};
        }

        return {handle, std::fclose};
    }

    operator FILE*() const noexcept { return handle_.get(); }

    unique_file handle_ {nullptr, &fclose};
};

/// in-memory data source
class memory_source {
public:
    memory_source() = default;

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

    void read(ptrdiff_t const n, char* const out, ptrdiff_t const out_size) noexcept {
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
    char const* first_ {};
    char const* last_  {};
    char const* pos_   {};
};

template <typename T, typename Source>
inline T read(Source& src) noexcept {
    T out;
    src.read(sizeof(T), reinterpret_cast<char*>(&out), sizeof(T));
    return out;
}

template <typename T, typename Source>
inline T read_at(Source& src, ptrdiff_t const offset) noexcept {
    src.seek(offset);
    return read<T>(src);
}

template <ptrdiff_t Bits, typename Source>
inline auto read_bits(Source& src) noexcept {
    using U = uint_t<round_up_bits_to_bytes(Bits)>;
    return read<U>(src);
}

template <typename Source>
buffer read_variable_field(Source& src, ptrdiff_t const size) {
    auto out = buffer {size};
    src.read(out.size(), out.data(), out.size());
    return out;
}

template <typename Source>
buffer read_variable_field_at(Source& src, ptrdiff_t const size, ptrdiff_t const offset) {
    src.seek(offset);
    return read_variable_field(src, size);
}

struct tag_scalar_field        {};
struct tag_array_field         {};
struct tag_constructible_field {};

template <typename T>
struct field_tag {
    using type = std::conditional_t<
        std::is_scalar<T>::value, tag_scalar_field, tag_constructible_field>;
};

template <typename T, size_t N>
struct field_tag<std::array<T, N>> {
    using type = tag_array_field;
};

template <typename Tag, typename T>
struct field_traits;

template <typename T>
struct field_traits<tag_scalar_field, T> {
    inline static constexpr ptrdiff_t size() noexcept { return sizeof(T); }

    template <typename Source>
    inline static T read(Source& src) noexcept {
        using U = uint_t<sizeof(T)>;
        return static_cast<T>(little_endian_to_host(detail::read<U>(src)));
    }
};

template <typename T>
struct field_traits<tag_array_field, T> {
    inline static constexpr ptrdiff_t size() noexcept { return sizeof(T); }

    template <typename Source>
    inline static T read(Source& src) noexcept {
        return detail::read<T>(src);
    }
};

template <typename T>
struct field_traits<tag_constructible_field, T> {
    inline static constexpr ptrdiff_t size() noexcept { return T::size; }

    template <typename Source>
    inline static T read(Source& src) noexcept {
        static_assert(std::is_nothrow_constructible<T, Source&>::value, "");
        return T {src};
    }
};

template <typename T, ptrdiff_t Offset = 0, typename Tag = typename field_tag<T>::type, typename Traits = field_traits<Tag, T>>
struct field : Traits {
    using traits = Traits;
    using tag    = Tag;
    using type   = T;

    inline static constexpr ptrdiff_t begin() noexcept { return Offset; }
    inline static constexpr ptrdiff_t end()   noexcept { return begin() + traits::size(); }

    template <typename Source>
    inline static T read_at(Source& src, ptrdiff_t const offset) noexcept {
        src.seek(offset);
        return traits::read(src);
    }

    template <typename Source>
    inline static T read_at(Source& src) noexcept {
        return read_at(src, begin());
    }
};

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

enum class tga_attribute_type : uint8_t {
    no_data              = 0
  , undefined_ignore     = 1
  , undefined_retain     = 2
  , alpha                = 3
  , alpha_pre_multiplied = 4
  , unknown_reserved     = 127
  , unknown_unassigned   = 128
};

/// Image version.
enum class tga_version {
    invalid, v1, v2
};

/// Image descriptor.
struct tga_image_descriptor {
    static constexpr size_t size = 1 * sizeof(uint8_t);

    tga_image_descriptor() = default;

    explicit tga_image_descriptor(uint8_t const n) noexcept
      : value {n}
    {
    }

    template <typename Source>
    explicit tga_image_descriptor(Source& src
        , std::enable_if_t<!std::is_integral<Source>::value>* = nullptr
    ) noexcept
      : tga_image_descriptor {detail::read<uint8_t>(src)}
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

/// Developer area
class tga_developer_area {
public:
    struct record_t {
        // intentionally not marked explicit
        record_t(uint16_t const tag_)
          : tag {tag_}
        {
        }

        template <typename Source>
        explicit record_t(Source& src
          , std::enable_if_t<!std::is_integral<Source>::value>* = nullptr
        )
          : tag    {detail::read<uint16_t>(src)}
          , offset {detail::read<uint32_t>(src)}
          , size   {detail::read<uint32_t>(src)}
        {
        }

        uint16_t tag    {};
        uint32_t offset {};
        uint32_t size   {};
    };

    tga_developer_area() = default;

    template <typename Source>
    tga_developer_area(Source&& src, ptrdiff_t const offset)
      : size_ {detail::read_at<uint16_t>(src, offset)}
    {
        records_.reserve(size_);
        std::generate_n(back_inserter(records_), size_, [&] {
            return record_t {src};
        });
    }

    template <typename Source>
    detail::buffer get_data(Source&& src, record_t const r) const {
        return detail::read_variable_field_at(src, r.size, r.offset);
    }

    auto   begin() const { return std::begin(records_); }
    auto   end()   const { return std::end(records_); }
    size_t size()  const { return size_; }
private:
    uint16_t              size_    {};
    std::vector<record_t> records_ {};
};

/// Extension area
class tga_extension_area {
public:
    struct time_stamp_t {
        static constexpr size_t size = 6 * sizeof(uint16_t);

        time_stamp_t() = default;

        template <typename Source>
        explicit time_stamp_t(Source& src) noexcept
          : month  {detail::read<uint16_t>(src)}
          , day    {detail::read<uint16_t>(src)}
          , year   {detail::read<uint16_t>(src)}
          , hour   {detail::read<uint16_t>(src)}
          , minute {detail::read<uint16_t>(src)}
          , second {detail::read<uint16_t>(src)}
        {
        }

        uint16_t month  {};
        uint16_t day    {};
        uint16_t year   {};
        uint16_t hour   {};
        uint16_t minute {};
        uint16_t second {};
    };

    struct job_time_t {
        static constexpr size_t size = 3 * sizeof(uint16_t);

        job_time_t() = default;

        template <typename Source>
        explicit job_time_t(Source& src) noexcept
          : hours   {detail::read<uint16_t>(src)}
          , minutes {detail::read<uint16_t>(src)}
          , seconds {detail::read<uint16_t>(src)}
        {
        }

        uint16_t hours   {};
        uint16_t minutes {};
        uint16_t seconds {};
    };

    struct software_version_t {
        static constexpr size_t size = sizeof(uint16_t) + sizeof(char);

        software_version_t() = default;

        template <typename Source>
        explicit software_version_t(Source& src) noexcept
          : number {detail::read<uint16_t>(src)}
          , letter {detail::read<char>(src)}
        {
        }

        uint16_t number {};
        char     letter {};
    };

    struct ratio_t {
        static constexpr size_t size = 2 * sizeof(uint16_t);

        ratio_t() = default;

        template <typename Source>
        explicit ratio_t(Source& src) noexcept
          : num {detail::read<uint16_t>(src)}
          , den {detail::read<uint16_t>(src)}
        {
        }

        uint16_t num {};
        uint16_t den {};
    };

    struct field {
        template <typename T, ptrdiff_t Offset>
        using f = detail::field<T, Offset>;

        using id_t = std::array<char, 41>;
        using comment_t = std::array<std::array<char, 81>, 4>;

        using extension_size          = f<uint16_t,           0>;
        using author_name             = f<id_t,               extension_size::end()>;
        using author_comments         = f<comment_t,          author_name::end()>;
        using date_time_stamp         = f<time_stamp_t,       author_comments::end()>;
        using job_name                = f<id_t,               date_time_stamp::end()>;
        using job_time                = f<job_time_t,         job_name::end()>;
        using software_name           = f<id_t,               job_time::end()>;
        using software_version        = f<software_version_t, software_name::end()>;
        using key_color               = f<uint32_t,           software_version::end()>;
        using pixel_aspect_ratio      = f<ratio_t,            key_color::end()>;
        using gamma_value             = f<ratio_t,            pixel_aspect_ratio::end()>;
        using color_correction_offset = f<uint32_t,           gamma_value::end()>;
        using postage_stamp_offset    = f<uint32_t,           color_correction_offset::end()>;
        using scan_line_offset        = f<uint32_t,           postage_stamp_offset::end()>;
        using attribute_type          = f<tga_attribute_type, scan_line_offset::end()>;
    };

    tga_extension_area() = default;

    template <typename Source>
    tga_extension_area(Source& src, ptrdiff_t const offset)
      : extension_size          {field::extension_size::read_at(src, offset)}
      , author_name             (field::author_name::read(src))
      , author_comments         (field::author_comments::read(src))
      , date_time_stamp         {field::date_time_stamp::read(src)}
      , job_name                (field::job_name::read(src))
      , job_time                {field::job_time::read(src)}
      , software_name           (field::software_name::read(src))
      , software_version        {field::software_version::read(src)}
      , key_color               {field::key_color::read(src)}
      , pixel_aspect_ratio      {field::pixel_aspect_ratio::read(src)}
      , gamma_value             {field::gamma_value::read(src)}
      , color_correction_offset {field::color_correction_offset::read(src)}
      , postage_stamp_offset    {field::postage_stamp_offset::read(src)}
      , scan_line_offset        {field::scan_line_offset::read(src)}
      , attribute_type          {field::attribute_type::read(src)}
    {
    }

    field::extension_size::type          extension_size          {};
    field::author_name::type             author_name             {};
    field::author_comments::type         author_comments         {};
    field::date_time_stamp::type         date_time_stamp         {};
    field::job_name::type                job_name                {};
    field::job_time::type                job_time                {};
    field::software_name::type           software_name           {};
    field::software_version::type        software_version        {};
    field::key_color::type               key_color               {};
    field::pixel_aspect_ratio::type      pixel_aspect_ratio      {};
    field::gamma_value::type             gamma_value             {};
    field::color_correction_offset::type color_correction_offset {};
    field::postage_stamp_offset::type    postage_stamp_offset    {};
    field::scan_line_offset::type        scan_line_offset        {};
    field::attribute_type::type          attribute_type          {tga_attribute_type::no_data};
};

class tga_descriptor {
public:
    struct field {
        using header_t    = std::array<char, tga_header_size>;
        using footer_t    = std::array<char, tga_footer_size>;
        using signature_t = std::array<char, tga_footer_signature_size>;

        template <typename T, ptrdiff_t Offset>
        using f = detail::field<T, Offset>;

        //header fields
        using id_length   = f<uint8_t,              0>;
        using cmap_type   = f<tga_color_map_type,   id_length::end()>;
        using img_type    = f<tga_image_type,       cmap_type::end()>;
        using cmap_start  = f<uint16_t,             img_type::end()>;
        using cmap_len    = f<uint16_t,             cmap_start::end()>;
        using cmap_depth  = f<uint8_t,              cmap_len::end()>;
        using x_offset    = f<uint16_t,             cmap_depth::end()>;
        using y_offset    = f<uint16_t,             x_offset::end()>;
        using width       = f<uint16_t,             y_offset::end()>;
        using height      = f<uint16_t,             width::end()>;
        using pixel_depth = f<uint8_t,              height::end()>;
        using image_desc  = f<tga_image_descriptor, pixel_depth::end()>;

        //footer fields
        using ext_offset  = f<uint32_t,             0>;
        using dev_offset  = f<uint32_t,             ext_offset::end()>;
        using signature   = f<signature_t,          dev_offset::end()>;
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
      : id_length   {field::id_length::read_at(src, 0)}
      , cmap_type   {field::cmap_type::read(src)}
      , img_type    {field::img_type::read(src)}
      , cmap_start  {field::cmap_start::read(src)}
      , cmap_len    {field::cmap_len::read(src)}
      , cmap_depth  {field::cmap_depth::read(src)}
      , x_offset    {field::x_offset::read(src)}
      , y_offset    {field::y_offset::read(src)}
      , width       {field::width::read(src)}
      , height      {field::height::read(src)}
      , pixel_depth {field::pixel_depth::read(src)}
      , image_desc  {field::image_desc::read(src)}
      , ext_offset  {field::ext_offset::read_at(src, fsize - tga_footer_size)}
      , dev_offset  {field::dev_offset::read(src)}
      , signature   (field::signature::read(src))
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
        auto const field_size = tga.is_color_mapped()
          ? tga.color_map_size() : 0;

        if (field_size <= 0) {
            return;
        }

        data_.reserve(tga.cmap_len);
        auto const buffer = detail::read_variable_field_at(
            src, field_size, tga.color_map_offset());

        auto data = detail::memory_source {buffer};

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
    explicit detect_result_t(std::error_code error)
      : error_code {std::move(error)}
    {
        tga.diagnostic = "system error";
    }

    explicit detect_result_t(Source&& src)
      : detect_result_t {tga_descriptor {src}, std::move(src)}
    {
    }

    explicit operator bool() const noexcept {
        return !error_code && tga.is_valid();
    }

    std::string error() const {
        return error_code
          ? error_code.message()
          : tga.diagnostic.to_string();
    }

    tga_descriptor  tga        {};
    Source          source     {};
    std::error_code error_code {};
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

/// RLE compresses (repeat) packets
template <ptrdiff_t Bpp, typename Source, typename ColorMapper, typename OutIt>
inline void decode_rle_compressed(
    ptrdiff_t   const  count
  , Source&            src
  , ColorMapper const& cmap
  , OutIt              out
) {
    std::fill_n(out, count, cmap(read_bits<Bpp>(src)));
}

/// RLE "raw" packets
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

/// decode RLE encoded data
template <ptrdiff_t Bpp, typename Source, typename ColorMapper, typename OutIt>
inline void decode_rle(
    tga_descriptor const& tga
  , Source&               src
  , ColorMapper    const& cmap
  , OutIt                 out
) {
    for (ptrdiff_t i = 0; i < tga.pixel_count(); ) {
        auto const packet = read<uint8_t>(src);
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

/// decode uncompressed data
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

template <ptrdiff_t Bpp>
inline auto make_color_mapper() noexcept {
    return [](auto const c) noexcept -> uint32_t {
        return to_rgba<Bpp>(c);
    };
}

template <ptrdiff_t Bpp, typename Source>
inline auto make_color_mapper(tga_descriptor const& tga, Source& src) {
    return [m = tga_color_map(tga, src)](auto const c) noexcept -> uint32_t {
        return m[static_cast<ptrdiff_t>(c)];
    };
}

/// Level 2 -- Choose color mapper.
template <ptrdiff_t Bpp, typename Source>
inline decode_t decode(tga_descriptor const& tga, Source& src) {
    using detail::decode;
    return tga.is_color_mapped()
      ? decode<Bpp>(tga, src, make_color_mapper<Bpp>(tga, src))
      : decode<Bpp>(tga, src, make_color_mapper<Bpp>());
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
    try {
        return detail::detect(detail::file_source {filename});
    } catch (std::system_error const& e) {
        return detect_result_t<detail::file_source> {e.code()};
    }
}

inline auto detect(read_from_file_t, std::string const& filename)
    -> detect_result_t<detail::file_source>
{
    return detect(read_from_file, string_view {filename});
}

template <size_t N>
inline auto detect(read_from_file_t, char const (&filename)[N])
    -> detect_result_t<detail::file_source>
{
    return detect(read_from_file, string_view {filename, N - 1});
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

//===----------------------------------------------------------------------===//
//                              API - read_id
//===----------------------------------------------------------------------===//
template <typename OutIt, typename Source>
OutIt read_id(detect_result_t<Source>& in, OutIt const dest) {
    auto const buffer = detail::read_variable_field_at(
         in.source, in.tga.id_length, in.tga.id_offset());

    return std::copy(std::begin(buffer), std::end(buffer), dest);
}

//===----------------------------------------------------------------------===//
//                              API - read_developer_area
//===----------------------------------------------------------------------===//
template <typename Source>
tga_developer_area read_developer_area(detect_result_t<Source>& in) {
    return (in && in.tga.dev_offset)
      ? tga_developer_area {in.source, in.tga.dev_offset}
      : tga_developer_area {};
}

//===----------------------------------------------------------------------===//
//                              API - read_extension_area
//===----------------------------------------------------------------------===//
template <typename Source>
tga_extension_area read_extension_area(detect_result_t<Source>& in) {
    return (in && in.tga.ext_offset)
      ? tga_extension_area {in.source, in.tga.ext_offset}
      : tga_extension_area {};
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
