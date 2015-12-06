#pragma once

#define _SCL_SECURE_NO_WARNINGS

#include <boost/utility/string_ref.hpp>

#include <fstream>
#include <vector>
#include <string>
#include <array>
#include <type_traits>

#include <cstdint>

namespace bktga {

using std::int8_t;
using std::int16_t;
using std::int32_t;

using std::uint8_t;
using std::uint16_t;
using std::uint32_t;

//TODO allow this to use the c++17 version as well
using string_view = boost::string_ref;

static constexpr size_t header_size = 18u;

namespace detail {

template <typename T>
inline size_t round_up_bits_to_bytes(T const n) noexcept {
    static_assert(std::is_unsigned<T>::value, "");
    return static_cast<size_t>((n + 7u) / 8u);
}

//=====================================================================================================================
//! Get the value of an enum as its underlying type.
//=====================================================================================================================
template <typename T>
constexpr auto underlying_value(T const value) noexcept {
    static_assert(std::is_enum<T>::value, "");
    return static_cast<std::underlying_type_t<T>>(value);
}

//=====================================================================================================================
//! Read n bytes of data from in into out, otherwise throw.
//! @pre out must be at least n bytes long.
//! @post in has exceptions turned on iff an error occurs.
//=====================================================================================================================
inline void read_or_throw(std::istream& in, char* const out, std::streamsize const n) {
    while (!in.read(out, n)) {
        in.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
    }
}

//=====================================================================================================================
//! Read a T where is_pod<T>::value is true, otherwise throw.
//! @post in has exceptions turned on iff an error occurs.
//=====================================================================================================================
template <typename T>
inline T read_or_throw(std::istream& in) {
    static_assert(std::is_pod<T>::value, "");

    T result;
    auto const buffer = static_cast<char*>(&result);
    for (size_t i = 0; i < sizeof(T); ++i) {
        read_or_throw(in, buffer + i, 1);
    }
}

} //namespace detail

enum class colormap_type : uint8_t {
    absent           = 0
  , present          = 1

  , unknown_reserved = 127
  , unknown_custom
};

enum class image_type : uint8_t {
    none             = 0
  , color_mapped     = 1
  , true_color       = 2
  , grayscale        = 3
  , rle_color_mapped = 9
  , rle_true_color   = 10
  , rle_grayscale    = 11

  , unknown_reserved = 127
  , unknown_custom
};

enum class origin : uint8_t {
    lo_left
  , lo_right
  , up_left
  , up_right
};

enum class interleave : uint8_t {
    none
  , even_odd
  , four_way
  , reserved
};

struct image_descriptor {
    uint8_t attribute_bits() const noexcept {
        return static_cast<uint8_t>(value & 0b00001111u);
    }

    origin origin() const noexcept {
        return static_cast<bktga::origin>((value & 0b00110000u) >> 4u);
    }

    interleave interleave() const noexcept {
        return static_cast<bktga::interleave>((value & uint8_t {0b11000000u}) >> 6u);
    }

    uint8_t value;
};

namespace detail {
template <typename T>
inline void read_pod_from_buffer(char const* const buffer, T& out, std::integral_constant<size_t, 1>) noexcept {
    auto const n = static_cast<uint8_t>(
        (static_cast<uint8_t>(buffer[0]) << 0 & 0xFFu));

    out = *reinterpret_cast<T const*>(&n);
}

template <typename T>
inline void read_pod_from_buffer(char const* const buffer, T& out, std::integral_constant<size_t, 2>) noexcept {
    auto const n = static_cast<uint16_t>(
        (static_cast<uint8_t>(buffer[0]) << 0 & 0x00FF)
      | (static_cast<uint8_t>(buffer[1]) << 8 & 0xFF00));

    out = *reinterpret_cast<T const*>(&n);
}

template <typename T>
inline void read_pod_from_buffer(char const* const buffer, T& out, std::integral_constant<size_t, 3>) noexcept {
    auto const n = static_cast<uint32_t>(
        (static_cast<uint8_t>(buffer[0]) << 0  & 0x0000FF)
      | (static_cast<uint8_t>(buffer[1]) << 8  & 0x00FF00)
      | (static_cast<uint8_t>(buffer[2]) << 16 & 0xFF0000));

    out = *reinterpret_cast<T const*>(&n);
}

template <typename T>
inline void read_pod_from_buffer(char const* const buffer, T& out, std::integral_constant<size_t, 4>) noexcept {
    auto const n = static_cast<uint32_t>(
        (static_cast<uint8_t>(buffer[0]) << 0  & 0x000000FF)
      | (static_cast<uint8_t>(buffer[1]) << 8  & 0x0000FF00)
      | (static_cast<uint8_t>(buffer[2]) << 16 & 0x00FF0000)
      | (static_cast<uint8_t>(buffer[3]) << 24 & 0xFF000000));

    out = *reinterpret_cast<T const*>(&n);
}

template <typename T, size_t Size>
T read_pod_from_buffer(std::array<char, Size> const& buffer, size_t& pos) noexcept {
    static_assert(std::is_pod<T>::value, "");

    if (pos + sizeof(T) > Size) {
        //TODO error
    }

    T const result = read_pod_from_buffer(buffer.data() + pos, out, std::integral_constant<size_t, sizeof(T)> {});
    pos += sizeof(T);
    return result;
}

using byte_t = std::integral_constant<size_t, 1u>;
using word_t = std::integral_constant<size_t, 2u>;

using long24_t = std::integral_constant<size_t, 3u>;
using long32_t = std::integral_constant<size_t, 4u>;

template <typename T>
inline T read_pod_from_buffer(char const* const in, size_t const off, byte_t) noexcept {
    auto const n = static_cast<uint8_t>(*(in + off));
    return *reinterpret_cast<T const*>(&n);
}

template <typename T>
inline T read_pod_from_buffer(char const* const in, size_t const off, word_t) noexcept {
    auto const b0 = static_cast<uint8_t>(*(in + off + 0));
    auto const b1 = static_cast<uint8_t>(*(in + off + 1));

    auto const n = (b0 & 0x00FF) | (b1 << 8 & 0xFF00);

    return *reinterpret_cast<T const*>(&n);
}

template <typename T>
inline T read_pod_from_buffer(char const* const in, size_t const off, long24_t) noexcept {
    auto const b0 = static_cast<uint8_t>(*(in + off + 0));
    auto const b1 = static_cast<uint8_t>(*(in + off + 1));
    auto const b2 = static_cast<uint8_t>(*(in + off + 2));

    auto const n = (b0 & 0x0000FF) | (b1 << 8 & 0x00FF00) | (b2 << 16 & 0xFF0000);

    return *reinterpret_cast<T const*>(&n);
}

template <typename T>
inline T read_pod_from_buffer(char const* const in, size_t const off) noexcept {
    auto const b0 = static_cast<uint8_t>(*(in + off + 0));
    auto const b1 = static_cast<uint8_t>(*(in + off + 1));
    auto const b2 = static_cast<uint8_t>(*(in + off + 2));
    auto const b3 = static_cast<uint8_t>(*(in + off + 3));

    auto const n = (b0 & 0x000000FF) | (b1 << 8 & 0x0000FF00) | (b2 << 16 & 0x00FF0000) | (b3 << 16 & 0xFF000000);

    return *reinterpret_cast<T const*>(&n);
}

template <typename T, size_t Size = sizeof(T)>
inline T read_pod_from_buffer(std::array<char, Size> const& in, size_t const off) noexcept {
    return read_pod_from_buffer<T>(in.data(), off);
}

template <typename T>
inline T read_pod_from_buffer(std::vector<char> const& in, size_t const off) noexcept {
    return read_pod_from_buffer<T>(in.data(), off);
}

template <typename T, size_t Offset>
struct field {
    using type = T;
    static constexpr size_t offset = Offset;

    field() = default;

    template <size_t Size>
    explicit field(std::array<char, Size> const& in) noexcept
      : value {detail::read_pod_from_buffer<type>(in.data(), Offset)}
    {
    }

    explicit field(std::vector<char> const& in) noexcept
      : value {detail::read_pod_from_buffer<type>(in.data(), Offset)}
    {
    }

    type value = type {};
};

inline auto read_header(std::istream& in) {
    std::array<char, header_size> result;

    if (!in.read(result.data(), header_size)) {
        //TODO fail
    }

    return result;
}

} //namespace detail

//=====================================================================================================================
//
//=====================================================================================================================
struct header {
    header(header&&) = default;
    header(header const&) = default;

    header& operator=(header const&) = default;
    header& operator=(header&&) = default;

    size_t color_map_data_size() const noexcept {
        return color_map_length.value * detail::round_up_bits_to_bytes(color_map_depth.value);
    }

    size_t image_data_size() const noexcept {
        return width.value * height.value * detail::round_up_bits_to_bytes(pixel_depth.value);
    }

    bool const has_id() const noexcept {
        return !!id_length.value;
    }

    bool const has_color_map_data() const noexcept {
        return !!color_map_length.value;
    }

    explicit header(std::array<char, header_size> const& data)
      : color_map_start  {data}
      , color_map_length {data}
      , x_offset         {data}
      , y_offset         {data}
      , width            {data}
      , height           {data}
      , id_length        {data}
      , color_map_depth  {data}
      , pixel_depth      {data}
      , image_descriptor {data}
      , color_map_type   {data}
      , image_type       {data}
    {
        switch (image_type.value) {
        case bktga::image_type::none :
            break;
        case bktga::image_type::rle_color_mapped :
        case bktga::image_type::color_mapped :
            if (color_map_type.value != colormap_type::present) {
            }

            if (!color_map_length.value) {
            }

            if (color_map_length.value > 256) {
            }

            if (pixel_depth.value != 8) {
            }

            break;
        case bktga::image_type::rle_true_color :
        case bktga::image_type::true_color :
            if (color_map_type.value != colormap_type::absent) {
            }

            switch (pixel_depth.value) {
            case 15 :
            case 16 :
            case 24 :
            case 32 :
                break;
            default :
                break;
            }

            break;
        case bktga::image_type::rle_grayscale :
        case bktga::image_type::grayscale :
            if (color_map_type.value != colormap_type::absent) {
            }

            if (pixel_depth.value != 8) {
            }

            break;
        default:
            break;
        }
    }

    explicit header(std::istream& in)
      : header {detail::read_header(in)}
    {
    }

    explicit header(std::istream&& in)
      : header {in}
    {
    }

    explicit header(char const* const filepath)
      : header {std::ifstream {filepath, std::ios::binary}}
    {
    }

    explicit header(string_view const filepath)
      : header {std::ifstream {filepath.to_string(), std::ios::binary}}
    {
    }

    detail::field<uint16_t,         0x3u>  color_map_start;
    detail::field<uint16_t,         0x5u>  color_map_length;
    detail::field<uint16_t,         0x8u>  x_offset;
    detail::field<uint16_t,         0xAu>  y_offset;
    detail::field<uint16_t,         0xCu>  width;
    detail::field<uint16_t,         0xEu>  height;
    detail::field<uint8_t,          0x0u>  id_length;
    detail::field<uint8_t,          0x7u>  color_map_depth;
    detail::field<uint8_t,          0x10u> pixel_depth;
    detail::field<image_descriptor, 0x11u> image_descriptor;
    detail::field<colormap_type,    0x01u> color_map_type;
    detail::field<image_type,       0x02u> image_type;
};

//=====================================================================================================================
//
//=====================================================================================================================
struct id {
    id(header const& hdr, std::istream& in) {
        // no length means no id field
        if (!hdr.has_id()) {
            return;
        }

        // length can be at most 255
        std::array<char, 256> buffer;
        detail::read_or_throw(in, buffer.data(), hdr.id_length.value);

        value.assign(begin(buffer), end(buffer));
    }

    explicit operator bool() const noexcept {
        return value.empty();
    }

    std::string value;
};

//=====================================================================================================================
//
//=====================================================================================================================
struct color_map {
    color_map(header const& hdr, std::istream& in)
      : first_ {hdr.color_map_start.value}
      , element_size_ {detail::round_up_bits_to_bytes(hdr.color_map_depth.value)}
    {
        // no map present
        if (!hdr.has_color_map_data()) {
            return;
        }

        auto const buffer_size = hdr.color_map_data_size();
        data_.resize(buffer_size);

        if (!in.read(data_.data(), buffer_size)) {
            //TODO error
        }
    }

    explicit operator bool() const noexcept { return !data_.empty(); }

    size_t size() const noexcept {
        return data_.size() / element_size_;
    }

    size_t element_size() const noexcept {
        return element_size_;
    }

    auto const& data() const noexcept { return data_; }
private:
    std::vector<char> data_;
    size_t            first_;
    size_t            element_size_ = 0;
};

namespace detail {
//=====================================================================================================================
//! Flip an image horizontally (around the y axis).
//! @param[in, out] data The data to flip in place.
//! @param[in] w The width, in pixels, of the data.
//! @param[in] h The height, in pixels, of the data.
//! @param[in] bytes_per_pixel The number of bytes used for each pixel.
//! @pre data must point to a buffer of at least w*h*bytes_per_pixel bytes.
//=====================================================================================================================
inline void flip_horizontal(char* const data, size_t const w, size_t const h, size_t const bytes_per_pixel) noexcept {
    auto const stride = w * bytes_per_pixel;

    for (size_t y = 0; y < h; ++y) {
        auto const start = y * stride;

        for (size_t x = 0; x < w / 2; ++x) {
            auto const xoff = x * bytes_per_pixel;

            for (size_t i = 0; i < bytes_per_pixel; ++i) {
                std::swap(data[start + xoff + i], data[start + stride - xoff + i]);
            }
        }
    }
}

//=====================================================================================================================
//! Flip an image vertically (around the x axis).
//! @param[in, out] data The data to flip in place.
//! @param[in] w The width, in pixels, of the data.
//! @param[in] h The height, in pixels, of the data.
//! @param[in] bytes_per_pixel The number of bytes used for each pixel.
//! @pre data must point to a buffer of at least w*h*bytes_per_pixel bytes.
//=====================================================================================================================
inline void flip_vertical(char* const data, size_t const w, size_t const h, size_t const bytes_per_pixel) {
    auto const stride = w * bytes_per_pixel;

    std::vector<char> line;
    line.resize(stride);

    for (size_t y = 0; y < h / 2; ++y) {
        auto const l0 = y * stride;
        auto const l1 = (h - y - 1) * stride;

        std::copy_n(&data[l0],   stride, line.data());
        std::copy_n(&data[l1],   stride, &data[l0]);
        std::copy_n(line.data(), stride, &data[l1]);
    }
}
}

//=====================================================================================================================
//
//=====================================================================================================================
struct image {
    image(header const& hdr, std::istream& in)
      : header_ {hdr}
      , id_ {hdr, in}
      , color_map_ {hdr, in}
    {
        auto const buffer_size = hdr.image_data_size();
        data_.resize(buffer_size);

        if (!in.read(data_.data(), buffer_size)) {
            //TODO error
        }
    }

    size_t stride() const noexcept {
        return header_.width.value * bytes_per_element();
    }

    uint32_t bytes_per_element() const noexcept {
        return (header_.pixel_depth.value + 7) / 8;
    }

    auto const& data() const noexcept { return data_; }

    std::vector<char> transform() const {
        //auto const element_size   = (header_.pixel_depth() + 7) / 8;
        //auto const w              = header_.width();
        //auto const h              = header_.height();
        //auto const element_stride = w * element_size;
        //auto const color_size     = color_map_.element_size();
        //auto const color_stride   = w * color_size;

        //auto const get =
        //      element_size == 1 ? &detail::read_n_bytes<1>
        //    : element_size == 2 ? &detail::read_n_bytes<2>
        //    : element_size == 3 ? &detail::read_n_bytes<3>
        //    : element_size == 4 ? &detail::read_n_bytes<4>
        //    :                     &detail::read_n_bytes<0>;

        //auto const copy =
        //      color_size == 1 ? &detail::copy_n_bytes<1>
        //    : color_size == 2 ? &detail::copy_n_bytes<2>
        //    : color_size == 3 ? &detail::copy_n_bytes<3>
        //    : color_size == 4 ? &detail::copy_n_bytes<4>
        //    :                   &detail::copy_n_bytes<0>;

        //auto const get_src = [&]() noexcept {
        //    //Ugly workaround for MSVC issue
        //    using get_src_t = char const* (size_t, color_map const&, uint32_t const&);

        //    auto const get_from_map = [](size_t const n, color_map const& cm, uint32_t const& i) noexcept {
        //        return &cm.data().data()[i * n];
        //    };

        //    auto const get_from_raw = [](size_t, color_map const&, uint32_t const& i) noexcept {
        //        return reinterpret_cast<char const*>(&i);
        //    };

        //    return header_.colormap_type() == colormap_type::present
        //      ? static_cast<get_src_t*>(get_from_map)
        //      : static_cast<get_src_t*>(get_from_raw);
        //}();

        std::vector<char> result;
        //result.resize(h * color_stride);

        //for (uint16_t y = 0; y < h; ++y) {
        //    auto const src_y_off = y * element_stride;
        //    auto const dst_y_off = y * color_stride;

        //    for (uint16_t x = 0; x < w; ++x) {
        //        auto const src_x_off = x * element_size;
        //        auto const dst_x_off = x * color_size;

        //        auto const i = get(data_.data(), src_y_off + src_x_off);
        //        copy(get_src(color_size, color_map_, i), &result[dst_y_off + dst_x_off]);
        //    }
        //}

        //auto const o = header_.origin();

        //if (o == origin::lo_right || o == origin::up_right) {
        //    detail::flip_horizontal(result.data(), w, h, color_size);
        //}

        //if (o == origin::lo_left || o == origin::lo_right) {
        //    detail::flip_vertical(result.data(), w, h, color_size);
        //}

        //std::ofstream out {"out.raw", std::ios::binary};

        //out.write(result.data(), color_stride * h);

        return result;
    }
private:
    header            header_;
    id                id_;
    color_map         color_map_;
    std::vector<char> data_;
};

} //namespace bktga
