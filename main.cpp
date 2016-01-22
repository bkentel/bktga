#include "tga.hpp"

#if defined(_MSC_VER)
#   pragma warning(push)
#   pragma warning(disable : 4061)
#   pragma warning(disable : 4571)
#   pragma warning(disable : 4625)
#   pragma warning(disable : 4626)
#   pragma warning(disable : 5026)
#   pragma warning(disable : 5027)
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

#define CATCH_CONFIG_MAIN
#include <Catch/catch.hpp>

#if defined(_MSC_VER)
#   pragma warning(pop)
#endif
