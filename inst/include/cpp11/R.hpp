// cpp11 version: 0.4.6
// vendored on: 2023-09-24
#pragma once

#ifdef R_INTERNALS_H_
#if !(defined(R_NO_REMAP) && defined(STRICT_R_HEADERS))
#error R headers were included before cpp11 headers \
  and at least one of R_NO_REMAP or STRICT_R_HEADERS \
  was not defined.
#endif
#endif

#define R_NO_REMAP
#define STRICT_R_HEADERS
#include "Rinternals.h"

// clang-format off
#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wattributes"
#endif

#ifdef __GNUC__
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wattributes"
#endif
// clang-format on

#include <type_traits>
#include "cpp11/altrep.hpp"

namespace cpp11 {
namespace literals {

constexpr R_xlen_t operator"" _xl(unsigned long long int value) { return value; }

}  // namespace literals

namespace traits {
template <typename T>
struct get_underlying_type {
  using type = T;
};
}  // namespace traits

template <typename T>
inline T na();

template <typename T>
inline typename std::enable_if<!std::is_same<typename std::decay<T>::type, double>::value,
                               bool>::type
is_na(const T& value) {
  return value == na<T>();
}

template <typename T>
inline typename std::enable_if<std::is_same<typename std::decay<T>::type, double>::value,
                               bool>::type
is_na(const T& value) {
  return ISNA(value);
}

}  // namespace cpp11
