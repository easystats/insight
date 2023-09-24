#include <iostream>
#include <vector>
#include <algorithm>

[[cpp11::register]]
bool has_single_value_(std::vector<double> x, bool na_rm = false) {
  if (na_rm) {
    x.erase(std::remove_if(x.begin(), x.end(), [](double value) {
      return std::isnan(value);
    }), x.end());
  }

  return !x.empty() && std::all_of(x.begin() + 1, x.end(), [&](double value) {
    return value == x[0];
  });
}
