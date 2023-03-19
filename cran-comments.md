## revdepcheck results

We checked 33 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw no new problem
 * We failed to check 0 packages

There are errors in the CRAN check results (https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-debian-gcc/insight-00check.html), however, these are not related to the *insight* package, but rather upstream-issues from the *tibble* package (https://cran.r-project.org/web/checks/check_results_tibble.html). The errors for *insight* should resolve once the issues in the *tibble* package are fixed.