This release includes some bug fixes and various improvements. Furthermore, there are two points which affect other packages on CRAN:

1) This release fixes an issue with the latest 'brms'-update that breaks tests in the 'bayestestR'-package (https://cran.r-project.org/web/checks/check_results_bayestestR.html). We expect that those errors will be solved once the 'insght'-update is released on CRAN.

2) This submission will break some tests of the 'parameters'-package, i.e. we expect some of the reverse-dependency checks to fail. This is of less of concern, because the affected funtions in 'parameters' still work. E.g., one tests fails because 'BFBayesFactor::anovaBF()' was not supported before (and hence, the test expects an error), but due to this 'insight'-update, these objects now also work in 'parameters', that's why the test now "fails" (because there is no longer an error in the test-result). 

We have prepared an update for the 'parameters'-package, which we can submit once the 'insight'-update is on CRAN. We excuse this "cyclic" dependency during the submission-process, but we'd kindly ask to first submit 'insight', then 'parameters', because 'parameters' depends on (new) functions from the 'insight' package.

Best Regards
Daniel Lüdecke, maintainer of the 'insight' and 'parameters' packages.
