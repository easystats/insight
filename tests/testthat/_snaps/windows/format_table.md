# reorder columns BF

    Code
      format_table(out)
    Output
          Parameter   Component Median         95% CI   pd % in ROPE       BF  Rhat
      1 b_Intercept conditional  32.22 [27.22, 35.76] 100%        0% 1.97e+06 1.004
      2        b_wt conditional  -3.76 [-4.97, -2.21] 100%        0%   330.18 1.001
      3       sigma       sigma   3.46 [ 2.65,  4.70] 100%        0% 7.29e+03 0.992
           ESS
      1  88.00
      2  92.00
      3 168.00

# significance stars

    Code
      print(out, stars = TRUE)
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(30) |         p
      ---------------------------------------------------------------------
      (Intercept) |       37.29 | 1.88 | [33.45, 41.12] | 19.86 | < .001***
      wt          |       -5.34 | 0.56 | [-6.49, -4.20] | -9.56 | < .001***
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

---

    Code
      print(out, stars = TRUE, stars_only = TRUE)
    Output
      Parameter   | Coefficient |   SE |         95% CI | t(30) |   p
      ---------------------------------------------------------------
      (Intercept) |       37.29 | 1.88 | [33.45, 41.12] | 19.86 | ***
      wt          |       -5.34 | 0.56 | [-6.49, -4.20] | -9.56 | ***
    Message
      
      Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
        using a Wald t-distribution approximation.

