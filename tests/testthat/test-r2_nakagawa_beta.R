skip_on_cran()

skip_if_not_installed("glmmTMB")
skip_if_not_installed("MuMIn")
skip_if_not_installed("performance")


# ==============================================================================
# beta mixed models, glmmTMB
# ==============================================================================

skip_if_not_installed("betareg")

test_that("glmmTMB, beta_family", {
  # dataset ---------------------------------
  data(FoodExpenditure, package = "betareg")
  m <- glmmTMB::glmmTMB(
    I(food / income) ~ income + (1 | persons),
    data = FoodExpenditure,
    family = glmmTMB::beta_family()
  )
  out1 <- suppressWarnings(MuMIn::r.squaredGLMM(m))
  out2 <- suppressWarnings(performance::r2_nakagawa(m, verbose = FALSE))
  # matches theoretical values
  expect_equal(out1[1, "R2m"], out2$R2_marginal, ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(out1[1, "R2c"], out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-4)
})


# ==============================================================================
# beta mixed models, GLMMadaptive
# ==============================================================================

skip_if_not_installed("GLMMadaptive")

test_that("glmmTMB and GLMMadaptive, beta_family", {
  df_ratio_episode <- data.frame(
    animal_id = factor(
      rep(
        c(
          "208", "209", "210", "214", "223", "228", "211", "213", "217", "222", "234",
          "241", "216", "230", "231", "240", "242", "244", "218", "220", "225", "237",
          "239", "219", "251", "252", "253", "254"
        ),
        each = 2L
      ),
      levels = c(
        "200", "204", "205", "206", "215", "224", "208", "209", "210", "214", "223",
        "228", "211", "213", "217", "222", "234", "241", "216", "230", "231", "240",
        "242", "244", "218", "220", "225", "237", "239", "245", "219", "236", "251",
        "252", "253", "254"
      )
    ),
    trial = rep(c(1, 2), 28),
    activity_ratio = c(
      0.1313027016689785, 0.08387917431645128, 0.1395420340967623,
      0.09844057594710427, 0.19414443290359096, 0.16304581176275632,
      0.17274983272168504, 0.17357956037939837, 0.09729583968716982,
      0.05138063319955499, 0.14298075594540044, 0.10179701101266003,
      0.09168390375802275, 0.11591243874797318, 0.2521345405747349,
      0.16335726666875724, 0.13436311090275369, 0.12012636336085161,
      0.13868852567209072, 0.12008249718946021, 0.27708418835127824,
      0.22042035159734397, 0.2649703945513039, 0.22158610629846917,
      0.2001770607989554, 0.2238562351804714, 0.1105503693420828,
      0.08255349183783911, 0.21927303214082697, 0.22211274055043914,
      0.10446530203550744, 0.11336175801811256, 0.0826812722435201,
      0.09328851878674252, 0.13701773797551595, 0.1297098120849381,
      0.05986226055235673, 0.14423247009476106, 0.19474645802355026,
      0.1713563584485577, 0.25663498351317365, 0.30249307043720924,
      0.09082761877930186, 0.10402396536249521, 0.21941679494558652,
      0.28459112981037343, 0.11218161441362348, 0.12449715062493952,
      0.18427917423975973, 0.14845015830783756, 0.19444224064643065,
      0.13471565660441723, 0.11247341287367296, 0.08660523675310272,
      0.1763980204528711, 0.1049572229068965
    )
  )

  m1 <- glmmTMB::glmmTMB(activity_ratio ~ trial + (1 | animal_id),
    data = df_ratio_episode,
    family = glmmTMB::beta_family
  )
  m2 <- GLMMadaptive::mixed_model(
    activity_ratio ~ trial,
    random = ~ 1 | animal_id,
    data = df_ratio_episode,
    family = GLMMadaptive::beta.fam()
  )

  out1 <- performance::r2_nakagawa(m1)
  out2 <- performance::r2_nakagawa(m2)
  expect_equal(out1$R2_conditional, out2$R2_conditional, ignore_attr = TRUE, tolerance = 1e-2)
})
