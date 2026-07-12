# =============================================================================
# verify_fit_targets.R: check the Pipeline A sampler gate and A/C anchors
# =============================================================================
#
# Purpose : Focused check. Confirms the empirical fit contract holds 20
#           Pipeline A plus 2 Pipeline C fits, that the Pipeline A sampler
#           gate passes (R-hat, ESS, zero divergences), that the archived
#           USA/Korea within/between ESCS estimates, standard errors, and
#           classic-Rubin degrees of freedom match their locked anchors, and
#           that Pipeline C reproduces its Pipeline A target to tolerance.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "03_cached_reference.R"))
source(file.path("code", "03_pisa", "05_cached_calibrated.R"))

a <- pisa_load_pipeline_a()
c <- pisa_load_pipeline_c()
pisa_assert(nrow(a$task_schedule) + length(c$countries) == 22L,
            "The empirical fit contract must contain 20 A fits and 2 C fits.")
pisa_assert(all(a$task_schedule$rhat_max <= 1.05) &&
              all(a$task_schedule$ess_bulk_min >= 100) &&
              all(a$task_schedule$ess_tail_min >= 100) &&
              sum(a$task_schedule$divergences) == 0L,
            "Pipeline A sampler gate failed.")

anchors <- data.frame(
  country = rep(c("USA", "KOR"), each = 2L),
  term = rep(c("b_ESCS_within", "b_ESCS_between"), 2L),
  estimate = c(25.083381396, 69.7401845973, 27.8634509841, 92.9388363584),
  std_error = c(1.93738793795384, 6.04729687728754,
                1.97988086043692, 6.54455471581504),
  df = c(349.972573636098, 1360.22214097538, 64.6253254892133, 536.760816868024),
  stringsAsFactors = FALSE
)
rows <- a$rows[match(paste(anchors$country, anchors$term),
                     paste(a$rows$country, a$rows$term)), ]
pisa_assert(max(abs(rows$estimate - anchors$estimate)) < 5e-10,
            "Pipeline A estimate anchor changed.")
pisa_assert(max(abs(rows$std_error - anchors$std_error)) < 5e-10,
            "Pipeline A SE anchor changed.")
pisa_assert(max(abs(rows$df - anchors$df)) < 5e-8,
            "Pipeline A archived classic-Rubin df anchor changed.")

c_rows <- c$rows[match(paste(a$rows$country, a$rows$term),
                       paste(c$rows$country, c$rows$term)), ]
pisa_assert(max(abs(c_rows$estimate - a$rows$estimate)) < 1e-12 &&
              max(abs(c_rows$std_error - a$rows$std_error)) < 1e-12,
            "Pipeline C no longer matches its locked A calibration target.")
cat("PISA fits and A/C targets: PASS\n")
