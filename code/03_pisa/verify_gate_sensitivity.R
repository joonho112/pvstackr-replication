# =============================================================================
# verify_gate_sensitivity.R: check the PSIS gate and sensitivity anchors
# =============================================================================
#
# Purpose : Focused check. Verifies that Korea's Pipeline B rows carry the
#           withheld status, that recomputed Pareto-k matches the archive,
#           that the gate makes USA reportable and Korea withheld, and that a
#           leakage probe cannot force blocked Korea values through. Also
#           reconstructs the reversal contrast, checks the legacy and declared
#           staged anchors, and confirms no strong-reversal claim is allowed.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "07_psis_reporting_gate.R"))
source(file.path("code", "03_pisa", "09_reversal_robustness.R"))

b <- pisa_load_psis()
pisa_assert(all(b$paper_rows$reporting_status[b$paper_rows$country == "KOR"] ==
                  "computed_but_withheld"),
            "Public Pipeline B rows must use the canonical withheld status.")
recomputed <- pisa_recompute_psis(b)
pisa_assert(max(abs(recomputed$pareto_khat - b$khat$pareto_khat)) < 1e-8,
            "Archived and recomputed Pareto-k values differ.")
pisa_assert(all(b$gate$computation_completed),
            "PSIS must have been computed for both countries.")
pisa_assert(b$gate$numeric_reporting_allowed[b$gate$country == "USA"] &&
              !b$gate$numeric_reporting_allowed[b$gate$country == "KOR"],
            "Expected USA reportable and Korea withheld.")

probe <- b$paper_rows
probe$estimate[probe$country == "KOR"] <- 999
probe$std_error[probe$country == "KOR"] <- 999
gated <- pisa_apply_reporting_gate(probe, b$khat)
pisa_assert(all(is.na(gated$estimate[gated$country == "KOR"])) &&
              all(is.na(gated$std_error[gated$country == "KOR"])),
            "Paper-facing extractor leaked blocked Korea values.")

s <- pisa_load_sensitivity()
reversal_recomputed <- pisa_recompute_reversal(s$reversal_country)
reversal_archived <- s$reversal[match(reversal_recomputed$weighting,
                                      s$reversal$weighting), ]
pisa_assert(max(abs(reversal_recomputed$estimate - reversal_archived$estimate)) < 1e-10 &&
              max(abs(reversal_recomputed$se - reversal_archived$se)) < 1e-10 &&
              max(abs(reversal_recomputed$df - reversal_archived$df)) < 1e-8,
            "Country rows do not reconstruct the reversal contrast.")
level1 <- s$reversal[s$reversal$weighting == "level1_final_weight_only", ]
staged <- s$reversal[s$reversal$weighting == "declared_staged_sns01", ]
pisa_assert(abs(level1$estimate - 23.5816978992317) < 1e-10,
            "Legacy level-1 reversal target changed.")
pisa_assert(abs(staged$estimate - 10.6588698435488) < 1e-10 &&
              abs(staged$p_value - 0.105194748219) < 1e-12,
            "Declared staged reversal anchor changed.")
pisa_assert(!isTRUE(s$robustness_summary$strong_reversal_claim_allowed[[1L]]),
            "Robustness evidence must not authorize a strong reversal claim.")
cat("PISA PSIS gate and sensitivity: PASS\n")
