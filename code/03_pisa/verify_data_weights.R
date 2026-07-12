# =============================================================================
# verify_data_weights.R: check source types, weights, and a negative control
# =============================================================================
#
# Purpose : Focused check. Rebuilds the analytic view in memory (no writes)
#           and asserts the source identifier/numeric column types, the 10
#           plausible-value and 80 replicate-weight counts, stable source row
#           order, mean-one normalized and unique-school weights, the staged
#           product-weight identity, and a corrupted-weight negative control
#           that must be detected. Requires the authorized local slice.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))
source(file.path("code", "03_pisa", "01_build_data.R"))

source_data <- pisa_read_source()
pisa_assert(identical(unname(vapply(source_data[1:3], typeof, character(1))),
                      rep("character", 3L)), "Identifier types changed.")
pisa_assert(all(vapply(source_data[-(1:3)], is.numeric, logical(1))),
            "Non-identifier columns must be numeric.")
pisa_assert(length(grep("^PV[0-9]+READ$", names(source_data))) == 10L,
            "Expected 10 reading PVs.")
pisa_assert(length(grep("^W_FSTURWT[0-9]+$", names(source_data))) == 80L,
            "Expected 80 replicate weights.")

built <- pisa_build_analysis(FALSE)
pisa_assert(identical(built$data$row_index_source,
                      sort(built$data$row_index_source)), "Source row order changed.")
pisa_assert(all(abs(built$weight_diagnostics$mean_w_norm_sample - 1) < 5e-14),
            "Normalized final weights do not have mean one.")
pisa_assert(all(abs(built$weight_diagnostics$mean_w_school_unique - 1) < 5e-14),
            "Unique school weights do not have mean one.")
pisa_assert(max(built$weight_diagnostics$max_product_error) < 2e-14,
            "Staged product reconstruction failed.")

corrupted <- built$data
corrupted$w_student_cond[[1L]] <- corrupted$w_student_cond[[1L]] * 2
corrupt_error <- max(abs(corrupted$w_school * corrupted$w_student_cond -
                         corrupted$w_norm_sample))
pisa_assert(corrupt_error > 1e-6,
            "Corrupted-weight negative control was not detected.")
cat("PISA data and weights: PASS\n")
