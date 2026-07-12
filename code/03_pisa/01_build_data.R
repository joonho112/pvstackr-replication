# =============================================================================
# 01_build_data.R: build the analytic view and staged survey weights
# =============================================================================
#
# Purpose : Stage 01. Projects the verified source slice to the modeling
#           columns, applies the complete-case-on-ESCS exclusion, and derives
#           the school-mean-centered within/between ESCS split and the
#           sum-to-n normalized final weight plus the staged school and
#           conditional-student weights for the survey-weighted
#           pseudo-likelihood. Writes the analytic view and diagnostic tables.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   pisa_build_analysis()  - sole entry point; returns the analytic-view list
#                            and (optionally) writes the derived outputs.
#     Stages within it:
#       - project to modeling columns; complete-case-on-ESCS exclusion
#       - sample cascade: released/analytic student & school counts + asserts
#       - staged weights: w_school, w_student_cond, w_norm_sample identity
#       - ESCS split: school-weighted mean -> ESCS_within / ESCS_between
#       - weight_diagnostics: per-country summaries + finiteness checks
#       - write analytic .rds, sample_cascade.csv, weight_diagnostics.csv
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

pisa_build_analysis <- function(write_outputs = TRUE) {
  source_data <- pisa_read_source()
  projected <- source_data[, PISA_PROJECTED_VARS, drop = FALSE]
  projected$row_index_source <- seq_len(nrow(projected))

  cascade <- do.call(rbind, lapply(PISA_COUNTRIES, function(country) {
    released <- projected[projected$CNT == country, , drop = FALSE]
    analytic <- released[!is.na(released$ESCS), , drop = FALSE]
    data.frame(
      country = country,
      released_students = nrow(released),
      released_schools = length(unique(released$CNTSCHID)),
      escs_missing_students = sum(is.na(released$ESCS)),
      analytic_students = nrow(analytic),
      analytic_schools = length(unique(analytic$CNTSCHID)),
      exclusion_rule = "complete case on ESCS",
      stringsAsFactors = FALSE
    )
  }))

  pisa_assert(identical(stats::setNames(cascade$released_students, cascade$country),
                        PISA_EXPECTED_RELEASED), "Released student counts changed.")
  pisa_assert(identical(stats::setNames(cascade$analytic_students, cascade$country),
                        PISA_EXPECTED_ANALYTIC), "Analytic student counts changed.")
  pisa_assert(identical(stats::setNames(cascade$released_schools, cascade$country),
                        PISA_EXPECTED_RELEASED_SCHOOLS), "Released school counts changed.")
  pisa_assert(identical(stats::setNames(cascade$analytic_schools, cascade$country),
                        PISA_EXPECTED_ANALYTIC_SCHOOLS), "Analytic school counts changed.")

  analytic <- projected[!is.na(projected$ESCS), , drop = FALSE]
  pieces <- lapply(PISA_COUNTRIES, function(country) {
    d <- analytic[analytic$CNT == country, , drop = FALSE]
    school <- as.character(d$CNTSCHID)
    school_weight <- tapply(d$W_SCHGRNRABWT, school, function(z) {
      pisa_assert(max(z) - min(z) < 1e-12,
                  "W_SCHGRNRABWT is not constant within a school.")
      z[[1L]]
    })
    sbar <- mean(school_weight)
    S <- unname(school_weight[school])
    I <- d$W_FSTUWT / S
    c_A <- nrow(d) / sum(S * I)
    d$school_id_model <- factor(d$CNTSCHID)
    d$w_school <- S / sbar
    d$w_student_cond <- c_A * sbar * I
    d$w_norm_sample <- nrow(d) * d$W_FSTUWT / sum(d$W_FSTUWT)
    pisa_assert(max(abs(d$w_school * d$w_student_cond - d$w_norm_sample)) < 1e-12,
                "Staged weights do not reconstruct the normalized final weight.")
    idx <- split(seq_len(nrow(d)), d$CNTSCHID)
    school_mean <- vapply(idx, function(i) {
      stats::weighted.mean(d$ESCS[i], d$w_norm_sample[i])
    }, numeric(1))
    d$ESCS_school_mean <- unname(school_mean[as.character(d$CNTSCHID)])
    d$ESCS_within <- d$ESCS - d$ESCS_school_mean
    d$ESCS_between <- d$ESCS_school_mean
    d
  })
  analytic <- do.call(rbind, pieces)
  rownames(analytic) <- NULL
  analytic <- analytic[order(analytic$row_index_source), , drop = FALSE]

  weight_diagnostics <- do.call(rbind, lapply(PISA_COUNTRIES, function(country) {
    d <- analytic[analytic$CNT == country, , drop = FALSE]
    school_unique <- !duplicated(d$CNTSCHID)
    data.frame(
      country = country,
      n_students = nrow(d),
      n_schools = sum(school_unique),
      mean_w_norm_sample = mean(d$w_norm_sample),
      mean_w_school_unique = mean(d$w_school[school_unique]),
      max_product_error = max(abs(d$w_school * d$w_student_cond - d$w_norm_sample)),
      min_weight = min(c(d$w_norm_sample, d$w_school, d$w_student_cond)),
      all_finite_positive = all(is.finite(c(d$w_norm_sample, d$w_school,
                                            d$w_student_cond))) &&
        all(c(d$w_norm_sample, d$w_school, d$w_student_cond) > 0),
      stringsAsFactors = FALSE
    )
  }))

  result <- list(
    schema_version = "pisa_analytic_view_v1",
    source = list(path = "data/pisa/local/source/pisa2022_read_usa_kor.rds",
                  sha256 = PISA_SOURCE_SHA256,
                  rows = PISA_SOURCE_DIM[[1L]],
                  columns = PISA_SOURCE_DIM[[2L]]),
    projection = list(retained_columns = PISA_PROJECTED_VARS,
                      row_order_key = "row_index_source",
                      exclusion_rule = "complete case on ESCS"),
    data = analytic,
    cascade = cascade,
    weight_diagnostics = weight_diagnostics
  )
  if (write_outputs) {
    saveRDS(result, PISA_ANALYTIC_FILE, compress = "xz", version = 3)
    utils::write.csv(cascade, file.path(PISA_PRECOMPUTED_DIR, "sample_cascade.csv"),
                     row.names = FALSE, na = "")
    utils::write.csv(weight_diagnostics,
                     file.path(PISA_PRECOMPUTED_DIR, "weight_diagnostics.csv"),
                     row.names = FALSE, na = "")
  }
  result
}

if (sys.nframe() == 0L) {
  x <- pisa_build_analysis(TRUE)
  cat(sprintf("Built analytic view: %d rows, %d columns\n",
              nrow(x$data), ncol(x$data)))
}
