# =============================================================================
# 03_cached_reference.R: load and inspect the cached Pipeline A reference
# =============================================================================
#
# Purpose : Cached Pipeline A. Reads the curated public cache and checks its
#           schema, that it holds 20 production fits (10 plausible values x
#           USA/Korea) backed by genuine, non-synthetic draw matrices, and
#           exposes the pooled per-PV reference estimates. Pipeline A is the
#           orthodox per-plausible-value reference that Pipeline C calibrates
#           to. Input: data/precomputed/pisa/pipeline_a_cached.rds.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

pisa_load_pipeline_a <- function() {
  path <- file.path(PISA_PRECOMPUTED_DIR, "pipeline_a_cached.rds")
  pisa_assert(file.exists(path), "Pipeline A cache is missing.")
  x <- readRDS(path)
  pisa_assert(identical(x$schema_version, "pisa_pipeline_a_public_cache_v1"),
              "Unexpected Pipeline A cache schema.")
  pisa_assert(nrow(x$task_schedule) == 20L, "Pipeline A must contain 20 fit records.")
  for (country in PISA_COUNTRIES) {
    pisa_assert(length(x$draws[[country]]) == 10L,
                paste("Pipeline A lacks 10 draw matrices for", country))
    pisa_assert(all(vapply(x$draws[[country]], is.matrix, logical(1))),
                "Every Pipeline A draw object must be a matrix.")
  }
  pisa_assert(all(x$task_schedule$production_fit) &&
                !any(x$task_schedule$fake_or_synthetic_draws),
              "Pipeline A cache is not wholly production evidence.")
  x
}

pisa_reference_table <- function(country = NULL) {
  rows <- pisa_load_pipeline_a()$rows
  if (!is.null(country)) rows <- rows[rows$country == country, , drop = FALSE]
  rows
}

if (sys.nframe() == 0L) {
  x <- pisa_load_pipeline_a()
  print(x$rows[, c("country", "term", "estimate", "std_error", "df")])
}
