# =============================================================================
# 02_contract.R: read and enforce the public PISA estimand contract
# =============================================================================
#
# Purpose : Loads estimand-contract.yml and asserts the invariants that fix
#           the analysis: USA/Korea order, the 10 reading plausible-value
#           outcomes, the co-primary within/between ESCS terms, the secondary
#           status of country gaps, the block on Korea Pipeline B numeric
#           output, and the all-PV Pareto-k = 0.7 reporting gate. Fails loudly
#           on any drift. Input: code/03_pisa/estimand-contract.yml.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

pisa_read_contract <- function(path = file.path(PISA_CODE_DIR, "estimand-contract.yml")) {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required.", call. = FALSE)
  contract <- yaml::read_yaml(path)
  required <- c("schema_version", "support_id", "countries", "outcomes",
                "model", "co_primary_terms", "not_primary", "weights",
                "pipelines", "psis_gate", "reporting")
  missing <- setdiff(required, names(contract))
  pisa_assert(!length(missing), paste("Estimand contract lacks:", paste(missing, collapse = ", ")))
  pisa_assert(identical(unlist(contract$countries), PISA_COUNTRIES), "Country order changed.")
  pisa_assert(identical(unlist(contract$outcomes), PISA_PV_VARS), "PV order changed.")
  pisa_assert(identical(unlist(contract$co_primary_terms),
                        c("b_ESCS_within", "b_ESCS_between")),
              "Co-primary terms changed.")
  pisa_assert(isFALSE(contract$reporting$allow_country_gap_promotion),
              "Country gaps must remain secondary.")
  pisa_assert(isFALSE(contract$reporting$allow_korea_pipeline_b_numeric_output),
              "Korea Pipeline B numeric output must remain blocked.")
  pisa_assert(isTRUE(contract$psis_gate$require_all_pv),
              "The reportability gate must require all 10 PVs.")
  pisa_assert(identical(as.numeric(contract$psis_gate$pareto_k_threshold), 0.7),
              "The Pareto-k threshold changed.")
  contract
}

if (sys.nframe() == 0L) {
  pisa_read_contract()
  cat("PISA estimand contract: PASS\n")
}
