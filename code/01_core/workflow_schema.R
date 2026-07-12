# =============================================================================
# workflow_schema.R: Workflow labels and result-schema validation
# =============================================================================
#
# Purpose : Presentation- and contract-layer helpers. workflow_labels maps the
#           internal archive labels (bayes_a, freq_per_pv, c_direct,
#           pipeline_b, oracle) to the paper-facing display names and the
#           A/B/C workflow letters. validate_result_schema checks that a
#           result data frame carries the required workflow, term, estimate,
#           and se columns and that estimates and standard errors are finite
#           (se nonnegative), guarding tables before they are reported.
#           Inputs: none, or a candidate result data frame. Output: a label
#           lookup data frame, or an invisible TRUE (else an error).
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

workflow_labels <- function() {
  # paper_label reproduces the submitted displays. The frozen simulation
  # freq_per_pv computation itself used ML; see known-source-discrepancies.md.
  data.frame(
    archive_label = c("bayes_a", "freq_per_pv", "c_direct", "pipeline_b", "oracle"),
    paper_label = c("Per-PV MCMC", "Per-PV REML", "Calibrated stack", "Reweighted stack", "Oracle"),
    paper_workflow = c("A", "A", "C", "B", NA_character_),
    stringsAsFactors = FALSE
  )
}

validate_result_schema <- function(x, required = c("workflow", "term", "estimate", "se")) {
  if (!is.data.frame(x)) stop("result must be a data frame", call. = FALSE)
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("missing result columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (any(!nzchar(as.character(x$workflow))) || any(!nzchar(as.character(x$term)))) {
    stop("workflow and term must be nonempty", call. = FALSE)
  }
  if (anyNA(x$estimate) || anyNA(x$se) || any(!is.finite(x$estimate)) ||
      any(!is.finite(x$se)) || any(x$se < 0)) {
    stop("estimate and se must be finite; se must be nonnegative", call. = FALSE)
  }
  invisible(TRUE)
}
