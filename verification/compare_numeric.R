# =============================================================================
# compare_numeric.R: Numeric table comparison helpers
# =============================================================================
#
# Purpose : Sourced helper (not a standalone script) providing
#           compare_numeric_tables() and assert_numeric_tables(). Aligns an
#           actual and expected data frame on key columns, computes the maximum
#           absolute difference per shared numeric column, and reports pass/fail
#           against a tolerance (default 1e-10); the assert form stops when any
#           column exceeds tolerance so callers fail closed.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
compare_numeric_tables <- function(actual, expected, keys, tolerance = 1e-10) {
  stopifnot(is.data.frame(actual), is.data.frame(expected), all(keys %in% names(actual)), all(keys %in% names(expected)))
  actual <- actual[do.call(order, actual[keys]),,drop=FALSE]
  expected <- expected[do.call(order, expected[keys]),,drop=FALSE]
  rownames(actual) <- rownames(expected) <- NULL
  if (!identical(actual[keys], expected[keys])) stop("Key mismatch", call.=FALSE)
  common <- intersect(names(actual), names(expected))
  numeric <- common[vapply(actual[common],is.numeric,logical(1)) & vapply(expected[common],is.numeric,logical(1))]
  diff <- vapply(numeric,function(n) max(abs(actual[[n]]-expected[[n]]),na.rm=TRUE),numeric(1))
  diff[!is.finite(diff)] <- 0
  data.frame(column=numeric,max_abs_difference=unname(diff),tolerance=tolerance,pass=diff<=tolerance)
}

assert_numeric_tables <- function(...) {
  x <- compare_numeric_tables(...)
  if (any(!x$pass)) stop("Numeric artifact difference exceeds tolerance: ",paste(x$column[!x$pass],collapse=", "),call.=FALSE)
  invisible(x)
}
