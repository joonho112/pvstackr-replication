# =============================================================================
# test_core.R: Core engine unit tests
# =============================================================================
#
# Purpose : Exercises the core engine on deterministic fixtures: Rubin pooling
#           with df provenance; the fixed-effect point identity via the GLS
#           fixed-X/common-V theorem; the CCC two-level calibration (moment
#           match, variance-component passthrough, rejected median centering);
#           and the strict PSIS gate that withholds Korea numeric estimates.
#           Sources code/01_core; writes verification/core-gate.json.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)

project_root <- function() root
project_path <- function(...) file.path(root, ...)
`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path(root, "code", "01_core", "load_core.R"), local = FALSE)
source(file.path(root, "code", "01_core", "per_pv.R"), local = FALSE)

# Rubin pooling and df provenance.
beta <- rbind(c(a = 1.0, b = 2.0), c(a = 1.2, b = 1.8), c(a = 0.8, b = 2.1))
U <- replicate(3L, diag(c(0.04, 0.09)), simplify = FALSE)
p <- pool_per_pv(beta, U, df_method = "classic")
stopifnot(identical(p$df_method, "classic"), all(is.finite(p$se)),
          max(abs(p$beta - colMeans(beta))) < 1e-14)

# Theorem fixed-X/common-V identity.
set.seed(10)
X <- cbind(`(Intercept)` = 1, x = seq(-1, 1, length.out = 30))
V <- diag(30) + 0.05
Y <- lapply(1:5, function(i) drop(X %*% c(0.3, 0.6) + rnorm(30, sd = 0.2)))
t22 <- validate_theorem22_gls(X, V, Y)
stopifnot(t22$pass, t22$delta < 1e-12)

# CCC fixed-effect moment calibration and variance-component passthrough.
set.seed(20)
draws <- cbind(b0 = rnorm(500), b1 = rnorm(500, 0.5), sigma = rexp(500))
target <- matrix(c(0.25, 0.03, 0.03, 0.16), 2, 2,
                 dimnames = list(c("b0", "b1"), c("b0", "b1")))
cfit <- ccc_twolevel(draws, param_map = list(fe_idx = c("b0", "b1"), vc_idx = "sigma"),
                     Sigma_target = target, center = c(b0 = 0.2, b1 = 0.7))
cal <- cfit$draws_calibrated
stopifnot(max(abs(colMeans(cal[, c("b0", "b1")]) - c(0.2, 0.7))) < 1e-10,
          max(abs(stats::cov(cal[, c("b0", "b1")]) - target)) < 1e-10,
          identical(unname(cal[, "sigma"]), unname(draws[, "sigma"])))
median_center <- try(
  ccc_twolevel(draws,
               param_map = list(fe_idx = c("b0", "b1"), vc_idx = "sigma"),
               Sigma_target = target, center = "posterior_median"),
  silent = TRUE
)
stopifnot(inherits(median_center, "try-error"))

# PSIS reporting gate is strict and withholds Korea numeric estimates.
diag <- data.frame(country = rep(c("USA", "KOR"), each = 10),
                   pareto_k = c(seq(.35, .67, length.out = 10), seq(.82, 1.34, length.out = 10)))
gate <- psis_reportability(diag)
stopifnot(gate$reportable[gate$country == "USA"], !gate$reportable[gate$country == "KOR"])
est <- data.frame(country = c("USA", "KOR"), workflow = "pipeline_b", term = "betaW",
                  estimate = c(25, 28), se = c(2, 2))
paper <- paper_facing_reweighted(est, diag)
stopifnot(is.finite(paper$estimate[paper$country == "USA"]),
          is.na(paper$estimate[paper$country == "KOR"]))

dir.create(file.path(root, "verification"), showWarnings = FALSE)
# Report the identity residuals at a platform-stable resolution. They are all
# machine-epsilon quantities far below 1e-10 -- the point identity and the CCC
# moment calibration hold to machine precision -- so they render as 0
# identically on every platform, keeping this gate file byte-reproducible
# across operating systems and BLAS/LAPACK builds.
stabilize <- function(x) {
  if (isTRUE(is.finite(x) && abs(x) < 1e-10)) "0" else format(signif(x, 6), scientific = TRUE)
}
json <- sprintf('{"phase":3,"status":"pass","theorem_delta":%s,"ccc_mean_residual":%s,"ccc_cov_residual":%s,"korea_withheld":true}',
                stabilize(t22$delta),
                stabilize(max(abs(colMeans(cal[, c("b0", "b1")]) - c(0.2, 0.7)))),
                stabilize(max(abs(stats::cov(cal[, c("b0", "b1")]) - target))))
writeLines(json, file.path(root, "verification", "core-gate.json"))
cat("Core engine tests passed.\n")
