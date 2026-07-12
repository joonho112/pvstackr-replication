# =============================================================================
# build_figures.R: base-R (grDevices) versions of the paper figures
# =============================================================================
#
# Purpose : Draw base-graphics versions of the paper figures from shipped
#           precomputed evidence: the Figure 1 workflow schematic, Figure 2
#           simulation coverage, Figure 3 PISA forest, Figure 4 Pareto k-hat,
#           and (when present) Figure S3 weighting sensitivity. Figures 2-4 and
#           S3 are subsequently replaced by the publication ggplot2 versions
#           from build_paper_figures.R and build_figure_s3.R.
#           Inputs : data/precomputed/{simulation,pisa}/*.csv.
#           Outputs: PDF figures in output/figures.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
.open_pdf <- function(path, width = 8, height = 5.5) {
  grDevices::pdf(path, width = width, height = height, family = "Helvetica", useDingbats = FALSE)
}

build_figures <- function() {
  out <- project_path("output", "figures")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  .open_pdf(file.path(out, "figure1_workflows.pdf"), 10, 4)
  graphics::plot.new(); graphics::plot.window(c(0, 1), c(0, 1))
  xs <- c(.18, .50, .82); labs <- c("A: Per-PV MCMC\nM fits -> Rubin pooling", "B: Reweighted stack\n1 fit -> M PSIS audits", "C: Calibrated stack\n1 fit -> Rubin target")
  for (i in seq_along(xs)) { graphics::rect(xs[i]-.14, .35, xs[i]+.14, .65, border = "#2C3E50", lwd = 2); graphics::text(xs[i], .5, labs[i], cex = .9) }
  graphics::arrows(.33, .5, .35, .5, length = .08); graphics::arrows(.65, .5, .67, .5, length = .08)
  graphics::title("Three plausible-value workflows")
  grDevices::dev.off()

  pc <- .read_first(c(project_path("data", "precomputed", "simulation", "parity_by_condition.csv")))
  pc <- pc[pc$leg %in% c("bayes_a", "freq_per_pv", "c_direct"), ]
  pc <- pc[order(pc$design_condition_id, pc$leg), ]
  labs <- c(bayes_a = "Per-PV MCMC", freq_per_pv = "Per-PV REML", c_direct = "Calibrated")
  cols <- c(bayes_a = "#1B6CA8", freq_per_pv = "#666666", c_direct = "#C44E52")
  .open_pdf(file.path(out, "figure2_simulation_coverage.pdf"), 11, 7.5)
  graphics::par(mfrow = c(2, 1), mar = c(4.5, 4, 2, 1))
  for (term in c("W", "B")) {
    y <- pc[[paste0("cov", term)]]; x <- seq_along(y)
    graphics::plot(x, y, pch = 19, col = cols[pc$leg], ylim = c(.85, 1.01), xaxt = "n", xlab = "Condition (three estimators each)", ylab = "Coverage")
    graphics::abline(h = .95, lty = 2); graphics::axis(1, at = seq(2, nrow(pc), 3), labels = unique(pc$design_condition_id), las = 2, cex.axis = .55)
    graphics::title(sprintf("%s-school slope", if (term == "W") "Within" else "Between"))
  }
  graphics::legend("bottomleft", legend = labs, col = cols, pch = 19,
                   horiz = TRUE, bty = "n", cex = .85)
  grDevices::dev.off()

  p <- .read_first(c(project_path("data", "precomputed", "pisa", "pisa_final_abc.csv"), project_path("data", "precomputed", "pisa", "pisa_phase7r_final_abc_estimate_se.csv"), project_path("data", "precomputed", "pisa", "abc_estimates.csv")))
  forest <- p[p$pipeline %in% c("A", "C"), ]
  forest <- forest[order(forest$country, forest$term, forest$pipeline), ]
  y <- rev(seq_len(nrow(forest)))
  .open_pdf(file.path(out, "figure3_pisa_forest.pdf"), 8, 5.5)
  graphics::par(mar = c(5, 9, 4, 2) + .1)
  graphics::plot(forest$estimate, y, xlim = range(c(forest$ci_low, forest$ci_high)), yaxt = "n", ylab = "", xlab = "Slope estimate", pch = ifelse(forest$pipeline == "A", 19, 1), col = ifelse(forest$country == "USA", "#1B6CA8", "#C44E52"))
  graphics::segments(forest$ci_low, y, forest$ci_high, y, col = ifelse(forest$country == "USA", "#1B6CA8", "#C44E52"))
  graphics::axis(2, at = y, labels = paste(forest$country, ifelse(grepl("within", forest$term), "Within", "Between"), forest$pipeline), las = 1, cex.axis = .7)
  graphics::title("PISA fixed-effect estimates: Pipelines A and C")
  grDevices::dev.off()

  kh <- .read_first(c(project_path("data", "precomputed", "pisa", "pisa_psis_diagnostics.csv"), project_path("data", "precomputed", "pisa", "pisa_pipeline_b_pareto_k_phase7r.csv"), project_path("data", "precomputed", "pisa", "psis_diagnostics.csv")))
  kcol <- if ("pareto_khat" %in% names(kh)) "pareto_khat" else "pareto_k"
  pvcol <- if ("pv_index" %in% names(kh)) "pv_index" else "pv"
  .open_pdf(file.path(out, "figure4_pisa_khat.pdf"), 8, 5.5)
  graphics::plot(kh[[pvcol]] + ifelse(kh$country == "USA", -.08, .08), kh[[kcol]], pch = ifelse(kh$country == "USA", 19, 17), col = ifelse(kh$country == "USA", "#1B6CA8", "#C44E52"), xlab = "Plausible value", ylab = "Pareto k", xaxt = "n")
  graphics::axis(1, at = sort(unique(kh[[pvcol]]))); graphics::abline(h = .7, lty = 2, col = "#333333")
  graphics::legend("topleft", c("USA", "Korea", "reportability threshold"), pch = c(19, 17, NA), lty = c(NA, NA, 2), col = c("#1B6CA8", "#C44E52", "#333333"), bty = "n")
  grDevices::dev.off()

  reversal_file <- project_path("data", "precomputed", "pisa", "pisa_reversal_summary.csv")
  if (!file.exists(reversal_file)) reversal_file <- project_path("data", "precomputed", "pisa", "reversal_secondary_summary.csv")
  if (file.exists(reversal_file)) {
    r <- utils::read.csv(reversal_file, check.names = FALSE)
    r <- r[is.finite(r$estimate) & is.finite(r$std_error), ]
    crit <- stats::qt(.975, r$df); lo <- r$estimate - crit*r$std_error; hi <- r$estimate + crit*r$std_error
    .open_pdf(file.path(out, "figure_s3_weighting_sensitivity.pdf"), 8, 4)
    graphics::par(mar = c(5, 13, 4, 2) + .1)
    yy <- rev(seq_len(nrow(r))); graphics::plot(r$estimate, yy, xlim = range(c(lo, hi)), yaxt = "n", ylab = "", xlab = "Korea - USA between-school slope")
    graphics::segments(lo, yy, hi, yy); graphics::abline(v = 0, lty = 2)
    graphics::axis(2, at = yy, labels = r$weighting, las = 1, cex.axis = .75)
    graphics::title("Weighting sensitivity")
    grDevices::dev.off()
  }
  invisible(TRUE)
}
