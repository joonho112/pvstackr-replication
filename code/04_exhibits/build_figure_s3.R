# =============================================================================
# build_figure_s3.R: publication (ggplot2) Figure S3, weighting sensitivity
# =============================================================================
#
# Purpose : Regenerate the publication ggplot2 Figure S3 from shipped evidence:
#           the PISA weighting-sensitivity comparison of the final-student-
#           weight-only placement against the declared two-stage weights for
#           the Korea - United States between-school slope, with 95% intervals.
#           Input  : data/precomputed/pisa/reversal_secondary_summary.csv.
#           Output : output/figures/figure_s3_weighting_sensitivity.pdf.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
suppressPackageStartupMessages(library(ggplot2))

build_figure_s3 <- function(root) {
  source_file <- file.path(root, "data", "precomputed", "pisa", "reversal_secondary_summary.csv")
  rv <- utils::read.csv(source_file, stringsAsFactors = FALSE, check.names = FALSE)
  rv <- rv[rv$summary_row_id %in% c("REV-LEVEL1-FINAL-WEIGHT", "REV-DECLARED-SNS01"), ]
  rv$scheme <- ifelse(rv$summary_row_id == "REV-LEVEL1-FINAL-WEIGHT",
                      "Final student weight only\n(level-1 placement)",
                      "Declared two-stage weights\n(school and conditional student)")
  rv$lo <- rv$estimate - stats::qt(.975, rv$df) * rv$std_error
  rv$hi <- rv$estimate + stats::qt(.975, rv$df) * rv$std_error
  rv$plab <- ifelse(rv$p_value < .001, "p < .001", sprintf("p = %.3f", rv$p_value))
  rv$scheme <- factor(rv$scheme, levels = rev(c(
    "Final student weight only\n(level-1 placement)",
    "Declared two-stage weights\n(school and conditional student)")))
  p <- ggplot(rv, aes(x = estimate, y = scheme)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = .4) +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                  width = .12, linewidth = .5, colour = "#4C78A8") +
    geom_point(size = 2.6, colour = "#4C78A8") +
    geom_text(aes(label = plab), vjust = -1.3, size = 3.1, family = "Times") +
    labs(x = expression(paste("Korea - United States difference in ", beta^B, " (95% interval)")), y = NULL) +
    expand_limits(x = c(-5, 36)) +
    theme_minimal(base_size = 10, base_family = "Times") +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = .3, colour = "grey85"),
          axis.text = element_text(colour = "grey15"),
          plot.margin = margin(6, 10, 4, 6))
  path <- file.path(root, "output", "figures", "figure_s3_weighting_sensitivity.pdf")
  grDevices::pdf(path, width = 6.5, height = 2.4, family = "Times", useDingbats = FALSE)
  print(p)
  grDevices::dev.off()
  stopifnot(file.exists(path), file.info(path)$size > 1000)
  invisible(path)
}
