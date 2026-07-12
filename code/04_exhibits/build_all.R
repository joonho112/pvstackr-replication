#!/usr/bin/env Rscript
# =============================================================================
# build_all.R: orchestrate the quick exhibit rebuild from shipped evidence
# =============================================================================
#
# Purpose : Rebuild every paper and supplement exhibit from the shipped
#           precomputed evidence. Sources and runs the table builders
#           (build_main_tables, build_osm_tables) and the figure builders
#           (build_figures, then the publication build_paper_figures and
#           build_figure_s3), copies the shipped Figure 1 workflow schematic
#           into output/figures, and normalizes embedded PDF dates so the
#           figures are byte-stable across rebuilds.
#           Inputs : data/precomputed/**; assets/figure1_workflows.pdf.
#           Outputs: CSV/TeX tables in output/tables; PDF figures in
#           output/figures.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file0), "..", ".."), mustWork = TRUE)
project_root <- function() root
project_path <- function(...) file.path(root, ...)
dir.create(project_path("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(project_path("output", "figures"), recursive = TRUE, showWarnings = FALSE)
source(project_path("code", "04_exhibits", "build_main_tables.R"), local = FALSE)
source(project_path("code", "04_exhibits", "build_osm_tables.R"), local = FALSE)
source(project_path("code", "04_exhibits", "build_figures.R"), local = FALSE)
build_main_tables()
build_osm_tables()
build_figures()
invisible(file.copy(project_path("code", "04_exhibits", "assets", "figure1_workflows.pdf"),
                    project_path("output", "figures", "figure1_workflows.pdf"),
                    overwrite = TRUE))
source(project_path("code", "04_exhibits", "build_paper_figures.R"), local = FALSE)
source(project_path("code", "04_exhibits", "build_figure_s3.R"), local = FALSE)
build_figure_s3(root)
normalize_pdf_dates <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  replacement <- charToRaw("20000101000000")
  for (label in c("/CreationDate (D:", "/ModDate (D:")) {
    pattern <- charToRaw(label)
    candidates <- which(bytes == pattern[[1L]])
    for (start in candidates) {
      end <- start + length(pattern) - 1L
      if (end + length(replacement) <= length(bytes) &&
          identical(bytes[start:end], pattern)) {
        bytes[(end + 1L):(end + length(replacement))] <- replacement
      }
    }
  }
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(bytes, con)
  invisible(path)
}
generated_pdfs <- project_path("output", "figures", c(
  "figure2_simulation_coverage.pdf", "figure3_pisa_forest.pdf",
  "figure4_pisa_khat.pdf", "figure_s3_weighting_sensitivity.pdf"))
invisible(lapply(generated_pdfs, normalize_pdf_dates))
cat("Built all manuscript and OSM exhibits.\n")
