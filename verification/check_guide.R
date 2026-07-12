#!/usr/bin/env Rscript
# =============================================================================
# check_guide.R: Check the rendered Quarto reproducibility guide
# =============================================================================
#
# Purpose : Reads the eight expected pages of the rendered guide under
#           book/_book/ and confirms all are present. Extracts every href,
#           resolves local links, and fails closed on any broken link or on
#           unsafe or unfinished text (author paths, unfinished-work markers, or
#           control characters). Writes verification/reports/guide-qa.csv.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file0), ".."), mustWork = TRUE)
site <- file.path(root, "book", "_book")
expected <- c("index.html", "setup.html", "methods.html", "simulation.html",
              "pisa-data.html", "pisa-workflows.html", "reproducibility.html", "faq.html")
if (!all(file.exists(file.path(site, expected)))) stop("Rendered guide is incomplete", call. = FALSE)
html <- unlist(lapply(file.path(site, expected), readLines, warn = FALSE), use.names = FALSE)
matches <- regmatches(html, gregexpr('href="[^"]+"', html, perl = TRUE))
hrefs <- unique(gsub('^href="|"$', "", unlist(matches, use.names = FALSE)))
local <- hrefs[!grepl("^(https?:|mailto:|#|javascript:)", hrefs)]
local <- sub("[#?].*$", "", local)
local <- local[nzchar(local)]
missing <- local[!file.exists(file.path(site, local))]
if (length(missing)) stop("Broken local guide links: ", paste(unique(missing), collapse = ", "), call. = FALSE)
if (any(grepl("TODO|/Users/|/home/|[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", html,
              ignore.case = TRUE, perl = TRUE))) stop("Unsafe or unfinished guide text", call. = FALSE)
report <- data.frame(pages = length(expected), local_links = length(unique(local)),
                     broken_links = 0L, static_checks = "pass",
                     note = "Manual browser layout inspection is recorded separately in the Phase 7 gate")
dir.create(file.path(root, "verification", "reports"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(report, file.path(root, "verification", "reports", "guide-qa.csv"), row.names = FALSE)
cat(sprintf("Guide QA PASS: %d pages and %d local links.\n", length(expected), length(unique(local))))
