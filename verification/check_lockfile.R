#!/usr/bin/env Rscript
# =============================================================================
# check_lockfile.R: Check renv.lock pins and package sources
# =============================================================================
#
# Purpose : Reads renv.lock via jsonlite and requires R pinned at 4.6.0, all
#           required analysis packages present, no local package sources, and
#           every source from a repository. Confirms the only non-CRAN entry is
#           the pinned public r-universe cmdstanr with matching remote metadata,
#           and that both CI workflows pin R 4.6.0. Fails closed otherwise.
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
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required", call. = FALSE)
`%||%` <- function(x, y) if (is.null(x)) y else x
lock <- jsonlite::read_json(file.path(root, "renv.lock"), simplifyVector = FALSE)
if (!identical(lock$R$Version, "4.6.0")) {
  stop("renv.lock must pin R 4.6.0; found ", lock$R$Version, call. = FALSE)
}
required <- c("digest", "jsonlite", "yaml", "loo", "posterior", "cmdstanr",
              "lme4", "Matrix", "testthat", "ggplot2", "dplyr", "tidyr",
              "ggforce", "patchwork", "brms", "haven")
missing <- setdiff(required, names(lock$Packages))
if (length(missing)) stop("renv.lock misses: ", paste(missing, collapse = ", "), call. = FALSE)
local <- names(lock$Packages)[vapply(lock$Packages, function(x) identical(x$Source, "Local"), logical(1L))]
if (length(local)) stop("renv.lock contains local packages: ", paste(local, collapse = ", "), call. = FALSE)
non_repository <- names(lock$Packages)[vapply(
  lock$Packages, function(x) !identical(x$Source, "Repository"), logical(1L)
)]
if (length(non_repository)) {
  stop("renv.lock contains non-repository sources: ",
       paste(non_repository, collapse = ", "), call. = FALSE)
}
repository <- vapply(lock$Packages, function(x) x$Repository %||% "",
                     character(1L))
allowed_non_cran <- c(cmdstanr = "https://bbsbayes.r-universe.dev")
non_cran <- names(repository)[repository != "CRAN"]
if (!identical(stats::setNames(repository[non_cran], non_cran),
               allowed_non_cran)) {
  stop("Unexpected non-CRAN repository entries: ",
       paste(sprintf("%s=%s", non_cran, repository[non_cran]), collapse = ", "),
       call. = FALSE)
}
cmdstanr <- lock$Packages$cmdstanr
cmdstanr_expected <- list(
  Version = "0.8.0",
  RemoteUrl = "https://github.com/stan-dev/cmdstanr",
  RemoteRef = "v0.8.0",
  RemoteSha = "12fe0f8fd35226dadf3abc50de726537dc9ea475"
)
for (field in names(cmdstanr_expected)) {
  if (!identical(cmdstanr[[field]], cmdstanr_expected[[field]])) {
    stop("cmdstanr lock metadata mismatch for ", field, call. = FALSE)
  }
}
workflow_files <- file.path(root, ".github", "workflows",
                            c("validation.yml", "pages.yml"))
for (workflow in workflow_files) {
  lines <- readLines(workflow, warn = FALSE)
  if (!any(grepl("r-version:[[:space:]]*['\"]4[.]6[.]0['\"]", lines))) {
    stop("CI workflow does not pin R 4.6.0: ", basename(workflow),
         call. = FALSE)
  }
}
cat(sprintf(paste0("Lockfile PASS: R 4.6.0; %d required packages; ",
                   "no local sources; cmdstanr public r-universe source pinned.\n"),
            length(required)))
