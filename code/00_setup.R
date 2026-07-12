# =============================================================================
# 00_setup.R: shared, side-effect-light setup for all reproduction tracks
# =============================================================================
#
# Purpose : Prepare a deterministic, single-threaded R environment shared by
#           every reproduction track. Locates the package root, pins the BLAS
#           and OpenMP thread counts to one, sets the L'Ecuyer-CMRG RNG and
#           reproducible options, enforces the minimum R version, ensures the
#           output/ tree exists, and (optionally) checks per-track packages and
#           the heavy Stan toolchain before a track runs.
#           Inputs : an optional track name and a check-dependencies flag.
#           Outputs: environment side effects; a preflight report via --check.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   pv_setup()  Configure env/RNG/options, ensure outputs, optional dep check.
#   pv_required_packages()  Common plus per-track required packages.
#   pv_missing_packages()  Required packages that are not installed.
#   pv_check_heavy_toolchain()  CmdStan / make / c++ / tmpdir preflight.
#   pv_print_preflight()  Human-readable readiness report (used by --check).
# =============================================================================

.pv_setup_source <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
.pv_setup_candidates <- unique(c(
  if (!is.null(.pv_setup_source)) file.path(dirname(.pv_setup_source), "..") else NULL,
  getwd(),
  file.path(getwd(), "..")
))
.pv_setup_candidates <- normalizePath(.pv_setup_candidates, winslash = "/",
                                      mustWork = FALSE)
.pv_setup_hits <- .pv_setup_candidates[
  file.exists(file.path(.pv_setup_candidates, "pvstackr-replication.Rproj"))
]
if (!length(.pv_setup_hits)) {
  stop("Could not locate the replication-package root while loading code/00_setup.R",
       call. = FALSE)
}
.pv_setup_root <- .pv_setup_hits[[1L]]
source(file.path(.pv_setup_root, "code", "helpers", "paths.R"), local = FALSE)

pv_setup <- function(track = NULL, check_dependencies = FALSE,
                     create_outputs = TRUE) {
  root <- pv_find_root(.pv_setup_root)
  Sys.setenv(PVSTACKR_REPLICATION_ROOT = root)
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    BLIS_NUM_THREADS = "1"
  )
  RNGkind(kind = "L'Ecuyer-CMRG", normal.kind = "Inversion",
          sample.kind = "Rejection")
  options(stringsAsFactors = FALSE, useFancyQuotes = FALSE, encoding = "UTF-8")

  if (getRversion() < "4.3.0") {
    stop("R 4.3.0 or newer is required; found ", getRversion(), call. = FALSE)
  }
  if (isTRUE(create_outputs)) pv_ensure_output_directories(root)

  missing <- character()
  if (isTRUE(check_dependencies) && !is.null(track)) {
    missing <- pv_missing_packages(track)
    if (length(missing)) {
      stop(
        "Missing package(s) for track '", track, "': ",
        paste(missing, collapse = ", "),
        ". Restore the project environment before running this track.",
        call. = FALSE
      )
    }
    if (track %in% c("simulation", "pisa")) pv_check_heavy_toolchain()
  }
  invisible(list(root = root, track = track, missing_packages = missing))
}

pv_required_packages <- function(track) {
  contracts <- pv_track_contracts()
  if (!track %in% contracts$track) {
    stop("Unknown track: ", track, call. = FALSE)
  }
  common <- c("digest", "jsonlite", "yaml")
  specific <- switch(
    track,
    quick = c("ggplot2", "dplyr", "tidyr", "ggforce", "patchwork"),
    intermediate = c("Matrix", "loo", "posterior"),
    simulation = c("Matrix", "lme4", "brms", "cmdstanr", "posterior"),
    pisa = c("Matrix", "lme4", "loo", "posterior", "cmdstanr", "haven"),
    verify = c("testthat")
  )
  unique(c(common, specific))
}

pv_missing_packages <- function(track) {
  required <- pv_required_packages(track)
  required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
}

pv_check_heavy_toolchain <- function(error = TRUE) {
  cmdstan_path <- if (requireNamespace("cmdstanr", quietly = TRUE)) {
    tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  } else ""
  checks <- c(
    cmdstan = nzchar(cmdstan_path) && dir.exists(cmdstan_path),
    make = nzchar(Sys.which("make")),
    cpp = nzchar(Sys.which("c++")),
    space_free_tmpdir = !grepl(" ", tempdir(), fixed = TRUE)
  )
  if (error && any(!checks)) {
    stop("Heavy-track toolchain check failed: ",
         paste(names(checks)[!checks], collapse = ", "), call. = FALSE)
  }
  list(checks = checks,
       cmdstan_path = if (checks[["cmdstan"]]) cmdstan_path else NA_character_)
}

pv_print_preflight <- function() {
  cat("R:", R.version.string, "\n")
  cat("Quarto:", if (nzchar(Sys.which("quarto")))
    paste(system2("quarto", "--version", stdout = TRUE), collapse = " ") else "not found", "\n")
  for (track in pv_track_contracts()$track) {
    missing <- pv_missing_packages(track)
    cat(sprintf("%-12s R packages: %s\n", track,
                if (length(missing)) paste("missing", paste(missing, collapse = ", ")) else "ready"))
  }
  heavy <- pv_check_heavy_toolchain(error = FALSE)
  cat("Heavy toolchain:", paste(sprintf("%s=%s", names(heavy$checks), heavy$checks), collapse = "; "), "\n")
  if (!is.na(heavy$cmdstan_path)) cat("CmdStan:", heavy$cmdstan_path, "\n")
  invisible(heavy)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args) || "--check" %in% args) {
    pv_print_preflight()
  } else {
    stop("Unknown setup option. Use --check.", call. = FALSE)
  }
}
