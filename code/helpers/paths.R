# =============================================================================
# paths.R: self-locating root finder and path guard
# =============================================================================
#
# Purpose : Portable path helpers for the replication package. Discovers the
#           package root from the pvstackr-replication.Rproj marker (or the
#           PVSTACKR_REPLICATION_ROOT override), rejects absolute or parent-
#           escaping paths, resolves root-relative paths, and creates the
#           output/{figures,tables,results} tree. Also declares the five track
#           contracts (track name, entrypoint, default mode) the runner reads.
#           Inputs : a starting directory or relative path fragments.
#           Outputs: normalized absolute paths; the track-contract data frame.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

pv_is_absolute_path <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, !is.na(path))
  grepl("^(?:/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path, perl = TRUE)
}

pv_normalize_path <- function(path, must_work = FALSE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

pv_find_root <- function(start = getwd(), max_up = 12L) {
  override <- Sys.getenv("PVSTACKR_REPLICATION_ROOT", unset = "")
  if (nzchar(override)) {
    root <- pv_normalize_path(override, must_work = TRUE)
    marker <- file.path(root, "pvstackr-replication.Rproj")
    if (!file.exists(marker)) {
      stop("PVSTACKR_REPLICATION_ROOT does not contain pvstackr-replication.Rproj: ", root,
           call. = FALSE)
    }
    return(root)
  }

  current <- pv_normalize_path(start, must_work = TRUE)
  for (i in seq_len(as.integer(max_up) + 1L)) {
    if (file.exists(file.path(current, "pvstackr-replication.Rproj"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop("Could not locate the replication-package root from: ", start,
       call. = FALSE)
}

pv_assert_relative_path <- function(path, label = "path") {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop(label, " must be one non-empty character string", call. = FALSE)
  }
  if (pv_is_absolute_path(path)) {
    stop(label, " must be relative to the replication-package root: ", path,
         call. = FALSE)
  }
  parts <- strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1L]]
  if (any(parts == "..")) {
    stop(label, " must not escape the replication-package root: ", path,
         call. = FALSE)
  }
  invisible(path)
}

pv_path <- function(..., root = pv_find_root()) {
  pieces <- c(...)
  if (!length(pieces)) return(root)
  for (piece in pieces) pv_assert_relative_path(piece)
  file.path(root, pieces)
}

pv_relative_path <- function(path, root = pv_find_root()) {
  root_norm <- pv_normalize_path(root, must_work = TRUE)
  path_norm <- pv_normalize_path(path, must_work = FALSE)
  prefix <- paste0(root_norm, "/")
  if (identical(path_norm, root_norm)) return(".")
  if (!startsWith(path_norm, prefix)) {
    stop("Path is outside the replication-package root: ", path_norm,
         call. = FALSE)
  }
  substring(path_norm, nchar(prefix) + 1L)
}

pv_output_directories <- function(root = pv_find_root()) {
  file.path(root, "output", c("figures", "tables", "results"))
}

pv_ensure_output_directories <- function(root = pv_find_root()) {
  dirs <- pv_output_directories(root)
  ok <- vapply(dirs, dir.create, logical(1L), recursive = TRUE,
               showWarnings = FALSE)
  missing <- dirs[!dir.exists(dirs)]
  if (length(missing)) {
    stop("Could not create output directories: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(dirs)
}

pv_track_contracts <- function() {
  data.frame(
    track = c("quick", "intermediate", "simulation", "pisa", "verify"),
    entrypoint = c(
      "code/04_exhibits/run_quick.R",
      "code/04_exhibits/run_intermediate.R",
      "code/02_simulation/run_simulation.R",
      "code/03_pisa/run_pisa.R",
      "verification/run_verification.R"
    ),
    default_mode = c("shipped", "cached", "smoke", "cached", "quick"),
    stringsAsFactors = FALSE
  )
}

