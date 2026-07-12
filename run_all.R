#!/usr/bin/env Rscript
# =============================================================================
# run_all.R: single command-line entry point for the replication package
# =============================================================================
#
# Purpose : Parse the command line (--track, --config, --dry-run, --check-deps),
#           validate config/defaults.yml against schema
#           "pvstackr-replication-config-v1", run the shared environment setup,
#           then launch the selected track's entrypoint in a fresh vanilla R
#           subprocess. Dispatches the five tracks declared in the config:
#           quick, intermediate, simulation, pisa, and verify.
#           Inputs : command-line arguments; config/defaults.yml.
#           Outputs: the dispatched track's exhibits/results; a process exit
#           status that is nonzero on any failure.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   pv_usage()  Print the usage / help text for the runner.
#   pv_abort()  Write an error to stderr and exit with a status code.
#   pv_script_path()  Resolve this script's own path for root discovery.
#   pv_parse_args()  Parse options and the pass-through track arguments.
#   (main)  Validate track + config, run pv_setup(), launch the entrypoint.
# =============================================================================

pv_usage <- function() {
  cat(
    "pvstackr replication runner\n\n",
    "Usage:\n",
    "  Rscript run_all.R --track TRACK [OPTIONS] [-- TRACK_ARGS]\n",
    "  Rscript run_all.R TRACK [OPTIONS] [-- TRACK_ARGS]\n\n",
    "Tracks:\n",
    "  quick          Rebuild paper exhibits from shipped evidence\n",
    "  intermediate   Recompute pooled/calibrated summaries from shipped objects\n",
    "  simulation     Run a smoke or full synthetic parity study\n",
    "  pisa           Run cached or full PISA analysis\n",
    "  verify         Verify manifests, schemas, values, and reportability gates\n\n",
    "Options:\n",
    "  --track NAME       Select a track\n",
    "  --config PATH      Root-relative configuration (default: config/defaults.yml)\n",
    "  --dry-run          Print the resolved command without running it\n",
    "  --check-deps       Check packages declared for the selected track\n",
    "  -h, --help         Show this help\n",
    "  --                 Pass all remaining arguments to the track\n",
    sep = ""
  )
}

pv_abort <- function(message, status = 2L) {
  cat("Error: ", message, "\n", sep = "", file = stderr())
  quit(save = "no", status = as.integer(status), runLast = FALSE)
}

pv_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (!length(hit)) return(normalizePath("run_all.R", mustWork = TRUE))
  raw <- sub("^--file=", "", hit[[1L]])
  raw <- gsub("~+~", " ", raw, fixed = TRUE)
  normalizePath(raw, winslash = "/", mustWork = TRUE)
}

pv_parse_args <- function(args) {
  out <- list(track = NULL, config = "config/defaults.yml", dry_run = FALSE,
              check_deps = FALSE, help = FALSE, track_args = character())
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--")) {
      if (i < length(args)) out$track_args <- args[(i + 1L):length(args)]
      break
    } else if (arg %in% c("-h", "--help")) {
      out$help <- TRUE
    } else if (identical(arg, "--dry-run")) {
      out$dry_run <- TRUE
    } else if (identical(arg, "--check-deps")) {
      out$check_deps <- TRUE
    } else if (grepl("^--track=", arg)) {
      out$track <- sub("^--track=", "", arg)
    } else if (identical(arg, "--track")) {
      i <- i + 1L
      if (i > length(args)) pv_abort("--track requires a value")
      out$track <- args[[i]]
    } else if (grepl("^--config=", arg)) {
      out$config <- sub("^--config=", "", arg)
    } else if (identical(arg, "--config")) {
      i <- i + 1L
      if (i > length(args)) pv_abort("--config requires a value")
      out$config <- args[[i]]
    } else if (!startsWith(arg, "-") && is.null(out$track)) {
      out$track <- arg
    } else {
      pv_abort(paste0("unknown option or misplaced argument: ", arg,
                      ". Put track-specific arguments after --."))
    }
    i <- i + 1L
  }
  out
}

opts <- pv_parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help)) {
  pv_usage()
  quit(save = "no", status = 0L, runLast = FALSE)
}
if (is.null(opts$track) || !nzchar(opts$track)) {
  pv_usage()
  pv_abort("a track is required")
}

root <- dirname(pv_script_path())
source(file.path(root, "code", "helpers", "paths.R"), local = FALSE)
source(file.path(root, "code", "00_setup.R"), local = FALSE)

contracts <- pv_track_contracts()
row <- contracts[contracts$track == opts$track, , drop = FALSE]
if (nrow(row) != 1L) {
  pv_abort(paste0("unknown track: ", opts$track, ". Allowed: ",
                  paste(contracts$track, collapse = ", ")))
}

tryCatch(pv_assert_relative_path(opts$config, "--config"),
         error = function(e) pv_abort(conditionMessage(e)))
config_path <- file.path(root, opts$config)
if (!file.exists(config_path)) pv_abort(paste0("configuration not found: ", opts$config))
if (!requireNamespace("yaml", quietly = TRUE)) {
  pv_abort("package 'yaml' is required to read --config; restore renv.lock")
}
config <- tryCatch(yaml::read_yaml(config_path),
                   error = function(e) pv_abort(paste0("invalid configuration: ", conditionMessage(e))))
if (!identical(config$schema_version, "pvstackr-replication-config-v1") ||
    !is.list(config$tracks) || !setequal(names(config$tracks), contracts$track)) {
  pv_abort("configuration must use schema pvstackr-replication-config-v1 and define exactly the five supported tracks")
}
for (name in names(config$tracks)) {
  item <- config$tracks[[name]]
  if (!is.list(item) || !is.character(item$entrypoint) ||
      length(item$entrypoint) != 1L || !nzchar(item$entrypoint) ||
      !is.character(item$default_mode) || length(item$default_mode) != 1L ||
      !nzchar(item$default_mode)) {
    pv_abort(paste0("invalid track contract in configuration: ", name))
  }
  tryCatch(pv_assert_relative_path(item$entrypoint, paste0("config track ", name, " entrypoint")),
           error = function(e) pv_abort(conditionMessage(e)))
}
if (!is.list(config$paths) ||
    any(!vapply(config$paths, function(x) is.character(x) && length(x) == 1L && nzchar(x), logical(1L)))) {
  pv_abort("configuration paths must be named non-empty relative strings")
}
for (name in names(config$paths)) {
  tryCatch(pv_assert_relative_path(config$paths[[name]], paste0("config path ", name)),
           error = function(e) pv_abort(conditionMessage(e)))
}

setup <- tryCatch(
  pv_setup(opts$track, check_dependencies = opts$check_deps),
  error = function(e) pv_abort(conditionMessage(e))
)
entry_rel <- config$tracks[[opts$track]]$entrypoint
entry <- file.path(root, entry_rel)

mode_display <- config$tracks[[opts$track]]$default_mode
mode_inline <- grep("^--mode=", opts$track_args, value = TRUE)
if (length(mode_inline)) mode_display <- sub("^--mode=", "", mode_inline[[1L]])
mode_pos <- match("--mode", opts$track_args)
if (!is.na(mode_pos) && mode_pos < length(opts$track_args)) mode_display <- opts$track_args[[mode_pos + 1L]]
cat("Track:      ", opts$track, "\n", sep = "")
cat("Mode:       ", mode_display, "\n", sep = "")
cat("Config:     ", opts$config, "\n", sep = "")
cat("Entrypoint: ", entry_rel, "\n", sep = "")

cmd_args <- c("--vanilla", entry, opts$track_args)
if (isTRUE(opts$dry_run)) {
  cat("Ready:      ", if (file.exists(entry)) "yes" else "no (entrypoint not installed yet)", "\n", sep = "")
  cat("Command:    ", paste(c("Rscript", vapply(cmd_args, shQuote, character(1L))), collapse = " "), "\n", sep = "")
  quit(save = "no", status = 0L, runLast = FALSE)
}
if (!file.exists(entry)) {
  pv_abort(paste0("track entrypoint is missing: ", entry_rel), status = 3L)
}

old <- setwd(root)
on.exit(setwd(old), add = TRUE)
old_env <- Sys.getenv(c("PVSTACKR_REPLICATION_ROOT", "PVSTACKR_TRACK",
                       "PVSTACKR_CONFIG"), unset = NA_character_)
on.exit({
  for (name in names(old_env)) {
    if (is.na(old_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
  }
}, add = TRUE)
Sys.setenv(
  PVSTACKR_REPLICATION_ROOT = root,
  PVSTACKR_TRACK = opts$track,
  PVSTACKR_CONFIG = opts$config
)
status <- tryCatch(
  system2(file.path(R.home("bin"), "Rscript"),
          args = c("--vanilla", entry_rel, opts$track_args)),
  error = function(e) pv_abort(paste0("could not launch track: ", conditionMessage(e)), status = 4L)
)
if (!identical(as.integer(status), 0L)) {
  pv_abort(paste0("track '", opts$track, "' exited with status ", status),
           status = if (is.na(status)) 4L else as.integer(status))
}
