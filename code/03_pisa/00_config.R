# =============================================================================
# 00_config.R: shared paths, invariants, and guards for the PISA track
# =============================================================================
#
# Purpose : Foundational config sourced first by every PISA stage (00-10)
#           and verify_* check. Defines repository-relative paths, the frozen
#           11,006 x 99 source-slice contract (SHA-256, dimensions, column
#           order), the USA/Korea set, plausible-value/replicate-weight names,
#           expected sample and school counts, the Pareto-k = 0.7 gate, and
#           shared hashing, assertion, safe-read, and path-safety helpers.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

pisa_find_root <- function() {
  env_root <- Sys.getenv("PVSTACKR_REPLICATION_ROOT", unset = "")
  starts <- unique(c(env_root, getwd(), dirname(getwd()),
                     dirname(dirname(getwd())), dirname(dirname(dirname(getwd())))))
  starts <- starts[nzchar(starts)]
  for (candidate in starts) {
    marker <- file.path(candidate, "code", "03_pisa", "00_config.R")
    if (file.exists(marker)) return(normalizePath(candidate, mustWork = TRUE))
  }
  stop("Could not locate the replication-package root. Set PVSTACKR_REPLICATION_ROOT.",
       call. = FALSE)
}

PISA_ROOT <- pisa_find_root()
PISA_CODE_DIR <- file.path(PISA_ROOT, "code", "03_pisa")
PISA_SOURCE_DIR <- file.path(PISA_ROOT, "data", "pisa", "local", "source")
PISA_DERIVED_DIR <- file.path(PISA_ROOT, "data", "pisa", "local", "derived")
PISA_PRECOMPUTED_DIR <- file.path(PISA_ROOT, "data", "precomputed", "pisa")
PISA_SOURCE_FILE <- file.path(PISA_SOURCE_DIR, "pisa2022_read_usa_kor.rds")
PISA_ANALYTIC_FILE <- file.path(PISA_DERIVED_DIR, "pisa2022_read_analytic.rds")

PISA_SLICE_CONTRACT_FILE <- file.path(PISA_ROOT, "data", "pisa",
                                      "slice-contract.csv")
.pisa_slice_contract <- utils::read.csv(
  PISA_SLICE_CONTRACT_FILE, stringsAsFactors = FALSE, check.names = FALSE
)
if (nrow(.pisa_slice_contract) != 1L ||
    !identical(.pisa_slice_contract$contract_version,
               "pisa2022-reading-slice-v1")) {
  stop("The PISA slice contract is missing or malformed.", call. = FALSE)
}
PISA_SOURCE_SHA256 <- .pisa_slice_contract$sha256[[1L]]
PISA_SOURCE_DIM <- c(as.integer(.pisa_slice_contract$rows[[1L]]),
                     as.integer(.pisa_slice_contract$columns[[1L]]))
PISA_COUNTRIES <- c("USA", "KOR")
PISA_PV_VARS <- paste0("PV", seq_len(10L), "READ")
PISA_REPWT_VARS <- paste0("W_FSTURWT", seq_len(80L))
PISA_ID_VARS <- c("CNT", "CNTSCHID", "CNTSTUID")
PISA_PROJECTED_VARS <- c(PISA_ID_VARS, "ESCS", "W_FSTUWT",
                         PISA_REPWT_VARS, PISA_PV_VARS,
                         "W_SCHGRNRABWT")
PISA_SOURCE_COLUMNS <- c(
  "CNT", "CNTSCHID", "CNTSTUID", "ESCS", "ST004D01T", "IMMIG", "HISCED",
  "W_FSTUWT", PISA_REPWT_VARS, PISA_PV_VARS, "W_SCHGRNRABWT"
)
if (length(PISA_SOURCE_COLUMNS) != PISA_SOURCE_DIM[[2L]]) {
  stop("PISA source-column contract does not match the declared dimension.",
       call. = FALSE)
}
PISA_EXPECTED_RELEASED <- c(USA = 4552L, KOR = 6454L)
PISA_EXPECTED_ANALYTIC <- c(USA = 4342L, KOR = 6391L)
PISA_EXPECTED_RELEASED_SCHOOLS <- c(USA = 154L, KOR = 186L)
PISA_EXPECTED_ANALYTIC_SCHOOLS <- c(USA = 150L, KOR = 186L)
PISA_KHAT_THRESHOLD <- 0.7

pisa_sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for SHA-256 checks.", call. = FALSE)
  }
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

pisa_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}

pisa_read_source <- function() {
  pisa_assert(file.exists(PISA_SOURCE_FILE),
              "Local PISA slice is missing. Follow data/pisa/DATA_NOTICE.md; the OECD terms prohibit third-party redistribution.")
  pisa_assert(identical(pisa_sha256_file(PISA_SOURCE_FILE), PISA_SOURCE_SHA256),
              "Canonical PISA slice SHA-256 does not match the release contract.")
  x <- readRDS(PISA_SOURCE_FILE)
  pisa_assert(is.data.frame(x), "Canonical PISA slice must be a data frame.")
  pisa_assert(identical(dim(x), PISA_SOURCE_DIM),
              sprintf("Canonical PISA slice must be %s by %s.",
                      format(PISA_SOURCE_DIM[[1L]], big.mark = ","),
                      PISA_SOURCE_DIM[[2L]]))
  pisa_assert(identical(names(x), PISA_SOURCE_COLUMNS),
              "Canonical PISA columns or order changed.")
  x
}

pisa_safe_relative_path <- function(path) {
  !grepl("(^/|^[A-Za-z]:|~|\\\\Users\\\\|/Users/|/home/|token|secret)",
         path, ignore.case = TRUE)
}

pisa_scan_character_values <- function(x, where = "object") {
  bad <- character(0)
  walk <- function(z, label) {
    if (is.character(z)) {
      hit <- z[grepl("(/Users/|/home/|^[A-Za-z]:\\\\|BEGIN [A-Z ]*PRIVATE KEY|api[_-]?key|access[_-]?token)",
                     z, ignore.case = TRUE)]
      if (length(hit)) bad <<- c(bad, paste0(label, ": ", hit))
    } else if (is.list(z)) {
      nms <- names(z)
      for (i in seq_along(z)) walk(z[[i]], paste0(label, "$", if (is.null(nms)) i else nms[[i]]))
    }
  }
  walk(x, where)
  unique(bad)
}

dir.create(PISA_DERIVED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PISA_PRECOMPUTED_DIR, recursive = TRUE, showWarnings = FALSE)
