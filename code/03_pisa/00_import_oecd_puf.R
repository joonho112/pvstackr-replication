#!/usr/bin/env Rscript
# =============================================================================
# 00_import_oecd_puf.R: build the local USA/Korea slice from OECD PUFs
# =============================================================================
#
# Purpose : Stage 00. Rebuilds the canonical 11,006 x 99 analysis slice from
#           OECD 2022 student/school Public-Use Files the user supplies under
#           the OECD terms. Verifies each file's name, size, and SHA-256
#           against source-metadata.csv, projects the USA/Korea columns,
#           merges the school non-response weight, and writes the slice only
#           if its hash matches the frozen release. Redistributes no PISA data.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

read_oecd <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) stop("Package 'haven' is required.", call. = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (ext == "sav") return(as.data.frame(haven::read_sav(path)))
  if (ext %in% c("sas7bdat", "sas7bdat")) return(as.data.frame(haven::read_sas(path)))
  stop("Expected an OECD .sav or .sas7bdat file: ", path, call. = FALSE)
}

import_oecd_puf <- function(student_file, school_file,
                            output = PISA_SOURCE_FILE) {
  if (!file.exists(student_file) || !file.exists(school_file)) {
    stop("Student and school PUF files must exist locally.", call. = FALSE)
  }
  metadata_file <- file.path(PISA_ROOT, "data", "pisa", "source-metadata.csv")
  metadata <- utils::read.csv(metadata_file, stringsAsFactors = FALSE,
                              colClasses = "character")
  supplied <- c(student = student_file, school = school_file)
  for (role in names(supplied)) {
    expected <- metadata[metadata$role == role, , drop = FALSE]
    if (nrow(expected) != 1L) {
      stop("PISA source metadata must contain exactly one row for ", role, ".",
           call. = FALSE)
    }
    observed_name <- basename(supplied[[role]])
    if (!identical(toupper(observed_name), toupper(expected$extracted_member[[1L]]))) {
      stop("Unexpected ", role, " filename: ", observed_name,
           ". Expected ", expected$extracted_member[[1L]], ".", call. = FALSE)
    }
    observed_size <- as.numeric(file.info(supplied[[role]])$size)
    observed_hash <- pisa_sha256_file(supplied[[role]])
    if (!identical(observed_size, as.numeric(expected$size_bytes[[1L]])) ||
        !identical(observed_hash, expected$sha256[[1L]])) {
      stop("The ", role, " PUF does not match the frozen source version. ",
           "See data/pisa/DATA_NOTICE.md before proceeding.", call. = FALSE)
    }
  }
  student <- read_oecd(student_file)
  school <- read_oecd(school_file)
  required_student <- c("CNT", "CNTSCHID", "CNTSTUID", "ESCS", "ST004D01T",
                        "IMMIG", "HISCED", "W_FSTUWT", PISA_REPWT_VARS,
                        PISA_PV_VARS)
  required_school <- c("CNT", "CNTSCHID", "W_SCHGRNRABWT")
  miss_s <- setdiff(required_student, names(student))
  miss_c <- setdiff(required_school, names(school))
  if (length(miss_s) || length(miss_c)) {
    stop("Required PUF variables are absent. Student: ", paste(miss_s, collapse = ", "),
         "; school: ", paste(miss_c, collapse = ", "), call. = FALSE)
  }
  student <- student[as.character(student$CNT) %in% PISA_COUNTRIES, required_student, drop = FALSE]
  student$.source_order <- seq_len(nrow(student))
  school <- unique(school[as.character(school$CNT) %in% PISA_COUNTRIES,
                          required_school, drop = FALSE])
  out <- merge(student, school, by = c("CNT", "CNTSCHID"), all.x = TRUE, sort = FALSE)
  out <- out[order(out$.source_order), , drop = FALSE]
  out$.source_order <- NULL
  out <- out[, PISA_SOURCE_COLUMNS, drop = FALSE]
  if (!identical(dim(out), PISA_SOURCE_DIM) || anyNA(out$W_SCHGRNRABWT)) {
    stop(sprintf("Imported slice does not match the frozen %s x %s contract.",
                 format(PISA_SOURCE_DIM[[1L]], big.mark = ","),
                 PISA_SOURCE_DIM[[2L]]), call. = FALSE)
  }
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, output, version = 3)
  observed <- pisa_sha256_file(output)
  if (!identical(observed, PISA_SOURCE_SHA256)) {
    stop("The slice was created, but its hash differs from the frozen release. OECD may have revised the PUF. Observed: ",
         observed, call. = FALSE)
  }
  cat("Local canonical slice created and hash verified.\n")
  invisible(output)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  value <- function(flag) { i <- match(flag, args); if (is.na(i) || i == length(args)) NULL else args[[i+1L]] }
  student <- value("--student"); school <- value("--school")
  if (is.null(student) || is.null(school)) {
    stop("Usage: Rscript code/03_pisa/00_import_oecd_puf.R --student FILE --school FILE", call. = FALSE)
  }
  import_oecd_puf(student, school)
}
