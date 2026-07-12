# =============================================================================
# verify_public_safety.R: check public-artifact safety and manifest integrity
# =============================================================================
#
# Purpose : Focused check. Scans every curated public artifact (RDS, CSV,
#           JSON) for unsafe absolute paths or credentials, and verifies that
#           the public manifest lists only portable repository-relative paths,
#           that every listed file exists, that each SHA-256 still matches,
#           and that no OECD unit-record data is included in the manifest.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
source(file.path("code", "03_pisa", "00_config.R"))

artifact_files <- c(
  list.files(PISA_PRECOMPUTED_DIR, full.names = TRUE)
)
for (path in artifact_files) {
  ext <- tools::file_ext(path)
  object <- if (ext == "rds") {
    readRDS(path)
  } else if (ext == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")
    jsonlite::read_json(path, simplifyVector = FALSE)
  } else next
  bad <- pisa_scan_character_values(object, basename(path))
  pisa_assert(!length(bad), paste("Unsafe public artifact values:", paste(bad, collapse = "\n")))
}

manifest <- utils::read.csv(file.path(PISA_PRECOMPUTED_DIR, "public_manifest.csv"),
                            stringsAsFactors = FALSE)
pisa_assert(all(vapply(manifest$path, pisa_safe_relative_path, logical(1))),
            "Public manifest contains a non-portable path.")
pisa_assert(all(file.exists(manifest$path)), "A manifested PISA artifact is missing.")
observed_hashes <- vapply(manifest$path, pisa_sha256_file, character(1))
pisa_assert(identical(unname(observed_hashes), manifest$sha256),
            "A PISA artifact no longer matches the public manifest.")
pisa_assert(!any(grepl("data/pisa/local|data/pisa/source|data/pisa/derived", manifest$path)),
            "Public manifest must not include OECD unit-record data.")
cat("PISA public-artifact safety: PASS\n")
