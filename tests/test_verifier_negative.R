#!/usr/bin/env Rscript
# =============================================================================
# test_verifier_negative.R: Adversarial verifier negative-control suite
# =============================================================================
#
# Purpose : Adversarial negative controls proving the verifier fails closed.
#           In an isolated release copy it injects tampering and checks each
#           guard rejects it: a corrupted numeric anchor, a duplicate key, a
#           tampered listed file, an unlisted public file, an embedded binary
#           credential, a compressed archive, and RDS/YAML/authoring-trace
#           controls. Any wrongly accepted control exits the suite nonzero.
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
source(file.path(root, "verification", "compare_numeric.R"))
expected <- data.frame(id = c("a", "b"), value = c(1, 2))
corrupt <- data.frame(id = c("a", "b"), value = c(1, 2.1))
failed <- inherits(try(assert_numeric_tables(corrupt, expected, keys = "id", tolerance = 1e-8), silent = TRUE), "try-error")
if (!failed) stop("Corrupted numeric anchor did not fail", call. = FALSE)
duplicate <- rbind(expected, expected[1, ])
duplicate_failed <- inherits(try({
  if (anyDuplicated(duplicate$id)) stop("duplicate")
}, silent = TRUE), "try-error")
if (!duplicate_failed) stop("Duplicate-key control did not fail", call. = FALSE)

# Exercise the real release-boundary verifier and scanner in an isolated copy.
source(file.path(root, "tools", "public_files.R"), local = FALSE)
scratch <- tempfile("public-verifier-negative-")
dir.create(scratch, recursive = TRUE)
public <- c(pv_public_files(root), "manifest/artifacts.csv")
for (rel in public) {
  target <- file.path(scratch, rel)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(root, rel), target, overwrite = TRUE,
                 copy.mode = TRUE, copy.date = TRUE)) {
    stop("Could not prepare verifier fixture: ", rel, call. = FALSE)
  }
}
on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
run_tool <- function(rel) {
  out <- suppressWarnings(system2(rscript,
    c("--vanilla", shQuote(file.path(scratch, rel))),
    stdout = TRUE, stderr = TRUE))
  attr(out, "status") %||% 0L
}
`%||%` <- function(x, y) if (is.null(x)) y else x

if (run_tool("verification/verify_manifest.R") != 0L) {
  stop("Baseline manifest fixture did not verify.", call. = FALSE)
}

cascade_path <- file.path(scratch, "data", "precomputed", "pisa",
                          "sample_cascade.csv")
cascade_fixture <- read.csv(cascade_path, check.names = FALSE)
cascade_fixture$released_students[[1L]] <-
  cascade_fixture$released_students[[1L]] + 1L
write.csv(cascade_fixture, cascade_path, row.names = FALSE)
if (run_tool("verification/verify_claims.R") == 0L) {
  stop("Claim verifier accepted a changed public PISA row count.",
       call. = FALSE)
}
invisible(file.copy(file.path(root, "data", "precomputed", "pisa",
                              "sample_cascade.csv"), cascade_path,
                    overwrite = TRUE))

slice_contract_path <- file.path(scratch, "data", "pisa",
                                 "slice-contract.csv")
slice_fixture <- read.csv(slice_contract_path, check.names = FALSE)
slice_fixture$columns[[1L]] <- slice_fixture$columns[[1L]] + 1L
write.csv(slice_fixture, slice_contract_path, row.names = FALSE)
if (run_tool("verification/verify_claims.R") == 0L) {
  stop("Claim verifier accepted a changed PISA slice contract.",
       call. = FALSE)
}
invisible(file.copy(file.path(root, "data", "pisa", "slice-contract.csv"),
                    slice_contract_path, overwrite = TRUE))

listed <- file.path(scratch, ".gitattributes")
write("# deliberate byte change", listed, append = TRUE)
if (run_tool("verification/verify_manifest.R") == 0L) {
  stop("Real manifest verifier accepted a modified listed file.", call. = FALSE)
}
invisible(file.copy(file.path(root, ".gitattributes"), listed, overwrite = TRUE))

unlisted <- file.path(scratch, "docs", "unlisted-negative-control.md")
writeLines("deliberately unlisted", unlisted)
if (run_tool("verification/verify_manifest.R") == 0L) {
  stop("Real manifest verifier accepted an unlisted public file.", call. = FALSE)
}
unlink(unlisted)

binary_secret <- file.path(scratch, "data", "binary-secret-negative-control.bin")
writeBin(c(charToRaw("header"), as.raw(0),
           charToRaw(paste0("api_", "key = ", "\"not-a-real-key\"")), as.raw(0),
           charToRaw("footer")), binary_secret)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted a credential pattern in a binary file.",
       call. = FALSE)
}
unlink(binary_secret)

allowed_binary_secret <- file.path(scratch, "data",
                                   "binary-secret-negative-control.png")
writeBin(c(charToRaw("PNG fixture"), as.raw(0),
           charToRaw(paste0("api_", "key = ", "not-a-real-key"))),
         allowed_binary_secret)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted a credential in an allowlisted binary.",
       call. = FALSE)
}
unlink(allowed_binary_secret)

rds_secret <- file.path(scratch, "data", "rds-secret-negative-control.rds")
rds_fixture <- list(paste0("not-a-real-", "key"))
names(rds_fixture) <- paste0("api_", "key")
saveRDS(rds_fixture, rds_secret)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted a credential field in an RDS object.",
       call. = FALSE)
}
unlink(rds_secret)

rds_value_secret <- file.path(scratch, "data",
                              "rds-value-secret-negative-control.rds")
saveRDS(list(note = paste0("access_", "token: not-a-real-token")),
        rds_value_secret)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted a credential value in an RDS object.",
       call. = FALSE)
}
unlink(rds_value_secret)

rds_attribute_secret <- file.path(
  scratch, "data", "rds-attribute-secret-negative-control.rds"
)
rds_attribute_fixture <- 1
attr(rds_attribute_fixture, paste0("api_", "key")) <- "not-a-real-key"
saveRDS(rds_attribute_fixture, rds_attribute_secret)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted a credential attribute in an RDS object.",
       call. = FALSE)
}
unlink(rds_attribute_secret)

rds_unsupported <- file.path(scratch, "data",
                             "rds-unsupported-negative-control.rds")
saveRDS(new.env(parent = emptyenv()), rds_unsupported)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted an unsupported serialized environment.",
       call. = FALSE)
}
unlink(rds_unsupported)

corrupt_rds <- file.path(scratch, "data", "corrupt-negative-control.rds")
writeLines("not an R serialization", corrupt_rds)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted an unreadable RDS object.", call. = FALSE)
}
unlink(corrupt_rds)

yaml_secret <- file.path(scratch, "config", "secret-negative-control.yml")
writeLines(paste0("pass", "word: not-a-real-secret"), yaml_secret)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted an unquoted YAML credential.",
       call. = FALSE)
}
unlink(yaml_secret)

authoring_trace <- file.path(scratch, "docs", "authoring-negative-control.md")
writeLines(paste0("Generated by Chat", "GPT"), authoring_trace)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted a non-project authoring trace.",
       call. = FALSE)
}
unlink(authoring_trace)

compressed_secret <- file.path(scratch, "data",
                               "compressed-secret-negative-control.gz")
con <- gzfile(compressed_secret, open = "wb")
writeLines(paste0("api_", "key = ", "\"not-a-real-key\""), con)
close(con)
if (run_tool("verification/static_publication_scan.R") == 0L) {
  stop("Publication scanner accepted a compressed archive payload.",
       call. = FALSE)
}

cat("Verifier negative controls PASS: numeric and manifest tamper; text, YAML, allowlisted-binary, RDS, compressed, and non-project authoring controls.\n")
