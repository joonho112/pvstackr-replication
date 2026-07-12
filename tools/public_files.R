# =============================================================================
# public_files.R: Canonical public-release file boundary
# =============================================================================
#
# Purpose : Defines pv_public_files(root), the single source of truth for the
#           set of files inside the public replication-release boundary. It
#           walks the whitelisted directories and root files, normalizes
#           separators, and excludes build caches, rendered HTML, internal
#           tooling, local microdata, and full output shards. Input: package
#           root; output: sorted boundary-relative paths the tools enumerate.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================

pv_public_files <- function(root) {
  roots <- c("code", "config", "data", "docs", "book", "tests",
             "verification", "stan", ".github", "output", "manifest", "tools")
  top <- c("README.md", "LICENSE", "CITATION.cff", "renv.lock", "run_all.R",
           "pvstackr-replication.Rproj", ".gitignore", ".gitattributes")
  files <- top[file.exists(file.path(root, top))]
  for (dir in roots) {
    path <- file.path(root, dir)
    if (dir.exists(path)) {
      files <- c(files, file.path(dir, list.files(
        path, recursive = TRUE, all.files = TRUE, no.. = TRUE
      )))
    }
  }
  files <- unique(gsub("\\\\", "/", files))
  excluded <- grepl(
    "(^|/)(_book|_freeze|[.]quarto|[.]Rproj.user|cache|reports|scratch|[.]tmp)(/|$)|[.]html$|[.]DS_Store$",
    files
  )
  excluded <- excluded | files %in% c(
    "manifest/artifacts.csv", "verification/publication-scan.csv"
  )
  excluded <- excluded | grepl("^tools/internal/", files)
  excluded <- excluded | grepl("^data/pisa/local/", files)
  excluded <- excluded | grepl(
    "^output/(pisa_full|simulation-shards|simulation-smoke)/", files
  )
  excluded <- excluded | grepl("^output/results/simulation-full/", files)
  sort(files[!excluded & file.exists(file.path(root, files)) &
               !dir.exists(file.path(root, files))])
}
