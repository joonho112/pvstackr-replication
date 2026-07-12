#!/usr/bin/env Rscript
# =============================================================================
# verify_artifacts.R: Check rebuilt exhibits carry frozen printed values
# =============================================================================
#
# Purpose : Loads the rebuilt exhibit tables from output/tables/ and the figure
#           PDFs from output/figures/, then asserts each carries the exact
#           frozen printed values and structure reported in the manuscript.
#           Writes verification/reports/artifact-checks.csv and fails closed if
#           any check does not pass.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   - Table 1 (workflow comparison): six rows and labels; convergence-audit
#     and MCMC-fit cells; positive-definite and all-PV k-hat report gates.
#   - Table 2 (design): three strata; 12/2/10 conditions (24 total);
#     100/400/200 replications; varied-factor text.
#   - Table 3 (headline): nine rows; max |bias| < 0.0052; coverage in
#     [.945, .986].
#   - Table 4 (PISA main): eight rows; Pipeline A estimates to 1e-10; A = C
#     agreement by construction (< 1e-12); archived Rubin df.
#   - Table 5 (reweighting): two rows; differences from Pipeline A.
#   - OSM tables: 72 simulation rows; PSIS k-hat gate (USA all < 0.7,
#     KOR all > 0.7).
#   - Figures: all five PDFs exist and are non-trivial in size.
# =============================================================================
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
root <- normalizePath(file.path(dirname(file0), ".."), mustWork = TRUE)
read_out <- function(name) utils::read.csv(file.path(root, "output", "tables", name), stringsAsFactors = FALSE, check.names = FALSE)
checks <- list()
add <- function(id, pass, actual, expected, note = "") {
  checks[[length(checks)+1L]] <<- data.frame(check_id=id, pass=isTRUE(pass), actual=as.character(actual), expected=as.character(expected), note=note, stringsAsFactors=FALSE)
}
near <- function(x, y, tol) isTRUE(all(abs(x-y) <= tol))

t1 <- read_out("table1_workflows.csv")
add("table1_rows", nrow(t1)==6L, nrow(t1), 6)
add("table1_attributes", identical(t1$attribute, c("Role in this article", "MCMC fits per model", "After the fit", "External input", "Convergence audits", "Reportable when")), paste(t1$attribute, collapse=";"), "six manuscript row labels")
audit_row <- t1[t1$attribute == "Convergence audits", c("per_PV_workflow", "calibrated_stack", "reweighted_stack")]
add("table1_convergence_audits", nrow(audit_row)==1L && identical(as.character(audit_row[1,]), c("M","1","1")), paste(as.character(audit_row[1,]), collapse=","), "M,1,1")
fit_row <- t1[t1$attribute == "MCMC fits per model", c("per_PV_workflow", "calibrated_stack", "reweighted_stack")]
add("table1_mcmc_fits", nrow(fit_row)==1L && identical(as.character(fit_row[1,]), c("M","1","1 (reuses the stacked fit)")), paste(as.character(fit_row[1,]), collapse=","), "M;1;1 (reuses the stacked fit)")
add("table1_positive_definite_gate", grepl("positive definite", t1$calibrated_stack[t1$attribute=="Reportable when"], fixed=TRUE), t1$calibrated_stack[t1$attribute=="Reportable when"], "contains positive definite target gate")
add("table1_psis_gate", grepl("k-hat < 0.7 for all M PVs", t1$reweighted_stack[t1$attribute=="Reportable when"], fixed=TRUE), t1$reweighted_stack[t1$attribute=="Reportable when"], "contains all-PV k-hat gate")
t2 <- read_out("table2_simulation_design.csv")
add("table2_rows", nrow(t2)==3L, nrow(t2), 3)
add("table2_strata", identical(t2$stratum, c("Mainstream","Small-school stress","PV sensitivity")), paste(t2$stratum,collapse=";"), "three manuscript strata")
add("table2_conditions", identical(t2$conditions, c(12L,2L,10L)) && sum(t2$conditions)==24L, paste(t2$conditions,collapse=","), "12,2,10 (total 24)")
add("table2_replications", identical(t2$replications_per_condition, c(100L,400L,200L)), paste(t2$replications_per_condition,collapse=","), "100,400,200")
add("table2_factor_text", all(c(grepl("0.20/0.15",t2$varied_factors[1],fixed=TRUE), grepl("J = 50",t2$varied_factors[2],fixed=TRUE), grepl("rho_PV",t2$varied_factors[3],fixed=TRUE), grepl("M in {5, 10, 20}",t2$varied_factors[3],fixed=TRUE))), paste(t2$varied_factors,collapse=" | "), "profile, small-school, PV reliability/count clauses")
t3 <- read_out("table3_simulation_headline.csv")
add("table3_rows", nrow(t3)==9L, nrow(t3), 9)
add("table3_max_abs_bias", max(abs(c(t3$biasW,t3$biasB))) < .0052, max(abs(c(t3$biasW,t3$biasB))), "<0.0052")
add("table3_coverage_range", min(c(t3$covW,t3$covB)) >= .945 && max(c(t3$covW,t3$covB)) <= .9863, paste(range(c(t3$covW,t3$covB)), collapse=" to "), ".945 to .986 after three-decimal display")
t4 <- read_out("table4_pisa_main.csv")
add("table4_rows", nrow(t4)==8L, nrow(t4), 8)
expected_a <- c(25.083381396,69.7401845973,27.8634509841,92.9388363584)
actual_a <- t4$estimate[t4$pipeline=="A"]
add("table4_A_estimates", near(actual_a, expected_a, 1e-10), paste(actual_a,collapse=";"), paste(expected_a,collapse=";"))
aa <- t4[t4$pipeline=="A",c("country","effect","estimate","std_error")]
cc <- t4[t4$pipeline=="C",c("country","effect","estimate","std_error")]
ac <- merge(aa,cc,by=c("country","effect"),suffixes=c("_A","_C"))
add("table4_A_C_target", max(abs(c(ac$estimate_A-ac$estimate_C,ac$std_error_A-ac$std_error_C))) < 1e-12, max(abs(c(ac$estimate_A-ac$estimate_C,ac$std_error_A-ac$std_error_C))), "<1e-12", "agreement is by construction")
add("table4_df_provenance", near(t4$df[t4$pipeline=="A"],c(349.972573636098,1360.22214097538,64.6253254892133,536.760816868024),1e-9), paste(t4$df[t4$pipeline=="A"],collapse=";"), "archived classic Rubin df")
t5 <- read_out("table5_pisa_reweighting.csv")
add("table5_rows", nrow(t5)==2L, nrow(t5), 2)
add("table5_differences", near(t5$difference_from_A,c(-.0563499368087008,-.988659355672695),1e-10), paste(t5$difference_from_A,collapse=";"), "-.0563499368;-.9886593557")
osm <- read_out("osm_simulation_results.csv")
add("osm_simulation_rows", nrow(osm)==72L, nrow(osm), 72)
kh <- read_out("osm_psis_diagnostics.csv")
kcol <- if ("pareto_khat" %in% names(kh)) "pareto_khat" else "pareto_k"
add("usa_psis", nrow(kh[kh$country=="USA",])==10L && all(kh[kh$country=="USA",kcol] < .7), sum(kh$country=="USA" & kh[[kcol]] < .7), 10)
add("kor_psis", nrow(kh[kh$country=="KOR",])==10L && all(kh[kh$country=="KOR",kcol] > .7), sum(kh$country=="KOR" & kh[[kcol]] < .7), 0)
figs <- file.path(root,"output","figures",c("figure1_workflows.pdf","figure2_simulation_coverage.pdf","figure3_pisa_forest.pdf","figure4_pisa_khat.pdf","figure_s3_weighting_sensitivity.pdf"))
add("figures_exist", all(file.exists(figs) & file.info(figs)$size > 1000), sum(file.exists(figs) & file.info(figs)$size > 1000), length(figs))

result <- do.call(rbind, checks)
dir.create(file.path(root,"verification","reports"),recursive=TRUE,showWarnings=FALSE)
utils::write.csv(result,file.path(root,"verification","reports","artifact-checks.csv"),row.names=FALSE)
if (any(!result$pass)) stop("Artifact verification failed: ",paste(result$check_id[!result$pass],collapse=", "),call.=FALSE)
cat(sprintf("Artifact verification PASS: %d checks.\n",nrow(result)))
