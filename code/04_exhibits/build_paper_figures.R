# =============================================================================
# build_paper_figures.R: publication (ggplot2) Figures 2-4 and .tex table rows
# =============================================================================
#
# Purpose : Regenerate the publication ggplot2 versions of Figures 2-4 and the
#           LaTeX table-row fragments from shipped precomputed evidence. The
#           simulation exhibits use the beta-parity evidence (both fixed-effect
#           slopes, all estimators at full replication); the PISA exhibits and
#           the design grid use the frozen precomputed CSVs. Mode "final"
#           writes the final figure PDFs and .tex rows; mode "variants" writes
#           candidate layouts for selection.
#           Inputs : data/precomputed/{simulation,pisa}/*.csv.
#           Outputs: output/figures/figure2..4 PDFs; output/tables/tex/*.tex;
#           input snapshots in output/results/exhibit-data.
# Paper   : Lee, J., Williams, M. R., & Savitsky, T. D. (2026). One Markov Chain
#           Monte Carlo Fit for Many Plausible Values: A Calibrated Stacked
#           Posterior Workflow for Bayesian Multilevel Models of Large-Scale
#           Assessment Data. arXiv preprint.
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT
# =============================================================================
#
# Contents:
#   data load + snapshot  Read precomputed abc/khat/design/parity CSVs and
#     snapshot them into output/results/exhibit-data.
#   naming + theme  Workflow labels, Okabe-Ito palette, theme_v2(), pdfout().
#   Figure 2  Simulation coverage, both slopes, strata bands (patchwork).
#   Figure 3  PISA A-vs-C estimates forest.
#   Figure 4  PISA Pareto k-hat by plausible value.
#   final export  Final figure PDFs and the Table 3/4/5 and OSM .tex rows.
# =============================================================================
# =============================================================================
# v5 figure/table codebase.
# Simulation exhibits (Table 3, Figure 2, OSM Table E.2) read the 2026-07
# beta-parity evidence tables (dev/figtab/beta_parity/): all four estimators
# at full replication, BOTH fixed-effect slopes. PISA exhibits (Tables 4-5,
# OSM khat) and the design grid still read the frozen codebase-v2 CSVs and
# must stay byte-identical across regenerations.
# Run: Rscript --vanilla make_figtab_v5.R [variants|final]
# =============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(ggforce)
  library(patchwork)
})

args0 <- commandArgs(trailingOnly = FALSE)
file0 <- sub("^--file=", "", grep("^--file=", args0, value = TRUE)[1L])
ROOT <- normalizePath(file.path(dirname(file0), "..", ".."), mustWork = TRUE)
CB <- file.path(ROOT, "data", "precomputed")
PAR <- file.path(CB, "simulation")
VAR <- file.path(ROOT, "output", "figures", "variants")
FIG <- file.path(ROOT, "output", "figures")
FLT <- file.path(ROOT, "output", "tables", "tex")
OFG <- FIG
OFL <- FLT
SNAP <- file.path(ROOT, "output", "results", "exhibit-data")
for (d in c(VAR, FIG, FLT, OFG, OFL, SNAP)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

snap <- function(p) { file.copy(p, file.path(SNAP, basename(p)), overwrite = TRUE)
  read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA")) }

abc   <- snap(file.path(CB, "pisa", "abc_estimates.csv"))
khat  <- snap(file.path(CB, "pisa", "psis_diagnostics.csv"))
design<- snap(file.path(CB, "simulation", "design.csv"))
pcond <- snap(file.path(PAR, "parity_by_condition.csv"))
pstrat<- snap(file.path(PAR, "parity_by_stratum.csv"))

# ---- naming ------------------------------------------------------------------
WF <- c(bayes_a = "Per-PV (MCMC)", freq_per_pv = "Per-PV (REML)",
        c_direct = "Calibrated stack")
WF_LV <- c("Per-PV (MCMC)", "Per-PV (REML)", "Calibrated stack")
# Okabe-Ito colorblind-safe
COL <- c("Per-PV (MCMC)" = "#0072B2", "Per-PV (REML)" = "#7F7F7F",
         "Calibrated stack" = "#D55E00")
SHP <- c("Per-PV (MCMC)" = 16, "Per-PV (REML)" = 15, "Calibrated stack" = 17)

theme_v2 <- function(base = 11) {
  theme_minimal(base_size = base, base_family = "Times") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = .3, colour = "grey85"),
          strip.text = element_text(face = "bold", size = base, hjust = 0),
          strip.background = element_blank(),
          axis.title = element_text(size = base),
          axis.text = element_text(size = base - 1, colour = "grey15"),
          legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size = base - .5),
          plot.margin = margin(6, 10, 4, 6))
}
pdfout <- function(p, path, w, h) {
  grDevices::pdf(path, width = w, height = h, family = "Times", useDingbats = FALSE)
  print(p); grDevices::dev.off()
  stopifnot(file.exists(path), file.size(path) > 1000); cat("wrote", path, "\n")
}

# ---- shared data: 24 Gaussian conditions -------------------------------------
tiers <- design |> filter(!is.na(n_rep), n_rep > 0, design_condition_id != "",
                          route == "gaussian") |>
  distinct(design_condition_id, rep_tier, n_rep, profile, ICC_y, J, nbar, ICC_x, rho_PV, M)
stopifnot(nrow(tiers) == 24)
strat_lab <- c("mainstream" = "Mainstream",
               "F-tail"     = "Small-school stress",
               "sensitivity"= "PV sensitivity")

cov <- pcond |> filter(leg %in% names(WF)) |>
  mutate(wf = factor(WF[leg], levels = WF_LV),
         stratum = factor(strat_lab[rep_tier], levels = unname(strat_lab)))
stopifnot(nrow(cov) == 24 * 3, !any(is.na(cov$stratum)))

## ---- Figure 2 condition labels (v5): design factors, reader-facing ----------
## Mainstream and stress rows: outcome ICC (ICC_y), J, nbar (+ covariate ICC
## ICC_x where it departs from the profile pairing: 0.20<->0.15, 0.45<->0.30).
## PV-sensitivity rows: rho_PV and M around the ICC_y = 0.20 mainstream anchor
## (J = 100, nbar = 20); the two ICC_y = 0.45 rows are labeled by their
## deviations. Plotmath strings, parsed on the axis.
COND_ORDER <- c(
  ## mainstream: USA by (J, nbar), then USA covariate-ICC variant,
  ## then Korea by J, then Korea covariate-ICC variant
  "T2-c2","T2-c3","T2-c4","T2-c1","T2-c5","T2-c6","T3-c1","T2-c7","T3-c3",
  "T5-c1","T1-c6","T3-c4",
  ## small-school stress: USA, Korea
  "T1-c1","T4-c1",
  ## PV sensitivity: by rho_PV then M, Korea-profile rows last
  "T4-c3","SK-c1","T4-c4","SM-c1","SM-c2","T4-c5","T4-c6","T4-c7",
  "Q7-c2","Q8-c1")
mk_lab <- function(icc_y, J, nbar, iccx_extra = NA, rho = NA, M = NA,
                   korea_profile = FALSE) {
  if (!is.na(rho)) {                       # PV-sensitivity stratum
    if (korea_profile) {
      if (nbar != 20)
        sprintf('paste(ICC[y]=="%.2f", ", ", bar(italic(n))==%d)', icc_y, nbar)
      else
        sprintf('paste(ICC[y]=="%.2f", ", ", rho[PV]=="%.2f", ", ", italic(M)==%d)',
                icc_y, rho, M)
    } else {
      sprintf('paste(rho[PV]=="%.2f", ", ", italic(M)==%d)', rho, M)
    }
  } else if (!is.na(iccx_extra)) {
    sprintf('paste(ICC[y]=="%.2f", ", ", italic(J)==%d, ", ", bar(italic(n))==%d, ", ", ICC[x]=="%.2f")',
            icc_y, J, nbar, iccx_extra)
  } else {
    sprintf('paste(ICC[y]=="%.2f", ", ", italic(J)==%d, ", ", bar(italic(n))==%d)',
            icc_y, J, nbar)
  }
}
lab_tbl <- tiers |> mutate(
  iccx_base = ifelse(profile == "USA", 0.15, 0.30),
  lab = dplyr::case_when(
    rep_tier == "sensitivity" & profile == "KOR" ~
      mapply(mk_lab, ICC_y, J, nbar, NA, rho_PV, M, TRUE),
    rep_tier == "sensitivity" ~
      mapply(mk_lab, ICC_y, J, nbar, NA, rho_PV, M, FALSE),
    abs(ICC_x - iccx_base) > 1e-9 ~
      mapply(mk_lab, ICC_y, J, nbar, ICC_x),
    TRUE ~ mapply(mk_lab, ICC_y, J, nbar)
  ))
LBL <- setNames(lab_tbl$lab, lab_tbl$design_condition_id)
stopifnot(setequal(COND_ORDER, tiers$design_condition_id),
          !anyDuplicated(unname(LBL[COND_ORDER])))
cov$cond <- factor(cov$design_condition_id, levels = rev(COND_ORDER))

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[[1]] else "final"

# =============================================================================
# FIG 2 — simulation coverage, BOTH slopes (columns: beta^W | beta^B).
# Strata as stacked bands with bold TOP titles (the panel-heading style of
# the single-slope v5 figure), built with patchwork; slope headers appear
# once, on the top band; x axis once, on the bottom band.
# =============================================================================
covL <- bind_rows(
  cov |> transmute(cond, stratum, wf, slope = "W",
                   coverage = covW, lo = covW_lo, hi = covW_hi),
  cov |> transmute(cond, stratum, wf, slope = "B",
                   coverage = covB, lo = covB_lo, hi = covB_hi)) |>
  mutate(slope = factor(slope, levels = c("W", "B"),
                        labels = c('Within-school~slope~beta^W',
                                   'Between-school~slope~beta^B')))
band_title <- c("Mainstream"          = "Mainstream (12 conditions, R = 100)",
                "Small-school stress" = "Small-school stress (2 conditions, R = 400)",
                "PV sensitivity"      = "PV sensitivity (10 conditions, R = 200)")
mk_band <- function(strat, show_strip, show_xaxis) {
  ggplot(covL |> filter(stratum == strat),
         aes(coverage, cond, colour = wf, shape = wf)) +
    geom_vline(xintercept = 0.95, linewidth = .45, colour = "grey30") +
    geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0,
                  linewidth = .45, position = position_dodge(width = .7), alpha = .9) +
    geom_point(size = 2.0, position = position_dodge(width = .7)) +
    scale_colour_manual(values = COL) + scale_shape_manual(values = SHP) +
    scale_y_discrete(labels = function(ids) parse(text = unname(LBL[ids]))) +
    scale_x_continuous(limits = c(.828, 1.005), breaks = c(.85, .90, .95, 1.00)) +
    facet_grid(. ~ slope, labeller = labeller(slope = label_parsed)) +
    labs(x = if (show_xaxis)
               "Empirical coverage of nominal 95% intervals (Wilson 95% bars)"
             else NULL,
         y = NULL, title = band_title[[strat]]) +
    theme_v2() +
    theme(plot.title = element_text(face = "bold", size = 10, hjust = 0),
          strip.text.x = if (show_strip)
              element_text(face = "bold", size = 10, hjust = .5)
            else element_blank(),
          axis.text.y = element_text(size = 9),
          axis.text.x = if (show_xaxis) element_text(size = 10)
                        else element_blank(),
          panel.spacing.x = unit(14, "pt"),
          plot.margin = margin(2, 10, 2, 6))
}
f2a <- (mk_band("Mainstream", TRUE, FALSE) /
        mk_band("Small-school stress", FALSE, FALSE) /
        mk_band("PV sensitivity", FALSE, TRUE)) +
  patchwork::plot_layout(heights = c(14, 3.2, 12.6), guides = "collect") &
  theme(legend.position = "bottom")

if (mode == "variants") {
  pdfout(f2a, file.path(VAR, "fig2_A_coverage_only.pdf"), 7.5, 6.2)
}

# =============================================================================
# FIG 3 — PISA estimates forest
# =============================================================================
ac <- abc |> filter(pipeline %in% c("A", "C")) |>
  mutate(Estimand = ifelse(term == "b_ESCS_within", "Within-school ESCS effect",
                           "Between-school ESCS effect"),
         Estimand = factor(Estimand, levels = c("Within-school ESCS effect",
                                                "Between-school ESCS effect")),
         Country = factor(ifelse(country == "USA", "United States", "Korea"),
                          levels = c("Korea", "United States")),
         wf = factor(ifelse(pipeline == "A", "Per-PV workflow (10 MCMC fits)",
                            "Calibrated stack (one MCMC fit)"),
                     levels = c("Per-PV workflow (10 MCMC fits)",
                                "Calibrated stack (one MCMC fit)")))
COL3 <- c("Per-PV workflow (10 MCMC fits)" = "#0072B2",
          "Calibrated stack (one MCMC fit)" = "#D55E00")
SHP3 <- c("Per-PV workflow (10 MCMC fits)" = 16,
          "Calibrated stack (one MCMC fit)" = 17)
f3_base <- ggplot(ac, aes(estimate, Country, colour = wf, shape = wf)) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0,
                linewidth = .6, position = position_dodge(width = .55)) +
  geom_point(size = 2.8, position = position_dodge(width = .55)) +
  scale_colour_manual(values = COL3) + scale_shape_manual(values = SHP3) +
  labs(x = "Estimated ESCS effect on reading (score points per ESCS unit), 95% interval",
       y = NULL) + theme_v2(11.5)
f3a <- f3_base + facet_wrap(~ Estimand, ncol = 2, scales = "free_x") +
  scale_x_continuous(expand = expansion(mult = .08)) +
  theme(panel.spacing.x = unit(18, "pt"))
f3b <- f3_base + facet_wrap(~ Estimand, nrow = 2, scales = "free_x")
if (mode == "variants") {
  pdfout(f3a, file.path(VAR, "fig3_A_sidebyside.pdf"), 7.0, 2.9)
  pdfout(f3b, file.path(VAR, "fig3_B_stacked.pdf"), 5.6, 4.4)
}

# =============================================================================
# FIG 4 — Pareto k-hat by PV (promoted to main text)
# =============================================================================
kh <- khat |> mutate(Country = factor(ifelse(country == "USA", "United States", "Korea"),
                                      levels = c("United States", "Korea")),
                     ok = factor(ifelse(psis_ok, "Usable  (k-hat < 0.7)",
                                 "Unreliable  (k-hat >= 0.7)"),
                                 levels = c("Usable  (k-hat < 0.7)", "Unreliable  (k-hat >= 0.7)")))
COL4 <- c("Usable  (k-hat < 0.7)" = "#0072B2", "Unreliable  (k-hat >= 0.7)" = "#C24A4A")
f4a <- ggplot(kh, aes(factor(pv_index), pareto_khat, fill = ok)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.7, ymax = Inf,
           fill = "grey82", alpha = .35) +
  geom_hline(yintercept = 0.7, linetype = "22", colour = "grey25", linewidth = .5) +
  geom_col(width = .68, colour = "grey20", linewidth = .25) +
  facet_wrap(~ Country, ncol = 2) +
  scale_fill_manual(values = COL4) +
  scale_y_continuous(limits = c(0, 1.45), expand = c(0, 0)) +
  labs(x = "Plausible value",
       y = expression("Pareto " * hat(k) * " of the importance weights")) +
  theme_v2(11) + theme(panel.grid.major.x = element_blank(),
                       panel.grid.major.y = element_line(linewidth = .3, colour = "grey88"))
f4b <- ggplot(kh, aes(pareto_khat, factor(pv_index, levels = 10:1), colour = ok)) +
  annotate("rect", xmin = 0.7, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = .4, colour = NA) +
  geom_vline(xintercept = 0.7, linetype = "22", colour = "grey25", linewidth = .5) +
  geom_segment(aes(x = 0, xend = pareto_khat, yend = factor(pv_index, levels = 10:1)),
               linewidth = .7) +
  geom_point(size = 2.6) +
  facet_wrap(~ Country, ncol = 2) +
  scale_colour_manual(values = COL4) +
  scale_x_continuous(limits = c(0, 1.45), expand = c(0, 0)) +
  labs(x = expression("Pareto " * hat(k) * " of the importance weights"),
       y = "Plausible value") + theme_v2(11) +
  theme(panel.grid.major.y = element_blank())
if (mode == "variants") {
  pdfout(f4a, file.path(VAR, "fig4_A_bars.pdf"), 6.9, 3.1)
  pdfout(f4b, file.path(VAR, "fig4_B_lollipop.pdf"), 6.9, 3.4)
}

# =============================================================================
# FINAL EXPORT (after selection) + table fragments
# =============================================================================
if (mode == "final") {
  sel <- c("fig2=f2a", "fig3=f3a", "fig4=f4a")
  pick <- function(key) sub(paste0(key, "="), "", grep(paste0("^", key, "="), sel, value = TRUE))
  pdfout(get(pick("fig2")), file.path(FIG, "figure2_simulation_coverage.pdf"),
         ifelse(pick("fig2") == "f2a", 7.5, 8.6), 6.2)
  pdfout(get(pick("fig3")), file.path(FIG, "figure3_pisa_forest.pdf"),
         ifelse(pick("fig3") == "f3a", 7.0, 5.6), ifelse(pick("fig3") == "f3a", 2.9, 4.4))
  pdfout(get(pick("fig4")), file.path(FIG, "figure4_pisa_khat.pdf"),
         6.9, ifelse(pick("fig4") == "f4a", 3.1, 3.4))

  fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
  ## T3 rows: stratum x workflow, BOTH slopes (parity evidence tables)
  h <- pstrat |> filter(leg %in% names(WF)) |>
    mutate(stratum = factor(stratum,
                            levels = c("mainstream", "F-tail", "sensitivity")),
           wf = WF[leg]) |>
    arrange(stratum, match(leg, names(WF)))
  st_tex <- c(
    "mainstream"  = "Mainstream (12 conditions, $R=100$; 1{,}200 fits per estimator)",
    "F-tail"      = "Small-school stress (2 conditions, $R=400$; 800 fits per estimator)",
    "sensitivity" = "PV sensitivity (10 conditions, $R=200$; 2{,}000 fits per estimator)")
  lines3 <- character(0)
  for (s in levels(h$stratum)) {
    hs <- h[h$stratum == s, ]
    lines3 <- c(lines3, sprintf("\\multicolumn{7}{@{}l}{\\textit{%s}} \\\\[1pt]", st_tex[[s]]))
    lines3 <- c(lines3, apply(hs, 1, function(r) {
      sprintf("\\quad %s & %s & %s & %s & %s & %s & %s \\\\",
              r[["wf"]],
              fmt(as.numeric(r[["biasW"]]), 4), fmt(as.numeric(r[["mcseW"]]), 4),
              fmt(as.numeric(r[["covW"]]), 3),
              fmt(as.numeric(r[["biasB"]]), 4), fmt(as.numeric(r[["mcseB"]]), 4),
              fmt(as.numeric(r[["covB"]]), 3))
    }), "\\addlinespace[3pt]")
  }
  writeLines(head(lines3, -1), file.path(FLT, "tab3_rows.tex"))

  ## T4 rows: PISA per-PV vs calibrated stack
  a <- ac |> filter(pipeline == "A"); cd <- ac |> filter(pipeline == "C")
  t4 <- merge(a[, c("country","term","estimate","std_error","ci_low","ci_high")],
              cd[, c("country","term","estimate","std_error","ci_low","ci_high")],
              by = c("country","term"), suffixes = c("_A","_C"))
  t4 <- t4[order(t4$country == "KOR", t4$term != "b_ESCS_within"), ]
  lines4 <- apply(t4, 1, function(r) sprintf(
    "%s & %s & %s & %s & [%s, %s] & %s & %s & [%s, %s] \\\\",
    ifelse(r[["country"]] == "USA", "United States", "Korea"),
    ifelse(r[["term"]] == "b_ESCS_within", "Within", "Between"),
    fmt(as.numeric(r[["estimate_A"]]), 2), fmt(as.numeric(r[["std_error_A"]]), 2),
    fmt(as.numeric(r[["ci_low_A"]]), 2), fmt(as.numeric(r[["ci_high_A"]]), 2),
    fmt(as.numeric(r[["estimate_C"]]), 2), fmt(as.numeric(r[["std_error_C"]]), 2),
    fmt(as.numeric(r[["ci_low_C"]]), 2), fmt(as.numeric(r[["ci_high_C"]]), 2)))
  writeLines(lines4, file.path(FLT, "tab4_rows.tex"))

  ## T5 rows: USA reweighted stack (numbers now reportable in main)
  b_us <- abc |> filter(pipeline == "B", country == "USA")
  a_us <- abc |> filter(pipeline == "A", country == "USA")
  lines5 <- sapply(c("b_ESCS_within","b_ESCS_between"), function(tm) {
    b <- b_us[b_us$term == tm, ]; a1 <- a_us[a_us$term == tm, ]
    sprintf("%s & %s & %s & %s & %s \\\\",
            ifelse(tm == "b_ESCS_within", "Within-school ESCS", "Between-school ESCS"),
            fmt(b$estimate, 2), fmt(b$std_error, 2),
            fmt(b$estimate - a1$estimate, 3), fmt(b$std_error / a1$std_error, 3))
  })
  writeLines(lines5, file.path(FLT, "tab5_rows.tex"))

  ## design rows (24) for T2 / OSM; stratum labels use the manuscript names
  dg <- tiers |> arrange(factor(rep_tier, levels = c("mainstream","F-tail","sensitivity")),
                         design_condition_id) |>
    mutate(row = sprintf("%s & %s & %s & %.2f & %d & %d & %.2f & %.2f & %d \\\\",
                         design_condition_id, strat_lab[rep_tier], profile, ICC_y, J, nbar,
                         ICC_x, rho_PV, M)) |> pull(row)
  writeLines(dg, file.path(OFL, "tabS_design_rows.tex"))

  ## OSM: khat + n_eff table rows; per-condition L1 rows (24x3)
  linesK <- kh |> arrange(desc(Country), pv_index) |>
    mutate(row = sprintf("%s & %d & %s & %s & %s \\\\", Country, pv_index,
                         fmt(pareto_khat, 3), fmt(psis_n_eff, 1),
                         ifelse(psis_ok, "usable", "unreliable"))) |> pull(row)
  writeLines(linesK, file.path(OFL, "tabS_khat_rows.tex"))
  l1full <- cov |> arrange(stratum, design_condition_id, wf) |>
    mutate(row = sprintf("%s & %s & %s & %s [%s, %s] & %s & %s [%s, %s] \\\\",
                         design_condition_id, wf,
                         fmt(biasW, 4), fmt(covW, 3), fmt(covW_lo, 3), fmt(covW_hi, 3),
                         fmt(biasB, 4), fmt(covB, 3), fmt(covB_lo, 3), fmt(covB_hi, 3))) |>
    pull(row)
  writeLines(l1full, file.path(OFL, "tabS_L1_rows.tex"))
}
invisible(mode)
