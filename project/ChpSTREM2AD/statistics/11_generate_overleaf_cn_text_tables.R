root <- "I:/researchR/project/ChpSTREM2AD"

latest_lines <- trimws(readLines(file.path(root, "result", "LATEST.txt"), warn = FALSE))
run_id_line <- latest_lines[grepl("^run_id=", latest_lines)]
run_id <- if (length(run_id_line) > 0) sub("^run_id=", "", run_id_line[1]) else latest_lines[1]

result_dir <- file.path(root, "result", run_id)
analysis_csv <- file.path(result_dir, "data_clean", "ChpSTREM2AD_analysis_dataset.csv")

if (!file.exists(analysis_csv)) {
  candidate_dirs <- list.dirs(file.path(root, "result"), full.names = TRUE, recursive = FALSE)
  candidate_dirs <- candidate_dirs[grepl("[0-9]{8}_[0-9]{6}", basename(candidate_dirs))]
  candidate_dirs <- sort(candidate_dirs, decreasing = TRUE)
  found <- FALSE
  for (cand in candidate_dirs) {
    cand_csv <- file.path(cand, "data_clean", "ChpSTREM2AD_analysis_dataset.csv")
    if (file.exists(cand_csv)) {
      result_dir <- cand
      analysis_csv <- cand_csv
      found <- TRUE
      break
    }
  }
  if (!found) {
    stop("No valid result directory containing ChpSTREM2AD_analysis_dataset.csv was found.")
  }
}

summary_dir <- file.path(result_dir, "summary")
overleaf_dir <- file.path(root, "document", "Overleaf_CN")
tables_dir <- file.path(overleaf_dir, "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read.csv(analysis_csv, check.names = FALSE, stringsAsFactors = FALSE)
dat$Group <- ifelse(dat$S_DX_label == "AD", "Dementia", dat$S_DX_label)
dat$Group <- factor(dat$Group, levels = c("CN", "MCI", "Dementia"))

escape_tex <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

fmt_num <- function(x, digits = 2) {
  if (length(x) == 0 || all(is.na(x))) return("")
  formatC(x, digits = digits, format = "f")
}

fmt_p <- function(p) {
  if (length(p) == 0 || all(is.na(p))) return("")
  p <- as.numeric(p[1])
  if (is.na(p)) return("")
  if (p < 0.001) return("$<$0.001")
  formatC(p, digits = 3, format = "f")
}

mean_sd <- function(x, digits = 2) {
  if (all(is.na(x))) return("")
  sprintf("%s $\\pm$ %s",
          fmt_num(mean(x, na.rm = TRUE), digits),
          fmt_num(sd(x, na.rm = TRUE), digits))
}

median_iqr <- function(x, digits = 2) {
  if (all(is.na(x))) return("")
  q <- quantile(x, c(0.25, 0.50, 0.75), na.rm = TRUE)
  sprintf("%s [%s, %s]",
          fmt_num(q[[2]], digits),
          fmt_num(q[[1]], digits),
          fmt_num(q[[3]], digits))
}

n_pct <- function(x) {
  if (all(is.na(x))) return("")
  n <- sum(x, na.rm = TRUE)
  pct <- 100 * mean(x, na.rm = TRUE)
  sprintf("%d (%.1f%%)", n, pct)
}

group_n <- function(group) sum(dat$Group == group, na.rm = TRUE)

kw_p <- function(var) {
  d <- dat[!is.na(dat[[var]]) & !is.na(dat$Group), c(var, "Group")]
  if (nrow(d) == 0) return(NA_real_)
  kruskal.test(d[[var]] ~ d$Group)$p.value
}

chisq_p <- function(var) {
  d <- dat[!is.na(dat[[var]]) & !is.na(dat$Group), c(var, "Group")]
  if (nrow(d) == 0) return(NA_real_)
  suppressWarnings(chisq.test(table(d[[var]], d$Group))$p.value)
}

overall_p_map <- read.csv(file.path(summary_dir, "group_comparisons_overall.csv"),
                          check.names = FALSE, stringsAsFactors = FALSE)
overall_p_lookup <- function(variable) {
  idx <- match(variable, overall_p_map$variable)
  if (is.na(idx)) return(NA_real_)
  overall_p_map$p_value[idx]
}

write_main_table <- function(rows, path, colspec = ">{\\raggedright\\arraybackslash}p{0.30\\textwidth}ccccc") {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines(c(
    "\\begingroup",
    "\\small",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\renewcommand{\\arraystretch}{1.14}",
    sprintf("\\begin{tabularx}{\\textwidth}{%s}", colspec),
    "\\toprule",
    "Variable & Overall & CN & MCI & Dementia & Overall $P$ \\\\",
    "\\midrule"
  ), con)

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]
    if (isTRUE(row$is_section)) {
      writeLines(sprintf("\\multicolumn{6}{l}{\\textbf{%s}} \\\\", escape_tex(row$variable)), con)
    } else {
      cells <- c(row$variable, row$Overall, row$CN, row$MCI, row$Dementia, row$P)
      cells <- vapply(cells, escape_tex, character(1))
      writeLines(paste0(paste(cells, collapse = " & "), " \\\\"), con)
    }
  }

  writeLines(c(
    "\\bottomrule",
    "\\end{tabularx}",
    "\\endgroup"
  ), con)
}

write_long_table <- function(df, path, widths, font_cmd = "\\scriptsize", colsep = 4, arraystretch = 1.10) {
  stopifnot(length(widths) == ncol(df))
  colspec <- paste0(">{\\raggedright\\arraybackslash}p{", widths, "\\textwidth}", collapse = "")
  header <- paste(names(df), collapse = " & ")

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines(c(
    "\\begingroup",
    font_cmd,
    sprintf("\\setlength{\\tabcolsep}{%dpt}", colsep),
    sprintf("\\renewcommand{\\arraystretch}{%.2f}", arraystretch),
    sprintf("\\begin{longtable}{%s}", colspec),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endhead"
  ), con)

  for (i in seq_len(nrow(df))) {
    row <- vapply(df[i, , drop = FALSE], function(x) escape_tex(as.character(x)), character(1))
    writeLines(paste0(paste(row, collapse = " & "), " \\\\"), con)
  }

  writeLines(c(
    "\\bottomrule",
    "\\end{longtable}",
    "\\endgroup"
  ), con)
}

## ---- Main Table 1A ----
tab1a <- data.frame(
  variable = c(
    "Demographics",
    "Sample size, n",
    "Age, years",
    "Female, n (%)",
    "Education, years",
    "APOE $\\varepsilon$4 carrier, n (%)",
    "Clinical measures",
    "MMSE",
    "MoCA",
    "CDR-SB",
    "ADAS13"
  ),
  Overall = "",
  CN = "",
  MCI = "",
  Dementia = "",
  P = "",
  is_section = FALSE,
  stringsAsFactors = FALSE
)
tab1a$is_section[c(1, 7)] <- TRUE
tab1a[tab1a$variable == "Sample size, n", c("Overall", "CN", "MCI", "Dementia")] <- c(
  nrow(dat), group_n("CN"), group_n("MCI"), group_n("Dementia")
)
tab1a[tab1a$variable == "Age, years", c("Overall", "CN", "MCI", "Dementia")] <- c(
  mean_sd(dat$S_AGE), mean_sd(dat$S_AGE[dat$Group == "CN"]),
  mean_sd(dat$S_AGE[dat$Group == "MCI"]), mean_sd(dat$S_AGE[dat$Group == "Dementia"])
)
tab1a[tab1a$variable == "Age, years", "P"] <- fmt_p(overall_p_lookup("S_AGE"))

female_vec <- dat$S_PTGENDER_label == "Female"
tab1a[tab1a$variable == "Female, n (%)", c("Overall", "CN", "MCI", "Dementia")] <- c(
  n_pct(female_vec), n_pct(female_vec[dat$Group == "CN"]),
  n_pct(female_vec[dat$Group == "MCI"]), n_pct(female_vec[dat$Group == "Dementia"])
)
tab1a[tab1a$variable == "Female, n (%)", "P"] <- fmt_p(overall_p_lookup("S_PTGENDER"))

tab1a[tab1a$variable == "Education, years", c("Overall", "CN", "MCI", "Dementia")] <- c(
  mean_sd(dat$PTEDUCAT), mean_sd(dat$PTEDUCAT[dat$Group == "CN"]),
  mean_sd(dat$PTEDUCAT[dat$Group == "MCI"]), mean_sd(dat$PTEDUCAT[dat$Group == "Dementia"])
)
tab1a[tab1a$variable == "Education, years", "P"] <- fmt_p(kw_p("PTEDUCAT"))

apoe_vec <- dat$APOE401_label == "Carrier"
tab1a[tab1a$variable == "APOE $\\varepsilon$4 carrier, n (%)", c("Overall", "CN", "MCI", "Dementia")] <- c(
  n_pct(apoe_vec), n_pct(apoe_vec[dat$Group == "CN"]),
  n_pct(apoe_vec[dat$Group == "MCI"]), n_pct(apoe_vec[dat$Group == "Dementia"])
)
tab1a[tab1a$variable == "APOE $\\varepsilon$4 carrier, n (%)", "P"] <- fmt_p(chisq_p("APOE401_label"))

for (var in c("MMSE", "MOCA")) {
  row_name <- ifelse(var == "MMSE", "MMSE", "MoCA")
  tab1a[tab1a$variable == row_name, c("Overall", "CN", "MCI", "Dementia")] <- c(
    mean_sd(dat[[var]]), mean_sd(dat[[var]][dat$Group == "CN"]),
    mean_sd(dat[[var]][dat$Group == "MCI"]), mean_sd(dat[[var]][dat$Group == "Dementia"])
  )
  tab1a[tab1a$variable == row_name, "P"] <- fmt_p(kw_p(var))
}

for (var in c("CDRSB", "ADAS13")) {
  row_name <- ifelse(var == "CDRSB", "CDR-SB", "ADAS13")
  tab1a[tab1a$variable == row_name, c("Overall", "CN", "MCI", "Dementia")] <- c(
    median_iqr(dat[[var]]), median_iqr(dat[[var]][dat$Group == "CN"]),
    median_iqr(dat[[var]][dat$Group == "MCI"]), median_iqr(dat[[var]][dat$Group == "Dementia"])
  )
  tab1a[tab1a$variable == row_name, "P"] <- fmt_p(kw_p(var))
}
write_main_table(tab1a, file.path(tables_dir, "table_1a_main_text.tex"))

## ---- Main Table 1B ----
mean_sd_var <- function(var, digits = 2) {
  c(mean_sd(dat[[var]], digits),
    mean_sd(dat[[var]][dat$Group == "CN"], digits),
    mean_sd(dat[[var]][dat$Group == "MCI"], digits),
    mean_sd(dat[[var]][dat$Group == "Dementia"], digits))
}

tab1b <- data.frame(
  variable = c(
    "CSF biomarkers",
    "sTREM2, pg/mL",
    "A$\\beta$, pg/mL",
    "Tau, pg/mL",
    "P-Tau, pg/mL",
    "Neuroimaging features",
    "ChP/ICV",
    "Absolute ChP volume, mm$^3$",
    "Estimated total intracranial volume, mm$^3$",
    "Hippocampal volume, mm$^3$",
    "Lateral ventricular volume, mm$^3$"
  ),
  Overall = "",
  CN = "",
  MCI = "",
  Dementia = "",
  P = "",
  is_section = FALSE,
  stringsAsFactors = FALSE
)
tab1b$is_section[c(1, 6)] <- TRUE
tab1b[tab1b$variable == "sTREM2, pg/mL", c("Overall", "CN", "MCI", "Dementia")] <- mean_sd_var("MSD_STREM2CORRECTED", 2)
tab1b[tab1b$variable == "sTREM2, pg/mL", "P"] <- fmt_p(overall_p_lookup("MSD_STREM2CORRECTED"))

biomarker_map <- c(
  "A$\\beta$, pg/mL" = "S_ABETA",
  "Tau, pg/mL" = "TAU",
  "P-Tau, pg/mL" = "PTAU"
)
for (row_name in names(biomarker_map)) {
  var <- biomarker_map[[row_name]]
  tab1b[tab1b$variable == row_name, c("Overall", "CN", "MCI", "Dementia")] <- c(
    median_iqr(dat[[var]]), median_iqr(dat[[var]][dat$Group == "CN"]),
    median_iqr(dat[[var]][dat$Group == "MCI"]), median_iqr(dat[[var]][dat$Group == "Dementia"])
  )
  tab1b[tab1b$variable == row_name, "P"] <- fmt_p(overall_p_lookup(var))
}

tab1b[tab1b$variable == "ChP/ICV", c("Overall", "CN", "MCI", "Dementia")] <- mean_sd_var("ChPICV", 5)
tab1b[tab1b$variable == "ChP/ICV", "P"] <- fmt_p(kw_p("ChPICV"))
tab1b[tab1b$variable == "Absolute ChP volume, mm$^3$", c("Overall", "CN", "MCI", "Dementia")] <- mean_sd_var("ChP_SUM", 2)
tab1b[tab1b$variable == "Absolute ChP volume, mm$^3$", "P"] <- fmt_p(kw_p("ChP_SUM"))
tab1b[tab1b$variable == "Estimated total intracranial volume, mm$^3$", c("Overall", "CN", "MCI", "Dementia")] <- mean_sd_var("EstimatedTotalIntraCranialVol", 2)
tab1b[tab1b$variable == "Estimated total intracranial volume, mm$^3$", "P"] <- fmt_p(kw_p("EstimatedTotalIntraCranialVol"))
tab1b[tab1b$variable == "Hippocampal volume, mm$^3$", c("Overall", "CN", "MCI", "Dementia")] <- mean_sd_var("Hippocampus_SUM", 2)
tab1b[tab1b$variable == "Hippocampal volume, mm$^3$", "P"] <- fmt_p(kw_p("Hippocampus_SUM"))
tab1b[tab1b$variable == "Lateral ventricular volume, mm$^3$", c("Overall", "CN", "MCI", "Dementia")] <- mean_sd_var("LV_SUM", 2)
tab1b[tab1b$variable == "Lateral ventricular volume, mm$^3$", "P"] <- fmt_p(kw_p("LV_SUM"))
write_main_table(tab1b, file.path(tables_dir, "table_1b_main_text.tex"))

## ---- Main Table 2 ----
diag_models <- read.csv(file.path(summary_dir, "diagnosis_hierarchical_models.csv"),
                        check.names = FALSE, stringsAsFactors = FALSE)
ext_models <- read.csv(file.path(summary_dir, "advanced_biomarker_adjusted_overall.csv"),
                       check.names = FALSE, stringsAsFactors = FALSE)
timewin <- read.csv(file.path(summary_dir, "time_window_sensitivity_summary_table.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE)
lv_models <- read.csv(file.path(summary_dir, "lv_adjusted_models.csv"),
                      check.names = FALSE, stringsAsFactors = FALSE)
alt_defs <- read.csv(file.path(summary_dir, "alternative_chp_definition_models.csv"),
                     check.names = FALSE, stringsAsFactors = FALSE)

biomarker_label <- c(
  MSD_STREM2CORRECTED = "sTREM2",
  TAU = "Tau",
  PTAU = "P-Tau",
  S_ABETA = "A$\\beta$"
)
scope_label <- c(
  primary_adjusted = "Primary adjusted",
  diagnosis_adjusted = "+ Diagnosis",
  diagnosis_phase_protocol_site_adjusted = "+ Diagnosis + phase/site/T1 family"
)

diag_rows <- subset(diag_models, term == "exposure" & model_scope %in% names(scope_label) & group == "Overall")
diag_tab <- data.frame(
  `Analysis block` = "Primary regression models",
  Model = paste(unname(biomarker_label[diag_rows$exposure]), unname(scope_label[diag_rows$model_scope]), sep = " | "),
  `Std. beta` = fmt_num(diag_rows$std_beta, 3),
  `95\\% CI` = paste(fmt_num(diag_rows$conf.low, 3), "to", fmt_num(diag_rows$conf.high, 3)),
  P = vapply(diag_rows$p.value, fmt_p, character(1)),
  N = diag_rows$n,
  stringsAsFactors = FALSE
)

ext_rows <- subset(ext_models, term == "exposure")
ext_rows$Model <- ifelse(grepl("PTAU", ext_rows$model_name),
                         "sTREM2 adjusted for P-Tau + A$\\beta$",
                         "sTREM2 adjusted for Tau + A$\\beta$")
ext_tab <- data.frame(
  `Analysis block` = "Biomarker-adjusted models",
  Model = ext_rows$Model,
  `Std. beta` = fmt_num(ext_rows$estimate, 3),
  `95\\% CI` = paste(fmt_num(ext_rows$conf.low, 3), "to", fmt_num(ext_rows$conf.high, 3)),
  P = vapply(ext_rows$p.value, fmt_p, character(1)),
  N = ext_rows$n,
  stringsAsFactors = FALSE
)

time_tab <- data.frame(
  `Analysis block` = "Restricted time-window sensitivity",
  Model = paste(
    ifelse(grepl("sTREM2", timewin$Biomarker), "sTREM2",
           ifelse(grepl("PTAU|P-TAU", timewin$Biomarker, ignore.case = TRUE), "P-Tau",
                  ifelse(grepl("(?<!P-)TAU", timewin$Biomarker, ignore.case = TRUE, perl = TRUE), "Tau", "A$\\beta$"))),
    ifelse(grepl("30", timewin$Subset), "$\\leq$30 days",
           ifelse(grepl("90", timewin$Subset), "$\\leq$90 days", "Full sample")),
    sep = " | "
  ),
  `Std. beta` = timewin$`Std β`,
  `95\\% CI` = timewin$`95% CI`,
  P = timewin$P,
  N = ifelse(grepl("30", timewin$Subset), "700",
             ifelse(grepl("90", timewin$Subset), "727", "735")),
  stringsAsFactors = FALSE
)

lv_rows <- subset(lv_models, term == "exposure")
lv_rows$Model <- ifelse(grepl("lateral", lv_rows$model_spec),
                        paste(unname(biomarker_label[lv_rows$exposure]), "Primary + lateral ventricle volume", sep = " | "),
                        paste(unname(biomarker_label[lv_rows$exposure]), "Primary adjusted", sep = " | "))
lv_tab <- data.frame(
  `Analysis block` = "Lateral ventricle-adjusted models",
  Model = lv_rows$Model,
  `Std. beta` = fmt_num(lv_rows$std_beta, 3),
  `95\\% CI` = paste(fmt_num(lv_rows$conf.low, 3), "to", fmt_num(lv_rows$conf.high, 3)),
  P = vapply(lv_rows$p.value, fmt_p, character(1)),
  N = lv_rows$n,
  stringsAsFactors = FALSE
)

alt_subset <- alt_defs[alt_defs$group == "Overall" & alt_defs$exposure == "MSD_STREM2CORRECTED", ]
alt_order <- c("ratio_ChPICV", "absolute_ChP_SUM", "right_ChP_ratio")
alt_subset <- alt_subset[match(alt_order, alt_subset$model_name), ]
alt_tab <- data.frame(
  `Analysis block` = "Alternative ChP definitions",
  Model = c("ChP/ICV ratio | sTREM2", "Absolute ChP volume + ICV | sTREM2", "Right ChP/ICV ratio | sTREM2"),
  `Std. beta` = fmt_num(alt_subset$std_beta, 3),
  `95\\% CI` = paste(fmt_num(alt_subset$conf.low, 3), "to", fmt_num(alt_subset$conf.high, 3)),
  P = vapply(alt_subset$p.value, fmt_p, character(1)),
  N = alt_subset$n,
  stringsAsFactors = FALSE
)

table2 <- rbind(diag_tab, ext_tab, time_tab, lv_tab, alt_tab)
names(table2) <- c("Analysis block", "Model", "Std. beta", "95\\% CI", "P", "N")
write_long_table(table2, file.path(tables_dir, "table_2_main_text.tex"),
                 widths = c(0.20, 0.29, 0.10, 0.19, 0.08, 0.06),
                 font_cmd = "\\scriptsize", colsep = 3, arraystretch = 1.08)

## ---- Supplementary tables ----
matching <- read.csv(file.path(summary_dir, "matching_smd_summary.csv"),
                     check.names = FALSE, stringsAsFactors = FALSE)
s1_tab <- data.frame(
  Pair = matching$pair,
  Sample = matching$sample,
  Variable = matching$variable,
  `Absolute SMD` = fmt_num(abs(matching$smd), 3),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_long_table(s1_tab, file.path(tables_dir, "supp_table_s1.tex"),
                 widths = c(0.22, 0.16, 0.24, 0.14), font_cmd = "\\small", colsep = 4)

s1a <- read.csv(file.path(summary_dir, "stage_pairwise_key_variables.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
write_long_table(s1a, file.path(tables_dir, "supp_table_s1a.tex"),
                 widths = c(0.18, 0.10, 0.10, 0.11, 0.11, 0.11, 0.11, 0.10),
                 font_cmd = "\\scriptsize", colsep = 3, arraystretch = 1.05)

s2 <- subset(diag_models, term == "exposure")
s2$Model <- ifelse(s2$model_scope == "primary_adjusted", "Primary adjusted",
                   ifelse(s2$model_scope == "diagnosis_adjusted", "Primary + diagnosis", "Within-group"))
s2$Group <- ifelse(s2$group == "AD", "Dementia", s2$group)
s2_tab <- data.frame(
  Biomarker = unname(biomarker_label[s2$exposure]),
  Model = s2$Model,
  Group = s2$Group,
  `Std. beta` = fmt_num(s2$std_beta, 3),
  `95\\% CI` = paste(fmt_num(s2$conf.low, 3), "to", fmt_num(s2$conf.high, 3)),
  P = vapply(s2$p.value, fmt_p, character(1)),
  Q = vapply(s2$q_value, fmt_p, character(1)),
  N = s2$n,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_long_table(s2_tab, file.path(tables_dir, "supp_table_s2.tex"),
                 widths = c(0.11, 0.15, 0.10, 0.09, 0.17, 0.08, 0.08, 0.06),
                 font_cmd = "\\scriptsize", colsep = 3, arraystretch = 1.05)

s3 <- subset(read.csv(file.path(summary_dir, "biologic_sensitivity_models.csv"),
                      check.names = FALSE, stringsAsFactors = FALSE), term == "exposure")
s3_tab <- data.frame(
  Subset = c("A$\\beta$-negative (all)", "A$\\beta$-negative (all)", "A$\\beta$-negative (all)",
             "A$\\beta$-positive (all)", "A$\\beta$-positive (all)", "A$\\beta$-positive (all)",
             "A$\\beta$-negative CN", "A$\\beta$-negative CN", "A$\\beta$-negative CN",
             "A$\\beta$-positive symptomatic", "A$\\beta$-positive symptomatic", "A$\\beta$-positive symptomatic"),
  Biomarker = unname(biomarker_label[s3$exposure]),
  `Std. beta` = fmt_num(s3$std_beta, 3),
  `95\\% CI` = paste(fmt_num(s3$conf.low, 3), "to", fmt_num(s3$conf.high, 3)),
  P = vapply(s3$p.value, fmt_p, character(1)),
  Q = vapply(s3$q_value, fmt_p, character(1)),
  N = s3$n_subset,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_long_table(s3_tab, file.path(tables_dir, "supp_table_s3.tex"),
                 widths = c(0.24, 0.12, 0.09, 0.17, 0.08, 0.08, 0.06),
                 font_cmd = "\\scriptsize", colsep = 3, arraystretch = 1.05)

s4 <- subset(read.csv(file.path(summary_dir, "phase_site_protocol_sensitivity.csv"),
                      check.names = FALSE, stringsAsFactors = FALSE), term == "exposure")
scope_map <- c(
  primary_adjusted = "Primary adjusted",
  diagnosis_adjusted = "+ Diagnosis",
  diagnosis_phase_adjusted = "+ Diagnosis + phase",
  diagnosis_protocol_adjusted = "+ Diagnosis + T1 family",
  diagnosis_site_adjusted = "+ Diagnosis + site",
  diagnosis_phase_protocol_site_adjusted = "+ Diagnosis + phase/site/T1 family"
)
s4_tab <- data.frame(
  Biomarker = unname(biomarker_label[s4$exposure]),
  `Adjustment set` = unname(scope_map[s4$sensitivity_scope]),
  `Std. beta` = fmt_num(s4$std_beta, 3),
  `95\\% CI` = paste(fmt_num(s4$conf.low, 3), "to", fmt_num(s4$conf.high, 3)),
  P = vapply(s4$p.value, fmt_p, character(1)),
  Q = vapply(s4$q_value, fmt_p, character(1)),
  N = s4$n,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_long_table(s4_tab, file.path(tables_dir, "supp_table_s4.tex"),
                 widths = c(0.12, 0.26, 0.09, 0.17, 0.08, 0.08, 0.06),
                 font_cmd = "\\scriptsize", colsep = 3, arraystretch = 1.04)

s5a <- read.csv(file.path(summary_dir, "advanced_interaction_comparison.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
s5a_tab <- data.frame(
  Model = "ChP/ICV ~ sTREM2 × diagnosis",
  `Reduced adj. R$^2$` = fmt_num(s5a$reduced_adj_r_squared, 3),
  `Full adj. R$^2$` = fmt_num(s5a$full_adj_r_squared, 3),
  `Interaction F` = fmt_num(s5a$interaction_f, 3),
  `Interaction P` = fmt_p(s5a$interaction_p_value),
  N = s5a$n,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_long_table(s5a_tab, file.path(tables_dir, "supp_table_s5a.tex"),
                 widths = c(0.28, 0.13, 0.13, 0.10, 0.10, 0.06),
                 font_cmd = "\\small", colsep = 4)

s5b <- read.csv(file.path(summary_dir, "advanced_nonlinear_tests.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
test_map <- c(
  quadratic_term = "Quadratic term",
  linear_vs_quadratic = "Linear vs quadratic",
  linear_vs_spline = "Linear vs restricted spline"
)
s5b_tab <- data.frame(
  Model = "ChP/ICV ~ sTREM2",
  Test = unname(test_map[s5b$test_name]),
  Statistic = fmt_num(s5b$statistic, 3),
  P = vapply(s5b$p_value, fmt_p, character(1)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_long_table(s5b_tab, file.path(tables_dir, "supp_table_s5b.tex"),
                 widths = c(0.26, 0.22, 0.12, 0.10),
                 font_cmd = "\\small", colsep = 4)

s6 <- read.csv(file.path(summary_dir, "collinearity_diagnostics.csv"),
               check.names = FALSE, stringsAsFactors = FALSE)
term_map <- c(
  MSD_STREM2CORRECTED = "sTREM2",
  PTAU = "P-Tau",
  TAU = "Tau",
  S_ABETA = "A$\\beta$",
  S_PTGENDER = "Sex",
  S_AGE = "Age",
  PTEDUCAT = "Education",
  APOE401 = "APOE $\\varepsilon$4"
)
s6$model_name <- ifelse(grepl("PTAU", s6$model_name), "P-Tau model", "Tau model")
s6_tab <- data.frame(
  Model = s6$model_name,
  Predictor = ifelse(s6$term %in% names(term_map), unname(term_map[s6$term]), s6$term),
  VIF = fmt_num(s6$vif, 2),
  Tolerance = fmt_num(s6$tolerance, 3),
  N = s6$n,
  stringsAsFactors = FALSE
)
write_long_table(s6_tab, file.path(tables_dir, "supp_table_s6.tex"),
                 widths = c(0.16, 0.18, 0.10, 0.12, 0.06),
                 font_cmd = "\\small", colsep = 4)

s7 <- read.csv(file.path(summary_dir, "sem_bootstrap_key_indirects.csv"),
               check.names = FALSE, stringsAsFactors = FALSE)
effect_map <- c(
  indirect = "Indirect",
  total = "Total",
  direct = "Direct",
  serial_indirect = "Serial indirect",
  total_indirect = "Total indirect"
)
path_map <- c(
  ChP_to_sTREM2_to_Cognition = "ChP $\\rightarrow$ sTREM2 $\\rightarrow$ Cog",
  sTREM2_to_ChP_to_Cognition = "sTREM2 $\\rightarrow$ ChP $\\rightarrow$ Cog",
  ChP_to_TAU_to_Cognition = "ChP $\\rightarrow$ Tau $\\rightarrow$ Cog",
  TAU_to_ChP_to_Cognition = "Tau $\\rightarrow$ ChP $\\rightarrow$ Cog",
  ChP_to_PTAU_to_Cognition = "ChP $\\rightarrow$ P-Tau $\\rightarrow$ Cog",
  PTAU_to_ChP_to_Cognition = "P-Tau $\\rightarrow$ ChP $\\rightarrow$ Cog",
  ChP_to_ABETA_to_Cognition = "ChP $\\rightarrow$ A$\\beta$ $\\rightarrow$ Cog",
  ABETA_to_ChP_to_Cognition = "A$\\beta$ $\\rightarrow$ ChP $\\rightarrow$ Cog",
  PTAU_to_sTREM2_to_ChP_to_Cognition = "P-Tau $\\rightarrow$ sTREM2 $\\rightarrow$ ChP $\\rightarrow$ Cog",
  TAU_to_sTREM2_to_ChP_to_Cognition = "Tau $\\rightarrow$ sTREM2 $\\rightarrow$ ChP $\\rightarrow$ Cog"
)
s7_tab <- data.frame(
  Path = unname(path_map[s7$model_name]),
  Effect = unname(effect_map[s7$effect]),
  Estimate = fmt_num(s7$estimate, 3),
  `95\\% bootstrap CI` = paste(fmt_num(s7$conf_low, 3), "to", fmt_num(s7$conf_high, 3)),
  P = vapply(s7$p_value, fmt_p, character(1)),
  N = s7$n,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_long_table(s7_tab, file.path(tables_dir, "supp_table_s7.tex"),
                 widths = c(0.22, 0.10, 0.10, 0.20, 0.08, 0.06),
                 font_cmd = "\\scriptsize", colsep = 4, arraystretch = 1.06)

s8 <- read.csv(file.path(summary_dir, "sample_transparency_summary.csv"),
               check.names = FALSE, stringsAsFactors = FALSE)
write_long_table(s8, file.path(tables_dir, "supp_table_s8.tex"),
                 widths = c(0.16, 0.34, 0.16), font_cmd = "\\small", colsep = 4)

s9a <- read.csv(file.path(summary_dir, "cognition_model_fit_summary.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
write_long_table(s9a, file.path(tables_dir, "supp_table_s9a.tex"),
                 widths = c(0.20, 0.26, 0.06, 0.06, 0.07, 0.06, 0.05, 0.06),
                 font_cmd = "\\small", colsep = 4)

s9b <- read.csv(file.path(summary_dir, "cognition_measurement_diagnostics_summary.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
s9b <- s9b[, c("Section", "Item", "Estimate", "95% CI", "P", "Note")]
names(s9b)[4] <- "95\\% CI"
s9b$Note <- ifelse(s9b$Section == "Reference loadings",
                   paste0("Unstandardized loading. ", s9b$Note),
                   s9b$Note)
write_long_table(s9b, file.path(tables_dir, "supp_table_s9b.tex"),
                 widths = c(0.16, 0.13, 0.12, 0.18, 0.08, 0.24),
                 font_cmd = "\\scriptsize", colsep = 3)

s10 <- read.csv(file.path(summary_dir, "sem_primary_path_summary.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
s10$Pathway <- c(
  "ChP $\\rightarrow$ sTREM2 $\\rightarrow$ Cognition",
  "sTREM2 $\\rightarrow$ ChP $\\rightarrow$ Cognition",
  "ChP $\\rightarrow$ Tau $\\rightarrow$ Cognition",
  "Tau $\\rightarrow$ ChP $\\rightarrow$ Cognition",
  "ChP $\\rightarrow$ P-Tau $\\rightarrow$ Cognition",
  "P-Tau $\\rightarrow$ ChP $\\rightarrow$ Cognition",
  "ChP $\\rightarrow$ A$\\beta$ $\\rightarrow$ Cognition",
  "A$\\beta$ $\\rightarrow$ ChP $\\rightarrow$ Cognition"
)
names(s10) <- c(
  "Pathway",
  "a Std $\\beta$",
  "b Std $\\beta$",
  "c' Std $\\beta$",
  "Indirect estimate",
  "Indirect 95\\% boot CI",
  "Indirect P/q",
  "Total estimate",
  "Total 95\\% boot CI",
  "Fit",
  "FIML N"
)
write_long_table(s10, file.path(tables_dir, "supp_table_s10.tex"),
                 widths = c(0.20, 0.08, 0.08, 0.08, 0.08, 0.16, 0.10, 0.08, 0.15, 0.15, 0.05),
                 font_cmd = "\\scriptsize", colsep = 2, arraystretch = 1.04)

s11 <- read.csv(file.path(summary_dir, "time_window_sensitivity_summary_table.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
names(s11)[4] <- "95\\% CI"
write_long_table(s11, file.path(tables_dir, "supp_table_s11.tex"),
                 widths = c(0.18, 0.12, 0.09, 0.18, 0.08),
                 font_cmd = "\\small", colsep = 4)

s12 <- read.csv(file.path(summary_dir, "lv_adjusted_summary_table.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)
s12$Adjustment <- c("Primary adjusted", "Primary + lateral ventricle volume",
                    "Primary adjusted", "Primary + lateral ventricle volume",
                    "Primary adjusted", "Primary + lateral ventricle volume",
                    "Primary adjusted", "Primary + lateral ventricle volume")
names(s12)[4] <- "95\\% CI"
write_long_table(s12, file.path(tables_dir, "supp_table_s12.tex"),
                 widths = c(0.12, 0.22, 0.08, 0.18, 0.08),
                 font_cmd = "\\small", colsep = 4)

message("Clean Overleaf_CN textual tables generated in: ", tables_dir)
