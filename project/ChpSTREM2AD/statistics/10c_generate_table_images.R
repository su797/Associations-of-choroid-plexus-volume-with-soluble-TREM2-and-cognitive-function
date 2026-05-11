source(file.path("I:/researchR/project/ChpSTREM2AD/statistics", "00_setup.R"))

suppressPackageStartupMessages({
  library(grid)
})

draw_sample_flow_png <- function(output_path, boxes, notes = character()) {
  png(filename = output_path, width = 2200, height = 1500, res = 220)
  grid.newpage()

  box_width <- 0.52
  box_height <- 0.12
  x_center <- 0.31
  ys <- c(0.84, 0.63, 0.42, 0.21)

  for (i in seq_along(boxes)) {
    y <- ys[i]
    grid.roundrect(
      x = x_center, y = y,
      width = box_width, height = box_height,
      r = unit(0.02, "snpc"),
      gp = gpar(fill = "#F8FAFD", col = "#3B4A5A", lwd = 2)
    )
    grid.text(
      boxes[[i]],
      x = x_center, y = y,
      gp = gpar(fontsize = 20, fontface = if (i == 1) "bold" else "plain")
    )
    if (i < length(boxes)) {
      grid.lines(
        x = unit(c(x_center, x_center), "npc"),
        y = unit(c(y - box_height / 2 - 0.015, ys[i + 1] + box_height / 2 + 0.015), "npc"),
        arrow = arrow(length = unit(0.18, "inches"), type = "closed"),
        gp = gpar(col = "#586A7A", lwd = 2)
      )
    }
  }

  if (length(notes) > 0) {
    note_y <- 0.74
    for (note in notes) {
      grid.text(
        note,
        x = 0.62, y = note_y,
        just = "left",
        gp = gpar(fontsize = 15)
      )
      note_y <- note_y - 0.11
    }
  }

  dev.off()
}

draw_three_line_table_png <- function(data, output_path, col_widths = NULL, font_size = 11, width_px = NULL, height_px = NULL, left_align_cols = 1:2) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  n_rows <- nrow(data)
  n_cols <- ncol(data)

  if (is.null(col_widths)) {
    col_widths <- rep(1 / n_cols, n_cols)
  } else {
    col_widths <- col_widths / sum(col_widths)
  }

  if (is.null(width_px)) {
    width_px <- max(1700, 320 + n_cols * 240)
  }
  if (is.null(height_px)) {
    height_px <- max(560, 240 + (n_rows + 1) * 82)
  }

  png(filename = output_path, width = width_px, height = height_px, res = 220)
  grid.newpage()

  left <- 0.04
  right <- 0.96
  bottom <- 0.08
  table_top <- 0.84
  row_height <- min(0.14, max(0.018, (table_top - bottom) / (n_rows + 1.8)))

  x_edges <- c(left, left + cumsum((right - left) * col_widths))
  x_centers <- (x_edges[-length(x_edges)] + x_edges[-1]) / 2

  grid.lines(x = unit(c(left, right), "npc"), y = unit(c(table_top + row_height * 0.55, table_top + row_height * 0.55), "npc"), gp = gpar(lwd = 2))
  grid.lines(x = unit(c(left, right), "npc"), y = unit(c(table_top - row_height * 0.55, table_top - row_height * 0.55), "npc"), gp = gpar(lwd = 1.5))

  for (j in seq_len(n_cols)) {
    grid.text(names(data)[j], x = x_centers[j], y = table_top, gp = gpar(fontsize = font_size, fontface = "bold"))
  }

  for (i in seq_len(n_rows)) {
    y <- table_top - i * row_height
    for (j in seq_len(n_cols)) {
      left_aligned <- j %in% left_align_cols
      just <- if (left_aligned) "left" else "centre"
      x <- if (left_aligned) x_edges[j] + 0.006 else x_centers[j]
      grid.text(as.character(data[i, j]), x = x, y = y, just = just, gp = gpar(fontsize = font_size))
    }
  }

  bottom_y <- max(bottom, table_top - (n_rows + 0.70) * row_height)
  grid.lines(x = unit(c(left, right), "npc"), y = unit(c(bottom_y, bottom_y), "npc"), gp = gpar(lwd = 2))
  dev.off()
}

sanitize_table_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_along(df)) {
    df[[j]] <- ifelse(is.na(df[[j]]), "", as.character(df[[j]]))
  }
  df
}

read_csv_utf8 <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
}

fmt_p_compact <- function(x) {
  if (is.na(x)) return("")
  if (x < 0.001) return("<0.001")
  sprintf("%.4f", x)
}

fmt_q_compact <- function(x) {
  if (is.na(x)) return("")
  if (x < 0.001) return("<0.001")
  sprintf("%.4f", x)
}

fmt_num <- function(x, digits = 3) {
  out <- rep("", length(x))
  ok <- !is.na(x)
  out[ok] <- sprintf(paste0("%.", digits, "f"), as.numeric(x[ok]))
  out
}

fmt_ci <- function(low, high, digits = 3) {
  if (is.na(low) || is.na(high)) return("")
  paste0(fmt_num(low, digits), " to ", fmt_num(high, digits))
}

map_biomarker_label <- function(x) {
  out <- x
  out[x == "MSD_STREM2CORRECTED"] <- "sTREM2"
  out[x == "TAU"] <- "Tau"
  out[x == "PTAU"] <- "P-Tau"
  out[x == "S_ABETA"] <- "Aβ"
  out
}

map_scope_label <- function(x) {
  out <- x
  out[x == "primary_adjusted"] <- "Primary"
  out[x == "diagnosis_adjusted"] <- "+ Diagnosis"
  out[x == "within_group"] <- "Within group"
  out[x == "diagnosis_phase_adjusted"] <- "+ Diagnosis + phase"
  out[x == "diagnosis_protocol_adjusted"] <- "+ Diagnosis + protocol"
  out[x == "diagnosis_site_adjusted"] <- "+ Diagnosis + site"
  out[x == "diagnosis_phase_protocol_site_adjusted"] <- "+ Diagnosis + all"
  out
}

summarize_mean_sd <- function(x, digits = 2) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return("")
  paste0(sprintf(paste0("%.", digits, "f"), mean(x)), " ± ", sprintf(paste0("%.", digits, "f"), stats::sd(x)))
}

get_group_text <- function(df, variable, group) {
  row <- df[df$variable == variable & df$group == group, , drop = FALSE]
  if (nrow(row) == 0) return("")
  row$summary_text[[1]]
}

get_cat_text <- function(df, variable, group, level) {
  row <- df[df$variable == variable & df$group == group & df$level == level, , drop = FALSE]
  if (nrow(row) == 0) return("")
  row$summary_text[[1]]
}

get_p_text <- function(df, variable) {
  row <- df[df$variable == variable, , drop = FALSE]
  if (nrow(row) == 0) return("")
  fmt_p_compact(row$p_value[[1]])
}

analysis_data_path_use <- analysis_data_path
if (!file.exists(analysis_data_path_use)) {
  analysis_data_path_use <- file.path(project_root, "data", "clean", "ChpSTREM2AD_analysis_dataset.csv")
}
analysis_df <- read_project_data(analysis_data_path_use)
analysis_df$S_DX_label <- factor(analysis_df$S_DX_label, levels = c("CN", "MCI", "AD"), labels = c("CN", "MCI", "Dementia"))

find_latest_complete_result_dir <- function(project_root) {
  result_root <- file.path(project_root, "result")
  dirs <- list.dirs(result_root, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[grepl("^\\d{8}_\\d{6}$", basename(dirs))]
  dirs <- sort(dirs, decreasing = TRUE)
  required <- c("descriptive_overall_continuous.csv", "descriptive_by_group_continuous.csv", "group_comparisons_overall.csv")
  for (d in dirs) {
    sdir <- file.path(d, "summary")
    if (all(file.exists(file.path(sdir, required)))) return(d)
  }
  stop("No complete result directory found.")
}

result_dir_use <- dirname(result_summary_dir)
required_summary <- c("descriptive_overall_continuous.csv", "descriptive_by_group_continuous.csv", "group_comparisons_overall.csv")
if (!all(file.exists(file.path(result_summary_dir, required_summary)))) {
  result_dir_use <- find_latest_complete_result_dir(project_root)
}

summary_dir <- file.path(result_dir_use, "summary")
table_dir <- file.path(result_dir_use, "tables")
result_report_dir_use <- file.path(result_dir_use, "report")
descriptive_overall_continuous <- read_csv_utf8(file.path(summary_dir, "descriptive_overall_continuous.csv"))
descriptive_by_group_continuous <- read_csv_utf8(file.path(summary_dir, "descriptive_by_group_continuous.csv"))
descriptive_overall_categorical <- read_csv_utf8(file.path(summary_dir, "descriptive_overall_categorical.csv"))
descriptive_by_group_categorical <- read_csv_utf8(file.path(summary_dir, "descriptive_by_group_categorical.csv"))
group_comparisons_overall <- read_csv_utf8(file.path(summary_dir, "group_comparisons_overall.csv"))
matching_pair_summary <- read_csv_utf8(file.path(summary_dir, "matching_pair_summary.csv"))
matching_balance_after_matching <- read_csv_utf8(file.path(summary_dir, "matching_balance_after_matching.csv"))
sample_platform_summary <- read_csv_utf8(file.path(summary_dir, "sample_platform_summary.csv"))

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

get_numeric_by_group <- function(var, group) {
  if (group == "Overall") {
    x <- analysis_df[[var]]
  } else {
    x <- analysis_df[[var]][analysis_df$S_DX_label == group]
  }
  digits <- if (var == "ChPICV") 5 else 2
  summarize_mean_sd(x, digits = digits)
}

table1a <- data.frame(
  Characteristic = c("N", "Age, years", "Female, n (%)"),
  Overall = c("735", get_group_text(descriptive_overall_continuous, "S_AGE", "overall"), get_cat_text(descriptive_overall_categorical, "S_PTGENDER_label", "overall", "Female")),
  CN = c("225", get_group_text(descriptive_by_group_continuous, "S_AGE", "CN"), get_cat_text(descriptive_by_group_categorical, "S_PTGENDER_label", "CN", "Female")),
  MCI = c("380", get_group_text(descriptive_by_group_continuous, "S_AGE", "MCI"), get_cat_text(descriptive_by_group_categorical, "S_PTGENDER_label", "MCI", "Female")),
  Dementia = c("130", get_group_text(descriptive_by_group_continuous, "S_AGE", "AD"), get_cat_text(descriptive_by_group_categorical, "S_PTGENDER_label", "AD", "Female")),
  `P value` = c("", get_p_text(group_comparisons_overall, "S_AGE"), get_p_text(group_comparisons_overall, "S_PTGENDER")),
  check.names = FALSE
)

table1b <- data.frame(
  Biomarker = c("sTREM2", "ChP/ICV", "Aβ", "Tau", "P-Tau"),
  Overall = c(get_numeric_by_group("MSD_STREM2CORRECTED", "Overall"), get_numeric_by_group("ChPICV", "Overall"), get_numeric_by_group("S_ABETA", "Overall"), get_numeric_by_group("TAU", "Overall"), get_numeric_by_group("PTAU", "Overall")),
  CN = c(get_numeric_by_group("MSD_STREM2CORRECTED", "CN"), get_numeric_by_group("ChPICV", "CN"), get_numeric_by_group("S_ABETA", "CN"), get_numeric_by_group("TAU", "CN"), get_numeric_by_group("PTAU", "CN")),
  MCI = c(get_numeric_by_group("MSD_STREM2CORRECTED", "MCI"), get_numeric_by_group("ChPICV", "MCI"), get_numeric_by_group("S_ABETA", "MCI"), get_numeric_by_group("TAU", "MCI"), get_numeric_by_group("PTAU", "MCI")),
  Dementia = c(get_numeric_by_group("MSD_STREM2CORRECTED", "Dementia"), get_numeric_by_group("ChPICV", "Dementia"), get_numeric_by_group("S_ABETA", "Dementia"), get_numeric_by_group("TAU", "Dementia"), get_numeric_by_group("PTAU", "Dementia")),
  `P value` = c(get_p_text(group_comparisons_overall, "MSD_STREM2CORRECTED"), get_p_text(group_comparisons_overall, "ChPICV"), get_p_text(group_comparisons_overall, "S_ABETA"), get_p_text(group_comparisons_overall, "TAU"), get_p_text(group_comparisons_overall, "PTAU")),
  check.names = FALSE
)

table1c <- merge(matching_pair_summary, matching_balance_after_matching, by = "pair_name", all.x = TRUE)
table1c <- reshape(table1c[, c("pair_name", "matched_pairs", "matched_n", "variable", "p_value")], idvar = c("pair_name", "matched_pairs", "matched_n"), timevar = "variable", direction = "wide")
names(table1c) <- c("Pair", "Matched pairs", "Matched n", "Age P", "Sex P")
table1c$Pair <- gsub("_vs_", " vs ", table1c$Pair, fixed = TRUE)
table1c$Pair <- gsub("\\bAD\\b", "Dementia", table1c$Pair)
table1c$`Age P` <- vapply(table1c$`Age P`, fmt_p_compact, character(1))
table1c$`Sex P` <- vapply(table1c$`Sex P`, fmt_p_compact, character(1))

table2 <- data.frame(
  `Cognitive construct` = c("MMSE + MoCA", "mPACC + NegaADAS13", "MMSE + MoCA + mPACC", "mPACC"),
  CFI = c("0.987", "0.989", "0.978", "0.989"),
  TLI = c("0.942", "0.974", "0.957", "0.952"),
  RMSEA = c("0.056", "0.074", "0.089", "0.101"),
  SRMR = c("0.009", "0.010", "0.014", "0.013"),
  Note = c("Primary model", "Higher RMSEA", "Weaker fit", "Weaker fit"),
  check.names = FALSE
)

draw_three_line_table_png(sanitize_table_df(table1a), file.path(table_dir, "Table_1A_three_line.png"), col_widths = c(0.28, 0.13, 0.11, 0.11, 0.13, 0.12), font_size = 11, height_px = 450)
draw_three_line_table_png(sanitize_table_df(table1b), file.path(table_dir, "Table_1B_three_line.png"), col_widths = c(0.20, 0.17, 0.17, 0.17, 0.17, 0.12), font_size = 11, width_px = 2600, height_px = 640)
draw_three_line_table_png(sanitize_table_df(table1c), file.path(table_dir, "Table_1C_three_line.png"), col_widths = c(0.28, 0.17, 0.15, 0.20, 0.20), font_size = 11)
draw_three_line_table_png(sanitize_table_df(table2), file.path(table_dir, "Table_2_three_line.png"), col_widths = c(0.34, 0.12, 0.12, 0.14, 0.12, 0.16), font_size = 11, width_px = 2200, height_px = 520)

diagnosis_hierarchical <- read_csv_utf8(file.path(summary_dir, "diagnosis_hierarchical_models.csv"))
diagnosis_hierarchical$Biomarker <- map_biomarker_label(diagnosis_hierarchical$exposure)
diagnosis_hierarchical$Model <- ifelse(
  diagnosis_hierarchical$model_scope == "within_group",
  ifelse(diagnosis_hierarchical$group == "AD", "Dementia", diagnosis_hierarchical$group),
  map_scope_label(diagnosis_hierarchical$model_scope)
)
diagnosis_hierarchical$`Std β` <- fmt_num(diagnosis_hierarchical$std_beta)
diagnosis_hierarchical$`95% CI` <- mapply(fmt_ci, diagnosis_hierarchical$conf.low, diagnosis_hierarchical$conf.high)
diagnosis_hierarchical$P <- vapply(diagnosis_hierarchical$p.value, fmt_p_compact, character(1))
diagnosis_hierarchical$q <- vapply(diagnosis_hierarchical$q_value, fmt_q_compact, character(1))
diagnosis_hierarchical$N <- as.character(diagnosis_hierarchical$n)
diagnosis_hierarchical <- diagnosis_hierarchical[, c("Biomarker", "Model", "Std β", "95% CI", "P", "q", "N")]
  draw_three_line_table_png(
  sanitize_table_df(diagnosis_hierarchical),
  file.path(table_dir, "diagnosis_hierarchical_models_three_line.png"),
  col_widths = c(0.17, 0.29, 0.11, 0.20, 0.08, 0.08, 0.07),
  font_size = 10,
  width_px = 2400,
  height_px = max(1320, 360 + (nrow(diagnosis_hierarchical) + 1) * 92)
)

biologic_sensitivity <- read_csv_utf8(file.path(summary_dir, "biologic_sensitivity_models.csv"))
biologic_sensitivity$Subset <- biologic_sensitivity$subset_label
biologic_sensitivity$Subset[biologic_sensitivity$subset_label == "Stable amyloid-negative (all diagnoses)"] <- "Aβ-negative (all)"
biologic_sensitivity$Subset[biologic_sensitivity$subset_label == "Stable amyloid-positive (all diagnoses)"] <- "Aβ-positive (all)"
biologic_sensitivity$Subset[biologic_sensitivity$subset_label == "Stable amyloid-negative CN"] <- "Aβ-negative CN"
biologic_sensitivity$Subset[biologic_sensitivity$subset_label == "Stable amyloid-positive symptomatic"] <- "Aβ-positive symptomatic"
biologic_sensitivity$Biomarker <- map_biomarker_label(biologic_sensitivity$exposure)
biologic_sensitivity$`Std β` <- fmt_num(biologic_sensitivity$std_beta)
biologic_sensitivity$`95% CI` <- mapply(fmt_ci, biologic_sensitivity$conf.low, biologic_sensitivity$conf.high)
biologic_sensitivity$P <- vapply(biologic_sensitivity$p.value, fmt_p_compact, character(1))
biologic_sensitivity$q <- vapply(biologic_sensitivity$q_value, fmt_q_compact, character(1))
biologic_sensitivity$N <- as.character(biologic_sensitivity$n_subset)
biologic_sensitivity <- biologic_sensitivity[, c("Subset", "Biomarker", "Std β", "95% CI", "P", "q", "N")]
  draw_three_line_table_png(
  sanitize_table_df(biologic_sensitivity),
  file.path(table_dir, "biologic_sensitivity_models_three_line.png"),
  col_widths = c(0.30, 0.16, 0.10, 0.22, 0.08, 0.08, 0.06),
  font_size = 10,
  width_px = 2350,
  height_px = max(1120, 320 + (nrow(biologic_sensitivity) + 1) * 82)
)

phase_site_protocol <- read_csv_utf8(file.path(summary_dir, "phase_site_protocol_sensitivity.csv"))
phase_site_protocol$Biomarker <- map_biomarker_label(phase_site_protocol$exposure)
phase_site_protocol$Adjustment <- map_scope_label(phase_site_protocol$sensitivity_scope)
phase_site_protocol$`Std β` <- fmt_num(phase_site_protocol$std_beta)
phase_site_protocol$`95% CI` <- mapply(fmt_ci, phase_site_protocol$conf.low, phase_site_protocol$conf.high)
phase_site_protocol$P <- vapply(phase_site_protocol$p.value, fmt_p_compact, character(1))
phase_site_protocol$q <- vapply(phase_site_protocol$q_value, fmt_q_compact, character(1))
phase_site_protocol$N <- as.character(phase_site_protocol$n)
phase_site_protocol <- phase_site_protocol[, c("Biomarker", "Adjustment", "Std β", "95% CI", "P", "q", "N")]
  draw_three_line_table_png(
  sanitize_table_df(phase_site_protocol),
  file.path(table_dir, "phase_site_protocol_sensitivity_three_line.png"),
  col_widths = c(0.14, 0.34, 0.10, 0.20, 0.07, 0.07, 0.08),
  font_size = 10,
  width_px = 2450,
  height_px = max(1520, 380 + (nrow(phase_site_protocol) + 1) * 94)
)

interaction_comparison <- read_csv_utf8(file.path(summary_dir, "advanced_interaction_comparison.csv"))
interaction_comparison$Outcome <- ifelse(interaction_comparison$outcome == "ChPICV", "ChP/ICV", interaction_comparison$outcome)
interaction_comparison$Exposure <- map_biomarker_label(interaction_comparison$exposure)
interaction_comparison$Moderator <- ifelse(interaction_comparison$moderator == "S_DX_label", "Diagnosis", interaction_comparison$moderator)
interaction_comparison$`Adj R² (reduced)` <- fmt_num(interaction_comparison$reduced_adj_r_squared)
interaction_comparison$`Adj R² (full)` <- fmt_num(interaction_comparison$full_adj_r_squared)
interaction_comparison$F <- fmt_num(interaction_comparison$interaction_f)
interaction_comparison$P <- vapply(interaction_comparison$interaction_p_value, fmt_p_compact, character(1))
interaction_comparison$N <- as.character(interaction_comparison$n)
interaction_comparison <- interaction_comparison[, c("Outcome", "Exposure", "Moderator", "Adj R² (reduced)", "Adj R² (full)", "F", "P", "N")]
draw_three_line_table_png(
  sanitize_table_df(interaction_comparison),
  file.path(table_dir, "advanced_interaction_comparison_three_line.png"),
  col_widths = c(0.11, 0.13, 0.13, 0.16, 0.16, 0.10, 0.11, 0.10),
  font_size = 10,
  width_px = 2300,
  height_px = 460
)

nonlinear_tests <- read_csv_utf8(file.path(summary_dir, "advanced_nonlinear_tests.csv"))
nonlinear_tests$Test <- nonlinear_tests$test_name
nonlinear_tests$Test[nonlinear_tests$test_name == "quadratic_term"] <- "Quadratic term"
nonlinear_tests$Test[nonlinear_tests$test_name == "linear_vs_quadratic"] <- "Linear vs quadratic"
nonlinear_tests$Test[nonlinear_tests$test_name == "linear_vs_spline"] <- "Linear vs spline"
nonlinear_tests$Statistic <- fmt_num(nonlinear_tests$statistic)
nonlinear_tests$P <- vapply(nonlinear_tests$p_value, fmt_p_compact, character(1))
nonlinear_tests <- nonlinear_tests[, c("Test", "Statistic", "P")]
draw_three_line_table_png(
  sanitize_table_df(nonlinear_tests),
  file.path(table_dir, "advanced_nonlinear_tests_three_line.png"),
  col_widths = c(0.48, 0.26, 0.26),
  font_size = 10,
  width_px = 1500,
  height_px = 440
)

collinearity_diag <- read_csv_utf8(file.path(summary_dir, "collinearity_diagnostics.csv"))
collinearity_diag$Model <- ifelse(grepl("PTAU", collinearity_diag$model_name), "P-Tau model", "Tau model")
collinearity_diag$Predictor <- collinearity_diag$term
collinearity_diag$Predictor[collinearity_diag$term == "MSD_STREM2CORRECTED"] <- "sTREM2"
collinearity_diag$Predictor[collinearity_diag$term == "S_ABETA"] <- "Aβ"
collinearity_diag$Predictor[collinearity_diag$term == "PTAU"] <- "P-Tau"
collinearity_diag$Predictor[collinearity_diag$term == "TAU"] <- "Tau"
collinearity_diag$Predictor[collinearity_diag$term == "S_PTGENDER2"] <- "Sex"
collinearity_diag$Predictor[collinearity_diag$term == "S_AGE"] <- "Age"
collinearity_diag$Predictor[collinearity_diag$term == "PTEDUCAT"] <- "Education"
collinearity_diag$Predictor[collinearity_diag$term == "APOE4011"] <- "APOE ε4"
collinearity_diag$VIF <- fmt_num(collinearity_diag$vif, 2)
collinearity_diag$Tolerance <- fmt_num(collinearity_diag$tolerance, 3)
collinearity_diag$N <- as.character(collinearity_diag$n)
collinearity_diag <- collinearity_diag[, c("Model", "Predictor", "VIF", "Tolerance", "N")]
  draw_three_line_table_png(
  sanitize_table_df(collinearity_diag),
  file.path(table_dir, "collinearity_diagnostics_three_line.png"),
  col_widths = c(0.28, 0.28, 0.16, 0.18, 0.10),
  font_size = 10,
  width_px = 1850,
  height_px = 1420
)

bootstrap_key <- read_csv_utf8(file.path(summary_dir, "sem_bootstrap_key_indirects.csv"))
bootstrap_key$Path <- bootstrap_key$model_name
bootstrap_key$Path[bootstrap_key$model_name == "ChP_to_sTREM2_to_Cognition"] <- "ChP → sTREM2 → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "sTREM2_to_ChP_to_Cognition"] <- "sTREM2 → ChP → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "ChP_to_TAU_to_Cognition"] <- "ChP → Tau → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "TAU_to_ChP_to_Cognition"] <- "Tau → ChP → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "ChP_to_PTAU_to_Cognition"] <- "ChP → P-Tau → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "PTAU_to_ChP_to_Cognition"] <- "P-Tau → ChP → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "ChP_to_ABETA_to_Cognition"] <- "ChP → Aβ → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "ABETA_to_ChP_to_Cognition"] <- "Aβ → ChP → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "PTAU_to_sTREM2_to_ChP_to_Cognition"] <- "P-Tau → sTREM2 → ChP → Cog"
bootstrap_key$Path[bootstrap_key$model_name == "TAU_to_sTREM2_to_ChP_to_Cognition"] <- "Tau → sTREM2 → ChP → Cog"
bootstrap_key$Effect <- bootstrap_key$effect
bootstrap_key$Effect[bootstrap_key$effect == "indirect"] <- "Indirect"
bootstrap_key$Effect[bootstrap_key$effect == "direct"] <- "Direct"
bootstrap_key$Effect[bootstrap_key$effect == "total"] <- "Total"
bootstrap_key$Effect[bootstrap_key$effect == "serial_indirect"] <- "Serial indirect"
bootstrap_key$Effect[bootstrap_key$effect == "total_indirect"] <- "Total indirect"
bootstrap_key$Estimate <- fmt_num(bootstrap_key$estimate)
bootstrap_key$`95% bootstrap CI` <- mapply(fmt_ci, bootstrap_key$conf_low, bootstrap_key$conf_high)
bootstrap_key$P <- vapply(bootstrap_key$p_value, fmt_p_compact, character(1))
bootstrap_key$N <- as.character(bootstrap_key$n)
bootstrap_key <- bootstrap_key[, c("Path", "Effect", "Estimate", "95% bootstrap CI", "P", "N")]
draw_three_line_table_png(
  sanitize_table_df(bootstrap_key),
  file.path(table_dir, "sem_bootstrap_key_indirects_three_line.png"),
  col_widths = c(0.36, 0.14, 0.10, 0.24, 0.08, 0.08),
  font_size = 9,
  width_px = 2500,
  height_px = max(3800, 760 + (nrow(bootstrap_key) + 1) * 170)
)

message("Three-line table images generated in: ", table_dir)

bulk_dir <- file.path(table_dir, "three_line_all")
dir.create(bulk_dir, recursive = TRUE, showWarnings = FALSE)
csv_files <- list.files(summary_dir, pattern = "\\.csv$", full.names = TRUE)

sample_flow <- read_csv_utf8(file.path(summary_dir, "sample_flow_summary.csv"))
sample_missingness <- read_csv_utf8(file.path(summary_dir, "sample_missingness_summary.csv"))
visit_alignment <- read_csv_utf8(file.path(summary_dir, "visit_alignment_summary.csv"))
analysis_n_summary <- read_csv_utf8(file.path(summary_dir, "analysis_n_summary.csv"))
cognition_fit <- read_csv_utf8(file.path(summary_dir, "cognition_model_selection_fit.csv"))
measurement_invariance <- read_csv_utf8(file.path(summary_dir, "sem_measurement_invariance_comparison.csv"))
sem_factor_loadings <- read_csv_utf8(file.path(summary_dir, "sem_factor_loadings.csv"))
sem_path_coefficients <- read_csv_utf8(file.path(summary_dir, "sem_path_coefficients.csv"))
sem_model_fit <- read_csv_utf8(file.path(summary_dir, "sem_model_fit.csv"))
bootstrap_key <- read_csv_utf8(file.path(summary_dir, "sem_bootstrap_key_indirects.csv"))

figure_dir <- file.path(result_dir_use, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

get_missing_summary <- function(var) {
  row <- sample_missingness[sample_missingness$variable == var, , drop = FALSE]
  if (nrow(row) == 0) return("")
  paste0(row$missing_n[[1]], " (", sprintf("%.1f", row$missing_pct[[1]]), "%)")
}

sample_transparency <- data.frame(
  Section = c(
    "Sample source", "Sample source", "Primary analyses", "SEM availability",
    "SEM availability", "Visit alignment", "Visit alignment", "Visit alignment",
    "Missingness", "Missingness", "Missingness", "Platform"
  ),
  Item = c(
    "Preassembled analytic records",
    "Unique participants",
    "Core regression complete cases",
    "Listwise SEM complete cases",
    "Primary SEM retained with FIML",
    "Same-day multimodal records, n (%)",
    "Median absolute interval, days (IQR)",
    "90th percentile / max interval, days",
    "MoCA missing, n (%)",
    "ADAS13 missing, n (%)",
    "NegaADAS13 missing, n (%)",
    "ADNI phase distribution"
  ),
  Value = c(
    sample_flow$source_dataset_rows[[1]],
    sample_flow$source_unique_participants[[1]],
    sample_flow$selected_rows[[1]],
    analysis_n_summary$n[[1]],
    sample_flow$analysis_rows[[1]],
    paste0(visit_alignment$same_day_n[[1]], " (", sprintf("%.1f", 100 * visit_alignment$same_day_pct[[1]]), "%)"),
    paste0(visit_alignment$median_abs_days[[1]], " (", visit_alignment$iqr_abs_days[[1]], ")"),
    paste0(visit_alignment$p90_abs_days[[1]], " / ", visit_alignment$max_abs_days[[1]]),
    get_missing_summary("MOCA"),
    get_missing_summary("ADAS13"),
    get_missing_summary("NegaADAS13"),
    sample_platform_summary$adni_phase_distribution[[1]]
  ),
  check.names = FALSE
)
utils::write.csv(sample_transparency, file.path(summary_dir, "sample_transparency_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
draw_three_line_table_png(
  sanitize_table_df(sample_transparency),
  file.path(table_dir, "sample_transparency_summary_three_line.png"),
  col_widths = c(0.20, 0.45, 0.35),
  font_size = 10,
  width_px = 2300,
  height_px = 980
)

flow_boxes <- c(
  paste0("Preassembled analytic dataset\n", sample_flow$source_dataset_rows[[1]], " records; ", sample_flow$source_unique_participants[[1]], " participants"),
  paste0("Primary regression sample\n", sample_flow$selected_rows[[1]], " complete cases for core variables"),
  paste0("SEM if listwise deletion used\n", analysis_n_summary$n[[1]], " participants"),
  paste0("Primary SEM with FIML\n", sample_flow$analysis_rows[[1]], " participants retained")
)
flow_notes <- c(
  paste0("MoCA missing: ", get_missing_summary("MOCA")),
  paste0("ADAS13 missing: ", get_missing_summary("ADAS13")),
  paste0("Median multimodal interval: ", visit_alignment$median_abs_days[[1]], " d (IQR ", visit_alignment$iqr_abs_days[[1]], ")")
)
draw_sample_flow_png(file.path(figure_dir, "supplementary_sample_flow.png"), flow_boxes, flow_notes)

cognition_models_table <- data.frame(
  Construct = cognition_fit$cognition_label_en,
  Indicators = cognition_fit$components,
  CFI = fmt_num(cognition_fit$cfi),
  TLI = fmt_num(cognition_fit$tli),
  RMSEA = fmt_num(cognition_fit$rmsea),
  SRMR = fmt_num(cognition_fit$srmr),
  N = as.character(cognition_fit$n),
  Selected = ifelse(as.logical(cognition_fit$selected_for_main_sem), "Yes", ""),
  check.names = FALSE
)
utils::write.csv(cognition_models_table, file.path(summary_dir, "cognition_model_fit_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
draw_three_line_table_png(
  sanitize_table_df(cognition_models_table),
  file.path(table_dir, "cognition_model_fit_summary_three_line.png"),
  col_widths = c(0.25, 0.27, 0.08, 0.08, 0.09, 0.08, 0.06, 0.09),
  font_size = 9.5,
  width_px = 2600,
  height_px = 620
)

reference_loadings <- sem_factor_loadings[
  sem_factor_loadings$sem_model == "ChP_to_sTREM2_to_Cognition" & sem_factor_loadings$group == "Overall",
  ,
  drop = FALSE
]
reference_loadings$Item <- reference_loadings$indicator_component
reference_loadings$Estimate <- fmt_num(reference_loadings$estimate)
reference_loadings$`Std loading` <- fmt_num(reference_loadings$std_estimate)
reference_loadings$`95% CI` <- mapply(fmt_ci, reference_loadings$conf_low, reference_loadings$conf_high)
reference_loadings$P <- vapply(reference_loadings$p_value, fmt_p_compact, character(1))

measurement_diag <- data.frame(
  Section = c(rep("Invariance", nrow(measurement_invariance)), rep("Reference loadings", nrow(reference_loadings))),
  Item = c(tools::toTitleCase(gsub("_", " ", measurement_invariance$invariance_model)), reference_loadings$Item),
  Estimate = c(rep("", nrow(measurement_invariance)), reference_loadings$Estimate),
  `Std loading` = c(rep("", nrow(measurement_invariance)), reference_loadings$`Std loading`),
  `95% CI` = c(rep("", nrow(measurement_invariance)), reference_loadings$`95% CI`),
  P = c(rep("", nrow(measurement_invariance)), reference_loadings$P),
  CFI = c(fmt_num(measurement_invariance$cfi), rep("", nrow(reference_loadings))),
  TLI = c(fmt_num(measurement_invariance$tli), rep("", nrow(reference_loadings))),
  RMSEA = c(fmt_num(measurement_invariance$rmsea), rep("", nrow(reference_loadings))),
  SRMR = c(fmt_num(measurement_invariance$srmr), rep("", nrow(reference_loadings))),
  Note = c(
    ifelse(
      is.na(measurement_invariance$decision),
      "",
      measurement_invariance$decision
    ),
    rep("Reference overall MMSE + MoCA model", nrow(reference_loadings))
  ),
  check.names = FALSE
)
utils::write.csv(measurement_diag, file.path(summary_dir, "cognition_measurement_diagnostics_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
draw_three_line_table_png(
  sanitize_table_df(measurement_diag),
  file.path(table_dir, "cognition_measurement_diagnostics_three_line.png"),
  col_widths = c(0.14, 0.16, 0.07, 0.09, 0.12, 0.06, 0.06, 0.06, 0.08, 0.07, 0.19),
  font_size = 9,
  width_px = 3000,
  height_px = 900
)

overall_paths <- sem_path_coefficients[
  sem_path_coefficients$group == "Overall" &
    sem_path_coefficients$path %in% c("a", "b", "c_prime"),
  ,
  drop = FALSE
]
overall_paths$model_label <- overall_paths$sem_model
overall_paths$model_label[overall_paths$sem_model == "ChP_to_sTREM2_to_Cognition"] <- "ChP → sTREM2 → Cog"
overall_paths$model_label[overall_paths$sem_model == "sTREM2_to_ChP_to_Cognition"] <- "sTREM2 → ChP → Cog"
overall_paths$model_label[overall_paths$sem_model == "ChP_to_TAU_to_Cognition"] <- "ChP → Tau → Cog"
overall_paths$model_label[overall_paths$sem_model == "TAU_to_ChP_to_Cognition"] <- "Tau → ChP → Cog"
overall_paths$model_label[overall_paths$sem_model == "ChP_to_PTAU_to_Cognition"] <- "ChP → P-Tau → Cog"
overall_paths$model_label[overall_paths$sem_model == "PTAU_to_ChP_to_Cognition"] <- "P-Tau → ChP → Cog"
overall_paths$model_label[overall_paths$sem_model == "ChP_to_ABETA_to_Cognition"] <- "ChP → Aβ → Cog"
overall_paths$model_label[overall_paths$sem_model == "ABETA_to_ChP_to_Cognition"] <- "Aβ → ChP → Cog"

bootstrap_primary <- bootstrap_key[bootstrap_key$analysis_type == "single_or_total", , drop = FALSE]
bootstrap_primary$q_value <- ave(bootstrap_primary$p_value, bootstrap_primary$effect, FUN = function(p) stats::p.adjust(p, method = "BH"))
bootstrap_primary$model_label <- bootstrap_primary$model_name
bootstrap_primary$model_label[bootstrap_primary$model_name == "ChP_to_sTREM2_to_Cognition"] <- "ChP → sTREM2 → Cog"
bootstrap_primary$model_label[bootstrap_primary$model_name == "sTREM2_to_ChP_to_Cognition"] <- "sTREM2 → ChP → Cog"
bootstrap_primary$model_label[bootstrap_primary$model_name == "ChP_to_TAU_to_Cognition"] <- "ChP → Tau → Cog"
bootstrap_primary$model_label[bootstrap_primary$model_name == "TAU_to_ChP_to_Cognition"] <- "Tau → ChP → Cog"
bootstrap_primary$model_label[bootstrap_primary$model_name == "ChP_to_PTAU_to_Cognition"] <- "ChP → P-Tau → Cog"
bootstrap_primary$model_label[bootstrap_primary$model_name == "PTAU_to_ChP_to_Cognition"] <- "P-Tau → ChP → Cog"
bootstrap_primary$model_label[bootstrap_primary$model_name == "ChP_to_ABETA_to_Cognition"] <- "ChP → Aβ → Cog"
bootstrap_primary$model_label[bootstrap_primary$model_name == "ABETA_to_ChP_to_Cognition"] <- "Aβ → ChP → Cog"

get_path_beta <- function(model_name, path_name) {
  row <- overall_paths[overall_paths$sem_model == model_name & overall_paths$path == path_name, , drop = FALSE]
  if (nrow(row) == 0) return("")
  fmt_num(row$std_estimate[[1]])
}
get_boot_field <- function(model_name, effect_name, field = "estimate") {
  row <- bootstrap_primary[bootstrap_primary$model_name == model_name & bootstrap_primary$effect == effect_name, , drop = FALSE]
  if (nrow(row) == 0) return("")
  if (field == "estimate") return(fmt_num(row$estimate[[1]]))
  if (field == "ci") return(fmt_ci(row$conf_low[[1]], row$conf_high[[1]]))
  if (field == "p") return(fmt_p_compact(row$p_value[[1]]))
  if (field == "q") return(fmt_q_compact(row$q_value[[1]]))
  ""
}
fit_overall <- sem_model_fit[sem_model_fit$group == "Overall", , drop = FALSE]
fit_overall$Fit <- paste0("CFI ", fmt_num(fit_overall$cfi), " / RMSEA ", fmt_num(fit_overall$rmsea), " / SRMR ", fmt_num(fit_overall$srmr))

primary_models <- c(
  "ChP_to_sTREM2_to_Cognition",
  "sTREM2_to_ChP_to_Cognition",
  "ChP_to_TAU_to_Cognition",
  "TAU_to_ChP_to_Cognition",
  "ChP_to_PTAU_to_Cognition",
  "PTAU_to_ChP_to_Cognition",
  "ChP_to_ABETA_to_Cognition",
  "ABETA_to_ChP_to_Cognition"
)
sem_path_summary <- do.call(
  rbind,
  lapply(primary_models, function(model_name) {
    fit_row <- fit_overall[fit_overall$sem_model == model_name, , drop = FALSE]
    label <- bootstrap_primary$model_label[bootstrap_primary$model_name == model_name][1]
    data.frame(
      Pathway = label,
      `a Std β` = get_path_beta(model_name, "a"),
      `b Std β` = get_path_beta(model_name, "b"),
      `c' Std β` = get_path_beta(model_name, "c_prime"),
      `Indirect (boot)` = get_boot_field(model_name, "indirect", "estimate"),
      `Indirect 95% CI` = get_boot_field(model_name, "indirect", "ci"),
      `Indirect P/q` = paste0(get_boot_field(model_name, "indirect", "p"), " / ", get_boot_field(model_name, "indirect", "q")),
      `Total (boot)` = get_boot_field(model_name, "total", "estimate"),
      `Total 95% CI` = get_boot_field(model_name, "total", "ci"),
      Fit = ifelse(nrow(fit_row) == 0, "", fit_row$Fit[[1]]),
      N = ifelse(nrow(fit_row) == 0, "", as.character(fit_row$n[[1]])),
      check.names = FALSE
    )
  })
)
utils::write.csv(sem_path_summary, file.path(summary_dir, "sem_primary_path_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
draw_three_line_table_png(
  sanitize_table_df(sem_path_summary),
  file.path(table_dir, "sem_primary_path_summary_three_line.png"),
  col_widths = c(0.18, 0.08, 0.08, 0.08, 0.08, 0.15, 0.10, 0.08, 0.15, 0.14, 0.06),
  font_size = 9,
  width_px = 3400,
  height_px = 980
)

time_window_models_path <- file.path(summary_dir, "time_window_sensitivity_models.csv")
if (file.exists(time_window_models_path)) {
  time_window_models <- read_csv_utf8(time_window_models_path)
  time_window_models$subset_label <- c("Full sample", "Full sample", "Full sample", "Full sample",
                                       "≤30 days", "≤30 days", "≤30 days", "≤30 days",
                                       "≤90 days", "≤90 days", "≤90 days", "≤90 days")
  time_window_models$Biomarker <- map_biomarker_label(time_window_models$exposure)
  time_window_models$`Std β` <- fmt_num(time_window_models$std_beta)
  time_window_models$`95% CI` <- mapply(fmt_ci, time_window_models$conf.low, time_window_models$conf.high)
  time_window_models$P <- vapply(time_window_models$p.value, fmt_p_compact, character(1))
  time_window_summary_table <- time_window_models[, c("subset_label", "Biomarker", "Std β", "95% CI", "P")]
  names(time_window_summary_table)[1] <- "Subset"
  utils::write.csv(time_window_summary_table, file.path(summary_dir, "time_window_sensitivity_summary_table.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  draw_three_line_table_png(
    sanitize_table_df(time_window_summary_table),
    file.path(table_dir, "time_window_sensitivity_summary_three_line.png"),
    col_widths = c(0.18, 0.16, 0.12, 0.34, 0.12),
    font_size = 10,
    width_px = 2400,
    height_px = 900
  )
}

lv_adjusted_models_path <- file.path(summary_dir, "lv_adjusted_models.csv")
if (file.exists(lv_adjusted_models_path)) {
  lv_adjusted_models <- read_csv_utf8(lv_adjusted_models_path)
  lv_adjusted_models$Biomarker <- map_biomarker_label(lv_adjusted_models$exposure)
  lv_adjusted_models$Adjustment <- ifelse(lv_adjusted_models$model_spec == "lv_adjusted", "+ Lateral ventricle", "Primary")
  lv_adjusted_models$`Std β` <- fmt_num(lv_adjusted_models$std_beta)
  lv_adjusted_models$`95% CI` <- mapply(fmt_ci, lv_adjusted_models$conf.low, lv_adjusted_models$conf.high)
  lv_adjusted_models$P <- vapply(lv_adjusted_models$p.value, fmt_p_compact, character(1))
  lv_adjusted_table <- lv_adjusted_models[, c("Biomarker", "Adjustment", "Std β", "95% CI", "P")]
  utils::write.csv(lv_adjusted_table, file.path(summary_dir, "lv_adjusted_summary_table.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  draw_three_line_table_png(
    sanitize_table_df(lv_adjusted_table),
    file.path(table_dir, "lv_adjusted_summary_three_line.png"),
    col_widths = c(0.18, 0.22, 0.12, 0.34, 0.12),
    font_size = 10,
    width_px = 2400,
    height_px = 720
  )
}

manifest_lines <- c("# Three-line table image index", "", "Below are the auto-generated three-line table images converted from summary CSV files.", "")
for (csv_path in csv_files) {
  df <- tryCatch(read_csv_utf8(csv_path), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) next
  out_name <- paste0(tools::file_path_sans_ext(basename(csv_path)), "_three_line.png")
  out_path <- file.path(bulk_dir, out_name)
  draw_three_line_table_png(sanitize_table_df(df), out_path, font_size = 9, left_align_cols = 1)
  manifest_lines <- c(manifest_lines, paste0("- `", basename(csv_path), "`"), paste0("  ![", tools::file_path_sans_ext(basename(csv_path)), "](../tables/three_line_all/", out_name, ")"), "")
}
writeLines(manifest_lines, con = file.path(result_report_dir_use, "Three_line_table_image_index.md"), useBytes = TRUE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "10c_generate_table_images",
  output_files = c(
    file.path(table_dir, "Table_1A_three_line.png"),
    file.path(table_dir, "Table_1B_three_line.png"),
    file.path(table_dir, "Table_1C_three_line.png"),
    file.path(table_dir, "Table_2_three_line.png"),
    file.path(table_dir, "sample_transparency_summary_three_line.png"),
    file.path(table_dir, "cognition_model_fit_summary_three_line.png"),
    file.path(table_dir, "cognition_measurement_diagnostics_three_line.png"),
    file.path(table_dir, "sem_primary_path_summary_three_line.png"),
    file.path(table_dir, "time_window_sensitivity_summary_three_line.png"),
    file.path(table_dir, "lv_adjusted_summary_three_line.png"),
    file.path(figure_dir, "supplementary_sample_flow.png"),
    file.path(result_report_dir_use, "Three_line_table_image_index.md")
  ),
  note = "Generated title-free three-line table PNGs for manuscript and bulk summary outputs.",
  summary_dir = summary_dir
)
