source(file.path(getwd(), "00_setup.R"))

analysis_data <- read_project_data(analysis_data_path)
selected_data <- read_project_data(selected_data_path)
normality_table <- read_result_or_empty(normality_table_path)
conversion_log <- read_result_or_empty(conversion_log_path)
run_metadata <- read_result_or_empty(result_run_metadata_path)
group_overall <- read_result_or_empty(file.path(result_summary_dir, "group_comparisons_overall.csv"))
group_pairwise <- read_result_or_empty(file.path(result_summary_dir, "group_comparisons_pairwise.csv"))
group_adjusted <- read_result_or_empty(file.path(result_summary_dir, "group_comparisons_adjusted.csv"))
matching_decision <- read_result_or_empty(file.path(result_summary_dir, "matching_decision.csv"))
matching_summary <- read_result_or_empty(file.path(result_summary_dir, "matching_pair_summary.csv"))
matching_balance <- read_result_or_empty(file.path(result_summary_dir, "matching_balance_after_matching.csv"))
matched_targets <- read_result_or_empty(file.path(result_summary_dir, "matched_group_comparisons_targets.csv"))
chp_linear_overall <- read_result_or_empty(file.path(result_summary_dir, "chp_strem2_linear_overall.csv"))
chp_linear_by_group <- read_result_or_empty(file.path(result_summary_dir, "chp_strem2_linear_by_group.csv"))
chp_partial_overall <- read_result_or_empty(file.path(result_summary_dir, "chp_strem2_partial_overall.csv"))
chp_partial_by_group <- read_result_or_empty(file.path(result_summary_dir, "chp_strem2_partial_by_group.csv"))
ptau_linear_overall <- read_result_or_empty(file.path(result_summary_dir, "ptau_linear_overall.csv"))
ptau_linear_by_group <- read_result_or_empty(file.path(result_summary_dir, "ptau_linear_by_group.csv"))
ptau_partial_overall <- read_result_or_empty(file.path(result_summary_dir, "ptau_partial_overall.csv"))
ptau_partial_by_group <- read_result_or_empty(file.path(result_summary_dir, "ptau_partial_by_group.csv"))
sem_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_mediation_summary.csv"))
sem_paths <- read_result_or_empty(file.path(result_summary_dir, "sem_path_coefficients.csv"))
analysis_log <- read_result_or_empty(file.path(result_summary_dir, "analysis_log.csv"))

group_counts <- as.data.frame(table(analysis_data[[project_config$variables$group_label_var]]), stringsAsFactors = FALSE)
names(group_counts) <- c("Group", "N")

transformed_vars <- normality_table[normality_table$transform_method != "none", c("variable", "analysis_var", "transform_method", "note"), drop = FALSE]
chp_primary <- pick_term_rows(chp_linear_overall, term_name = "exposure")
chp_group_primary <- pick_term_rows(chp_linear_by_group, term_name = "exposure")
ptau_primary <- pick_term_rows(ptau_linear_overall, term_name = "exposure")
ptau_group_primary <- pick_term_rows(ptau_linear_by_group, term_name = "exposure")

significant_group_vars <- if (nrow(group_overall) > 0) {
  group_overall$variable[group_overall$p_value < 0.05]
} else {
  character(0)
}

report_lines <- c(
  paste0("# ", project_config$report$title),
  "",
  paste0("- Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("- Project: ", project_config$project_name),
  paste0("- Result version: `", result_version, "`"),
  paste0("- Result folder: `", result_run_dir, "`"),
  paste0("- Author: ", project_config$report$author),
  paste0("- Raw data: `", raw_data_path, "`"),
  paste0("- Analysis dataset rows: ", nrow(analysis_data)),
  ""
)

report_lines <- c(
  report_lines,
  "## 1. Dataset Overview",
  "",
  "### Run Metadata",
  "",
  markdown_table(run_metadata, digits = project_config$report$digits),
  "### Group Counts",
  "",
  markdown_table(group_counts, digits = project_config$report$digits)
)

report_lines <- c(
  report_lines,
  "### Selected Variables",
  "",
  markdown_table(read_result_or_empty(variable_dictionary_path), digits = project_config$report$digits)
)

report_lines <- c(
  report_lines,
  "## 2. Cleaning and Transformation",
  "",
  "### Numeric Conversion Log",
  "",
  markdown_table(conversion_log, digits = project_config$report$digits),
  "### Normality Screening and Transformation Plan",
  "",
  markdown_table(normality_table, digits = project_config$report$digits)
)

if (nrow(transformed_vars) > 0) {
  report_lines <- c(
    report_lines,
    "### Variables Transformed for Analysis",
    "",
    markdown_table(transformed_vars, digits = project_config$report$digits)
  )
}

report_lines <- c(
  report_lines,
  "## 3. Group Comparisons",
  "",
  paste0("Significant overall group-difference variables (`p < 0.05`): ", collapse_values(significant_group_vars)),
  "",
  "### Overall Tests",
  "",
  markdown_table(group_overall, digits = project_config$report$digits),
  "### Pairwise Comparisons",
  "",
  markdown_table(group_pairwise, digits = project_config$report$digits),
  "### Adjusted Group Models",
  "",
  markdown_table(group_adjusted, digits = project_config$report$digits)
)

report_lines <- c(
  report_lines,
  "## 4. Balance Check and Matching",
  "",
  "### Matching Decision",
  "",
  markdown_table(matching_decision, digits = project_config$report$digits),
  "### Matching Pair Summary",
  "",
  markdown_table(matching_summary, digits = project_config$report$digits),
  "### Balance After Matching",
  "",
  markdown_table(matching_balance, digits = project_config$report$digits),
  "### Target Variables After Matching",
  "",
  markdown_table(matched_targets, digits = project_config$report$digits)
)

report_lines <- c(
  report_lines,
  "## 5. ChPICV and sTREM2 Association",
  "",
  "### Overall Adjusted Linear Models",
  "",
  markdown_table(chp_primary, digits = project_config$report$digits),
  "### Group-specific Adjusted Linear Models",
  "",
  markdown_table(chp_group_primary, digits = project_config$report$digits),
  "### Overall Partial Correlation",
  "",
  markdown_table(chp_partial_overall, digits = project_config$report$digits),
  "### Group-specific Partial Correlation",
  "",
  markdown_table(chp_partial_by_group, digits = project_config$report$digits),
  "### Figures",
  "",
  markdown_image("../figures/scatter_ChPICV_on_sTREM2_overall.png", "ChPICV and sTREM2 overall"),
  "",
  markdown_image("../figures/scatter_sTREM2_on_ChPICV_overall.png", "sTREM2 and ChPICV overall"),
  ""
)

report_lines <- c(
  report_lines,
  "## 6. PTAU Association",
  "",
  "### Overall Adjusted Linear Models",
  "",
  markdown_table(ptau_primary, digits = project_config$report$digits),
  "### Group-specific Adjusted Linear Models",
  "",
  markdown_table(ptau_group_primary, digits = project_config$report$digits),
  "### Overall Partial Correlation",
  "",
  markdown_table(ptau_partial_overall, digits = project_config$report$digits),
  "### Group-specific Partial Correlation",
  "",
  markdown_table(ptau_partial_by_group, digits = project_config$report$digits),
  "### Figures",
  "",
  markdown_image("../figures/scatter_PTAU_on_ChPICV_overall.png", "PTAU and ChPICV overall"),
  "",
  markdown_image("../figures/scatter_PTAU_on_sTREM2_overall.png", "PTAU and sTREM2 overall"),
  ""
)

report_lines <- c(
  report_lines,
  "## 7. SEM-style Mediation",
  "",
  "### Mediation Summary",
  "",
  markdown_table(sem_summary, digits = project_config$report$digits),
  "### Path Coefficients",
  "",
  markdown_table(sem_paths, digits = project_config$report$digits),
  "### SEM Figures",
  "",
  markdown_image("../figures/sem_ChP_to_sTREM2_to_Cognition_Cog_PACC_ADAS.png", "ChP to sTREM2 to Cognition - PACC/ADAS"),
  "",
  markdown_image("../figures/sem_ChP_to_sTREM2_to_Cognition_Cog_MMSE_MOCA.png", "ChP to sTREM2 to Cognition - MMSE/MOCA"),
  "",
  markdown_image("../figures/sem_ChP_to_sTREM2_to_Cognition_Cog_Global.png", "ChP to sTREM2 to Cognition - Global"),
  "",
  markdown_image("../figures/sem_sTREM2_to_ChP_to_Cognition_Cog_PACC_ADAS.png", "sTREM2 to ChP to Cognition - PACC/ADAS"),
  "",
  markdown_image("../figures/sem_sTREM2_to_ChP_to_Cognition_Cog_MMSE_MOCA.png", "sTREM2 to ChP to Cognition - MMSE/MOCA"),
  "",
  markdown_image("../figures/sem_sTREM2_to_ChP_to_Cognition_Cog_Global.png", "sTREM2 to ChP to Cognition - Global"),
  ""
)

if (isTRUE(project_config$report$include_appendix)) {
  data_files <- list.files(result_data_clean_dir, full.names = TRUE)
  summary_files <- list.files(result_summary_dir, full.names = TRUE)
  table_files <- list.files(result_tables_dir, full.names = TRUE)
  figure_files <- list.files(result_figures_dir, full.names = TRUE)

  report_lines <- c(
    report_lines,
    "## 8. Output Inventory",
    "",
    "### Versioned Data Files",
    "",
    markdown_bullet_links(data_files, relative_dir = "../data_clean"),
    "### Summary Files",
    "",
    markdown_bullet_links(summary_files, relative_dir = "../summary"),
    "### Table Files",
    "",
    markdown_bullet_links(table_files, relative_dir = "../tables"),
    "### Figure Files",
    "",
    markdown_bullet_links(figure_files, relative_dir = "../figures"),
    "### Analysis Log",
    "",
    markdown_table(analysis_log, digits = project_config$report$digits)
  )
}

ensure_dir(report_dir)
writeLines(report_lines, con = report_md_path, useBytes = TRUE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "08_generate_markdown_report",
  output_files = report_md_path,
  note = "Generated an integrated Markdown report inside the current versioned result folder.",
  summary_dir = result_summary_dir
)
