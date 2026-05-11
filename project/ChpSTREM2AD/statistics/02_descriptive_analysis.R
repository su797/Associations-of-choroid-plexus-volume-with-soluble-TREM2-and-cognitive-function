source(file.path(getwd(), "00_setup.R"))

analysis_data <- read_project_data(clean_data_path)

continuous_vars <- c("sTREM2", "chp_volume", "chp_thickness", "age", "education", "icv")
categorical_vars <- c("sex", "diagnosis_group", "amyloid_status")
group_var <- "diagnosis_group"

cont_summary <- summarize_continuous(analysis_data, continuous_vars, group_var = group_var)
cat_summary <- summarize_categorical(analysis_data, categorical_vars, group_var = group_var)
ttest_result <- compare_groups_ttest(analysis_data, continuous_vars, group_var = group_var)
chisq_result <- compare_groups_chisq(analysis_data, categorical_vars, group_var = group_var)

cont_path <- file.path(project_root, "result", "summary", "descriptive_continuous.csv")
cat_path <- file.path(project_root, "result", "summary", "descriptive_categorical.csv")
test_path <- file.path(project_root, "result", "summary", "group_comparison_tests.csv")

write_csv_utf8(cont_summary, cont_path)
write_csv_utf8(cat_summary, cat_path)
write_csv_utf8(rbind(ttest_result, chisq_result), test_path)

append_analysis_log(
  project_root = project_root,
  analysis_name = "02_descriptive_analysis",
  output_files = c(cont_path, cat_path, test_path),
  note = "Descriptive statistics and group comparisons exported."
)
