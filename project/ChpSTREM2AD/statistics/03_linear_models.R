source(file.path(getwd(), "00_setup.R"))

analysis_data <- read_project_data(clean_data_path)

outcomes <- c("chp_volume", "chp_thickness")
exposure <- "sTREM2"
covariates <- c("age", "sex", "education", "icv")

linear_results <- fit_linear_models_batch(
  data = analysis_data,
  outcomes = outcomes,
  exposure = exposure,
  covariates = covariates
)

linear_results <- extract_primary_terms(linear_results)
linear_table <- prepare_regression_table(linear_results)

result_csv <- file.path(project_root, "result", "summary", "linear_models_strem2_chp.csv")
table_csv <- file.path(project_root, "result", "tables", "table_linear_models.csv")
table_html <- file.path(project_root, "result", "tables", "table_linear_models.html")
figure_path <- file.path(project_root, "result", "figures", "scatter_strem2_chp_volume.png")

write_csv_utf8(linear_table, result_csv)
export_three_line_table(
  data = linear_table,
  csv_path = table_csv,
  html_path = table_html,
  title = "Linear Regression Results"
)

scatter_plot <- plot_scatter_with_lm(
  data = analysis_data,
  x = "sTREM2",
  y = "chp_volume",
  color_var = "diagnosis_group",
  title = "sTREM2 and Choroid Plexus Volume"
)

save_plot_file(scatter_plot, figure_path)

append_analysis_log(
  project_root = project_root,
  analysis_name = "03_linear_models",
  output_files = c(result_csv, table_csv, table_html, figure_path),
  note = "Batch linear regression for sTREM2 and choroid plexus MRI markers."
)
