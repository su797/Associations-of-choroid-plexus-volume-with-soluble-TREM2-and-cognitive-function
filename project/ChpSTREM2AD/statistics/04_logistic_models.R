source(file.path(getwd(), "00_setup.R"))

analysis_data <- read_project_data(clean_data_path)

binary_outcomes <- c("amyloid_status")
exposure <- "sTREM2"
covariates <- c("age", "sex", "education")

logistic_results <- fit_logistic_models_batch(
  data = analysis_data,
  outcomes = binary_outcomes,
  exposure = exposure,
  covariates = covariates
)

logistic_results <- logistic_results[logistic_results$term != "(Intercept)", , drop = FALSE]
logistic_table <- prepare_regression_table(logistic_results)

result_csv <- file.path(project_root, "result", "summary", "logistic_models_strem2.csv")
table_csv <- file.path(project_root, "result", "tables", "table_logistic_models.csv")
table_html <- file.path(project_root, "result", "tables", "table_logistic_models.html")

write_csv_utf8(logistic_table, result_csv)
export_three_line_table(
  data = logistic_table,
  csv_path = table_csv,
  html_path = table_html,
  title = "Logistic Regression Results"
)

append_analysis_log(
  project_root = project_root,
  analysis_name = "04_logistic_models",
  output_files = c(result_csv, table_csv, table_html),
  note = "Logistic regression outputs exported."
)
