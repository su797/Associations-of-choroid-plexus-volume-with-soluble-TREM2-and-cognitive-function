statistics_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(statistics_root, ".."), winslash = "/", mustWork = TRUE)
common_root <- normalizePath(file.path(project_root, "..", "..", "common", "R"), winslash = "/", mustWork = TRUE)
common_config_root <- normalizePath(file.path(project_root, "..", "..", "common", "config"), winslash = "/", mustWork = TRUE)

source(file.path(common_root, "utils_io.R"))
load_common_functions(common_root)
source(file.path(statistics_root, "project_config.R"))

common_defaults_path <- file.path(common_config_root, "defaults.json")
locale_config_path <- file.path(common_config_root, "locale.json")
project_local_config_path <- file.path(statistics_root, "local.json")

runtime_settings <- load_runtime_settings(common_defaults_path, project_local_config_path)
if (!is.null(runtime_settings$project_config_override)) {
  project_config <- merge_lists_recursive(project_config, runtime_settings$project_config_override)
}
if (!is.null(runtime_settings$report)) {
  project_config$report <- merge_lists_recursive(project_config$report, runtime_settings$report)
}

locale_bundle <- read_json_config(locale_config_path, default = list())
report_language <- resolve_language(runtime_settings, default = "zh")
report_digits <- runtime_settings$report$digits %||% project_config$report$digits
project_report_title <- resolve_multilingual_value(runtime_settings$report_titles, report_language, fallback = project_config$report$title)

set.seed(project_config$seed)
current_result_version <- Sys.getenv("RESEARCHR_RESULT_VERSION", unset = "")
result_config <- project_config$results
if (nzchar(current_result_version)) {
  result_config$run_id <- current_result_version
}

result_context <- create_result_dirs(project_root, result_config = result_config)
Sys.setenv(RESEARCHR_RESULT_VERSION = result_context$run_id)
result_version <- result_context$run_id
result_run_dir <- result_context$run_dir
result_summary_dir <- result_context$summary_dir
result_tables_dir <- result_context$tables_dir
result_figures_dir <- result_context$figures_dir
result_report_dir <- result_context$report_dir
result_data_clean_dir <- result_context$data_clean_dir
latest_clean_dir <- result_context$latest_clean_dir

selected_data_path <- file.path(result_data_clean_dir, "ChpSTREM2AD_selected_dataset.csv")
analysis_data_path <- file.path(result_data_clean_dir, "ChpSTREM2AD_analysis_dataset.csv")
selected_data_latest_path <- file.path(latest_clean_dir, "ChpSTREM2AD_selected_dataset.csv")
analysis_data_latest_path <- file.path(latest_clean_dir, "ChpSTREM2AD_analysis_dataset.csv")
raw_data_path <- file.path(project_root, "data", "raw", "Data.csv")
raw_data_all_path <- file.path(project_root, "data", "raw", "Data_all.csv")

normality_table_path <- file.path(result_summary_dir, "normality_screening.csv")
conversion_log_path <- file.path(result_summary_dir, "numeric_conversion_log.csv")
transformation_table_path <- file.path(result_summary_dir, "transformation_plan.csv")
cognition_map_path <- file.path(result_summary_dir, "cognition_component_map.csv")
variable_dictionary_path <- file.path(result_summary_dir, "selected_variable_dictionary.csv")
result_run_metadata_path <- file.path(result_run_dir, "run_metadata.csv")
report_dir <- result_report_dir
report_md_path <- file.path(report_dir, project_config$report$file_name)

write_csv_utf8(
  data.frame(
    project_name = project_config$project_name,
    result_version = result_version,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    language = report_language,
    raw_data_path = raw_data_path,
    raw_data_all_path = raw_data_all_path,
    report_md_path = report_md_path,
    stringsAsFactors = FALSE
  ),
  result_run_metadata_path,
  row.names = FALSE
)
