source(file.path(getwd(), "00_setup.R"))

raw_data <- read_project_data(raw_data_path)

# 请根据真实数据修改这里的变量名
analysis_data <- subset(
  raw_data,
  select = c(
    "id",
    "sTREM2",
    "chp_volume",
    "chp_thickness",
    "age",
    "sex",
    "education",
    "icv",
    "diagnosis_group",
    "amyloid_status"
  )
)

write_csv_utf8(analysis_data, clean_data_path, row.names = FALSE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "01_data_prep",
  output_files = clean_data_path,
  note = "Initial analysis dataset created."
)
