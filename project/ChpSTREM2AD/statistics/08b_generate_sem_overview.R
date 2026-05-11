source(file.path(getwd(), "00_setup.R"))

sem_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_mediation_summary.csv"))
sem_fit <- read_result_or_empty(file.path(result_summary_dir, "sem_model_fit.csv"))
sem_paths <- read_result_or_empty(file.path(result_summary_dir, "sem_path_coefficients.csv"))

translate_fit_quality <- function(text_value, language = "zh") {
  text_value <- as.character(text_value %||% "")
  if (language == "zh") {
    if (grepl("good", text_value, ignore.case = TRUE)) {
      return("\u826f\u597d")
    }
    if (grepl("acceptable", text_value, ignore.case = TRUE)) {
      return("\u5c1a\u53ef")
    }
    if (grepl("weak", text_value, ignore.case = TRUE)) {
      return("\u8f83\u5f31")
    }
  }
  text_value
}

format_effect_star <- function(estimate, p_value, digits = 4) {
  if (is.null(estimate) || length(estimate) == 0 || is.na(estimate)) {
    return("NA")
  }
  paste0(format_numeric_human(estimate, digits = digits), significance_mark_suffix(p_value))
}

format_sig_flag <- function(p_value) {
  if (is.null(p_value) || length(p_value) == 0 || is.na(p_value)) {
    return("NA")
  }
  if (p_value < 0.001) {
    return("\u663e\u8457**")
  }
  if (p_value < 0.05) {
    return("\u663e\u8457*")
  }
  "\u4e0d\u663e\u8457"
}

fit_idx_text <- function(row, digits = 3) {
  if (nrow(row) == 0) {
    return("NA")
  }
  paste0(
    "CFI=", format_numeric_human(row$cfi[[1]], digits = digits),
    ", TLI=", format_numeric_human(row$tli[[1]], digits = digits),
    ", RMSEA=", format_numeric_human(row$rmsea[[1]], digits = digits),
    ", SRMR=", format_numeric_human(row$srmr[[1]], digits = digits)
  )
}

effect_pattern_label <- function(opposite_direction, proportion_pct) {
  if (is.na(opposite_direction)) {
    return("NA")
  }
  base_label <- if (isTRUE(opposite_direction)) {
    "\u906e\u853d\u6548\u5e94"
  } else {
    "\u4fc3\u8fdb\u6548\u5e94"
  }
  if (is.null(proportion_pct) || length(proportion_pct) == 0 || is.na(proportion_pct)) {
    return(base_label)
  }
  paste0(base_label, "\uff08", format_numeric_human(proportion_pct, digits = 1), "%\uff09")
}

extract_path_row <- function(path_data, sem_model, cognition_model, group_name, path_name) {
  path_data[
    path_data$sem_model == sem_model &
      path_data$cognition_model == cognition_model &
      path_data$group == group_name &
      path_data$path == path_name,
    ,
    drop = FALSE
  ]
}

group_order <- c(
  "Overall",
  runtime_settings$group_order %||% sort(unique(setdiff(as.character(sem_summary$group), "Overall")))
)
cognition_order <- names(project_config$cognition_models)
sem_model_order <- unique(sem_summary$sem_model)

overview_rows <- lapply(seq_len(nrow(sem_summary)), function(i) {
  summary_row <- sem_summary[i, , drop = FALSE]
  fit_row <- sem_fit[
    sem_fit$sem_model == summary_row$sem_model[[1]] &
      sem_fit$cognition_model == summary_row$cognition_model[[1]] &
      sem_fit$group == summary_row$group[[1]],
    ,
    drop = FALSE
  ]

  a_row <- extract_path_row(sem_paths, summary_row$sem_model[[1]], summary_row$cognition_model[[1]], summary_row$group[[1]], "a")
  b_row <- extract_path_row(sem_paths, summary_row$sem_model[[1]], summary_row$cognition_model[[1]], summary_row$group[[1]], "b")
  c_row <- extract_path_row(sem_paths, summary_row$sem_model[[1]], summary_row$cognition_model[[1]], summary_row$group[[1]], "c_prime")

  data.frame(
    group = summary_row$group[[1]],
    cognition_model = summary_row$cognition_model[[1]],
    sem_model = summary_row$sem_model[[1]],
    x = summary_row$x_var[[1]],
    mediator = summary_row$mediator_var[[1]],
    fit_quality = translate_fit_quality(fit_row$model_reasonable[[1]], language = report_language),
    fit_indices = fit_idx_text(fit_row, digits = 3),
    a_path = format_effect_star(a_row$estimate[[1]], a_row$p_value[[1]], digits = report_digits),
    a_sig = format_sig_flag(a_row$p_value[[1]]),
    b_path = format_effect_star(b_row$estimate[[1]], b_row$p_value[[1]], digits = report_digits),
    b_sig = format_sig_flag(b_row$p_value[[1]]),
    c_path = format_effect_star(c_row$estimate[[1]], c_row$p_value[[1]], digits = report_digits),
    c_sig = format_sig_flag(c_row$p_value[[1]]),
    indirect = format_effect_star(summary_row$indirect[[1]], summary_row$indirect_p[[1]], digits = report_digits),
    indirect_sig = format_sig_flag(summary_row$indirect_p[[1]]),
    direct = format_effect_star(summary_row$direct[[1]], summary_row$direct_p[[1]], digits = report_digits),
    direct_sig = format_sig_flag(summary_row$direct_p[[1]]),
    total = format_effect_star(summary_row$total[[1]], summary_row$total_p[[1]], digits = report_digits),
    total_sig = format_sig_flag(summary_row$total_p[[1]]),
    effect_pattern = effect_pattern_label(summary_row$opposite_direction[[1]], summary_row$proportion_mediated_pct[[1]]),
    stringsAsFactors = FALSE
  )
})

overview_table <- if (length(overview_rows) > 0) do.call(rbind, overview_rows) else data.frame()

if (nrow(overview_table) > 0) {
  overview_table$group <- factor(overview_table$group, levels = group_order)
  overview_table$cognition_model <- factor(overview_table$cognition_model, levels = cognition_order)
  overview_table$sem_model <- factor(overview_table$sem_model, levels = sem_model_order)
  overview_table <- overview_table[order(overview_table$group, overview_table$cognition_model, overview_table$sem_model), , drop = FALSE]
  overview_table$group <- as.character(overview_table$group)
  overview_table$cognition_model <- as.character(overview_table$cognition_model)
  overview_table$sem_model <- as.character(overview_table$sem_model)
}

overview_display <- overview_table
if (nrow(overview_display) > 0) {
  overview_display$group_label <- vapply(overview_display$group, function(x) {
    if (identical(x, "Overall")) {
      return("\u5168\u4f53")
    }
    resolve_level_label("S_DX_label", x, settings = runtime_settings, language = report_language, fallback = x)
  }, character(1))
  overview_display$cognition_label <- vapply(overview_display$cognition_model, function(x) {
    resolve_variable_label(x, settings = runtime_settings, language = report_language, fallback = x)
  }, character(1))
  overview_display$model_label <- vapply(overview_display$sem_model, function(x) {
    resolve_variable_label(x, settings = runtime_settings, language = report_language, fallback = x)
  }, character(1))
  overview_display$x_label <- vapply(overview_display$x, function(x) {
    resolve_variable_label(x, settings = runtime_settings, language = report_language, fallback = x)
  }, character(1))
  overview_display$mediator_label <- vapply(overview_display$mediator, function(x) {
    resolve_variable_label(x, settings = runtime_settings, language = report_language, fallback = x)
  }, character(1))

  overview_display <- overview_display[, c(
    "group_label", "cognition_label", "model_label", "x_label", "mediator_label",
    "fit_quality", "fit_indices",
    "a_path", "a_sig", "b_path", "b_sig", "c_path", "c_sig",
    "indirect", "indirect_sig", "direct", "direct_sig", "total", "total_sig",
    "effect_pattern"
  )]

  names(overview_display) <- c(
    "\u5206\u7ec4", "\u8ba4\u77e5\u529f\u80fd", "\u6a21\u578b", "X", "\u4e2d\u4ecb",
    "\u6a21\u578b\u62df\u5408", "\u62df\u5408\u6307\u6807",
    "a\u8def\u5f84", "a\u662f\u5426\u663e\u8457", "b\u8def\u5f84", "b\u662f\u5426\u663e\u8457", "c\u8def\u5f84", "c\u662f\u5426\u663e\u8457",
    "\u95f4\u63a5\u6548\u5e94", "\u95f4\u63a5\u662f\u5426\u663e\u8457", "\u76f4\u63a5\u6548\u5e94", "\u76f4\u63a5\u662f\u5426\u663e\u8457", "\u603b\u6548\u5e94", "\u603b\u6548\u5e94\u662f\u5426\u663e\u8457",
    "\u6548\u5e94\u7c7b\u578b"
  )
}

focus_mask <- overview_table$sem_model %in% c("ChP_to_sTREM2_to_Cognition", "sTREM2_to_ChP_to_Cognition")
focus_display <- if (nrow(overview_display) > 0) overview_display[focus_mask, , drop = FALSE] else data.frame()

overview_csv_path <- file.path(result_summary_dir, "sem_overview_significance_table.csv")
overview_focus_csv_path <- file.path(result_summary_dir, "sem_overview_strem2_chp_only.csv")
overview_html_path <- file.path(result_tables_dir, "table_sem_overview_significance.html")
overview_md_path <- file.path(result_report_dir, "SEM_overview_fit_and_significance.md")

write_csv_utf8(overview_display, overview_csv_path, row.names = FALSE)
write_csv_utf8(focus_display, overview_focus_csv_path, row.names = FALSE)
write_simple_html_table(overview_display, overview_html_path, title = "SEM \u6a21\u578b\u62df\u5408\u4e0e\u8def\u5f84\u663e\u8457\u6027\u603b\u89c8")

overview_lines <- c(
  "# SEM\u6a21\u578b\u62df\u5408\u4e0e\u8def\u5f84\u663e\u8457\u6027\u603b\u89c8",
  "",
  "\u8fd9\u4efd\u603b\u89c8\u8868\u7528\u4e8e\u5feb\u901f\u5224\u65ad\u6bcf\u4e2aSEM\u6a21\u578b\u7684\u62df\u5408\u60c5\u51b5\uff0c\u4ee5\u53ca a / b / c \u8def\u5f84\u3001\u95f4\u63a5\u6548\u5e94\u3001\u76f4\u63a5\u6548\u5e94\u3001\u603b\u6548\u5e94\u662f\u5426\u663e\u8457\u3002",
  "",
  "- `*` \u8868\u793a p < 0.05\uff0c`**` \u8868\u793a p < 0.001\u3002",
  "- `\u6a21\u578b\u62df\u5408` \u4e3a\u5bf9 CFI / TLI / RMSEA / SRMR \u7684\u76f4\u89c2\u5f52\u7eb3\u3002",
  "- `\u6548\u5e94\u7c7b\u578b` \u4e2d\uff0c\u906e\u853d\u6548\u5e94\u8868\u793a\u76f4\u63a5\u6548\u5e94\u4e0e\u95f4\u63a5\u6548\u5e94\u65b9\u5411\u76f8\u53cd\uff1b\u4fc3\u8fdb\u6548\u5e94\u8868\u793a\u65b9\u5411\u4e00\u81f4\u3002",
  "",
  "## \u5168\u90e8\u6a21\u578b\u603b\u89c8",
  ""
)

overview_lines <- c(overview_lines, markdown_table(overview_display, digits = report_digits))
overview_lines <- c(
  overview_lines,
  "## sTREM2\u4e0eChPICV\u91cd\u70b9\u6a21\u578b",
  ""
)
overview_lines <- c(overview_lines, markdown_table(focus_display, digits = report_digits))

writeLines(enc2utf8(overview_lines), con = overview_md_path, useBytes = TRUE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "08b_generate_sem_overview",
  output_files = c(overview_csv_path, overview_focus_csv_path, overview_html_path, overview_md_path),
  note = "Generated compact SEM overview tables for model fit and path significance.",
  summary_dir = result_summary_dir
)
