source(file.path(getwd(), "00_setup.R"))

analysis_data <- read_project_data(analysis_data_path)
descriptive_overall_cont <- read_result_or_empty(file.path(result_summary_dir, "descriptive_overall_continuous.csv"))
descriptive_overall_cat <- read_result_or_empty(file.path(result_summary_dir, "descriptive_overall_categorical.csv"))
group_overall <- read_result_or_empty(file.path(result_summary_dir, "group_comparisons_overall.csv"))
matching_decision <- read_result_or_empty(file.path(result_summary_dir, "matching_decision.csv"))
matching_balance <- read_result_or_empty(file.path(result_summary_dir, "matching_balance_after_matching.csv"))
chp_linear_overall <- read_result_or_empty(file.path(result_summary_dir, "chp_strem2_linear_overall.csv"))
chp_linear_by_group <- read_result_or_empty(file.path(result_summary_dir, "chp_strem2_linear_by_group.csv"))
chp_partial_overall <- read_result_or_empty(file.path(result_summary_dir, "chp_strem2_partial_overall.csv"))
ptau_linear_overall <- read_result_or_empty(file.path(result_summary_dir, "ptau_linear_overall.csv"))
ptau_partial_overall <- read_result_or_empty(file.path(result_summary_dir, "ptau_partial_overall.csv"))
advanced_biomarker_overall <- read_result_or_empty(file.path(result_summary_dir, "advanced_biomarker_adjusted_overall.csv"))
advanced_interaction_comparison <- read_result_or_empty(file.path(result_summary_dir, "advanced_interaction_comparison.csv"))
advanced_nonlinear_summary <- read_result_or_empty(file.path(result_summary_dir, "advanced_nonlinear_model_summary.csv"))
advanced_nonlinear_tests <- read_result_or_empty(file.path(result_summary_dir, "advanced_nonlinear_tests.csv"))
sem_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_mediation_summary.csv"))
sem_fit <- read_result_or_empty(file.path(result_summary_dir, "sem_model_fit.csv"))
sem_stage_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_stage_direction_summary.csv"))
sem_indirect_difference_tests <- read_result_or_empty(file.path(result_summary_dir, "sem_indirect_difference_tests.csv"))

journal_report_path <- file.path(
  report_dir,
  project_config$report$journal_file_name %||% "ChpSTREM2AD_journal_style_report.md"
)

fmt_num <- function(x, digits = 3) format_numeric_human(x, digits = digits)
fmt_p <- function(x, digits = 4) format_p_value_human(x, digits = digits, markdown = TRUE)

pick_row <- function(data, filters = list()) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  keep <- rep(TRUE, nrow(data))
  for (nm in names(filters)) {
    keep <- keep & data[[nm]] == filters[[nm]]
  }
  data[keep, , drop = FALSE]
}

first_value <- function(data, column) {
  if (is.null(data) || nrow(data) == 0 || !column %in% names(data)) {
    return(NA)
  }
  data[[column]][[1]]
}

safe_table <- function(data, digits = report_digits) {
  markdown_table(data, digits = digits, empty_text = "_隴鯉｣ｰ隰ｨ・ｰ隰撰ｽｮ_")
}

group_counts <- as.data.frame(table(analysis_data[[project_config$variables$group_label_var]]), stringsAsFactors = FALSE)
names(group_counts) <- c("group", "n")
group_counts <- group_counts[match(c("CN", "MCI", "AD"), group_counts$group), , drop = FALSE]

age_row <- descriptive_overall_cont[descriptive_overall_cont$variable == "S_AGE", , drop = FALSE]
female_row <- descriptive_overall_cat[
  descriptive_overall_cat$variable == "S_PTGENDER_label" & descriptive_overall_cat$level == "Female",
  ,
  drop = FALSE
]

overall_chp_on_strem2 <- pick_row(chp_linear_overall, list(model_name = "ChPICV_on_sTREM2", term = "exposure"))
overall_strem2_on_chp <- pick_row(chp_linear_overall, list(model_name = "sTREM2_on_ChPICV", term = "exposure"))
overall_partial <- chp_partial_overall

cn_chp_on_strem2 <- pick_row(chp_linear_by_group, list(model_name = "ChPICV_on_sTREM2", term = "exposure", group = "CN"))
mci_chp_on_strem2 <- pick_row(chp_linear_by_group, list(model_name = "ChPICV_on_sTREM2", term = "exposure", group = "MCI"))
ad_chp_on_strem2 <- pick_row(chp_linear_by_group, list(model_name = "ChPICV_on_sTREM2", term = "exposure", group = "AD"))

ptau_on_chp <- pick_row(ptau_linear_overall, list(model_name = "PTAU_on_ChPICV", term = "exposure"))
ptau_on_strem2 <- pick_row(ptau_linear_overall, list(model_name = "PTAU_on_sTREM2", term = "exposure"))
ptau_partial_chp <- pick_row(ptau_partial_overall, list(model_name = "PTAU_with_ChPICV"))
ptau_partial_strem2 <- pick_row(ptau_partial_overall, list(model_name = "PTAU_with_MSD_STREM2CORRECTED"))

ptau_adj_model <- advanced_biomarker_overall[
  advanced_biomarker_overall$model_name == "ChPICV_on_sTREM2_PTAU_ABETA" &
    advanced_biomarker_overall$term %in% c("exposure", "PTAU", "S_ABETA"),
  ,
  drop = FALSE
]
tau_adj_model <- advanced_biomarker_overall[
  advanced_biomarker_overall$model_name == "ChPICV_on_sTREM2_TAU_ABETA" &
    advanced_biomarker_overall$term %in% c("exposure", "TAU", "S_ABETA"),
  ,
  drop = FALSE
]
interaction_row <- advanced_interaction_comparison[1, , drop = FALSE]
nonlinear_best <- if (nrow(advanced_nonlinear_summary) > 0) advanced_nonlinear_summary[which.min(advanced_nonlinear_summary$aic), , drop = FALSE] else data.frame()
nonlinear_spline_test <- pick_row(advanced_nonlinear_tests, list(test_name = "linear_vs_spline"))

sem_summary_overall <- if ("group" %in% names(sem_summary)) sem_summary[sem_summary$group == "Overall", , drop = FALSE] else sem_summary
sem_fit_overall <- if ("group" %in% names(sem_fit)) sem_fit[sem_fit$group == "Overall", , drop = FALSE] else sem_fit
sem_diff_overall <- if ("group" %in% names(sem_indirect_difference_tests)) sem_indirect_difference_tests[sem_indirect_difference_tests$group == "Overall", , drop = FALSE] else sem_indirect_difference_tests

fit_mmse_moca <- sem_fit_overall[sem_fit_overall$cognition_model == "Cog_MMSE_MOCA", , drop = FALSE]
fit_mpacc <- sem_fit_overall[sem_fit_overall$cognition_model == "Cog_mPACC", , drop = FALSE]

primary_sem_strem2 <- pick_row(sem_summary_overall, list(sem_model = "ChP_to_sTREM2_to_Cognition", cognition_model = "Cog_MMSE_MOCA"))
primary_sem_ptau <- pick_row(sem_summary_overall, list(sem_model = "ChP_to_PTAU_to_Cognition", cognition_model = "Cog_MMSE_MOCA"))
primary_sem_abeta <- pick_row(sem_summary_overall, list(sem_model = "ChP_to_ABETA_to_Cognition", cognition_model = "Cog_MMSE_MOCA"))
reverse_sem_abeta <- pick_row(sem_summary_overall, list(sem_model = "ABETA_to_ChP_to_Cognition", cognition_model = "Cog_MMSE_MOCA"))
reverse_sem_strem2 <- pick_row(sem_summary_overall, list(sem_model = "sTREM2_to_ChP_to_Cognition", cognition_model = "Cog_MMSE_MOCA"))
reverse_sem_ptau <- pick_row(sem_summary_overall, list(sem_model = "PTAU_to_ChP_to_Cognition", cognition_model = "Cog_MMSE_MOCA"))

primary_regression_table <- data.frame(
  analysis = c(
    "驛､・ｿ隲､・ｧ陜玲ｧｫ・ｽ繝ｻ ChPICV ~ sTREM2",
    "驛､・ｿ隲､・ｧ陜玲ｧｫ・ｽ繝ｻ sTREM2 ~ ChPICV",
    "陋帛・蠍瑚怦・ｳ: ChPICV vs sTREM2",
    "驛､・ｿ隲､・ｧ陜玲ｧｫ・ｽ繝ｻ P-Tau ~ ChPICV",
    "驛､・ｿ隲､・ｧ陜玲ｧｫ・ｽ繝ｻ P-Tau ~ sTREM2",
    "陋帛・蠍瑚怦・ｳ: P-Tau vs ChPICV",
    "陋帛・蠍瑚怦・ｳ: P-Tau vs sTREM2"
  ),
  estimate = c(
    fmt_num(first_value(overall_chp_on_strem2, "std_beta")),
    fmt_num(first_value(overall_strem2_on_chp, "std_beta")),
    fmt_num(first_value(overall_partial, "estimate")),
    fmt_num(first_value(ptau_on_chp, "std_beta")),
    fmt_num(first_value(ptau_on_strem2, "std_beta")),
    fmt_num(first_value(ptau_partial_chp, "estimate")),
    fmt_num(first_value(ptau_partial_strem2, "estimate"))
  ),
  p_value = c(
    fmt_p(first_value(overall_chp_on_strem2, "p.value")),
    fmt_p(first_value(overall_strem2_on_chp, "p.value")),
    fmt_p(first_value(overall_partial, "p_value")),
    fmt_p(first_value(ptau_on_chp, "p.value")),
    fmt_p(first_value(ptau_on_strem2, "p.value")),
    fmt_p(first_value(ptau_partial_chp, "p_value")),
    fmt_p(first_value(ptau_partial_strem2, "p_value"))
  ),
  stringsAsFactors = FALSE
)

advanced_table <- data.frame(
  analysis = c(
    "ChPICV ~ sTREM2 + P-Tau + A・趣ｽｲ",
    "ChPICV ~ sTREM2 + Tau + A・趣ｽｲ",
    "sTREM2 ・・・diagnosis",
    "linear vs spline"
  ),
  result = c(
    paste0(
      "sTREM2 ・趣ｽｲ=", fmt_num(first_value(ptau_adj_model[ptau_adj_model$term == "exposure", , drop = FALSE], "estimate")),
      ", p=", fmt_p(first_value(ptau_adj_model[ptau_adj_model$term == "exposure", , drop = FALSE], "p.value")),
      "; P-Tau p=", fmt_p(first_value(ptau_adj_model[ptau_adj_model$term == "PTAU", , drop = FALSE], "p.value")),
      "; A・趣ｽｲ p=", fmt_p(first_value(ptau_adj_model[ptau_adj_model$term == "S_ABETA", , drop = FALSE], "p.value"))
    ),
    paste0(
      "sTREM2 ・趣ｽｲ=", fmt_num(first_value(tau_adj_model[tau_adj_model$term == "exposure", , drop = FALSE], "estimate")),
      ", p=", fmt_p(first_value(tau_adj_model[tau_adj_model$term == "exposure", , drop = FALSE], "p.value")),
      "; Tau p=", fmt_p(first_value(tau_adj_model[tau_adj_model$term == "TAU", , drop = FALSE], "p.value")),
      "; A・趣ｽｲ p=", fmt_p(first_value(tau_adj_model[tau_adj_model$term == "S_ABETA", , drop = FALSE], "p.value"))
    ),
    paste0("闔・､闔蟶晢ｽ｡・ｹ p=", fmt_p(first_value(interaction_row, "interaction_p_value"))),
    paste0("隴崢闖ｴ・ｳ AIC 隶難ｽ｡陜吶・", first_value(nonlinear_best, "model_type"), "; linear vs spline p=", fmt_p(first_value(nonlinear_spline_test, "p_value")))
  ),
  stringsAsFactors = FALSE
)

sem_compact <- merge(
  sem_summary_overall[, c(
    "sem_model", "cognition_model", "group", "n", "indirect", "indirect_p", "direct", "direct_p",
    "total", "total_p", "proportion_mediated_pct", "opposite_direction", "mediation_type"
  ), drop = FALSE],
  sem_fit_overall[, c("sem_model", "cognition_model", "group", "cfi", "tli", "rmsea", "srmr", "chisq", "df", "model_reasonable"), drop = FALSE],
  by = c("sem_model", "cognition_model", "group"),
  all.x = TRUE
)
sem_compact <- data.frame(
  model = paste(sem_compact$sem_model, sem_compact$cognition_model, sep = " / "),
  n = sem_compact$n,
  indirect = vapply(sem_compact$indirect, fmt_num, FUN.VALUE = character(1)),
  indirect_p = vapply(sem_compact$indirect_p, fmt_p, FUN.VALUE = character(1)),
  direct = vapply(sem_compact$direct, fmt_num, FUN.VALUE = character(1)),
  direct_p = vapply(sem_compact$direct_p, fmt_p, FUN.VALUE = character(1)),
  total = vapply(sem_compact$total, fmt_num, FUN.VALUE = character(1)),
  total_p = vapply(sem_compact$total_p, fmt_p, FUN.VALUE = character(1)),
  contribution_pct = ifelse(is.na(sem_compact$proportion_mediated_pct), NA_character_, paste0(vapply(sem_compact$proportion_mediated_pct, fmt_num, FUN.VALUE = character(1)), "%")),
  pattern = ifelse(sem_compact$opposite_direction, "遮蔽效应/不一致中介", "促进效应/一致中介"),
  CFI = vapply(sem_compact$cfi, fmt_num, FUN.VALUE = character(1)),
  TLI = vapply(sem_compact$tli, fmt_num, FUN.VALUE = character(1)),
  RMSEA = vapply(sem_compact$rmsea, fmt_num, FUN.VALUE = character(1)),
  SRMR = vapply(sem_compact$srmr, fmt_num, FUN.VALUE = character(1)),
  chisq = vapply(sem_compact$chisq, fmt_num, FUN.VALUE = character(1)),
  df = sem_compact$df,
  fit_judgement = sem_compact$model_reasonable,
  stringsAsFactors = FALSE
)

sem_stage_display <- sem_stage_summary
if (nrow(sem_stage_display) > 0) {
  sem_stage_display$cognition <- vapply(sem_stage_display$cognition, function(x) resolve_variable_label(x, runtime_settings, report_language, fallback = x), FUN.VALUE = character(1))
}

sem_diff_table <- data.frame(
  comparison = sem_diff_overall$comparison_name,
  cognition = sem_diff_overall$cognition_model,
  mediator_a = sem_diff_overall$mediator_a,
  mediator_b = sem_diff_overall$mediator_b,
  indirect_a = vapply(sem_diff_overall$indirect_a, fmt_num, FUN.VALUE = character(1)),
  p_a = vapply(sem_diff_overall$indirect_a_p, fmt_p, FUN.VALUE = character(1)),
  indirect_b = vapply(sem_diff_overall$indirect_b, fmt_num, FUN.VALUE = character(1)),
  p_b = vapply(sem_diff_overall$indirect_b_p, fmt_p, FUN.VALUE = character(1)),
  difference = vapply(sem_diff_overall$indirect_diff, fmt_num, FUN.VALUE = character(1)),
  p_diff = vapply(sem_diff_overall$indirect_diff_p, fmt_p, FUN.VALUE = character(1)),
  significant = ifelse(sem_diff_overall$difference_significant, "隴擾ｽｯ", "陷ｷ・ｦ"),
  stringsAsFactors = FALSE
)

make_fit_sentence <- function(data, label) {
  if (nrow(data) == 0) {
    return(paste0(label, "隴幢ｽｪ髣費ｽｷ陟墓懷ｺ・囓・｣鬩･鬘泌飭隲｡貅ｷ邊区沿謐ｺ譽｡邵ｲ繝ｻ))
  }
  paste0(
    label, "騾ｧ繝ｻ萓幄惺莠･邇・ｫ｣・ｴ闕ｳ・ｺ繝ｻ蜥ｾFI ", fmt_num(min(data$cfi, na.rm = TRUE)), "遯ｶ繝ｻ, fmt_num(max(data$cfi, na.rm = TRUE)),
    "繝ｻ蜊ｦLI ", fmt_num(min(data$tli, na.rm = TRUE)), "遯ｶ繝ｻ, fmt_num(max(data$tli, na.rm = TRUE)),
    "繝ｻ霆庚SEA ", fmt_num(min(data$rmsea, na.rm = TRUE)), "遯ｶ繝ｻ, fmt_num(max(data$rmsea, na.rm = TRUE)),
    "繝ｻ驛｡RMR ", fmt_num(min(data$srmr, na.rm = TRUE)), "遯ｶ繝ｻ, fmt_num(max(data$srmr, na.rm = TRUE)), "邵ｲ繝ｻ
  )
}

journal_lines <- c(
  "# sTREM2邵ｲ竏ｬﾑ・沿諛会ｽｸ蟶ｷ・ｻ謐ｺ譯ｷ闕ｳ謇具ｽｮ・､驕擾ｽ･陷画ｺｯ繝ｻ騾ｧ繝ｻ邏幄惺閧ｲ・ｻ謐ｺ譽｡隰夲ｽ･陷ｻ繝ｻ,
  "",
  paste0("- 騾墓ｻ薙・隴鯉ｽｶ鬮｣・ｴ: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("- 謇域瑳譽｡霑壼沺謔ｽ: `", result_version, "`"),
  paste0("- 陷ｴ貅ｷ・ｧ蛹ｺ辟夊ｬ撰ｽｮ: `", raw_data_path, "`"),
  "",
  "## 謇域瑳譽｡隲､・ｻ髫励・,
  "",
  paste0(
    "隴幢ｽｬ隹ｺ・｡陋ｻ繝ｻ譴ｵ驛､・ｳ陷茨ｽ･ ", nrow(analysis_data), " 關灘唱螂ｳ髫ｸ謌環繝ｻ・ｼ謔溘・闕ｳ・ｭ CN ",
    group_counts$n[group_counts$group == "CN"], " 關灘・・ｼ蜍ｲCI ",
    group_counts$n[group_counts$group == "MCI"], " 關灘・・ｼ遯櫂 ",
    group_counts$n[group_counts$group == "AD"], " 關謎ｹ敖繝ｻ
  ),
  paste0(
    "隲､・ｻ闖ｴ轣假ｽｹ・ｴ魄ｴ繝ｻ・ｸ・ｺ ", first_value(age_row, "summary_text"),
    "繝ｻ蟶幢ｽ･・ｳ隲､・ｧ雎域ｯ費ｽｾ蛟ｶ・ｸ・ｺ ", first_value(female_row, "summary_text"), "邵ｲ繝ｻ
  ),
  paste0(
    "謇医・鮴崎ｱ育｢托ｽｾ繝ｻ莉樣♂・ｺ ChPICV邵ｲ縲・ｾ趣ｽｲ邵ｲ繧拌u 陷･繝ｻP-Tau 陜ｨ・ｨ髫ｸ鬆大ｦ呎沿繝ｻ鮴崎攬繝ｻ・ｭ莨懈Β隴擾ｽｾ髣｡諤懶ｽｷ・ｮ陟代ｑ・ｼ迹堋繝ｻsTREM2 隴幢ｽｬ髴・ｽｫ隴幢ｽｪ髫冷扱莉樣裡遉ｼ・ｻ繝ｻ鮴崎淦・ｮ陟代ｑ・ｼ繝ｻ = ",
    fmt_p(first_value(group_overall[group_overall$variable == "MSD_STREM2CORRECTED", , drop = FALSE], "p_value")), "繝ｻ蟲ｨﾂ繝ｻ
  ),
  "陝ｷ・ｴ魄ｴ繝ｻ譟ｱ隲､・ｧ陋ｻ・ｫ陜ｨ・ｨ謇医・鮴崎氛莨懈Β闕ｳ讎奇ｽｹ・ｳ髯ｦ・｡繝ｻ謔溷ｱ剰ｱ・ｽ､隰・ｽｧ髯ｦ蠕｡・ｺ繝ｻ貅宣ｩ滓ｦ翫・隴ｫ謦ｰ・ｼ蟶帶ｺ宣ｩ滓ｦ企堅陝ｷ・ｴ魄ｴ繝ｻ譟ｱ隲､・ｧ陋ｻ・ｫ騾ｧ繝ｻ・ｻ繝ｻ鮴崎淦・ｮ陟代ｇ謠定叉讎翫・隴擾ｽｾ髣｡證ｦ・ｼ譴ｧ鄂ｲ驕会ｽｺ陷ｷ螳茨ｽｻ・ｭ謇医・鮴肴沿謐ｺ譽｡闕ｳ讎奇ｽ､・ｪ陷ｿ・ｯ髢ｭ・ｽ陞ｳ謔溘・騾包ｽｱ髴大姓・ｸ・､闕ｳ・ｪ陜難ｽｺ驛､・ｿ陜暦｣ｰ驍擾｣ｰ鬯ｩ・ｱ陷会ｽｨ邵ｲ繝ｻ,
  "",
  "## 闕ｳ・ｻ髫補悪・ｻ謐ｺ譽｡",
  "",
  "### 1. ChPICV 闕ｳ繝ｻsTREM2 騾ｧ繝ｻﾂ・ｻ闖ｴ轣倥・髢ｨ繝ｻ,
  "",
  paste0(
    "陜ｨ・ｨ陷茨ｽｨ隴ｬ・ｷ隴幢ｽｬ闕ｳ・ｭ繝ｻ髱ｴhPICV 闕ｳ繝ｻsTREM2 陷ｻ蝓滉ｻ樣裡闍難ｽｴ貅ｽ蠍瑚怦・ｳ邵ｲ繧・ｽｻ・･ ChPICV 闕ｳ・ｺ陜暦｣ｰ陷ｿ蛟ｬ纃ｼ騾ｧ繝ｻ・ｰ繝ｻ邏幄惺螳茨ｽｺ・ｿ隲､・ｧ陜玲ｧｫ・ｽ蜻井ｻ樣♂・ｺ繝ｻ闌撒REM2 騾ｧ繝ｻ・ｰ繝ｻ繩･陋ｹ髢螻楢門､・ｳ・ｻ隰ｨ・ｰ闕ｳ・ｺ ",
    fmt_num(first_value(overall_chp_on_strem2, "std_beta")), "繝ｻ邯・= ",
    fmt_p(first_value(overall_chp_on_strem2, "p.value")), "繝ｻ蟶帶ｸ夊惺隨ｬ・ｨ・｡陜吶・sTREM2 ~ ChPICV 謇域瑳譽｡隴・ｽｹ陷ｷ蜿ｰ・ｸﾂ髢ｾ・ｴ繝ｻ蝓滂｣ｰ繝ｻ繩･陋ｹ繝ｻ・趣ｽｲ = ",
    fmt_num(first_value(overall_strem2_on_chp, "std_beta")), "繝ｻ邯・= ",
    fmt_p(first_value(overall_strem2_on_chp, "p.value")), "繝ｻ蟲ｨﾂ繝ｻ
  ),
  paste0(
    "陋ｻ繝ｻ莠ｳ隹ｿ・ｵ陋ｻ繝ｻ譴ｵ隰蜊・ｽ､・ｺ髫ｸ・･髮肴ｺｽ蠍瑚怦・ｳ闕ｳ・ｻ髫補握・ｧ竏ｽ・ｺ繝ｻCN 謇医・・ｼ螟ｷ・ｲ = ", fmt_num(first_value(cn_chp_on_strem2, "std_beta")), ", p = ", fmt_p(first_value(cn_chp_on_strem2, "p.value")),
    "繝ｻ迚呎浤 MCI 謇医・・ｼ螟ｷ・ｲ = ", fmt_num(first_value(mci_chp_on_strem2, "std_beta")), ", p = ", fmt_p(first_value(mci_chp_on_strem2, "p.value")),
    "繝ｻ莨夲ｽｼ迹堋謔滓Β AD 謇医・・ｸ・ｭ闕ｳ閧ｴ莉樣裡證ｦ・ｼ螟ｷ・ｲ = ", fmt_num(first_value(ad_chp_on_strem2, "std_beta")), ", p = ", fmt_p(first_value(ad_chp_on_strem2, "p.value")), "繝ｻ蟲ｨﾂ繝ｻ
  ),
  paste0(
    "陋帛・蠍瑚怦・ｳ陋ｻ繝ｻ譴ｵ謇域瑳譽｡闕ｳ荳ｻ螻楢門宴・ｸﾂ髢ｾ・ｴ繝ｻ繝ｻ = ", fmt_num(first_value(overall_partial, "estimate")), ", p = ",
    fmt_p(first_value(overall_partial, "p_value")), "繝ｻ莨夲ｽｼ譴ｧ鬮ｪ隰悶・ChPICV 闕ｳ繝ｻsTREM2 陜ｨ・ｨ隲､・ｻ闖ｴ謐ｺ・ｰ・ｷ隴幢ｽｬ闕ｳ・ｭ陝・ｼ懈Β驕橸ｽｳ陋幢ｽ･騾ｧ繝ｻ貂夊惺螟ｧ繝ｻ髢ｨ譁青繝ｻ
  ),
  "",
  safe_table(primary_regression_table),
  "",
  "### 2. 謇井ｸ槭・ AD 騾墓ｺｽ鮟・ｭｬ繝ｻ・ｿ遉ｼ鮟・ｧ繝ｻ・｡・･陷医・・ｧ・｣鬩･繝ｻ,
  "",
  paste0(
    "P-Tau 闕ｳ繝ｻChPICV 陷ｻ驛・ｽｴ貅ｽ蠍瑚怦・ｳ繝ｻ蝓滂｣ｰ繝ｻ繩･陋ｹ繝ｻ・趣ｽｲ = ", fmt_num(first_value(ptau_on_chp, "std_beta")), ", p = ",
    fmt_p(first_value(ptau_on_chp, "p.value")), "繝ｻ莨夲ｽｼ蠕｡・ｸ繝ｻsTREM2 陷ｻ蝓滂ｽｭ・｣騾ｶ・ｸ陷茨ｽｳ繝ｻ蝓滂｣ｰ繝ｻ繩･陋ｹ繝ｻ・趣ｽｲ = ",
    fmt_num(first_value(ptau_on_strem2, "std_beta")), ", p = ", fmt_p(first_value(ptau_on_strem2, "p.value")), "繝ｻ蟲ｨﾂ繝ｻ
  ),
  paste0(
    "陜ｨ・ｨ陷ｷ譴ｧ諷ｮ驛､・ｳ陷茨ｽ･ P-Tau 陷･繝ｻA・趣ｽｲ 騾ｧ繝ｻ・ｨ・｡陜吝ｶ・ｸ・ｭ繝ｻ闌撒REM2 陝・ｽｹ ChPICV 騾ｧ繝ｻ蟲｡驕ｶ蛹ｺ隴懆取ｳ後詐陟托ｽｱ髢ｾ・ｳ髴趣ｽｹ驛帑ｿｶ莉樣裡證ｦ・ｼ螟ｷ・ｲ = ",
    fmt_num(first_value(ptau_adj_model[ptau_adj_model$term == "exposure", , drop = FALSE], "estimate")), ", p = ",
    fmt_p(first_value(ptau_adj_model[ptau_adj_model$term == "exposure", , drop = FALSE], "p.value")),
    "繝ｻ莨夲ｽｼ迹堋繝ｻP-Tau 闕ｳ繝ｻA・趣ｽｲ 闔牙ｺ・ｿ譎・亜隴擾ｽｾ髣｡蜉ｱﾂ繧会ｽｺ・ｳ陷茨ｽ･ Tau 陷･繝ｻA・趣ｽｲ 陷ｷ荳ｻ・ｾ諤懆寒騾ｶ・ｸ陷ｷ迹夲ｽｶ蜿･貍｢邵ｲ繝ｻ
  ),
  "髴大揃・ｯ・ｴ隴上・reviewer 隴崢陷ｿ・ｯ髢ｭ・ｽ髴托ｽｽ鬮｣・ｮ騾ｧ繝ｻﾂ蜻ＩPICV-sTREM2 陷茨ｽｳ驍会ｽｻ隴擾ｽｯ陷ｷ・ｦ陷ｿ・ｯ髯ｲ・ｫ謇井ｸ槭・ AD 騾槭・轤願ｬ・髫暦ｽ｣鬩･蟯ｩﾂ蜷晄Β陟也§辯戊ｬｨ・ｰ隰撰ｽｮ闕ｳ・ｭ騾ｧ繝ｻ・ｭ逍ｲ・｡蝓溷ｳｩ隰暦ｽ･髴鷹ｯ崢蛟ｬﾎ夊崕繝ｻ蠎・脂・･髫暦ｽ｣鬩･螂・ｽｼ蠕｡・ｽ繝ｻ・ｹ・ｶ闕ｳ蟠弱・陞ｳ謔溘・隴厄ｽｿ闔会ｽ｣ sTREM2 隰・闔会ｽ｣髯ｦ・ｨ騾ｧ繝ｻﾂ螟奇ｽｷ・ｯ闖ｫ・｡隲ｱ・ｯ遯ｶ蜷ｶﾂ繝ｻ,
  "",
  safe_table(advanced_table),
  "",
  "### 3. 闔・､闔蜑・ｽｽ諛・舞闕ｳ譛ｱ謦ｼ驛､・ｿ隲､・ｧ",
  "",
  paste0(
    "sTREM2 ・・・diagnosis 闔・､闔蟶晢ｽ｡・ｹ隴幢ｽｪ髴趣ｽｾ謇域ｺｯ・ｮ・｡陝・ｽｦ隴擾ｽｾ髣｡證ｦ・ｼ繝ｻ = ", fmt_p(first_value(interaction_row, "interaction_p_value")),
    "繝ｻ莨夲ｽｼ謔溷ｱ剰ｱ・ｽ､陟也§辯戊ｬｨ・ｰ隰撰ｽｮ陝・｣ｻ・ｸ蟠趣ｽｶ・ｳ闔会ｽ･闕ｳ・･隴ｬ・ｼ髫ｸ竏ｵ繝ｻ stage-dependent interaction邵ｲ繝ｻ
  ),
  paste0(
    "鬮ｱ讓抵ｽｺ・ｿ隲､・ｧ陋ｻ繝ｻ譴ｵ闕ｳ・ｭ繝ｻ闌姿line 隶難ｽ｡陜吝唱繝ｻ隴帷判諤呵抄・ｳ AIC繝ｻ蠕｡・ｽ繝ｻlinear vs spline 雎育｢托ｽｾ繝ｻ・ｻ繝ｻ莠蛾屆蜿･貍｢隲､・ｧ繝ｻ繝ｻ = ",
    fmt_p(first_value(nonlinear_spline_test, "p_value")),
    "繝ｻ莨夲ｽｼ迹夲ｽｯ・ｴ隴上・sTREM2 騾ｧ繝ｻ謦ｼ驛､・ｿ隲､・ｧ髯ｦ蠕｡・ｸ・ｺ陋滂ｽｼ陟募ｶｺ・ｿ譎芽風髫ｶ・ｨ髫ｶ・ｺ繝ｻ蠕｡・ｽ繝ｻ蝌ｯ闕ｳ讎奇ｽｮ諛会ｽｽ諛会ｽｸ・ｺ闕ｳ・ｻ謇域･｢・ｮ・ｺ邵ｲ繝ｻ
  ),
  "",
  "### 4. Latent-variable SEM 騾ｧ繝ｻ・ｰ・ｸ陟｢繝ｻ譖ｸ驍・ｽｰ",
  "",
  make_fit_sentence(fit_mmse_moca, "MMSE + MoCA 貎懷序驥乗ｨ｡蝙・),
  make_fit_sentence(fit_mpacc, "mPACC 貎懷序驥乗ｨ｡蝙・),
  "",
  paste0(
    "陜ｨ・ｨ隲｡貅ｷ邊玖ｭ崢驕橸ｽｳ陞ｳ螟ょ飭 MMSE + MoCA 雋取・蠎城ｩ･荵暦ｽｨ・｡陜吝ｶ・ｸ・ｭ繝ｻ髱ｴhPICV 遶翫・sTREM2 遶翫・cognition 騾ｧ繝ｻ鮴崎ｬ暦ｽ･隰ｨ莠･・ｺ豈費ｽｸ・ｺ ",
    fmt_num(first_value(primary_sem_strem2, "indirect")), "繝ｻ繝ｻ = ", fmt_p(first_value(primary_sem_strem2, "indirect_p")),
    "繝ｻ莨夲ｽｼ蠕｡・ｸ豈費ｽｸ螳亥ｳｩ隰暦ｽ･隰ｨ莠･・ｺ逍ｲ蟀ｿ陷ｷ驢榊ｶ瑚愾謳ｾ・ｼ謔滂ｽｱ讓費ｽｺ譛ｱ繝ｻ髦｡・ｽ隰ｨ莠･・ｺ繝ｻ闕ｳ蝣ｺ・ｸﾂ髢ｾ・ｴ闕ｳ・ｭ闔我ｹ敖繧奇ｽｿ蜻守ｽｲ驕会ｽｺ陜ｨ・ｨ隲､・ｻ闖ｴ謐ｺ・ｰ・ｷ隴幢ｽｬ闕ｳ・ｭ繝ｻ闌撒REM2 隴厄ｽｴ陷剃ｹ怜ｼ崎舉・ｨ陟托ｽｱ陋ｹ繝ｻChPICV 陝・ｽｹ髫ｶ・､驕擾ｽ･陷画ｺｯ繝ｻ騾ｧ繝ｻ・ｸ讎願懸陟厄ｽｱ陷ｩ髦ｪﾂ繝ｻ
  ),
  paste0(
    "騾ｶ・ｸ陝・ｽｹ陜ｨ・ｰ繝ｻ髱ｴhPICV 遶翫・P-Tau 遶翫・cognition 騾ｧ繝ｻ・ｷ・ｯ陟輔・莉樣♂・ｺ P-Tau 隴厄ｽｴ隰暦ｽ･髴醍ｬｬ蜿幄棔・ｧ闕ｳ讎願懸髫ｶ・､驕擾ｽ･隰ｨ莠･・ｺ逧ｮ蝎ｪ鬨ｾ螟奇ｽｷ・ｯ繝ｻ蟷・繝ｻChPICV 遶翫・A・趣ｽｲ 遶翫・cognition 隴擾ｽｯ陟也§辯戊ｭ崢驕橸ｽｳ陞ｳ螢ｹﾂ竏ｵ諤呵叉ﾂ髢ｾ・ｴ騾ｧ繝ｻ・ｸ・ｭ闔臥事・ｷ・ｯ陟輔・・ｼ蠕｡・ｸ・ｭ闔臥事・ｴ・｡霑ｪ・ｮ驍・・・ｺ・ｦ闕ｳ・ｺ ",
    fmt_num(first_value(primary_sem_abeta, "proportion_mediated_pct")), "%邵ｲ繝ｻ
  ),
  paste0(
    "陷ｿ讎企ｫ・恪・ｯ陟輔・・ｹ貊捺剰｢繝ｻ・ｦ竏ｫ・ｺ・ｳ陷茨ｽ･陋ｻ・､隴・ｽｭ繝ｻ蝌傍REM2 遶翫・ChPICV 遶翫・cognition邵ｲ・｣-Tau 遶翫・ChPICV 遶翫・cognition 闔会ｽ･陷ｿ繝ｻA・趣ｽｲ 遶翫・ChPICV 遶翫・cognition 陜ｮ繝ｻ・ｷ・ｲ闔ｨ・ｰ髫ｶ・｡繝ｻ謔溘・闕ｳ・ｭ A・趣ｽｲ 遶翫・ChPICV 遶翫・cognition 髯鯉ｽｽ霎滂ｽｶ隴擾ｽｾ髣｡證ｦ・ｼ蠕｡・ｽ繝ｻ・ｴ・｡霑ｪ・ｮ驍・・・ｾ繝ｻ・ｽ雜｣・ｼ閧ｲ・ｺ・ｦ ",
    fmt_num(first_value(reverse_sem_abeta, "proportion_mediated_pct")), "%繝ｻ莨夲ｽｼ譴ｧ蟲ｩ陷剃ｹ怜ｼ崎ｰｺ・｡驛､・ｧ髴搾ｽｯ陟輔・ﾂ繝ｻ
  ),
  "闕ｳ・ｭ闔臥事・ｴ・｡霑ｪ・ｮ驍・・・ｶ繝ｻ・ｿ繝ｻ100% 陝ｷ・ｶ闕ｳ蝣ｺ・ｻ・｣髯ｦ・ｨ隶難ｽ｡陜吝唱・､・ｱ隰ｨ闌ｨ・ｼ迹堋譴ｧ蠑崎摎・ｰ闕ｳ・ｺ鬮｣・ｴ隰暦ｽ･隰ｨ莠･・ｺ豈費ｽｸ螳亥ｳｩ隰暦ｽ･隰ｨ莠･・ｺ逍ｲ蟀ｿ陷ｷ驢榊ｶ瑚愾謳ｾ・ｼ譴ｧﾂ・ｻ隰ｨ莠･・ｺ遒托ｽ｢・ｫ鬩幢ｽｨ陋ｻ繝ｻ諷｣雎ｸ闌ｨ・ｼ蟷・ｽｿ蜥擾ｽｧ閧ｴ繝･陟厄ｽ｢陜ｨ・ｨ謇域ｺｯ・ｮ・｡闕ｳ鬘費ｽｧ・ｰ闕ｳ・ｺ闕ｳ蝣ｺ・ｸﾂ髢ｾ・ｴ闕ｳ・ｭ闔牙玄繝ｻ鬩包ｽｮ髦｡・ｽ隰ｨ莠･・ｺ雋ｻ・ｼ謔滂ｽｺ遒托ｽｯ・･髯ｲ・ｫ髫暦ｽ｣鬩･雍具ｽｸ・ｺ遯ｶ莨懶ｽｭ莨懈Β騾ｶ・ｸ闔蜻域・雎ｸ閧ｲ蝎ｪ陝ｷ・ｶ髯ｦ譴ｧ諠ｻ陋ｻ・ｶ遯ｶ蜻ｻ・ｼ迹堋蠕｡・ｸ閧ｴ蠑咲ｪｶ蛟ｩ・ｧ・｣鬩･鬆托ｽｯ豈費ｽｾ邇厄ｽｶ繝ｻ・ｿ繝ｻ謔・楜讚∵ｿ陜暦ｽｴ遯ｶ蜷ｶﾂ繝ｻ,
  "髴大姓・ｹ貅ｯ・ｧ・｣鬩･雍具ｽｺ繝ｻ・ｽ・ｰ騾ｶ・ｮ陷大ｴ趣ｽｧ繧・ｽｯ貅ｷ闃ｦ騾ｧ繝ｻ魘ｫ髮趣ｽ｡繝ｻ螢ｻ・ｻ蜿鳴・ｻ闖ｴ隰趣ｽｸ鬘疲★繝ｻ闌撒REM2 闕ｳ繝ｻChP 陞溘・・ｺ荳ｻ貂夊惺螟ｧ繝ｻ驍会ｽｻ繝ｻ譴ｧ蟲ｩ隰暦ｽ･髴大床・ｿ譎・ｾ・ｫ､・ｧ隰御ｹ滂ｽｼ轣倥・隲､・ｧ鬨ｾ螟奇ｽｷ・ｯ繝ｻ蟷・繝ｻP-Tau 陷･繝ｻA・趣ｽｲ 隴厄ｽｴ隰暦ｽ･髴大床・ｿ繝ｻ・ｿ蟷・ｽｮ・､驕擾ｽ･闖ｴ諠ｹ・ｸ迢怜飭隰ｾ・ｾ陞滂ｽｧ鬨ｾ螟奇ｽｷ・ｯ邵ｲ繧・ｽｸ蜿厄ｽｭ・､陷ｷ譴ｧ諷ｮ繝ｻ譴ｧ驥瑚閉隴√・骰ｵ隰蜊・ｽ､・ｺ P-Tau 闕ｳ繝ｻChP 陟閉陟閉闕ｳ・ｺ髮肴ｺｽ蠍瑚怦・ｳ繝ｻ蠕｡・ｸ繝ｻChP 闕ｳ謇具ｽｮ・､驕擾ｽ･闕ｵ貅ｷ蠎・惱驛・ｽｴ貅ｽ蠍瑚怦・ｳ繝ｻ謔溷ｱ剰ｱ・ｽ､陟也§辯墓沿謐ｺ譽｡闕ｳ蜿夜㈹陟閉謇域瑳譽｡闕ｵ遏ｩ鮴埼ｧ繝ｻ・ｷ・ｮ陟代ｑ・ｼ謔滂ｽｺ豈費ｽｼ莨懊・闔牙ｮ域・騾槭・莠ｳ隹ｿ・ｵ陟代ｊ・ｴ・ｨ隲､・ｧ邵ｲ竏ｬ・ｷ・ｯ陟輔・繝ｻ髦｡・ｽ隰ｨ莠･・ｺ豈費ｽｻ・･陷ｿ雍具ｽｸ讎企・髫ｶ・､驕擾ｽ･雋取・蠎城ｩ･蜀暦ｽｻ謐ｺ譯ｷ隴夲ｽ･騾・・・ｧ・｣繝ｻ迹堋蠕｡・ｸ閧ｴ蠑埼ｂﾂ陷頑・・ｧ繝ｻ・ｸ・ｺ陷・ｲ驕ｯ竏堋繝ｻ,
  "",
  safe_table(sem_compact),
  "",
  "### 5. 陋ｻ繝ｻ莠ｳ隹ｿ・ｵ SEM 謇域瑳譽｡",
  "",
  "闕ｳ邇厄ｽ｡・ｨ雎弱・ﾂ・ｻ闔繝ｻ繝ｻ闖ｴ阮卍・君邵ｲ・CI 陷･繝ｻAD 陜怜ｸ托ｽｸ・ｪ陞ｻ繧区島騾ｧ繝ｻSEM 髴搾ｽｯ陟輔・・ｻ謐ｺ譽｡邵ｲ・｢邵ｲ・懃ｸｲ窶ｦ 髴搾ｽｯ陟輔・骭倬ｧ繝ｻ`*` 髯ｦ・ｨ驕会ｽｺ p < 0.05繝ｻ蠖｢**` 髯ｦ・ｨ驕会ｽｺ p < 0.001繝ｻ蟶ｶ諤呵惺諠ｹ・ｸﾂ陋ｻ遉ｼ・ｻ蜷昴・闔繝ｻ・ｿ繝ｻ・ｿ蟶ｶ隴懆守夢繝ｻ鬩包ｽｮ髦｡・ｽ隰ｨ莠･・ｺ豕梧園陷茨ｽｶ闕ｳ・ｭ闔臥事・ｴ・｡霑ｪ・ｮ驍・・ﾂ繝ｻ,
  "",
  safe_table(sem_stage_display),
  "",
  "闔我ｸｻ繝ｻ鬮ｦ・ｶ隹ｿ・ｵ謇域瑳譽｡騾ｵ蜈ｷ・ｼ髱ｴhP-sTREM2-cognition 髴大綜謫・恪・ｯ陟輔・・ｹ・ｶ闕ｳ閧ｴ蠑崎舉・ｨ隰・隴幄崟莠ｳ隹ｿ・ｵ鬩幢ｽｽ陞ｳ謔溘・騾ｶ・ｸ陷ｷ蠕個繧按・ｻ闖ｴ謐ｺ・ｰ・ｷ隴幢ｽｬ闕ｳ・ｭ髫励ｇ・ｯ貅ｷ闃ｦ騾ｧ繝ｻ繝ｻ髦｡・ｽ隰ｨ莠･・ｺ豕鯉ｽｾ莠･蠎・妙・ｽ隴擾ｽｯ闕ｳ讎企・騾搾ｽｾ騾槭・莠ｳ隹ｿ・ｵ隰ｨ莠･・ｺ逍ｲ蟀ｿ陷ｷ蜿ｰ・ｸ荳ｻ・ｼ・ｺ陟趣ｽｦ陷ｿ・ｰ陷会｣ｰ陷ｷ螳亥飭謇域瑳譽｡繝ｻ蟶帛ｱ剰ｱ・ｽ､隰壽・・ｨ・ｿ隴鯉ｽｶ陟取ｳ鯉ｽｼ・ｺ髫ｹ繝ｻﾂ菫ｶﾂ・ｻ闖ｴ骰具ｽｻ謐ｺ譽｡ + 陋ｻ繝ｻ莠ｳ隹ｿ・ｵ謇域瑳譽｡遯ｶ蜷昴・陷ｷ迹夲ｽｧ・｣鬩･螂・ｽｼ迹堋蠕｡・ｸ蟠趣ｽｦ竏晏ｮｵ陟第・逡題ｫ､・ｻ闖ｴ繝ｻSEM邵ｲ繝ｻ,
  "",
  "### 6. 闕ｳ・ｭ闔牙玄隴懆取ｳ鯉ｽｷ・ｮ陟代ｈ・｣ﾂ鬯ｪ繝ｻ,
  "",
  "陜ｨ・ｨ騾ｶ・ｸ陷ｷ譴ｧ蝙馴ｫｴ・ｲ邵ｲ竏ｫ蠍瑚惺譴ｧ・ｽ諛ｷ蠎城ｩ･蜑ｰ・ｮ・､驕擾ｽ･謇育§・ｱﾂ陷･讙主ｶ瑚惺謔溷綾陷ｿ蛟ｬ纃ｼ隴夲ｽ｡闔会ｽｶ闕ｳ蜈ｷ・ｼ譴ｧ繝ｻ闔会ｽｬ髴大ｸ托ｽｸﾂ雎・ｽ･雎育｢托ｽｾ繝ｻ・ｺ繝ｻ・ｸ讎企・闕ｳ・ｭ闔臥洸ﾂ螟奇ｽｷ・ｯ騾ｧ繝ｻ鮴崎ｬ暦ｽ･隰ｨ莠･・ｺ豕鯉ｽｼ・ｺ陟趣ｽｦ邵ｲ繝ｻ,
  "",
  safe_table(sem_diff_table),
  "",
  "## 謇茨ｽｼ陷ｷ驛・ｽｧ・｣鬩･繝ｻ,
  "",
  "1. ChPICV 闕ｳ繝ｻsTREM2 陜ｨ・ｨ隲､・ｻ闖ｴ隰趣ｽｸ髮∽ｺ蛾◇・ｳ陋幢ｽ･髮肴ｺｽ蠍瑚怦・ｳ繝ｻ迹夲ｽｿ蜷ｩ・ｸﾂ陷茨ｽｳ驍会ｽｻ闕ｳ・ｻ髫補・・ｭ莨懈Β闔繝ｻCN 陷･繝ｻMCI 鬮ｦ・ｶ隹ｿ・ｵ繝ｻ譴ｧ鄂ｲ驕会ｽｺ陷茨ｽｶ隴厄ｽｴ陷ｿ・ｯ髢ｭ・ｽ陷ｿ閧ｴ荳宣搾ｽｾ騾槭・謗隴帶ｻ薙・陷大涵・ｩ・ｱ隴帶ｺｽ蝎ｪ髯ｦ・･陋幢ｽｿ隲､・ｧ/陷ｿ讎奇ｽｺ逍ｲﾂ・ｧ霓､螳郁｣ｸ髴代・・ｨ荵敖繝ｻ,
  "2. P-Tau 陷･繝ｻA・趣ｽｲ 闕ｳ繝ｻChP-髫ｶ・､驕擾ｽ･髴難ｽｴ騾ｧ繝ｻ繝ｻ驍会ｽｻ隴厄ｽｴ隰暦ｽ･髴大床・ｿ繝ｻ・ｿ蟶ｶﾂ・ｧ騾槭・轤企ｨｾ螟奇ｽｷ・ｯ繝ｻ謔滂ｽｰ・､陷茨ｽｶ A・趣ｽｲ 髴搾ｽｯ陟輔・諠 latent SEM 闕ｳ・ｭ隴崢驕橸ｽｳ陞ｳ螟ｲ・ｼ迹堋繝ｻP-Tau 髴搾ｽｯ陟輔・繝ｻ隰蜊・ｽ､・ｺ陝・ｼ懈Β隴厄ｽｴ陞溯ざ謠ｩ騾ｧ繝ｻ莠ｳ隹ｿ・ｵ隲､・ｧ闕ｳ譛ｱ繝ｻ髦｡・ｽ隰ｨ莠･・ｺ譁青繝ｻ,
  "3. sTREM2 騾ｧ繝ｻﾂ螟奇ｽｷ・ｯ隴厄ｽｴ陷剃ｹ怜ｼ崎舉・ｨ鬩幢ｽｨ陋ｻ繝ｻ諷｣雎ｸ繝ｻChP 陝・ｽｹ髫ｶ・､驕擾ｽ･陷画ｺｯ繝ｻ騾ｧ繝ｻ・ｸ讎願懸陟厄ｽｱ陷ｩ謳ｾ・ｼ迹堋遒∵直陷頑・・ｺ・ｯ陷会｣ｰ鬩･蜥ｲ鄒・・・・ｼ蟷・ｽｿ蜥上○陜ｨ・ｨ陟也§辯戊ｬｨ・ｰ隰撰ｽｮ鬩･迹夲ｽ｡・ｨ驍・ｽｰ闕ｳ・ｺ闕ｳ蝣ｺ・ｸﾂ髢ｾ・ｴ闕ｳ・ｭ闔我ｹ敖繝ｻ,
  "4. P-Tau 闕ｳ繝ｻChP 騾ｧ繝ｻ繝ｻ驍会ｽｻ髯鯉ｽｽ霎滂ｽｶ陜ｨ・ｨ隴鯉ｽ｢陟閉驕千坩・ｩ・ｶ闕ｳ・ｭ陝ｶ・ｸ髯ｲ・ｫ隰蜑ｰ・ｿ・ｰ闕ｳ・ｺ髮肴ｺｷ鬮・・蠕｡・ｽ繝ｻ諠隴幢ｽｬ驕千坩・ｩ・ｶ闕ｳ・ｭ陷茨ｽｶ陝・ｽｹ髫ｶ・､驕擾ｽ･鬨ｾ螟奇ｽｷ・ｯ騾ｧ繝ｻ・ｧ螳夂横闕ｳ蟠弱・陷ｿ・ｪ騾包ｽｱ驍つ陷頑・蠍瑚怦・ｳ驍会ｽｻ隰ｨ・ｰ陞ｳ螢ｻ・ｹ莨夲ｽｼ譴ｧ蟲ｩ陟守坩・ｻ轣倡ｲ矩ｶ・ｴ隰暦ｽ･隰ｨ莠･・ｺ譁青繝ｻ鮴崎ｬ暦ｽ･隰ｨ莠･・ｺ豕梧浤陋ｻ繝ｻ莠ｳ隹ｿ・ｵ謇域瑳譽｡謇茨ｽｼ陷ｷ莠･諢幄ｭ・ｽｭ邵ｲ繝ｻ,
  "5. 髣搾ｽ･闔会ｽ･ IF > 8 隴帶ｺｷ繝ｻ隰壽・・ｨ・ｿ闕ｳ・ｺ騾ｶ・ｮ隴ｬ繝ｻ・ｼ謔滂ｽｽ轣倡√隴崢鬨ｾ繧・ｲ矩ｧ繝ｻ・ｸ・ｻ謇域瑳譽｡謇域瑳譯ｷ隴擾ｽｯ繝ｻ螢ｽﾂ・ｻ闖ｴ轣伜ｱ楢悶・+ biomarker-adjusted model + latent SEM + 陋ｻ繝ｻ莠ｳ隹ｿ・ｵ SEM繝ｻ蟷・蠕｡・ｺ・､闔雋樊浤鬮ｱ讓抵ｽｺ・ｿ隲､・ｧ謇域瑳譽｡隴厄ｽｴ鬨ｾ繧・ｲ玖抄諛会ｽｸ・ｺ隰ｾ・ｯ隰問扱ﾂ・ｧ/隰暦ｽ｢驍擾ｽ｢隲､・ｧ陋ｻ繝ｻ譴ｵ邵ｲ繝ｻ,
  ""
)

writeLines(journal_lines, con = journal_report_path, useBytes = TRUE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "09_generate_journal_style_report_v2",
  output_files = journal_report_path,
  note = "Generated a clean Chinese journal-style integrated report with stage-stratified SEM interpretation and mediator-pattern synthesis.",
  summary_dir = result_summary_dir
)
