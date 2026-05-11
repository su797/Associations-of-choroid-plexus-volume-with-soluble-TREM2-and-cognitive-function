source(file.path(getwd(), "00_setup.R"))

analysis_data <- read_project_data(analysis_data_path)
normality_table <- read_result_or_empty(normality_table_path)
conversion_log <- read_result_or_empty(conversion_log_path)
run_metadata <- read_result_or_empty(result_run_metadata_path)
variable_dictionary <- read_result_or_empty(variable_dictionary_path)
descriptive_overall_cont <- read_result_or_empty(file.path(result_summary_dir, "descriptive_overall_continuous.csv"))
descriptive_overall_cat <- read_result_or_empty(file.path(result_summary_dir, "descriptive_overall_categorical.csv"))
descriptive_group_cont <- read_result_or_empty(file.path(result_summary_dir, "descriptive_by_group_continuous.csv"))
descriptive_group_cat <- read_result_or_empty(file.path(result_summary_dir, "descriptive_by_group_categorical.csv"))
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
tau_linear_overall <- read_result_or_empty(file.path(result_summary_dir, "tau_linear_overall.csv"))
tau_linear_by_group <- read_result_or_empty(file.path(result_summary_dir, "tau_linear_by_group.csv"))
tau_partial_overall <- read_result_or_empty(file.path(result_summary_dir, "tau_partial_overall.csv"))
tau_partial_by_group <- read_result_or_empty(file.path(result_summary_dir, "tau_partial_by_group.csv"))
ptau_linear_overall <- read_result_or_empty(file.path(result_summary_dir, "ptau_linear_overall.csv"))
ptau_linear_by_group <- read_result_or_empty(file.path(result_summary_dir, "ptau_linear_by_group.csv"))
ptau_partial_overall <- read_result_or_empty(file.path(result_summary_dir, "ptau_partial_overall.csv"))
ptau_partial_by_group <- read_result_or_empty(file.path(result_summary_dir, "ptau_partial_by_group.csv"))
advanced_biomarker_overall <- read_result_or_empty(file.path(result_summary_dir, "advanced_biomarker_adjusted_overall.csv"))
advanced_biomarker_by_group <- read_result_or_empty(file.path(result_summary_dir, "advanced_biomarker_adjusted_by_group.csv"))
advanced_interaction_terms <- read_result_or_empty(file.path(result_summary_dir, "advanced_interaction_terms.csv"))
advanced_interaction_comparison <- read_result_or_empty(file.path(result_summary_dir, "advanced_interaction_comparison.csv"))
advanced_nonlinear_summary <- read_result_or_empty(file.path(result_summary_dir, "advanced_nonlinear_model_summary.csv"))
advanced_nonlinear_tests <- read_result_or_empty(file.path(result_summary_dir, "advanced_nonlinear_tests.csv"))
abeta_truncation_summary <- read_result_or_empty(file.path(result_summary_dir, "abeta_truncation_summary.csv"))
abeta_truncation_regression <- read_result_or_empty(file.path(result_summary_dir, "abeta_truncation_regression_sensitivity.csv"))
abeta_truncation_partial <- read_result_or_empty(file.path(result_summary_dir, "abeta_truncation_partial_sensitivity.csv"))
sem_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_mediation_summary.csv"))
sem_paths <- read_result_or_empty(file.path(result_summary_dir, "sem_path_coefficients.csv"))
sem_fit <- read_result_or_empty(file.path(result_summary_dir, "sem_model_fit.csv"))
sem_loadings <- read_result_or_empty(file.path(result_summary_dir, "sem_factor_loadings.csv"))
sem_modification_indices <- read_result_or_empty(file.path(result_summary_dir, "sem_modification_indices_top.csv"))
sem_standardization_sensitivity <- read_result_or_empty(file.path(result_summary_dir, "sem_standardization_sensitivity.csv"))
sem_indirect_difference_tests <- read_result_or_empty(file.path(result_summary_dir, "sem_indirect_difference_tests.csv"))
sem_parallel_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_parallel_mediation_summary.csv"))
sem_parallel_fit <- read_result_or_empty(file.path(result_summary_dir, "sem_parallel_model_fit.csv"))
sem_parallel_multigroup_tests <- read_result_or_empty(file.path(result_summary_dir, "sem_parallel_multigroup_tests.csv"))
sem_moderated_mediation_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_moderated_mediation_summary.csv"))
sem_stage_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_stage_direction_summary.csv"))
sem_multigroup_fit <- read_result_or_empty(file.path(result_summary_dir, "sem_multigroup_model_fit.csv"))
sem_multigroup_paths <- read_result_or_empty(file.path(result_summary_dir, "sem_multigroup_group_estimates.csv"))
sem_multigroup_tests <- read_result_or_empty(file.path(result_summary_dir, "sem_multigroup_path_tests.csv"))
sem_mechanism_interpretation <- read_result_or_empty(file.path(result_summary_dir, "sem_mechanism_interpretation.csv"))
analysis_log <- read_result_or_empty(file.path(result_summary_dir, "analysis_log.csv"))

tr <- function(key, values = list()) {
  translate_text(locale_bundle, report_language, key, values = values)
}

empty_md_text <- paste0("_", tr("text_no_data"), "_")
empty_file_text <- paste0("- _", tr("text_no_files"), "_")
group_order <- unlist(runtime_settings$group_order %||% c("CN", "MCI", "AD"), use.names = FALSE)
date_time_format <- runtime_settings$report$date_time_format %||% "%Y-%m-%d %H:%M:%S"

var_label <- function(variable_name) {
  resolve_variable_label(variable_name, runtime_settings, report_language, fallback = variable_name)
}

group_label <- function(group_name) {
  resolve_level_label(project_config$variables$group_label_var, group_name, runtime_settings, report_language, fallback = group_name)
}

level_label <- function(variable_name, level_value) {
  resolve_level_label(variable_name, level_value, runtime_settings, report_language, fallback = as.character(level_value))
}

extract_p_value <- function(row) {
  if ("p.value" %in% names(row)) {
    return(row[["p.value"]])
  }
  if ("p_value" %in% names(row)) {
    return(row[["p_value"]])
  }
  NA_real_
}

extract_path_value <- function(path_data, path_name, value_col = "estimate") {
  if (nrow(path_data) == 0) {
    return(NA_real_)
  }
  out <- path_data[path_data$path == path_name, value_col, drop = TRUE]
  if (length(out) == 0) {
    return(NA_real_)
  }
  out[[1]]
}

format_descriptive_categorical <- function(data, grouped = FALSE) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  split_keys <- if (grouped) {
    paste(data$group, data$variable, sep = "||")
  } else {
    data$variable
  }

  groups <- split(data, split_keys)
  vapply(groups, function(df) {
    pieces <- vapply(seq_len(nrow(df)), function(idx) {
      paste(level_label(df$variable[[idx]], df$level[[idx]]), df$summary_text[[idx]])
    }, FUN.VALUE = character(1))

    variable_name <- var_label(df$variable[[1]])
    if (grouped) {
      tr(
        "text_descriptive_cat_group",
        list(
          group = group_label(df$group[[1]]),
          variable = variable_name,
          value = paste(pieces, collapse = "； ")
        )
      )
    } else {
      tr(
        "text_descriptive_cat_overall",
        list(
          variable = variable_name,
          value = paste(pieces, collapse = "； ")
        )
      )
    }
  }, FUN.VALUE = character(1))
}

format_descriptive_continuous <- function(data, grouped = FALSE) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  vapply(seq_len(nrow(data)), function(idx) {
    row <- data[idx, , drop = FALSE]
    if (grouped) {
      tr(
        "text_descriptive_cont_group",
        list(
          group = group_label(row$group[[1]]),
          variable = var_label(row$variable[[1]]),
          value = row$summary_text[[1]],
          n = row$n[[1]]
        )
      )
    } else {
      tr(
        "text_descriptive_cont_overall",
        list(
          variable = var_label(row$variable[[1]]),
          value = row$summary_text[[1]],
          n = row$n[[1]]
        )
      )
    }
  }, FUN.VALUE = character(1))
}

format_group_result_sentences <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  data <- data[order(data$p_value), , drop = FALSE]
  vapply(seq_len(nrow(data)), function(idx) {
    tr(
      "text_group_result",
      list(
        variable = var_label(data$variable[[idx]]),
        p = format_p_value_human(data$p_value[[idx]], digits = report_digits)
      )
    )
  }, FUN.VALUE = character(1))
}

format_linear_sentences <- function(overall_data, by_group_data) {
  sentences <- character(0)

  if (nrow(overall_data) > 0) {
    for (pair_cfg in project_config$chp_strem2$linear_pairs) {
      row <- overall_data[overall_data$model_name == pair_cfg$name, , drop = FALSE]
      if (nrow(row) == 0) {
        next
      }
      sentences <- c(
        sentences,
        tr(
          "text_linear_overall",
          list(
            outcome = var_label(row$outcome[[1]]),
            exposure = var_label(row$exposure[[1]]),
            beta = format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
            p = format_p_value_human(extract_p_value(row), digits = report_digits)
          )
        )
      )
    }
  }

  if (nrow(by_group_data) > 0) {
    by_group_data <- order_by_group(by_group_data, "group", group_order = group_order)
    for (pair_cfg in project_config$chp_strem2$linear_pairs) {
      pair_rows <- by_group_data[by_group_data$model_name == pair_cfg$name, , drop = FALSE]
      if (nrow(pair_rows) == 0) {
        next
      }
      pair_rows <- pair_rows[pair_rows$term == "exposure", , drop = FALSE]
      for (idx in seq_len(nrow(pair_rows))) {
        row <- pair_rows[idx, , drop = FALSE]
        sentences <- c(
          sentences,
          tr(
            "text_linear_group",
            list(
              group = group_label(row$group[[1]]),
              outcome = var_label(row$outcome[[1]]),
              exposure = var_label(row$exposure[[1]]),
              beta = format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
              p = format_p_value_human(extract_p_value(row), digits = report_digits)
            )
          )
        )
      }
    }
  }

  sentences
}

format_partial_sentences <- function(overall_data, by_group_data) {
  sentences <- character(0)

  if (nrow(overall_data) > 0) {
    for (idx in seq_len(nrow(overall_data))) {
      row <- overall_data[idx, , drop = FALSE]
      sentences <- c(
        sentences,
        tr(
          "text_partial_overall",
          list(
            x = var_label(row$x[[1]]),
            y = var_label(row$y[[1]]),
            estimate = format_numeric_human(row$estimate[[1]], digits = report_digits),
            p = format_p_value_human(row$p_value[[1]], digits = report_digits)
          )
        )
      )
    }
  }

  if (nrow(by_group_data) > 0) {
    by_group_data <- order_by_group(by_group_data, "group", group_order = group_order)
    for (idx in seq_len(nrow(by_group_data))) {
      row <- by_group_data[idx, , drop = FALSE]
      sentences <- c(
        sentences,
        tr(
          "text_partial_group",
          list(
            group = group_label(row$group[[1]]),
            x = var_label(row$x[[1]]),
            y = var_label(row$y[[1]]),
            estimate = format_numeric_human(row$estimate[[1]], digits = report_digits),
            p = format_p_value_human(row$p_value[[1]], digits = report_digits)
          )
        )
      )
    }
  }

  sentences
}

format_biomarker_adjusted_sentences <- function(overall_data, by_group_data) {
  if (nrow(overall_data) == 0) {
    return(character(0))
  }

  sentences <- character(0)
  model_names <- unique(overall_data$model_name)
  for (model_name in model_names) {
    model_rows <- overall_data[overall_data$model_name == model_name, , drop = FALSE]
    exposure_row <- model_rows[model_rows$term == "exposure", , drop = FALSE]
    biomarker_rows <- model_rows[model_rows$term %in% c("PTAU", "TAU", "S_ABETA"), , drop = FALSE]
    if (nrow(exposure_row) > 0) {
      biomarker_text <- paste(
        vapply(seq_len(nrow(biomarker_rows)), function(idx) {
          row <- biomarker_rows[idx, , drop = FALSE]
          paste0(
            var_label(row$term_label[[1]]),
            " β = ",
            format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
            "，p = ",
            format_p_value_human(extract_p_value(row), digits = report_digits)
          )
        }, FUN.VALUE = character(1)),
        collapse = "； "
      )
      sentences <- c(
        sentences,
        paste0(
          "在全体样本中，将 ",
          paste(vapply(model_rows$term[model_rows$term %in% c("PTAU", "TAU", "S_ABETA")], var_label, FUN.VALUE = character(1)), collapse = "、"),
          " 纳入同一模型后，",
          var_label(exposure_row$outcome[[1]]),
          " 与 ",
          var_label(exposure_row$exposure[[1]]),
          " 的关联 β = ",
          format_beta_human(exposure_row$estimate[[1]], exposure_row$std_beta[[1]], digits = report_digits),
          "，p = ",
          format_p_value_human(extract_p_value(exposure_row), digits = report_digits),
          "。其余生物标志物参数为：",
          biomarker_text,
          "。"
        )
      )
    }

    if (nrow(by_group_data) > 0) {
      model_group_rows <- by_group_data[by_group_data$model_name == model_name & by_group_data$term == "exposure", , drop = FALSE]
      model_group_rows <- order_by_group(model_group_rows, "group", group_order = group_order)
      if (nrow(model_group_rows) > 0) {
        group_text <- paste(
          vapply(seq_len(nrow(model_group_rows)), function(idx) {
            row <- model_group_rows[idx, , drop = FALSE]
            paste0(
              group_label(row$group[[1]]),
              "组 β = ",
              format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
              "，p = ",
              format_p_value_human(extract_p_value(row), digits = report_digits)
            )
          }, FUN.VALUE = character(1)),
          collapse = "； "
        )
        sentences <- c(sentences, paste0("分组分析中，", group_text, "。"))
      }
    }
  }
  sentences
}

format_interaction_sentences <- function(comparison_data, term_data) {
  if (nrow(comparison_data) == 0) {
    return(character(0))
  }

  vapply(seq_len(nrow(comparison_data)), function(idx) {
    row <- comparison_data[idx, , drop = FALSE]
    model_terms <- term_data[term_data$model_name == row$model_name[[1]] & grepl("^exposure:diagnosis", term_data$term), , drop = FALSE]
    model_terms <- model_terms[order(model_terms$term), , drop = FALSE]
    term_text <- if (nrow(model_terms) > 0) {
      paste(
        vapply(seq_len(nrow(model_terms)), function(jdx) {
          term_row <- model_terms[jdx, , drop = FALSE]
          paste0(
            term_row$term_label[[1]],
            " 的交互系数 β = ",
            format_beta_human(term_row$estimate[[1]], term_row$std_beta[[1]], digits = report_digits),
            "，p = ",
            format_p_value_human(extract_p_value(term_row), digits = report_digits)
          )
        }, FUN.VALUE = character(1)),
        collapse = "； "
      )
    } else {
      "未提取到可解释的交互项系数"
    }

    paste0(
      "在 ",
      var_label(row$outcome[[1]]),
      " ~ ",
      var_label(row$exposure[[1]]),
      " × 诊断组 的交互模型中，整体交互检验 p = ",
      format_p_value_human(row$interaction_p_value[[1]], digits = report_digits),
      "，说明 ",
      var_label(row$exposure[[1]]),
      " 与诊断阶段之间",
      if (!is.na(row$interaction_p_value[[1]]) && row$interaction_p_value[[1]] < 0.05) "存在" else "未见",
      "统计学上的阶段依赖效应。具体交互项结果：",
      term_text,
      "。"
    )
  }, FUN.VALUE = character(1))
}

format_nonlinear_sentences <- function(summary_data, test_data) {
  if (nrow(summary_data) == 0) {
    return(character(0))
  }

  model_names <- unique(summary_data$model_name)
  vapply(model_names, function(model_name) {
    model_summary <- summary_data[summary_data$model_name == model_name, , drop = FALSE]
    test_rows <- test_data[test_data$model_name == model_name, , drop = FALSE]
    best_row <- model_summary[which.min(model_summary$aic), , drop = FALSE]
    quad_p <- test_rows$p_value[test_rows$test_name == "quadratic_term"]
    spline_p <- test_rows$p_value[test_rows$test_name == "linear_vs_spline"]
    paste0(
      "对 ",
      var_label(best_row$outcome[[1]]),
      " 与 ",
      var_label(best_row$exposure[[1]]),
      " 的关系进一步进行非线性检验后，AIC 最优模型为 ",
      best_row$model_type[[1]],
      " 模型（AIC = ",
      format_numeric_human(best_row$aic[[1]], digits = report_digits),
      "）。二次项检验 p = ",
      format_p_value_human(quad_p[[1]], digits = report_digits),
      "，样条模型相对于线性模型的比较 p = ",
      format_p_value_human(spline_p[[1]], digits = report_digits),
      "。"
    )
  }, FUN.VALUE = character(1))
}

format_abeta_truncation_sentences <- function(summary_data, regression_data, partial_data) {
  if (nrow(summary_data) == 0) {
    return(character(0))
  }

  sentences <- character(0)
  full_overall <- summary_data[summary_data$sample_set == "full" & summary_data$group == "Overall", , drop = FALSE]
  restricted_overall <- summary_data[summary_data$sample_set == "restricted_no_ge_1700" & summary_data$group == "Overall", , drop = FALSE]

  if (nrow(full_overall) > 0 && nrow(restricted_overall) > 0) {
    removed_n <- full_overall$n_ceiling[[1]]
    removed_pct <- if (!is.na(full_overall$proportion_ceiling[[1]])) full_overall$proportion_ceiling[[1]] * 100 else NA_real_
    if (report_language == "en") {
      sentences <- c(
        sentences,
        paste0(
          "Because ADNI CSF Aβ values at or above ", full_overall$threshold[[1]],
          " pg/mL were truncated at the assay ceiling, a sensitivity analysis excluding ceiling observations was conducted. ",
          removed_n, " participants (", format_numeric_human(removed_pct, digits = 1),
          "%) were removed from the full sample."
        )
      )
    } else if (report_language == "ja") {
      sentences <- c(
        sentences,
        paste0(
          "ADNI の CSF Aβ は ", full_overall$threshold[[1]],
          " pg/mL 以上で測定上限に丸め込まれているため、上限到達例を除外した感度分析を追加した。全体では ",
          removed_n, " 例（", format_numeric_human(removed_pct, digits = 1), "%）が ceiling case であった。"
        )
      )
    } else {
      sentences <- c(
        sentences,
        paste0(
          "由于 ADNI 的 CSF Aβ 在 ", full_overall$threshold[[1]],
          " pg/mL 及以上被统一截断为测定上限，本研究进一步进行了去除 ceiling case 的敏感性分析。全样本中共有 ",
          removed_n, " 例（", format_numeric_human(removed_pct, digits = 1), "%）属于上限截断值。"
        )
      )
    }
  }

  if (nrow(regression_data) > 0) {
    target_rows <- regression_data[
      regression_data$model_name %in% c("ChPICV_on_ABETA", "ABETA_on_ChPICV") &
        regression_data$term == "exposure" &
        regression_data$group == "Overall",
      ,
      drop = FALSE
    ]
    if (nrow(target_rows) > 0) {
      model_order <- c("ChPICV_on_ABETA", "ABETA_on_ChPICV")
      target_rows <- target_rows[match(model_order, target_rows$model_name), , drop = FALSE]
      target_rows <- target_rows[!is.na(target_rows$model_name), , drop = FALSE]
      for (model_name in unique(target_rows$model_name)) {
        full_row <- target_rows[target_rows$model_name == model_name & target_rows$sample_set == "full", , drop = FALSE]
        restricted_row <- target_rows[target_rows$model_name == model_name & target_rows$sample_set == "restricted_no_ge_1700", , drop = FALSE]
        if (nrow(full_row) == 0 || nrow(restricted_row) == 0) {
          next
        }
        if (report_language == "en") {
          sentences <- c(
            sentences,
            paste0(
              "For ", var_label(full_row$outcome[[1]]), " and ", var_label(full_row$exposure[[1]]),
              ", the full-sample estimate was ", format_beta_human(full_row$estimate[[1]], full_row$std_beta[[1]], digits = report_digits),
              " (", format_p_value_human(extract_p_value(full_row), digits = report_digits), "), whereas the ceiling-excluded estimate was ",
              format_beta_human(restricted_row$estimate[[1]], restricted_row$std_beta[[1]], digits = report_digits),
              " (", format_p_value_human(extract_p_value(restricted_row), digits = report_digits), ")."
            )
          )
        } else if (report_language == "ja") {
          sentences <- c(
            sentences,
            paste0(
              var_label(full_row$outcome[[1]]), " と ", var_label(full_row$exposure[[1]]),
              " の関連は、全例解析で ", format_beta_human(full_row$estimate[[1]], full_row$std_beta[[1]], digits = report_digits),
              "（", format_p_value_human(extract_p_value(full_row), digits = report_digits), "）であり、ceiling case 除外後は ",
              format_beta_human(restricted_row$estimate[[1]], restricted_row$std_beta[[1]], digits = report_digits),
              "（", format_p_value_human(extract_p_value(restricted_row), digits = report_digits), "）であった。"
            )
          )
        } else {
          sentences <- c(
            sentences,
            paste0(
              var_label(full_row$outcome[[1]]), " 与 ", var_label(full_row$exposure[[1]]),
              " 的关联在全样本中为 ", format_beta_human(full_row$estimate[[1]], full_row$std_beta[[1]], digits = report_digits),
              "（", format_p_value_human(extract_p_value(full_row), digits = report_digits), "），去除 ceiling case 后为 ",
              format_beta_human(restricted_row$estimate[[1]], restricted_row$std_beta[[1]], digits = report_digits),
              "（", format_p_value_human(extract_p_value(restricted_row), digits = report_digits), "）。"
            )
          )
        }
      }
    }
  }

  if (nrow(partial_data) > 0) {
    full_partial <- partial_data[partial_data$sample_set == "full" & partial_data$group == "Overall", , drop = FALSE]
    restricted_partial <- partial_data[partial_data$sample_set == "restricted_no_ge_1700" & partial_data$group == "Overall", , drop = FALSE]
    if (nrow(full_partial) > 0 && nrow(restricted_partial) > 0) {
      if (report_language == "en") {
        sentences <- c(
          sentences,
          paste0(
            "The adjusted partial correlation between ChP/ICV and Aβ was ",
            format_numeric_human(full_partial$estimate[[1]], digits = report_digits),
            " (", format_p_value_human(full_partial$p_value[[1]], digits = report_digits),
            ") in the full sample and ",
            format_numeric_human(restricted_partial$estimate[[1]], digits = report_digits),
            " (", format_p_value_human(restricted_partial$p_value[[1]], digits = report_digits),
            ") after excluding ceiling observations."
          )
        )
      } else if (report_language == "ja") {
        sentences <- c(
          sentences,
          paste0(
            "ChP/ICV と Aβ の調整偏相関は、全例解析で ",
            format_numeric_human(full_partial$estimate[[1]], digits = report_digits),
            "（", format_p_value_human(full_partial$p_value[[1]], digits = report_digits),
            "）、ceiling case 除外後で ",
            format_numeric_human(restricted_partial$estimate[[1]], digits = report_digits),
            "（", format_p_value_human(restricted_partial$p_value[[1]], digits = report_digits), "）であった。"
          )
        )
      } else {
        sentences <- c(
          sentences,
          paste0(
            "ChP/ICV 与 Aβ 的协变量调整偏相关在全样本中为 ",
            format_numeric_human(full_partial$estimate[[1]], digits = report_digits),
            "（", format_p_value_human(full_partial$p_value[[1]], digits = report_digits),
            "），去除 ceiling case 后为 ",
            format_numeric_human(restricted_partial$estimate[[1]], digits = report_digits),
            "（", format_p_value_human(restricted_partial$p_value[[1]], digits = report_digits), "）。"
          )
        )
      }
    }
  }

  sentences
}

format_fit_metric <- function(value, digits = 3) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("NA")
  }
  sprintf(paste0("%.", digits, "f"), round(value, digits))
}

describe_mediation_pattern <- function(indirect_p, opposite_direction) {
  significant_indirect <- !is.na(indirect_p) && indirect_p < 0.05
  if (!significant_indirect) {
    if (report_language == "en") {
      return("no significant mediation (the indirect effect did not reach statistical significance)")
    }
    if (report_language == "ja") {
      return("有意な媒介効果なし（間接効果は統計学的有意に達しなかった）")
    }
    return("无显著中介效应（间接效应未达到统计学显著）")
  }

  if (isTRUE(opposite_direction)) {
    if (report_language == "en") {
      return("inconsistent mediation (suppression effect; the indirect and direct effects point in opposite directions)")
    }
    if (report_language == "ja") {
      return("inconsistent mediation（抑制効果；間接効果と直接効果の方向が逆）")
    }
    return("inconsistent mediation（遮蔽效应；间接效应与直接效应方向相反）")
  }

  if (report_language == "en") {
    return("consistent mediation (facilitative mediation; the indirect and direct effects point in the same direction)")
  }
  if (report_language == "ja") {
    return("consistent mediation（促進的媒介；間接効果と直接効果の方向が一致）")
  }
  "consistent mediation（促进性中介；间接效应与直接效应方向一致）"
}

build_mi_summary_text <- function(mi_row) {
  if (nrow(mi_row) == 0) {
    if (report_language == "ja") {
      return("当前模型未输出可用的 modification indices。")
    }
    if (report_language == "en") {
      return("No usable modification index was reported for the current model.")
    }
    return("当前模型未输出可用的 modification indices。")
  }

  top_row <- mi_row[order(-mi_row$mi), , drop = FALSE][1, , drop = FALSE]
  relation_text <- paste(top_row$lhs[[1]], top_row$op[[1]], top_row$rhs[[1]])

  if (report_language == "ja") {
    return(paste0(
      "当前模型中最大的 modification index 为 ",
      relation_text,
      "（MI = ", format_fit_metric(top_row$mi[[1]]), "）。"
    ))
  }
  if (report_language == "en") {
    return(paste0(
      "The largest modification index in the current model is ",
      relation_text,
      " (MI = ", format_fit_metric(top_row$mi[[1]]), ")."
    ))
  }
  paste0(
    "当前模型中最大的 modification index 为 ",
    relation_text,
    "（MI = ", format_fit_metric(top_row$mi[[1]]), "）。"
  )
}

localize_sem_text <- function(text) {
  if (length(text) == 0 || is.null(text) || is.na(text)) {
    return(text)
  }

  if (report_language == "en") {
    return(text)
  }

  mapping_zh <- c(
    "Global fit is good." = "模型整体拟合良好。",
    "Global fit is acceptable but not ideal." = "模型整体拟合尚可，但并不理想。",
    "Global fit is weak." = "模型整体拟合较弱。",
    "CFI/TLI >= 0.95 and RMSEA/SRMR are within common recommended thresholds." = "CFI/TLI 达到或超过 0.95，且 RMSEA/SRMR 位于常用推荐阈值范围内。",
    "The model passes relaxed practical thresholds, but the measurement structure should still be interpreted cautiously." = "模型达到了相对宽松的实践阈值，但测量结构仍需谨慎解释。",
    "At least one of CFI/TLI/RMSEA/SRMR falls outside common acceptable thresholds; consider revising indicators or model structure." = "CFI、TLI、RMSEA、SRMR 中至少有一项超出常用可接受阈值，建议进一步调整指标或模型结构。",
    "Model estimated successfully, but one or more global fit indices are unavailable or not informative." = "模型估计成功，但一个或多个整体拟合指标不可用或信息量有限。",
    "This usually occurs when the model is close to just-identified or when fit indices cannot be stably computed from the available indicator structure." = "这通常发生在模型接近刚好识别，或现有指标结构无法稳定计算整体拟合指标时。",
    "Residual covariance between the two mPACC indicators was added because they belong to the same neuropsychological composite." = "由于两个 mPACC 指标属于同一神经心理复合测验，因此加入了它们之间的残差相关。",
    "Residual covariance between MMSE and MoCA was added because both reflect closely related global cognitive screening performance." = "由于 MMSE 与 MoCA 都反映相近的整体认知筛查表现，因此加入了两者之间的残差相关。",
    "Residual covariances were added within the screening pair and within the mPACC pair to improve measurement-model stability while preserving the substantive structure." = "为在保留理论结构的前提下提升测量模型稳定性，在筛查指标对和 mPACC 指标对内部加入了残差相关。",
    "No additional measurement-model adjustment was configured." = "当前未配置额外的测量模型修正。",
    "Model-specific measurement adjustment was applied." = "已应用该模型专属的测量模型修正。"
  )

  mapping_ja <- c(
    "Global fit is good." = "モデル全体の適合は良好です。",
    "Global fit is acceptable but not ideal." = "モデル全体の適合は許容範囲ですが、理想的ではありません。",
    "Global fit is weak." = "モデル全体の適合は弱いです。",
    "CFI/TLI >= 0.95 and RMSEA/SRMR are within common recommended thresholds." = "CFI/TLI は 0.95 以上で、RMSEA/SRMR も一般的な推奨閾値内です。",
    "The model passes relaxed practical thresholds, but the measurement structure should still be interpreted cautiously." = "モデルはやや緩い実務的閾値を満たしていますが、測定構造の解釈には注意が必要です。",
    "At least one of CFI/TLI/RMSEA/SRMR falls outside common acceptable thresholds; consider revising indicators or model structure." = "CFI/TLI/RMSEA/SRMR の少なくとも 1 つが一般的な許容閾値を外れており、指標またはモデル構造の見直しが推奨されます。",
    "Model estimated successfully, but one or more global fit indices are unavailable or not informative." = "モデル推定は成功しましたが、1 つ以上の全体適合指標が利用できないか、情報量が限定的です。",
    "This usually occurs when the model is close to just-identified or when fit indices cannot be stably computed from the available indicator structure." = "これは、モデルがちょうど識別に近い場合や、現在の指標構造では適合指標を安定して計算できない場合によく見られます。",
    "Residual covariance between the two mPACC indicators was added because they belong to the same neuropsychological composite." = "2 つの mPACC 指標は同じ神経心理学的複合指標に属するため、その残差共分散を追加しました。",
    "Residual covariance between MMSE and MoCA was added because both reflect closely related global cognitive screening performance." = "MMSE と MoCA は近い全般認知スクリーニング機能を反映するため、両者の残差共分散を追加しました。",
    "Residual covariances were added within the screening pair and within the mPACC pair to improve measurement-model stability while preserving the substantive structure." = "理論構造を保ちながら測定モデルの安定性を高めるため、スクリーニング指標対と mPACC 指標対の内部に残差共分散を追加しました。",
    "No additional measurement-model adjustment was configured." = "追加の測定モデル調整は設定されていません。",
    "Model-specific measurement adjustment was applied." = "このモデル専用の測定モデル調整を適用しました。"
  )

  mapping <- if (report_language == "ja") mapping_ja else mapping_zh
  if (!is.null(mapping[[text]])) {
    return(unname(mapping[[text]]))
  }
  text
}

localize_sem_fit_table <- function(fit_row) {
  if (nrow(fit_row) == 0) {
    return(fit_row)
  }

  localized <- fit_row
  if ("model_reasonable" %in% names(localized)) {
    localized$model_reasonable <- vapply(localized$model_reasonable, localize_sem_text, FUN.VALUE = character(1))
  }
  if ("fit_note" %in% names(localized)) {
    localized$fit_note <- vapply(localized$fit_note, localize_sem_text, FUN.VALUE = character(1))
  }
  if ("optimization_note" %in% names(localized)) {
    localized$optimization_note <- vapply(localized$optimization_note, localize_sem_text, FUN.VALUE = character(1))
  }
  localized
}

localize_sem_sensitivity_table <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  localized <- data
  for (col_name in c("primary_model_reasonable", "sensitivity_model_reasonable")) {
    if (col_name %in% names(localized)) {
      localized[[col_name]] <- vapply(localized[[col_name]], localize_sem_text, FUN.VALUE = character(1))
    }
  }
  localized
}

format_sem_sentences <- function(summary_data, path_data) {
  if (nrow(summary_data) == 0) {
    return(character(0))
  }

  sem_model_lookup <- setNames(project_config$sem$models, vapply(project_config$sem$models, `[[`, FUN.VALUE = character(1), "name"))
  sem_order <- vapply(project_config$sem$models, `[[`, FUN.VALUE = character(1), "name")
  cognition_order <- names(project_config$cognition_models)
  summary_data$sem_model <- factor(summary_data$sem_model, levels = sem_order)
  summary_data$cognition_model <- factor(summary_data$cognition_model, levels = cognition_order)
  summary_data <- summary_data[order(summary_data$sem_model, summary_data$cognition_model), , drop = FALSE]
  summary_data$sem_model <- as.character(summary_data$sem_model)
  summary_data$cognition_model <- as.character(summary_data$cognition_model)

  vapply(seq_len(nrow(summary_data)), function(idx) {
    row <- summary_data[idx, , drop = FALSE]
    sem_cfg <- sem_model_lookup[[row$sem_model[[1]]]]
    path_rows <- path_data[
      path_data$sem_model == row$sem_model[[1]] &
      path_data$cognition_model == row$cognition_model[[1]],
      ,
      drop = FALSE
    ]

    base_sentence <- tr(
      "text_sem_sentence",
      list(
        model = paste(
          resolve_variable_label(row$sem_model[[1]], runtime_settings, report_language, fallback = row$sem_model[[1]]),
          var_label(row$cognition_model[[1]]),
          sep = " / "
        ),
        x = var_label(sem_cfg$x),
        mediator = var_label(sem_cfg$mediator),
        y = var_label(row$cognition_model[[1]]),
        a = format_numeric_human(extract_path_value(path_rows, "a"), digits = report_digits),
        a_p = format_p_value_human(extract_path_value(path_rows, "a", "p_value"), digits = report_digits),
        b = format_numeric_human(extract_path_value(path_rows, "b"), digits = report_digits),
        b_p = format_p_value_human(extract_path_value(path_rows, "b", "p_value"), digits = report_digits),
        direct = format_numeric_human(extract_path_value(path_rows, "c_prime"), digits = report_digits),
        direct_p = format_p_value_human(extract_path_value(path_rows, "c_prime", "p_value"), digits = report_digits),
        total = format_numeric_human(extract_path_value(path_rows, "c_total"), digits = report_digits),
        total_p = format_p_value_human(extract_path_value(path_rows, "c_total", "p_value"), digits = report_digits),
        indirect = format_numeric_human(extract_path_value(path_rows, "indirect"), digits = report_digits),
        indirect_p = format_p_value_human(extract_path_value(path_rows, "indirect", "p_value"), digits = report_digits)
      )
    )

    paste0(
      base_sentence,
      " 中介贡献率为 ",
      format_numeric_human(row$proportion_mediated_pct[[1]], digits = 2),
      "%。",
      if (isTRUE(row$opposite_direction[[1]])) {
        " 直接效应与间接效应方向相反，提示为不一致中介。由于总效应受到相反方向效应抵消，中介贡献率可出现负值或超过 100%，此时不宜将其解释为常规比例。"
      } else {
        " 直接效应与间接效应方向一致。"
      }
    )
  }, FUN.VALUE = character(1))
}

format_sem_sentences <- function(summary_data, path_data) {
  if (nrow(summary_data) == 0) {
    return(character(0))
  }

  sem_model_lookup <- setNames(project_config$sem$models, vapply(project_config$sem$models, `[[`, FUN.VALUE = character(1), "name"))
  sem_order <- vapply(project_config$sem$models, `[[`, FUN.VALUE = character(1), "name")
  cognition_order <- names(project_config$cognition_models)
  summary_data$sem_model <- factor(summary_data$sem_model, levels = sem_order)
  summary_data$cognition_model <- factor(summary_data$cognition_model, levels = cognition_order)
  summary_data <- summary_data[order(summary_data$sem_model, summary_data$cognition_model), , drop = FALSE]
  summary_data$sem_model <- as.character(summary_data$sem_model)
  summary_data$cognition_model <- as.character(summary_data$cognition_model)

  vapply(seq_len(nrow(summary_data)), function(idx) {
    row <- summary_data[idx, , drop = FALSE]
    sem_cfg <- sem_model_lookup[[row$sem_model[[1]]]]
    path_rows <- path_data[
      path_data$sem_model == row$sem_model[[1]] &
      path_data$cognition_model == row$cognition_model[[1]],
      ,
      drop = FALSE
    ]

    base_sentence <- tr(
      "text_sem_sentence",
      list(
        model = paste(
          resolve_variable_label(row$sem_model[[1]], runtime_settings, report_language, fallback = row$sem_model[[1]]),
          var_label(row$cognition_model[[1]]),
          sep = " / "
        ),
        x = var_label(sem_cfg$x),
        mediator = var_label(sem_cfg$mediator),
        y = var_label(row$cognition_model[[1]]),
        a = format_numeric_human(extract_path_value(path_rows, "a"), digits = report_digits),
        a_p = format_p_value_human(extract_path_value(path_rows, "a", "p_value"), digits = report_digits),
        b = format_numeric_human(extract_path_value(path_rows, "b"), digits = report_digits),
        b_p = format_p_value_human(extract_path_value(path_rows, "b", "p_value"), digits = report_digits),
        direct = format_numeric_human(extract_path_value(path_rows, "c_prime"), digits = report_digits),
        direct_p = format_p_value_human(extract_path_value(path_rows, "c_prime", "p_value"), digits = report_digits),
        total = format_numeric_human(extract_path_value(path_rows, "c_total"), digits = report_digits),
        total_p = format_p_value_human(extract_path_value(path_rows, "c_total", "p_value"), digits = report_digits),
        indirect = format_numeric_human(extract_path_value(path_rows, "indirect"), digits = report_digits),
        indirect_p = format_p_value_human(extract_path_value(path_rows, "indirect", "p_value"), digits = report_digits)
      )
    )

    mediation_note <- describe_mediation_pattern(row$indirect_p[[1]], row$opposite_direction[[1]])
    if (!is.na(row$indirect_p[[1]]) && row$indirect_p[[1]] < 0.05) {
      paste0(
        base_sentence,
        " 中介模式：",
        mediation_note,
        "；中介效应比例为 ",
        format_numeric_human(row$proportion_mediated_pct[[1]], digits = 2),
        "%。"
      )
    } else {
      paste0(
        base_sentence,
        " 中介模式：",
        mediation_note,
        "。"
      )
    }
  }, FUN.VALUE = character(1))
}

build_sem_figure_lines <- function(summary_data) {
  if (nrow(summary_data) == 0) {
    return(character(0))
  }

  sem_order <- vapply(project_config$sem$models, `[[`, FUN.VALUE = character(1), "name")
  cognition_order <- names(project_config$cognition_models)
  summary_data$sem_model <- factor(summary_data$sem_model, levels = sem_order)
  summary_data$cognition_model <- factor(summary_data$cognition_model, levels = cognition_order)
  summary_data <- summary_data[order(summary_data$sem_model, summary_data$cognition_model), , drop = FALSE]

  figure_lines <- character(0)
  for (idx in seq_len(nrow(summary_data))) {
    row <- summary_data[idx, , drop = FALSE]
    figure_group <- if ("group" %in% names(row)) as.character(row$group[[1]]) else "Overall"
    figure_file <- paste0("sem_", figure_group, "_", as.character(row$sem_model[[1]]), "_", as.character(row$cognition_model[[1]]), ".png")
    figure_alt <- paste(
      if (figure_group == "Overall") "Overall" else group_label(figure_group),
      resolve_variable_label(as.character(row$sem_model[[1]]), runtime_settings, report_language, fallback = as.character(row$sem_model[[1]])),
      var_label(as.character(row$cognition_model[[1]])),
      sep = " / "
    )
    figure_lines <- c(
      figure_lines,
      markdown_image(file.path("../figures", figure_file), figure_alt),
      ""
    )
  }
  figure_lines
}

sem_section_labels <- function() {
  if (report_language == "en") {
    return(list(
      fit_header = "Model fit and interpretation",
      summary_header = "Model summary",
      paths_header = "Path coefficients",
      figure_header = "Model figure",
      fit_sentence = function(fit_row) {
        if (nrow(fit_row) == 0) {
          return("Model fit information is unavailable.")
        }
        paste0(
          "Adjusted covariates: `", fit_row$covariates[[1]], "`. ",
          "n = ", fit_row$n[[1]], ". ",
          fit_row$model_reasonable[[1]], " ",
          fit_row$fit_note[[1]]
        )
      }
    ))
  }
  if (report_language == "ja") {
    return(list(
      fit_header = "モデル適合と解釈",
      summary_header = "モデル要約",
      paths_header = "パス係数",
      figure_header = "モデル図",
      fit_sentence = function(fit_row) {
        if (nrow(fit_row) == 0) {
          return("モデル適合情報は利用できません。")
        }
        paste0(
          "共変量: `", fit_row$covariates[[1]], "`。",
          " n = ", fit_row$n[[1]], "。 ",
          fit_row$model_reasonable[[1]], " ",
          fit_row$fit_note[[1]]
        )
      }
    ))
  }
  list(
    fit_header = "模型评价与解释",
    summary_header = "模型摘要",
    paths_header = "路径系数",
    figure_header = "模型图",
    fit_sentence = function(fit_row) {
      if (nrow(fit_row) == 0) {
        return("当前没有可用的模型评价信息。")
      }
      paste0(
        "协变量：`", fit_row$covariates[[1]], "`；",
        "样本量 n = ", fit_row$n[[1]], "。",
        fit_row$model_reasonable[[1]], " ",
        fit_row$fit_note[[1]]
      )
    }
  )
}

sem_section_labels_v2 <- function() {
  if (report_language == "en") {
    return(list(
      fit_header = "Model fit and interpretation",
      summary_header = "Model summary",
      paths_header = "Path coefficients",
      loadings_header = "Factor loadings",
      mi_header = "Top modification indices",
      figure_header = "Model figure",
      fit_sentence = function(fit_row) {
        if (nrow(fit_row) == 0) {
          return("Model fit information is unavailable.")
        }
        paste0(
          "Adjusted covariates: `", fit_row$covariates[[1]], "`. ",
          "n = ", fit_row$n[[1]], ". ",
          "chisq = ", format_fit_metric(fit_row$chisq[[1]]), ", ",
          "df = ", format_fit_metric(fit_row$df[[1]], digits = 0), ". ",
          "CFI = ", format_fit_metric(fit_row$cfi[[1]]), ", ",
          "TLI = ", format_fit_metric(fit_row$tli[[1]]), ", ",
          "RMSEA = ", format_fit_metric(fit_row$rmsea[[1]]), ", ",
          "SRMR = ", format_fit_metric(fit_row$srmr[[1]]), ". ",
          "Indicator standardized = ", ifelse(isTRUE(fit_row$indicator_standardized[[1]]), "TRUE", "FALSE"), ". ",
          "Residual covariances: ", ifelse(nzchar(fit_row$residual_covariances[[1]]), fit_row$residual_covariances[[1]], "none"), ". ",
          localize_sem_text(fit_row$optimization_note[[1]]), " ",
          localize_sem_text(fit_row$model_reasonable[[1]]), " ",
          localize_sem_text(fit_row$fit_note[[1]])
        )
      }
    ))
  }
  if (report_language == "ja") {
    return(list(
      fit_header = "モデル適合と解釈",
      summary_header = "モデル要約",
      paths_header = "パス係数",
      loadings_header = "因子負荷量",
      mi_header = "上位 modification indices",
      figure_header = "モデル図",
      fit_sentence = function(fit_row) {
        if (nrow(fit_row) == 0) {
          return("モデル適合情報は利用できません。")
        }
        paste0(
          "共変量: `", fit_row$covariates[[1]], "`。 ",
          "n = ", fit_row$n[[1]], "。 ",
          "chisq = ", format_fit_metric(fit_row$chisq[[1]]), "、",
          "df = ", format_fit_metric(fit_row$df[[1]], digits = 0), "。 ",
          "CFI = ", format_fit_metric(fit_row$cfi[[1]]), "、",
          "TLI = ", format_fit_metric(fit_row$tli[[1]]), "、",
          "RMSEA = ", format_fit_metric(fit_row$rmsea[[1]]), "、",
          "SRMR = ", format_fit_metric(fit_row$srmr[[1]]), "。 ",
          "指標標準化 = ", ifelse(isTRUE(fit_row$indicator_standardized[[1]]), "TRUE", "FALSE"), "。 ",
          "残差共分散: ", ifelse(nzchar(fit_row$residual_covariances[[1]]), fit_row$residual_covariances[[1]], "なし"), "。 ",
          localize_sem_text(fit_row$optimization_note[[1]]), " ",
          localize_sem_text(fit_row$model_reasonable[[1]]), " ",
          localize_sem_text(fit_row$fit_note[[1]])
        )
      }
    ))
  }
  list(
    fit_header = "模型评价与解释",
    summary_header = "模型摘要",
    paths_header = "路径系数",
    loadings_header = "因子载荷",
    mi_header = "主要 modification indices",
    figure_header = "模型图",
    fit_sentence = function(fit_row) {
      if (nrow(fit_row) == 0) {
        return("当前没有可用的模型评价信息。")
      }
      paste0(
        "协变量：`", fit_row$covariates[[1]], "`；",
        "样本量 n = ", fit_row$n[[1]], "；",
        "chisq = ", format_fit_metric(fit_row$chisq[[1]]), "，",
        "df = ", format_fit_metric(fit_row$df[[1]], digits = 0), "；",
        "CFI = ", format_fit_metric(fit_row$cfi[[1]]), "，",
        "TLI = ", format_fit_metric(fit_row$tli[[1]]), "，",
        "RMSEA = ", format_fit_metric(fit_row$rmsea[[1]]), "，",
        "SRMR = ", format_fit_metric(fit_row$srmr[[1]]), "。 ",
        "指标标准化 = ", ifelse(isTRUE(fit_row$indicator_standardized[[1]]), "TRUE", "FALSE"), "。 ",
        "残差相关: ", ifelse(nzchar(fit_row$residual_covariances[[1]]), fit_row$residual_covariances[[1]], "无"), "。 ",
        localize_sem_text(fit_row$optimization_note[[1]]), " ",
        localize_sem_text(fit_row$model_reasonable[[1]]), " ",
        localize_sem_text(fit_row$fit_note[[1]])
      )
    }
  )
}

build_sem_model_blocks <- function(summary_data, path_data, fit_data, loading_data, mi_data) {
  if (nrow(summary_data) == 0) {
    return(c(empty_md_text, ""))
  }

  sem_order <- vapply(project_config$sem$models, `[[`, FUN.VALUE = character(1), "name")
  cognition_order <- names(project_config$cognition_models)
  summary_data$sem_model <- factor(summary_data$sem_model, levels = sem_order)
  summary_data$cognition_model <- factor(summary_data$cognition_model, levels = cognition_order)
  summary_data <- summary_data[order(summary_data$sem_model, summary_data$cognition_model), , drop = FALSE]
  summary_data$sem_model <- as.character(summary_data$sem_model)
  summary_data$cognition_model <- as.character(summary_data$cognition_model)

  section_text <- sem_section_labels_v2()
  blocks <- character(0)

  for (idx in seq_len(nrow(summary_data))) {
    row <- summary_data[idx, , drop = FALSE]
    sem_name <- row$sem_model[[1]]
    cognition_name <- row$cognition_model[[1]]
    figure_group <- if ("group" %in% names(row)) row$group[[1]] else "Overall"
    model_title <- paste(
      if (figure_group == "Overall") "Overall" else group_label(figure_group),
      resolve_variable_label(sem_name, runtime_settings, report_language, fallback = sem_name),
      var_label(cognition_name),
      sep = " / "
    )
    row_paths <- path_data[
      path_data$sem_model == sem_name &
        path_data$cognition_model == cognition_name &
        if ("group" %in% names(path_data)) path_data$group == figure_group else TRUE,
      ,
      drop = FALSE
    ]
    row_fit <- fit_data[
      fit_data$sem_model == sem_name &
        fit_data$cognition_model == cognition_name &
        if ("group" %in% names(fit_data)) fit_data$group == figure_group else TRUE,
      ,
      drop = FALSE
    ]
    row_fit_display <- localize_sem_fit_table(row_fit)
    row_loadings <- loading_data[
      loading_data$sem_model == sem_name &
        loading_data$cognition_model == cognition_name &
        if ("group" %in% names(loading_data)) loading_data$group == figure_group else TRUE,
      ,
      drop = FALSE
    ]
    row_mi <- mi_data[
      mi_data$sem_model == sem_name &
        mi_data$cognition_model == cognition_name &
        if ("group" %in% names(mi_data)) mi_data$group == figure_group else TRUE,
      ,
      drop = FALSE
    ]
    figure_file <- paste0("sem_", figure_group, "_", sem_name, "_", cognition_name, ".png")
    sentence <- format_sem_sentences(row, row_paths)

    blocks <- c(
      blocks,
      paste0("#### ", model_title),
      "",
      make_paragraph_block(sentence, empty_text = empty_md_text),
      paste0("##### ", section_text$fit_header),
      "",
      section_text$fit_sentence(row_fit),
      "",
      markdown_table(row_fit_display, digits = report_digits, empty_text = empty_md_text),
      paste0("##### ", section_text$summary_header),
      "",
      markdown_table(row, digits = report_digits, empty_text = empty_md_text),
      paste0("##### ", section_text$paths_header),
      "",
      markdown_table(row_paths, digits = report_digits, empty_text = empty_md_text),
      paste0("##### ", section_text$loadings_header),
      "",
      markdown_table(row_loadings, digits = report_digits, empty_text = empty_md_text),
      paste0("##### ", section_text$mi_header),
      "",
      build_mi_summary_text(row_mi),
      "",
      markdown_table(row_mi, digits = report_digits, empty_text = empty_md_text),
      paste0("##### ", section_text$figure_header),
      "",
      markdown_image(file.path("../figures", figure_file), model_title),
      ""
    )
  }

  blocks
}

format_sem_sensitivity_sentences <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  data <- data[order(data$cognition_model, data$sem_model), , drop = FALSE]
  vapply(seq_len(nrow(data)), function(idx) {
    row <- data[idx, , drop = FALSE]
    model_name <- paste(
      resolve_variable_label(row$sem_model[[1]], runtime_settings, report_language, fallback = row$sem_model[[1]]),
      var_label(row$cognition_model[[1]]),
      sep = " / "
    )
    paste0(
      "在“", model_name, "”模型中，标准化与否的敏感性分析显示：",
      "标准化模型的间接效应为 ", format_numeric_human(row$primary_indirect[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$primary_indirect_p[[1]], digits = report_digits), "），",
      "未标准化模型的间接效应为 ", format_numeric_human(row$sensitivity_indirect[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$sensitivity_indirect_p[[1]], digits = report_digits), "）；",
      "标准化模型的直接效应为 ", format_numeric_human(row$primary_direct[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$primary_direct_p[[1]], digits = report_digits), "），",
      "未标准化模型的直接效应为 ", format_numeric_human(row$sensitivity_direct[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$sensitivity_direct_p[[1]], digits = report_digits), "）。",
      "两种处理下间接效应差值为 ", format_numeric_human(row$indirect_delta[[1]], digits = report_digits),
      "，直接效应差值为 ", format_numeric_human(row$direct_delta[[1]], digits = report_digits), "。"
    )
  }, FUN.VALUE = character(1))
}

localize_sem_difference_table <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  localized <- data
  if ("note" %in% names(localized)) {
    localized$note <- if (report_language == "zh") {
      "在同一暴露变量、同一认知潜变量结局、相同协变量条件下，将两个中介同时纳入平行中介 latent SEM，对两条间接效应的差异进行检验。"
    } else {
      localized$note
    }
  }
  localized
}

localize_sem_multigroup_table <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  localized <- data
  localized$path <- vapply(localized$path, function(x) {
    if (x == "c_prime") {
      return("c_prime")
    }
    x
  }, FUN.VALUE = character(1))
  localized$scope <- ifelse(localized$scope == "omnibus", "overall", localized$scope)
  localized$significant <- ifelse(localized$significant, "yes", "no")
  localized
}

localize_sem_mechanism_table <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  localized <- data
  if ("pattern" %in% names(localized)) {
    localized$pattern <- ifelse(
      localized$pattern == "consistent_mediation",
      "consistent mediation",
      ifelse(localized$pattern == "inconsistent_mediation", "inconsistent mediation", "no significant mediation")
    )
  }
  localized
}

format_sem_difference_sentences <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  vapply(seq_len(nrow(data)), function(idx) {
    row <- data[idx, , drop = FALSE]
    paste0(
      "在“", var_label(row$x_var[[1]]), "”作为共同暴露、",
      "“", var_label(row$cognition_model[[1]]), "”作为共同结局的条件下，",
      "比较中介 “", var_label(row$mediator_a[[1]]), "” 与 “", var_label(row$mediator_b[[1]]), "” 的间接效应差异：",
      var_label(row$mediator_a[[1]]), " 路径的间接效应为 ",
      format_numeric_human(row$indirect_a[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$indirect_a_p[[1]], digits = report_digits), "），",
      var_label(row$mediator_b[[1]]), " 路径的间接效应为 ",
      format_numeric_human(row$indirect_b[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$indirect_b_p[[1]], digits = report_digits), "），",
      "两条间接效应差值为 ",
      format_numeric_human(row$indirect_diff[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$indirect_diff_p[[1]], digits = report_digits), "）。",
      if (isTRUE(row$difference_significant[[1]])) {
        " 两条中介路径的间接效应差异具有统计学意义。"
      } else {
        " 两条中介路径的间接效应差异无统计学意义。"
      }
    )
  }, FUN.VALUE = character(1))
}

format_sem_multigroup_sentences <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  data <- data[data$scope == "omnibus", , drop = FALSE]
  if (nrow(data) == 0) {
    return(character(0))
  }

  vapply(seq_len(nrow(data)), function(idx) {
    row <- data[idx, , drop = FALSE]
    significance_text <- if (!is.na(row$p_value[[1]]) && row$p_value[[1]] < 0.05) {
      "存在显著组间差异"
    } else {
      "未见显著组间差异"
    }

    paste0(
      "在 `",
      var_label(row$sem_model[[1]]),
      " / ",
      var_label(row$cognition_model[[1]]),
      "` 的多组 SEM 中，路径 `",
      row$path[[1]],
      "` 的整体组间差异检验 ",
      significance_text,
      "（p = ",
      format_p_value_human(row$p_value[[1]], digits = report_digits),
      "）。"
    )
  }, FUN.VALUE = character(1))
}

format_sem_mechanism_sentences <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  data <- order_by_group(data, "group", group_order = c("Overall", group_order))
  vapply(seq_len(nrow(data)), function(idx) {
    row <- data[idx, , drop = FALSE]
    paste0(
      "在 `",
      ifelse(row$group[[1]] == "Overall", "全体", group_label(row$group[[1]])),
      " / ",
      var_label(row$sem_model[[1]]),
      " / ",
      var_label(row$cognition_model[[1]]),
      "` 模型中，当前模式为 `",
      row$pattern_zh[[1]],
      "`，其解释为：",
      row$interpretation[[1]]
    )
  }, FUN.VALUE = character(1))
}

localize_parallel_sem_table <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  localized <- data
  if ("parallel_model" %in% names(localized)) {
    localized$parallel_model <- vapply(localized$parallel_model, var_label, FUN.VALUE = character(1))
  }
  if ("group" %in% names(localized)) {
    localized$group <- vapply(localized$group, function(x) ifelse(x == "Overall", "整体", group_label(x)), FUN.VALUE = character(1))
  }
  if ("cognition_model" %in% names(localized)) {
    localized$cognition_model <- vapply(localized$cognition_model, var_label, FUN.VALUE = character(1))
  }
  if ("x_var" %in% names(localized)) {
    localized$x_var <- vapply(localized$x_var, var_label, FUN.VALUE = character(1))
  }
  if ("mediator_var" %in% names(localized)) {
    localized$mediator_var <- vapply(localized$mediator_var, function(x) {
      if (x == "TOTAL_INDIRECT") "总间接效应" else var_label(x)
    }, FUN.VALUE = character(1))
  }
  if ("mediation_type" %in% names(localized)) {
    localized$mediation_type <- ifelse(
      localized$mediation_type == "consistent_mediation",
      "促进性中介",
      ifelse(localized$mediation_type == "inconsistent_mediation", "遮蔽效应", localized$mediation_type)
    )
  }
  keep_cols <- c("parallel_model", "group", "mediator_var", "a", "a_p", "b", "b_p", "indirect", "indirect_p", "direct", "direct_p", "total_indirect", "total_indirect_p", "total", "total_p", "mediation_type")
  keep_cols <- keep_cols[keep_cols %in% names(localized)]
  localized <- localized[, keep_cols, drop = FALSE]
  names(localized) <- c("并行模型", "分组", "中介变量", "a", "a_p", "b", "b_p", "间接效应", "间接p", "直接效应", "直接p", "总间接效应", "总间接p", "总效应", "总效应p", "机制类型")[seq_along(keep_cols)]
  localized
}

format_parallel_sem_sentences <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }
  overall_data <- data[data$group == "Overall", , drop = FALSE]
  if (nrow(overall_data) == 0) {
    overall_data <- data
  }
  sig_data <- overall_data[!is.na(overall_data$indirect_p) & overall_data$indirect_p < 0.05, , drop = FALSE]
  if (nrow(sig_data) == 0) {
    return("并行中介模型中，各中介在彼此同时进入模型后未见稳定的显著间接效应。")
  }
  vapply(seq_len(nrow(sig_data)), function(i) {
    row <- sig_data[i, , drop = FALSE]
    paste0(
      "在 `", var_label(row$parallel_model[[1]]), "` 中，中介 `", var_label(row$mediator_var[[1]]),
      "` 的独立间接效应为 ", format_numeric_human(row$indirect[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$indirect_p[[1]], digits = report_digits), "），",
      "直接效应为 ", format_numeric_human(row$direct[[1]], digits = report_digits),
      "（p = ", format_p_value_human(row$direct_p[[1]], digits = report_digits), "）。"
    )
  }, FUN.VALUE = character(1))
}

localize_moderated_mediation_table <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  localized <- data
  localized$analysis_type <- ifelse(localized$analysis_type == "parallel_mediator", "并行中介", "单中介")
  localized$model_name <- vapply(localized$model_name, var_label, FUN.VALUE = character(1))
  localized$cognition_model <- vapply(localized$cognition_model, var_label, FUN.VALUE = character(1))
  localized$mediator_component <- vapply(localized$mediator_component, function(x) {
    if (x == "indirect") return("间接效应")
    if (x == "total_indirect") return("总间接效应")
    if (grepl("^indirect_", x)) return(var_label(sub("^indirect_", "", x)))
    x
  }, FUN.VALUE = character(1))
  localized$scope <- ifelse(localized$scope == "omnibus", "整体", "两两比较")
  localized$group_pair[is.na(localized$group_pair)] <- "CN vs MCI vs AD"
  localized$significant <- ifelse(localized$significant, "是", "否")
  keep_cols <- c("analysis_type", "model_name", "mediator_component", "cognition_model", "scope", "group_pair", "statistic", "df", "p_value", "significant")
  keep_cols <- keep_cols[keep_cols %in% names(localized)]
  localized <- localized[, keep_cols, drop = FALSE]
  names(localized) <- c("分析类型", "模型", "检验对象", "认知模型", "范围", "组别比较", "统计量", "df", "p值", "显著")[seq_along(keep_cols)]
  localized
}

format_moderated_mediation_sentences <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }
  data <- data[data$scope == "omnibus", , drop = FALSE]
  sig <- data[!is.na(data$p_value) & data$p_value < 0.05, , drop = FALSE]
  if (nrow(sig) == 0) {
    return("正式的 moderated mediation / multigroup SEM 检验未见稳定的整体组间间接效应差异。")
  }
  vapply(seq_len(nrow(sig)), function(i) {
    row <- sig[i, , drop = FALSE]
    paste0(
      "`", ifelse(row$analysis_type[[1]] == "parallel_mediator", "并行中介", "单中介"), "` 模型 `",
      var_label(row$model_name[[1]]), "` 中，`", row$mediator_component[[1]],
      "` 的组间差异达到显著（p = ", format_p_value_human(row$p_value[[1]], digits = report_digits), "）。"
    )
  }, FUN.VALUE = character(1))
}

chp_primary <- pick_term_rows(chp_linear_overall, term_name = "exposure")
chp_group_primary <- pick_term_rows(chp_linear_by_group, term_name = "exposure")
tau_primary <- pick_term_rows(tau_linear_overall, term_name = "exposure")
tau_group_primary <- pick_term_rows(tau_linear_by_group, term_name = "exposure")
ptau_primary <- pick_term_rows(ptau_linear_overall, term_name = "exposure")
ptau_group_primary <- pick_term_rows(ptau_linear_by_group, term_name = "exposure")
advanced_biomarker_primary <- advanced_biomarker_overall[advanced_biomarker_overall$term %in% c("exposure", "PTAU", "TAU", "S_ABETA"), , drop = FALSE]
advanced_biomarker_group_primary <- advanced_biomarker_by_group[advanced_biomarker_by_group$term == "exposure", , drop = FALSE]
advanced_interaction_primary <- advanced_interaction_terms[grepl("^exposure:diagnosis", advanced_interaction_terms$term), , drop = FALSE]

group_counts <- as.data.frame(table(analysis_data[[project_config$variables$group_label_var]]), stringsAsFactors = FALSE)
names(group_counts) <- c("group", "n")
group_counts$group <- vapply(group_counts$group, group_label, FUN.VALUE = character(1))

if (nrow(run_metadata) > 0 && "language" %in% names(run_metadata)) {
  run_metadata$language <- report_language
}

transformed_vars <- normality_table[normality_table$transform_method != "none", c("variable", "analysis_var", "transform_method", "note"), drop = FALSE]
significant_group_vars <- if (nrow(group_overall) > 0) {
  var_names <- group_overall$variable[group_overall$p_value < 0.05]
  if (length(var_names) == 0) character(0) else vapply(var_names, var_label, FUN.VALUE = character(1))
} else {
  character(0)
}
significant_group_vars_text <- if (length(significant_group_vars) == 0) {
  tr("text_no_data")
} else {
  paste(significant_group_vars, collapse = "; ")
}

descriptive_sentences <- c(
  format_descriptive_continuous(descriptive_overall_cont, grouped = FALSE),
  format_descriptive_categorical(descriptive_overall_cat, grouped = FALSE),
  format_descriptive_continuous(order_by_group(descriptive_group_cont, "group", group_order = group_order), grouped = TRUE),
  format_descriptive_categorical(order_by_group(descriptive_group_cat, "group", group_order = group_order), grouped = TRUE)
)

group_narrative <- if (isTRUE(runtime_settings$narrative$include_group_comparison)) {
  format_group_result_sentences(group_overall[group_overall$p_value < 0.05, , drop = FALSE])
} else {
  character(0)
}

matching_sentence <- if (nrow(matching_decision) > 0 && isTRUE(matching_decision$matching_required[[1]])) {
  tr("text_matching_needed")
} else {
  tr("text_matching_not_needed")
}

chp_narrative <- c(
  if (isTRUE(runtime_settings$narrative$include_linear)) format_linear_sentences(chp_primary, chp_group_primary) else character(0),
  if (isTRUE(runtime_settings$narrative$include_partial)) format_partial_sentences(chp_partial_overall, chp_partial_by_group) else character(0)
)

tau_narrative <- c(
  if (nrow(tau_primary) > 0 && isTRUE(runtime_settings$narrative$include_linear)) {
    sentences <- character(0)
    for (idx in seq_len(nrow(tau_primary))) {
      row <- tau_primary[idx, , drop = FALSE]
      sentences <- c(
        sentences,
        tr(
          "text_linear_overall",
          list(
            outcome = var_label(row$outcome[[1]]),
            exposure = var_label(row$exposure[[1]]),
            beta = format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
            p = format_p_value_human(extract_p_value(row), digits = report_digits)
          )
        )
      )
    }
    tau_group_primary <- order_by_group(tau_group_primary, "group", group_order = group_order)
    for (idx in seq_len(nrow(tau_group_primary))) {
      row <- tau_group_primary[idx, , drop = FALSE]
      sentences <- c(
        sentences,
        tr(
          "text_linear_group",
          list(
            group = group_label(row$group[[1]]),
            outcome = var_label(row$outcome[[1]]),
            exposure = var_label(row$exposure[[1]]),
            beta = format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
            p = format_p_value_human(extract_p_value(row), digits = report_digits)
          )
        )
      )
    }
    sentences
  } else {
    character(0)
  },
  if (isTRUE(runtime_settings$narrative$include_partial)) format_partial_sentences(tau_partial_overall, tau_partial_by_group) else character(0)
)

ptau_narrative <- c(
  if (nrow(ptau_primary) > 0 && isTRUE(runtime_settings$narrative$include_linear)) {
    sentences <- character(0)
    for (idx in seq_len(nrow(ptau_primary))) {
      row <- ptau_primary[idx, , drop = FALSE]
      sentences <- c(
        sentences,
        tr(
          "text_linear_overall",
          list(
            outcome = var_label(row$outcome[[1]]),
            exposure = var_label(row$exposure[[1]]),
            beta = format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
            p = format_p_value_human(extract_p_value(row), digits = report_digits)
          )
        )
      )
    }
    ptau_group_primary <- order_by_group(ptau_group_primary, "group", group_order = group_order)
    for (idx in seq_len(nrow(ptau_group_primary))) {
      row <- ptau_group_primary[idx, , drop = FALSE]
      sentences <- c(
        sentences,
        tr(
          "text_linear_group",
          list(
            group = group_label(row$group[[1]]),
            outcome = var_label(row$outcome[[1]]),
            exposure = var_label(row$exposure[[1]]),
            beta = format_beta_human(row$estimate[[1]], row$std_beta[[1]], digits = report_digits),
            p = format_p_value_human(extract_p_value(row), digits = report_digits)
          )
        )
      )
    }
    sentences
  } else {
    character(0)
  },
  if (isTRUE(runtime_settings$narrative$include_partial)) format_partial_sentences(ptau_partial_overall, ptau_partial_by_group) else character(0)
)

advanced_narrative <- c(
  format_biomarker_adjusted_sentences(advanced_biomarker_primary, advanced_biomarker_group_primary),
  format_interaction_sentences(advanced_interaction_comparison, advanced_interaction_terms),
  format_nonlinear_sentences(advanced_nonlinear_summary, advanced_nonlinear_tests),
  format_abeta_truncation_sentences(abeta_truncation_summary, abeta_truncation_regression, abeta_truncation_partial),
  if (!("ABETA_ratio" %in% names(analysis_data) || "S_ABETA_RATIO" %in% names(analysis_data))) {
    "当前原始数据集中未包含 Aβ ratio 变量，因此本轮以 S_ABETA 作为 amyloid 指标；后续若补充 Aβ42/40 或其他 ratio，可直接替换配置后复跑。"
  } else {
    character(0)
  }
)

sem_group_order <- c("Overall", group_order)
sem_summary_overall <- if ("group" %in% names(sem_summary)) sem_summary[sem_summary$group == "Overall", , drop = FALSE] else sem_summary
sem_paths_overall <- if ("group" %in% names(sem_paths)) sem_paths[sem_paths$group == "Overall", , drop = FALSE] else sem_paths
sem_fit_overall <- if ("group" %in% names(sem_fit)) sem_fit[sem_fit$group == "Overall", , drop = FALSE] else sem_fit
sem_loadings_overall <- if ("group" %in% names(sem_loadings)) sem_loadings[sem_loadings$group == "Overall", , drop = FALSE] else sem_loadings
sem_modification_indices_overall <- if ("group" %in% names(sem_modification_indices)) sem_modification_indices[sem_modification_indices$group == "Overall", , drop = FALSE] else sem_modification_indices
sem_standardization_sensitivity_overall <- if ("group" %in% names(sem_standardization_sensitivity)) sem_standardization_sensitivity[sem_standardization_sensitivity$group == "Overall", , drop = FALSE] else sem_standardization_sensitivity
sem_indirect_difference_tests_overall <- if ("group" %in% names(sem_indirect_difference_tests)) sem_indirect_difference_tests[sem_indirect_difference_tests$group == "Overall", , drop = FALSE] else sem_indirect_difference_tests
sem_parallel_summary_overall <- if ("group" %in% names(sem_parallel_summary)) sem_parallel_summary[sem_parallel_summary$group == "Overall", , drop = FALSE] else sem_parallel_summary
sem_multigroup_tests_display <- localize_sem_multigroup_table(sem_multigroup_tests)
sem_mechanism_display <- localize_sem_mechanism_table(sem_mechanism_interpretation)
sem_parallel_display <- localize_parallel_sem_table(sem_parallel_summary)
sem_moderated_mediation_display <- localize_moderated_mediation_table(sem_moderated_mediation_summary)

sem_narrative <- if (isTRUE(runtime_settings$narrative$include_sem)) {
  format_sem_sentences(sem_summary_overall, sem_paths_overall)
} else {
  character(0)
}
sem_parallel_narrative <- format_parallel_sem_sentences(sem_parallel_summary_overall)
sem_sensitivity_narrative <- format_sem_sensitivity_sentences(sem_standardization_sensitivity_overall)
sem_difference_narrative <- format_sem_difference_sentences(sem_indirect_difference_tests_overall)
sem_multigroup_narrative <- format_sem_multigroup_sentences(sem_multigroup_tests)
sem_mechanism_narrative <- format_sem_mechanism_sentences(sem_mechanism_interpretation)
sem_moderated_mediation_narrative <- format_moderated_mediation_sentences(sem_moderated_mediation_summary)
sem_figure_lines <- build_sem_figure_lines(sem_summary_overall)
sem_model_blocks <- build_sem_model_blocks(sem_summary_overall, sem_paths_overall, sem_fit_overall, sem_loadings_overall, sem_modification_indices_overall)

report_lines <- c(
  paste0("# ", project_report_title),
  "",
  paste0("- ", tr("report_generated_at"), ": ", format(Sys.time(), date_time_format)),
  paste0("- ", tr("report_project"), ": ", project_config$project_name),
  paste0("- ", tr("report_version"), ": `", result_version, "`"),
  paste0("- ", tr("report_folder"), ": `", result_run_dir, "`"),
  paste0("- ", tr("report_author"), ": ", project_config$report$author),
  paste0("- ", tr("report_raw_data"), ": `", raw_data_path, "`"),
  paste0("- ", tr("report_rows"), ": ", nrow(analysis_data)),
  ""
)

report_lines <- c(
  report_lines,
  tr("section_dataset_overview"),
  "",
  tr("sub_run_metadata"),
  "",
  markdown_table(run_metadata, digits = report_digits, empty_text = empty_md_text),
  tr("sub_group_counts"),
  "",
  markdown_table(group_counts, digits = report_digits, empty_text = empty_md_text),
  tr("sub_selected_variables"),
  "",
  markdown_table(variable_dictionary, digits = report_digits, empty_text = empty_md_text)
)

if (isTRUE(runtime_settings$report$include_descriptive)) {
  report_lines <- c(
    report_lines,
    tr("section_descriptive"),
    "",
    tr("sub_narrative"),
    "",
    make_paragraph_block(descriptive_sentences, empty_text = empty_md_text),
    tr("sub_overall_descriptive_cont"),
    "",
    markdown_table(descriptive_overall_cont, digits = report_digits, empty_text = empty_md_text),
    tr("sub_overall_descriptive_cat"),
    "",
    markdown_table(descriptive_overall_cat, digits = report_digits, empty_text = empty_md_text),
    tr("sub_group_descriptive_cont"),
    "",
    markdown_table(order_by_group(descriptive_group_cont, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
    tr("sub_group_descriptive_cat"),
    "",
    markdown_table(order_by_group(descriptive_group_cat, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text)
  )
}

report_lines <- c(
  report_lines,
  tr("section_cleaning"),
  "",
  tr("sub_numeric_conversion"),
  "",
  markdown_table(conversion_log, digits = report_digits, empty_text = empty_md_text),
  tr("sub_normality"),
  "",
  markdown_table(normality_table, digits = report_digits, empty_text = empty_md_text),
  tr("sub_transformed_variables"),
  "",
  markdown_table(transformed_vars, digits = report_digits, empty_text = empty_md_text)
)

report_lines <- c(
  report_lines,
  tr("section_group"),
  "",
  tr("sub_narrative"),
  "",
  tr("text_significant_group_vars", list(value = significant_group_vars_text)),
  "",
  make_paragraph_block(group_narrative, empty_text = empty_md_text),
  tr("sub_overall_tests"),
  "",
  markdown_table(group_overall, digits = report_digits, empty_text = empty_md_text),
  tr("sub_pairwise"),
  "",
  markdown_table(group_pairwise, digits = report_digits, empty_text = empty_md_text),
  tr("sub_adjusted_models"),
  "",
  markdown_table(group_adjusted, digits = report_digits, empty_text = empty_md_text)
)

report_lines <- c(
  report_lines,
  tr("section_matching"),
  "",
  tr("sub_narrative"),
  "",
  make_paragraph_block(matching_sentence),
  tr("sub_matching_decision"),
  "",
  markdown_table(matching_decision, digits = report_digits, empty_text = empty_md_text),
  tr("sub_matching_summary"),
  "",
  markdown_table(matching_summary, digits = report_digits, empty_text = empty_md_text),
  tr("sub_matching_balance"),
  "",
  markdown_table(matching_balance, digits = report_digits, empty_text = empty_md_text),
  tr("sub_matching_targets"),
  "",
  markdown_table(matched_targets, digits = report_digits, empty_text = empty_md_text)
)

report_lines <- c(
  report_lines,
  tr("section_chp"),
  "",
  tr("sub_narrative"),
  "",
  make_paragraph_block(chp_narrative, empty_text = empty_md_text),
  tr("sub_linear_overall"),
  "",
  markdown_table(chp_primary, digits = report_digits, empty_text = empty_md_text),
  tr("sub_linear_group"),
  "",
  markdown_table(order_by_group(chp_group_primary, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
  tr("sub_partial_overall"),
  "",
  markdown_table(chp_partial_overall, digits = report_digits, empty_text = empty_md_text),
  tr("sub_partial_group"),
  "",
  markdown_table(order_by_group(chp_partial_by_group, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
  tr("sub_figures"),
  "",
  markdown_image("../figures/scatter_ChPICV_on_sTREM2_overall.png", "ChPICV and sTREM2 overall"),
  "",
  markdown_image("../figures/scatter_sTREM2_on_ChPICV_overall.png", "sTREM2 and ChPICV overall"),
  ""
)

report_lines <- c(
  report_lines,
  tr("section_tau"),
  "",
  tr("sub_narrative"),
  "",
  make_paragraph_block(tau_narrative, empty_text = empty_md_text),
  tr("sub_linear_overall"),
  "",
  markdown_table(tau_primary, digits = report_digits, empty_text = empty_md_text),
  tr("sub_linear_group"),
  "",
  markdown_table(order_by_group(tau_group_primary, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
  tr("sub_partial_overall"),
  "",
  markdown_table(tau_partial_overall, digits = report_digits, empty_text = empty_md_text),
  tr("sub_partial_group"),
  "",
  markdown_table(order_by_group(tau_partial_by_group, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
  tr("sub_figures"),
  "",
  markdown_image("../figures/scatter_TAU_on_ChPICV_overall.png", "Tau and ChPICV overall"),
  "",
  markdown_image("../figures/scatter_TAU_on_sTREM2_overall.png", "Tau and sTREM2 overall"),
  ""
)

report_lines <- c(
  report_lines,
  tr("section_ptau"),
  "",
  tr("sub_narrative"),
  "",
  make_paragraph_block(ptau_narrative, empty_text = empty_md_text),
  tr("sub_linear_overall"),
  "",
  markdown_table(ptau_primary, digits = report_digits, empty_text = empty_md_text),
  tr("sub_linear_group"),
  "",
  markdown_table(order_by_group(ptau_group_primary, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
  tr("sub_partial_overall"),
  "",
  markdown_table(ptau_partial_overall, digits = report_digits, empty_text = empty_md_text),
  tr("sub_partial_group"),
  "",
  markdown_table(order_by_group(ptau_partial_by_group, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
  tr("sub_figures"),
  "",
  markdown_image("../figures/scatter_PTAU_on_ChPICV_overall.png", "PTAU and ChPICV overall"),
  "",
  markdown_image("../figures/scatter_PTAU_on_sTREM2_overall.png", "PTAU and sTREM2 overall"),
  ""
)

report_lines <- c(
  report_lines,
  tr("section_advanced"),
  "",
  tr("sub_narrative"),
  "",
  make_paragraph_block(advanced_narrative, empty_text = empty_md_text),
  tr("sub_biomarker_adjusted_overall"),
  "",
  markdown_table(advanced_biomarker_primary, digits = report_digits, empty_text = empty_md_text),
  tr("sub_biomarker_adjusted_group"),
  "",
  markdown_table(order_by_group(advanced_biomarker_group_primary, "group", group_order = group_order), digits = report_digits, empty_text = empty_md_text),
  tr("sub_interaction"),
  "",
  markdown_table(advanced_interaction_comparison, digits = report_digits, empty_text = empty_md_text),
  markdown_table(advanced_interaction_primary, digits = report_digits, empty_text = empty_md_text),
  tr("sub_nonlinear"),
  "",
  markdown_table(advanced_nonlinear_summary, digits = report_digits, empty_text = empty_md_text),
  markdown_table(advanced_nonlinear_tests, digits = report_digits, empty_text = empty_md_text),
  "### Aβ ceiling sensitivity",
  "",
  markdown_table(abeta_truncation_summary, digits = report_digits, empty_text = empty_md_text),
  markdown_table(abeta_truncation_regression, digits = report_digits, empty_text = empty_md_text),
  markdown_table(abeta_truncation_partial, digits = report_digits, empty_text = empty_md_text),
  tr("sub_figures"),
  "",
  markdown_image("../figures/nonlinear_ChPICV_on_sTREM2_nonlinearity_overall.png", "ChPICV and sTREM2 nonlinearity"),
  ""
)

report_lines <- c(
  report_lines,
  tr("section_sem"),
  "",
  tr("sub_narrative"),
  "",
  make_paragraph_block(sem_narrative, empty_text = empty_md_text),
  "### SEM 并行中介模型",
  "",
  make_paragraph_block(sem_parallel_narrative, empty_text = empty_md_text),
  markdown_table(sem_parallel_display, digits = report_digits, empty_text = empty_md_text),
  "### SEM 多组路径差异检验",
  "",
  make_paragraph_block(sem_multigroup_narrative, empty_text = empty_md_text),
  markdown_table(sem_multigroup_tests_display, digits = report_digits, empty_text = empty_md_text),
  "### SEM 正式调节中介检验",
  "",
  make_paragraph_block(sem_moderated_mediation_narrative, empty_text = empty_md_text),
  markdown_table(sem_moderated_mediation_display, digits = report_digits, empty_text = empty_md_text),
  "### SEM 机制解释表",
  "",
  make_paragraph_block(sem_mechanism_narrative, empty_text = empty_md_text),
  markdown_table(sem_mechanism_display, digits = report_digits, empty_text = empty_md_text),
  "### SEM 中介效应差异检验",
  "",
  make_paragraph_block(sem_difference_narrative, empty_text = empty_md_text),
  markdown_table(localize_sem_difference_table(sem_indirect_difference_tests), digits = report_digits, empty_text = empty_md_text),
  "### SEM 标准化敏感性分析",
  "",
  make_paragraph_block(sem_sensitivity_narrative, empty_text = empty_md_text),
  markdown_table(localize_sem_sensitivity_table(sem_standardization_sensitivity), digits = report_digits, empty_text = empty_md_text),
  sem_model_blocks
)

if (isTRUE(project_config$report$include_appendix)) {
  data_files <- list.files(result_data_clean_dir, full.names = TRUE)
  summary_files <- list.files(result_summary_dir, full.names = TRUE)
  table_files <- list.files(result_tables_dir, full.names = TRUE)
  figure_files <- list.files(result_figures_dir, full.names = TRUE)

  report_lines <- c(
    report_lines,
    tr("section_inventory"),
    "",
    tr("sub_data_files"),
    "",
    markdown_bullet_links(data_files, relative_dir = "../data_clean", empty_text = empty_file_text),
    tr("sub_summary_files"),
    "",
    markdown_bullet_links(summary_files, relative_dir = "../summary", empty_text = empty_file_text),
    tr("sub_table_files"),
    "",
    markdown_bullet_links(table_files, relative_dir = "../tables", empty_text = empty_file_text),
    tr("sub_figure_files"),
    "",
    markdown_bullet_links(figure_files, relative_dir = "../figures", empty_text = empty_file_text),
    tr("sub_analysis_log"),
    "",
    markdown_table(analysis_log, digits = report_digits, empty_text = empty_md_text)
  )
}

ensure_dir(report_dir)
writeLines(report_lines, con = report_md_path, useBytes = TRUE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "08_generate_markdown_report_v2",
  output_files = report_md_path,
  note = paste("Generated an integrated multilingual Markdown report in", report_language, "for the current result version."),
  summary_dir = result_summary_dir
)
