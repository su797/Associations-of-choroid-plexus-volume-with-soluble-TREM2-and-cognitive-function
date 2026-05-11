source(file.path(getwd(), "00_setup.R"))

serial_coverage <- read_result_or_empty(file.path(result_summary_dir, "sem_serial_suppression_coverage.csv"))
serial_multigroup_tests <- read_result_or_empty(file.path(result_summary_dir, "sem_serial_multigroup_tests.csv"))
if (nrow(serial_coverage) == 0) quit(save = "no")

coverage_plot_path <- file.path(result_figures_dir, "Figure_11_sTREM2_serial_coverage.png")
coverage_md_report_path <- file.path(result_report_dir, "SEM_serial_followup_summary.md")
coverage_md_document_path <- file.path(project_root, "document", "SEM串联遮蔽覆盖率说明.md")

coverage_plot_data <- serial_coverage[serial_coverage$single_sem_model %in% c("PTAU_to_ChP_to_Cognition", "TAU_to_ChP_to_Cognition"), , drop = FALSE]
coverage_plot_data$model_label <- ifelse(
  coverage_plot_data$single_sem_model == "PTAU_to_ChP_to_Cognition",
  "P-Tau -> ChP/ICV -> Cognition",
  "Tau -> ChP/ICV -> Cognition"
)
coverage_plot_data$group <- factor(coverage_plot_data$group, levels = c("Overall", "CN", "MCI", "AD"))
coverage_plot_data$group_label <- c("Overall" = "Overall", "CN" = "CN", "MCI" = "MCI", "AD" = "Dementia")[as.character(coverage_plot_data$group)]
coverage_plot_data <- coverage_plot_data[order(coverage_plot_data$model_label, coverage_plot_data$group), , drop = FALSE]

plot_fun <- function() {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(7.2, 5.4, 2.4, 1.8), oma = c(0.2, 0.2, 0.2, 0.2), mgp = c(3.1, 0.95, 0), xpd = NA)

  model_levels <- unique(coverage_plot_data$model_label)
  bar_cols <- c("Single mediator indirect" = "#C7D7E5", "Serial path via sTREM2" = "#2F5D8A")

  for (m in seq_along(model_levels)) {
    model_name <- model_levels[[m]]
    df <- coverage_plot_data[coverage_plot_data$model_label == model_name, , drop = FALSE]
    mat <- rbind(df$indirect, df$serial_indirect)
    colnames(mat) <- df$group_label
    rownames(mat) <- c("Single mediator indirect", "Serial path via sTREM2")

    ymax <- max(abs(mat), na.rm = TRUE)
    if (!is.finite(ymax) || ymax == 0) ymax <- 0.1
    ymax <- ymax * 1.55
    ymin <- min(-0.05, min(mat, na.rm = TRUE) * 1.35)

    mids <- graphics::barplot(
      mat,
      beside = TRUE,
      ylim = c(ymin, ymax),
      col = bar_cols[rownames(mat)],
      border = NA,
      las = 1,
      ylab = "Indirect effect estimate",
      cex.names = 1.02,
      cex.lab = 1.06
    )
    graphics::segments(
      x0 = par("usr")[1],
      y0 = 0,
      x1 = par("usr")[2],
      y1 = 0,
      col = "#8F8F8F",
      lty = 2,
      lwd = 1
    )
    graphics::mtext(LETTERS[m], side = 3, line = -0.6, adj = 0, font = 2, cex = 1.18)
    graphics::mtext(model_name, side = 3, line = 0.2, font = 2, cex = 1.02)

    flat_vals <- as.vector(mat)
    for (i in seq_along(flat_vals)) {
      if (is.na(flat_vals[[i]])) next
      row_id <- ((i - 1) %% nrow(mat)) + 1
      col_id <- ((i - 1) %/% nrow(mat)) + 1
      share <- df$serial_share_of_single_indirect_pct[[col_id]]
      label_y <- if (flat_vals[[i]] >= 0) flat_vals[[i]] + 0.04 * ymax else flat_vals[[i]] - 0.06 * ymax
      label_text <- format_numeric_human(flat_vals[[i]], digits = 3)
      if (row_id == 2 && !is.na(share) && is.finite(share)) {
        label_text <- paste0(label_text, "\n(", format_numeric_human(share, digits = 1), "%)")
      }
      graphics::text(mids[[i]], label_y, labels = label_text, cex = if (row_id == 2) 0.86 else 0.9)
    }

    graphics::legend("top", inset = c(0, -0.02), legend = rownames(mat), fill = bar_cols[rownames(mat)], bty = "n", cex = 0.94)
  }
}

save_plot_file(plot_fun, path = coverage_plot_path, width = 14.2, height = 7.0, dpi = 320)

overall_ptau <- coverage_plot_data[coverage_plot_data$model_label == "P-Tau -> ChP/ICV -> Cognition" & coverage_plot_data$group == "Overall", , drop = FALSE]
overall_tau <- coverage_plot_data[coverage_plot_data$model_label == "Tau -> ChP/ICV -> Cognition" & coverage_plot_data$group == "Overall", , drop = FALSE]
sig_serial_tests <- serial_multigroup_tests[serial_multigroup_tests$path == "serial_indirect" & isTRUE(serial_multigroup_tests$significant), , drop = FALSE]

md_lines <- c(
  "# SEM串联遮蔽覆盖率说明",
  "",
  "该图比较单中介路径的间接效应，以及其中经 sTREM2 串联路径可以解释的部分。",
  "",
  paste0("![Serial coverage](../result/", result_version, "/figures/Figure_11_sTREM2_serial_coverage.png)"),
  "",
  "## 主要结果",
  ""
)
if (nrow(overall_ptau) > 0) {
  md_lines <- c(md_lines, paste0("- Overall 样本中，P-Tau -> ChP/ICV -> Cognition 的单中介间接效应为 ", format_numeric_human(overall_ptau$indirect[[1]], 4), "，其中经 sTREM2 串联路径的间接效应为 ", format_numeric_human(overall_ptau$serial_indirect[[1]], 4), "，覆盖比例为 ", format_numeric_human(overall_ptau$serial_share_of_single_indirect_pct[[1]], 1), "%。"))
}
if (nrow(overall_tau) > 0) {
  md_lines <- c(md_lines, paste0("- Overall 样本中，Tau -> ChP/ICV -> Cognition 的单中介间接效应为 ", format_numeric_human(overall_tau$indirect[[1]], 4), "，其中经 sTREM2 串联路径的间接效应为 ", format_numeric_human(overall_tau$serial_indirect[[1]], 4), "，覆盖比例为 ", format_numeric_human(overall_tau$serial_share_of_single_indirect_pct[[1]], 1), "%。"))
}
if (nrow(sig_serial_tests) == 0) {
  md_lines <- c(md_lines, "- CN、MCI 与 Dementia 之间，串联间接效应未见显著组间差异。")
}

writeLines(enc2utf8(md_lines), con = coverage_md_report_path, useBytes = TRUE)
writeLines(enc2utf8(md_lines), con = coverage_md_document_path, useBytes = TRUE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "10b_generate_serial_followup_outputs",
  output_files = c(coverage_plot_path, coverage_md_report_path, coverage_md_document_path),
  note = "Generated title-free serial coverage figure and concise narrative summary.",
  summary_dir = result_summary_dir
)
