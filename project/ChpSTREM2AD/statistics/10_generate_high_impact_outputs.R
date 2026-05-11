args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  setwd(script_dir)
}

project_root <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
common_r_dir <- normalizePath(file.path(project_root, "..", "..", "common", "R"), winslash = "/", mustWork = TRUE)

source(file.path(common_r_dir, "utils_io.R"))
source(file.path(common_r_dir, "data_processing.R"))
source(file.path(common_r_dir, "association_models.R"))
source(file.path(common_r_dir, "table_utils.R"))
source(file.path(getwd(), "project_config.R"))

write_utf8_lines <- function(lines, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(lines), con = con)
  invisible(path)
}

app_path <- function(path) {
  paste0("/", normalizePath(path, winslash = "/", mustWork = FALSE))
}

read_latest_run_dir <- function(project_root) {
  latest_path <- file.path(project_root, "result", "LATEST.txt")
  latest_lines <- readLines(latest_path, warn = FALSE, encoding = "UTF-8")
  run_dir_line <- latest_lines[grepl("^run_dir=", latest_lines)]
  if (length(run_dir_line) == 0) {
    stop("LATEST.txt does not contain run_dir.", call. = FALSE)
  }
  normalizePath(sub("^run_dir=", "", run_dir_line[1]), winslash = "/", mustWork = TRUE)
}

fmt_p <- function(x, digits = 3) {
  out <- rep("", length(x))
  keep <- !is.na(x)
  out[keep] <- ifelse(
    x[keep] < 0.001,
    "<0.001",
    sprintf(paste0("%.", digits, "f"), round(x[keep], digits))
  )
  out
}

fmt_num <- function(x, digits = 3) {
  out <- rep("", length(x))
  keep <- !is.na(x)
  if (!any(keep)) {
    return(out)
  }
  abs_x <- abs(x[keep])
  sci <- abs_x > 0 & abs_x < 0.001
  out_keep <- rep("", sum(keep))
  out_keep[sci] <- formatC(x[keep][sci], format = "e", digits = 2)
  out_keep[!sci] <- sprintf(paste0("%.", digits, "f"), round(x[keep][!sci], digits))
  out[keep] <- out_keep
  out
}

fmt_mean_sd <- function(x, digits = 2) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return("")
  }
  paste0(fmt_num(mean(x), digits), " (", fmt_num(stats::sd(x), digits), ")")
}

fmt_count_pct <- function(n, total) {
  if (is.na(n) || is.na(total) || total == 0) {
    return("")
  }
  paste0(n, " (", sprintf("%.1f", 100 * n / total), "%)")
}

fmt_effect <- function(beta, p, digits = 3) {
  paste0(fmt_num(beta, digits), " (p=", fmt_p(p), ")")
}

html_table_section <- function(title, data) {
  header_html <- paste(sprintf("<th>%s</th>", html_escape(names(data))), collapse = "")
  body_html <- apply(data, 1, function(row) {
    paste0(
      "<tr>",
      paste(sprintf("<td>%s</td>", html_escape(as.character(row))), collapse = ""),
      "</tr>"
    )
  })

  c(
    sprintf("<h2>%s</h2>", html_escape(title)),
    "<table class='three-line'>",
    sprintf("<thead><tr>%s</tr></thead>", header_html),
    "<tbody>",
    body_html,
    "</tbody>",
    "</table>"
  )
}

write_multi_table_html <- function(path, title, sections) {
  lines <- c(
    "<!doctype html>",
    "<html>",
    "<head>",
    "<meta charset='utf-8'>",
    "<style>",
    "body { font-family: Arial, Helvetica, sans-serif; margin: 24px; color: #111; line-height: 1.45; }",
    "h1 { margin-bottom: 24px; }",
    "h2 { margin-top: 28px; margin-bottom: 10px; font-size: 18px; }",
    "table.three-line { border-collapse: collapse; width: 100%; margin-bottom: 22px; }",
    "table.three-line thead tr { border-top: 2px solid #000; border-bottom: 1.5px solid #000; }",
    "table.three-line tbody tr:last-child { border-bottom: 2px solid #000; }",
    "table.three-line th, table.three-line td { padding: 7px 10px; text-align: left; vertical-align: top; font-size: 13px; }",
    "p.note { margin-top: 6px; margin-bottom: 14px; color: #333; font-size: 12px; }",
    "</style>",
    "</head>",
    "<body>",
    sprintf("<h1>%s</h1>", html_escape(title))
  )

  for (section in sections) {
    lines <- c(lines, html_table_section(section$title, section$data))
    if (!is.null(section$note) && nzchar(section$note)) {
      lines <- c(lines, sprintf("<p class='note'>%s</p>", html_escape(section$note)))
    }
  }

  lines <- c(lines, "</body>", "</html>")
  write_utf8_lines(lines, path)
  invisible(path)
}

save_scatter_with_labels <- function(data, x, y, path, title, xlab, ylab) {
  plot_fun <- function() {
    keep <- stats::complete.cases(data[, c(x, y), drop = FALSE])
    plot_df <- data[keep, , drop = FALSE]
    graphics::par(mar = c(4.6, 4.6, 3.2, 1.2))
    graphics::plot(
      plot_df[[x]],
      plot_df[[y]],
      pch = 19,
      col = grDevices::adjustcolor("#2F5D8A", alpha.f = 0.55),
      cex = 0.9,
      xlab = xlab,
      ylab = ylab,
      main = title,
      cex.main = 1.1,
      cex.lab = 1.1
    )
    if (nrow(plot_df) >= 3) {
      graphics::abline(stats::lm(plot_df[[y]] ~ plot_df[[x]]), col = "#B03A2E", lwd = 2.2)
    }
  }

  save_plot_file(plot_fun, path = path, width = 6.5, height = 5, dpi = 300)
}

sig_stars <- function(p_value) {
  if (is.na(p_value)) {
    return("")
  }
  if (p_value < 0.001) {
    return("***")
  }
  if (p_value < 0.01) {
    return("**")
  }
  if (p_value < 0.05) {
    return("*")
  }
  ""
}

fmt_p_sig <- function(p_value, digits = 3) {
  paste0(fmt_p(p_value, digits = digits), sig_stars(p_value))
}

fit_pair_model <- function(data, outcome, exposure, transformation_table, covariates, factor_vars) {
  fit <- fit_linear_model_base(
    data = data,
    outcome = outcome,
    exposure = exposure,
    covariates = covariates,
    factor_vars = factor_vars,
    transformation_table = transformation_table
  )
  fit[fit$term == "exposure", , drop = FALSE]
}

save_combined_scatter_panel <- function(data, pair_configs, transformation_table, covariates, factor_vars, path) {
  panel_stats <- do.call(
    rbind,
    lapply(pair_configs, function(cfg) {
      row <- fit_pair_model(
        data = data,
        outcome = cfg$outcome,
        exposure = cfg$exposure,
        transformation_table = transformation_table,
        covariates = covariates,
        factor_vars = factor_vars
      )
      data.frame(
        panel = cfg$panel,
        x = cfg$exposure,
        y = cfg$outcome,
        x_label = cfg$xlab,
        y_label = cfg$ylab,
        beta = row$std_beta[[1]],
        p_value = row$p.value[[1]],
        stringsAsFactors = FALSE
      )
    })
  )

  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(2, 2), mar = c(5.0, 5.0, 4.2, 1.4), oma = c(0, 0, 2.4, 0), mgp = c(2.8, 0.9, 0))
    group_cols <- c("CN" = "#3B7EA1", "MCI" = "#E08E2B", "AD" = "#B54D3D")
    group_display <- c("CN" = "CN", "MCI" = "MCI", "AD" = "Dementia")

    for (cfg in pair_configs) {
      keep <- stats::complete.cases(data[, c(cfg$exposure, cfg$outcome, "S_DX_label"), drop = FALSE])
      plot_df <- data[keep, , drop = FALSE]
      plot_cols <- group_cols[as.character(plot_df$S_DX_label)]
      if (any(is.na(plot_cols))) {
        plot_cols[is.na(plot_cols)] <- "#666666"
      }

      graphics::plot(
        plot_df[[cfg$exposure]],
        plot_df[[cfg$outcome]],
        pch = 19,
        col = grDevices::adjustcolor(plot_cols, alpha.f = 0.65),
        cex = 0.8,
        xlab = cfg$xlab,
        ylab = cfg$ylab,
        main = cfg$panel,
        cex.main = 1.08,
        font.main = 2,
        cex.lab = 1.0
      )
      if (nrow(plot_df) >= 3) {
        graphics::abline(stats::lm(plot_df[[cfg$outcome]] ~ plot_df[[cfg$exposure]]), col = "#222222", lwd = 2)
      }

      stat_row <- panel_stats[panel_stats$panel == cfg$panel, , drop = FALSE]
      usr <- graphics::par("usr")
      text_x <- usr[1] + 0.04 * (usr[2] - usr[1])
      text_y <- usr[4] - 0.08 * (usr[4] - usr[3])
        graphics::text(
          x = text_x,
          y = text_y,
          labels = paste0("Adj. beta=", fmt_num(stat_row$beta[[1]], 3), ", p=", fmt_p_sig(stat_row$p_value[[1]])),
          adj = c(0, 1),
          cex = 0.85,
          font = ifelse(!is.na(stat_row$p_value[[1]]) && stat_row$p_value[[1]] < 0.05, 2, 1)
        )

      graphics::legend(
        "topright",
        inset = 0.02,
        legend = unname(group_display[c("CN", "MCI", "AD")]),
        col = grDevices::adjustcolor(unname(group_cols[c("CN", "MCI", "AD")]), alpha.f = 0.85),
        pch = 19,
        bty = "n",
        cex = 0.85
      )
    }
  }

  save_plot_file(plot_fun, path = path, width = 10.5, height = 8.5, dpi = 320)
  panel_stats
}

save_stage_regression_panel <- function(data, pair_configs, transformation_table, covariates, factor_vars, path) {
  stage_levels <- c("Overall", "CN", "MCI", "AD")
  group_cols <- c("CN" = "#3B7EA1", "MCI" = "#E08E2B", "AD" = "#B54D3D")
  stage_display <- c("Overall" = "Overall", "CN" = "CN", "MCI" = "MCI", "AD" = "Dementia")
  legend_display <- c("CN" = "CN", "MCI" = "MCI", "AD" = "Dementia")

  stats_rows <- list()
  idx <- 1

  for (cfg in pair_configs) {
    for (stage in stage_levels) {
      stage_data <- if (stage == "Overall") {
        data
      } else {
        data[data$S_DX_label == stage, , drop = FALSE]
      }

      keep <- stats::complete.cases(stage_data[, c(cfg$exposure, cfg$outcome), drop = FALSE])
      analysis_n <- sum(keep)
      fit_row <- NULL

      if (analysis_n >= 8) {
        fit_row <- tryCatch(
          fit_pair_model(
            data = stage_data,
            outcome = cfg$outcome,
            exposure = cfg$exposure,
            transformation_table = transformation_table,
            covariates = covariates,
            factor_vars = factor_vars
          ),
          error = function(e) NULL
        )
      }

      stats_rows[[idx]] <- data.frame(
        panel = cfg$panel,
        panel_title = cfg$title,
        stage = stage,
        x = cfg$exposure,
        y = cfg$outcome,
        x_label = cfg$xlab,
        y_label = cfg$ylab,
        n = analysis_n,
        beta = if (!is.null(fit_row) && nrow(fit_row) > 0) fit_row$std_beta[[1]] else NA_real_,
        p_value = if (!is.null(fit_row) && nrow(fit_row) > 0) fit_row$p.value[[1]] else NA_real_,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }

  panel_stats <- do.call(rbind, stats_rows)

  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(
      mfrow = c(length(pair_configs), length(stage_levels)),
      mar = c(6.6, 5.8, 4.1, 1.2),
      oma = c(6.8, 10.6, 3.8, 0.8),
      mgp = c(2.6, 0.8, 0),
      cex.axis = 1.32,
      xaxs = "r",
      yaxs = "r"
    )

    for (cfg in pair_configs) {
      all_keep <- stats::complete.cases(data[, c(cfg$exposure, cfg$outcome), drop = FALSE])
      all_df <- data[all_keep, , drop = FALSE]
      xlim <- range(all_df[[cfg$exposure]], na.rm = TRUE)
      ylim <- range(all_df[[cfg$outcome]], na.rm = TRUE)

      for (stage in stage_levels) {
        stage_data <- if (stage == "Overall") {
          data
        } else {
          data[data$S_DX_label == stage, , drop = FALSE]
        }

        keep <- stats::complete.cases(stage_data[, c(cfg$exposure, cfg$outcome, "S_DX_label"), drop = FALSE])
        plot_df <- stage_data[keep, , drop = FALSE]

        plot_cols <- if (stage == "Overall") {
          cols <- group_cols[as.character(plot_df$S_DX_label)]
          cols[is.na(cols)] <- "#666666"
          grDevices::adjustcolor(cols, alpha.f = 0.7)
        } else {
          grDevices::adjustcolor(group_cols[[stage]], alpha.f = 0.75)
        }

        graphics::plot(
          if (nrow(plot_df) > 0) plot_df[[cfg$exposure]] else NA,
          if (nrow(plot_df) > 0) plot_df[[cfg$outcome]] else NA,
          pch = 19,
          col = if (nrow(plot_df) > 0) plot_cols else NA,
          cex = 0.95,
          xlab = cfg$xlab,
          ylab = if (stage == "Overall") cfg$ylab else "",
          xlim = xlim,
          ylim = ylim,
          main = if (cfg$panel == "A") stage_display[[stage]] else "",
          cex.main = 1.88,
          font.main = 2,
          cex.lab = 1.62
        )

        if (nrow(plot_df) >= 3) {
          line_col <- if (stage == "Overall") "#222222" else group_cols[[stage]]
          graphics::abline(stats::lm(plot_df[[cfg$outcome]] ~ plot_df[[cfg$exposure]]), col = line_col, lwd = 2.5)
        }

        if (stage == "Overall") {
          graphics::mtext(
            paste0(cfg$panel, ". ", cfg$title),
            side = 2,
            line = 7.2,
            cex = 1.24,
            font = 2
          )
        }

        stat_row <- panel_stats[panel_stats$panel == cfg$panel & panel_stats$stage == stage, , drop = FALSE]
        usr <- graphics::par("usr")
        text_x <- usr[1] + 0.04 * (usr[2] - usr[1])
        text_y <- usr[4] - 0.08 * (usr[4] - usr[3])
        stat_label <- if (!is.na(stat_row$beta[[1]])) {
          paste0("beta=", fmt_num(stat_row$beta[[1]], 3), ", p=", fmt_p_sig(stat_row$p_value[[1]]))
        } else {
          paste0("n=", stat_row$n[[1]], ", model unavailable")
        }
          graphics::text(
            x = text_x,
            y = text_y,
            labels = stat_label,
            adj = c(0, 1),
          cex = 1.36,
          font = ifelse(!is.na(stat_row$p_value[[1]]) && stat_row$p_value[[1]] < 0.05, 2, 1)
        )

        if (stage == "Overall") {
          graphics::legend(
            "topright",
            inset = 0.02,
            legend = unname(legend_display[c("CN", "MCI", "AD")]),
            col = grDevices::adjustcolor(unname(group_cols[c("CN", "MCI", "AD")]), alpha.f = 0.85),
            pch = 19,
            bty = "n",
            cex = 1.30,
            pt.cex = 1.38
          )
        } else {
          graphics::legend(
            "topright",
            inset = 0.02,
            legend = stage_display[[stage]],
            col = grDevices::adjustcolor(group_cols[[stage]], alpha.f = 0.85),
            pch = 19,
            bty = "n",
            cex = 1.30,
            pt.cex = 1.38
          )
        }
      }
    }
  }

  save_plot_file(plot_fun, path = path, width = 21.0, height = 19.6, dpi = 320)
  panel_stats
}

build_partial_heatmap_table <- function(data, vars, labels, transformation_table, covariates, factor_vars) {
  out <- list()
  idx <- 1
  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      if (i == j) {
        next
      }
      row <- run_partial_correlation(
        data = data,
        x_var = vars[[j]],
        y_var = vars[[i]],
        covariates = covariates,
        factor_vars = factor_vars,
        transformation_table = transformation_table
      )
      out[[idx]] <- data.frame(
        row_var = vars[[i]],
        col_var = vars[[j]],
        row_label = labels[[i]],
        col_label = labels[[j]],
        estimate = row$estimate[[1]],
        p_value = row$p_value[[1]],
        stars = sig_stars(row$p_value[[1]]),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  do.call(rbind, out)
}

save_partial_heatmap <- function(heatmap_df, labels, path) {
  n_vars <- length(labels)
  mat <- matrix(NA_real_, nrow = n_vars, ncol = n_vars, dimnames = list(labels, labels))
  p_mat <- matrix(NA_real_, nrow = n_vars, ncol = n_vars, dimnames = list(labels, labels))

  for (i in seq_len(nrow(heatmap_df))) {
    row <- heatmap_df[i, , drop = FALSE]
    mat[row$row_label[[1]], row$col_label[[1]]] <- row$estimate[[1]]
    p_mat[row$row_label[[1]], row$col_label[[1]]] <- row$p_value[[1]]
  }
  diag(mat) <- NA_real_
  diag(p_mat) <- NA_real_

  color_limit <- max(abs(mat), na.rm = TRUE)
  if (!is.finite(color_limit) || color_limit <= 0) {
    color_limit <- 0.5
  }
  palette_colors <- grDevices::colorRampPalette(c("#4EA3D8", "#F7F7F7", "#F2A65A"))(101)

  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))

    graphics::par(mar = c(7.5, 8.5, 4.0, 1.4), mgp = c(3.1, 0.9, 0))
    z <- t(mat[nrow(mat):1, , drop = FALSE])
    graphics::image(
      x = seq_len(n_vars),
      y = seq_len(n_vars),
      z = z,
      col = palette_colors,
      zlim = c(-color_limit, color_limit),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      main = "",
      cex.main = 1.18,
      font.main = 2
    )
    graphics::axis(1, at = seq_len(n_vars), labels = labels, las = 1, tick = FALSE, cex.axis = 0.9)
    graphics::axis(2, at = seq_len(n_vars), labels = rev(labels), las = 2, tick = FALSE, cex.axis = 0.9)
    for (k in seq_len(n_vars + 1)) {
      graphics::abline(v = k - 0.5, col = "#FFFFFF", lwd = 1)
      graphics::abline(h = k - 0.5, col = "#FFFFFF", lwd = 1)
    }

    for (row_idx in seq_len(n_vars)) {
      for (col_idx in seq_len(n_vars)) {
        value <- mat[row_idx, col_idx]
        if (is.na(value)) {
          next
        }
        plot_y <- n_vars - row_idx + 1
        label_text <- paste0(sprintf("%.2f", value), sig_stars(p_mat[row_idx, col_idx]))
        graphics::text(
          x = col_idx,
          y = plot_y,
          labels = label_text,
          cex = 0.8,
          font = ifelse(!is.na(p_mat[row_idx, col_idx]) && p_mat[row_idx, col_idx] < 0.05, 2, 1)
        )
      }
    }

    graphics::par(mar = c(7.5, 1.5, 4.0, 4.2), mgp = c(3.1, 0.9, 0))
    legend_vals <- seq(-color_limit, color_limit, length.out = 101)
    graphics::image(
      x = c(0, 1),
      y = legend_vals,
      z = matrix(rep(legend_vals, each = 2), nrow = 2),
      col = palette_colors,
      xaxt = "n",
      xlab = "",
      ylab = ""
    )
    graphics::axis(4, las = 2, cex.axis = 0.85, lwd = 0, lwd.ticks = 1)
    graphics::mtext("Adjusted r", side = 4, line = 2.2, cex = 0.9, font = 2)
  }

  save_plot_file(plot_fun, path = path, width = 10.5, height = 8.0, dpi = 320)
}

get_model_term <- function(df, model_name, term_name) {
  row <- df[df$model_name == model_name & df$term == term_name, , drop = FALSE]
  if (nrow(row) == 0) {
    return(NULL)
  }
  row[1, , drop = FALSE]
}

run_dir <- read_latest_run_dir(project_root)
run_id <- basename(run_dir)
summary_dir <- file.path(run_dir, "summary")
tables_dir <- ensure_dir(file.path(run_dir, "tables"))
report_dir <- ensure_dir(file.path(run_dir, "report"))
figures_dir <- ensure_dir(file.path(run_dir, "figures"))
document_dir <- ensure_dir(file.path(project_root, "document"))

analysis_data <- read_project_data(file.path(run_dir, "data_clean", "ChpSTREM2AD_analysis_dataset.csv"))
analysis_data$S_DX_label <- factor(as.character(analysis_data$S_DX_label), levels = c("CN", "MCI", "AD"))
transformation_table <- read_project_data(file.path(summary_dir, "normality_screening.csv"))
covariates <- project_config$variables$covariates
factor_vars <- project_config$variables$categorical_covariates

group_comp <- read_project_data(file.path(summary_dir, "group_comparisons_overall.csv"))
chp_linear <- read_project_data(file.path(summary_dir, "chp_strem2_linear_overall.csv"))
advanced_models <- read_project_data(file.path(summary_dir, "advanced_biomarker_adjusted_overall.csv"))
interaction_comp <- read_project_data(file.path(summary_dir, "advanced_interaction_comparison.csv"))
interaction_terms <- read_project_data(file.path(summary_dir, "advanced_interaction_terms.csv"))
nonlinear_tests <- read_project_data(file.path(summary_dir, "advanced_nonlinear_tests.csv"))
sem_summary <- read_project_data(file.path(summary_dir, "sem_mediation_summary.csv"))
sem_fit <- read_project_data(file.path(summary_dir, "sem_model_fit.csv"))
cognition_fit <- read_project_data(file.path(summary_dir, "cognition_model_selection_fit.csv"))
multigroup_tests <- read_project_data(file.path(summary_dir, "sem_multigroup_path_tests.csv"))
mechanism_table <- read_project_data(file.path(summary_dir, "sem_mechanism_interpretation.csv"))
tau_linear <- read_project_data(file.path(summary_dir, "tau_linear_overall.csv"))

group_p <- setNames(group_comp$p_value, group_comp$variable)
group_sizes <- c(
  Overall = nrow(analysis_data),
  CN = sum(analysis_data$S_DX_label == "CN", na.rm = TRUE),
  MCI = sum(analysis_data$S_DX_label == "MCI", na.rm = TRUE),
  AD = sum(analysis_data$S_DX_label == "AD", na.rm = TRUE)
)

group_slice <- function(group_name) {
  if (group_name == "Overall") {
    return(analysis_data)
  }
  analysis_data[analysis_data$S_DX_label == group_name, , drop = FALSE]
}

continuous_specs <- list(
  list(label = "Age, years", var = "S_AGE", digits = 2, p_key = "S_AGE"),
  list(label = "sTREM2", var = "MSD_STREM2CORRECTED", digits = 2, p_key = "MSD_STREM2CORRECTED"),
  list(label = "ChP/ICV", var = "ChPICV", digits = 4, p_key = "ChPICV"),
  list(label = "Aβ", var = "S_ABETA", digits = 2, p_key = "S_ABETA"),
  list(label = "Tau", var = "TAU", digits = 2, p_key = "TAU"),
  list(label = "P-Tau", var = "PTAU", digits = 2, p_key = "PTAU")
)

table1_rows <- list(
  data.frame(
    Characteristic = "N",
    Overall = as.character(group_sizes["Overall"]),
    CN = as.character(group_sizes["CN"]),
    MCI = as.character(group_sizes["MCI"]),
    AD = as.character(group_sizes["AD"]),
    `P value` = "",
    stringsAsFactors = FALSE
  ),
  data.frame(
    Characteristic = "Female sex, n (%)",
    Overall = fmt_count_pct(sum(analysis_data$S_PTGENDER_label == "Female", na.rm = TRUE), nrow(analysis_data)),
    CN = fmt_count_pct(sum(group_slice("CN")$S_PTGENDER_label == "Female", na.rm = TRUE), nrow(group_slice("CN"))),
    MCI = fmt_count_pct(sum(group_slice("MCI")$S_PTGENDER_label == "Female", na.rm = TRUE), nrow(group_slice("MCI"))),
    AD = fmt_count_pct(sum(group_slice("AD")$S_PTGENDER_label == "Female", na.rm = TRUE), nrow(group_slice("AD"))),
    `P value` = fmt_p(group_p[["S_PTGENDER"]]),
    stringsAsFactors = FALSE
  )
)

for (spec in continuous_specs) {
  table1_rows[[length(table1_rows) + 1]] <- data.frame(
    Characteristic = spec$label,
    Overall = fmt_mean_sd(analysis_data[[spec$var]], digits = spec$digits),
    CN = fmt_mean_sd(group_slice("CN")[[spec$var]], digits = spec$digits),
    MCI = fmt_mean_sd(group_slice("MCI")[[spec$var]], digits = spec$digits),
    AD = fmt_mean_sd(group_slice("AD")[[spec$var]], digits = spec$digits),
    `P value` = fmt_p(group_p[[spec$p_key]]),
    stringsAsFactors = FALSE
  )
}

table1 <- do.call(rbind, table1_rows)
names(table1) <- c("Characteristic", "Overall", "CN", "MCI", "AD", "P value")

primary_row <- get_model_term(chp_linear, "ChPICV_on_sTREM2", "exposure")
reverse_row <- get_model_term(chp_linear, "sTREM2_on_ChPICV", "exposure")
ptaubeta_s_row <- get_model_term(advanced_models, "ChPICV_on_sTREM2_PTAU_ABETA", "exposure")
ptaubeta_p_row <- get_model_term(advanced_models, "ChPICV_on_sTREM2_PTAU_ABETA", "PTAU")
ptaubeta_a_row <- get_model_term(advanced_models, "ChPICV_on_sTREM2_PTAU_ABETA", "S_ABETA")
taubeta_s_row <- get_model_term(advanced_models, "ChPICV_on_sTREM2_TAU_ABETA", "exposure")
taubeta_t_row <- get_model_term(advanced_models, "ChPICV_on_sTREM2_TAU_ABETA", "TAU")
taubeta_a_row <- get_model_term(advanced_models, "ChPICV_on_sTREM2_TAU_ABETA", "S_ABETA")

table2 <- data.frame(
  Model = c(
    "Primary association model",
    "Reverse association model",
    "Biomarker-adjusted model",
    "Biomarker-adjusted model",
    "Biomarker-adjusted model",
    "Tau-adjusted model",
    "Tau-adjusted model",
    "Tau-adjusted model"
  ),
  Outcome = c(
    "ChP/ICV",
    "sTREM2",
    "ChP/ICV",
    "ChP/ICV",
    "ChP/ICV",
    "ChP/ICV",
    "ChP/ICV",
    "ChP/ICV"
  ),
  `Key term` = c(
    "sTREM2",
    "ChP/ICV",
    "sTREM2",
    "P-Tau",
    "Aβ",
    "sTREM2",
    "Tau",
    "Aβ"
  ),
  `β (95% CI)` = c(
    paste0(fmt_num(primary_row$estimate), " (", fmt_num(primary_row$conf.low), " to ", fmt_num(primary_row$conf.high), ")"),
    paste0(fmt_num(reverse_row$estimate), " (", fmt_num(reverse_row$conf.low), " to ", fmt_num(reverse_row$conf.high), ")"),
    paste0(fmt_num(ptaubeta_s_row$estimate), " (", fmt_num(ptaubeta_s_row$conf.low), " to ", fmt_num(ptaubeta_s_row$conf.high), ")"),
    paste0(fmt_num(ptaubeta_p_row$estimate), " (", fmt_num(ptaubeta_p_row$conf.low), " to ", fmt_num(ptaubeta_p_row$conf.high), ")"),
    paste0(fmt_num(ptaubeta_a_row$estimate), " (", fmt_num(ptaubeta_a_row$conf.low), " to ", fmt_num(ptaubeta_a_row$conf.high), ")"),
    paste0(fmt_num(taubeta_s_row$estimate), " (", fmt_num(taubeta_s_row$conf.low), " to ", fmt_num(taubeta_s_row$conf.high), ")"),
    paste0(fmt_num(taubeta_t_row$estimate), " (", fmt_num(taubeta_t_row$conf.low), " to ", fmt_num(taubeta_t_row$conf.high), ")"),
    paste0(fmt_num(taubeta_a_row$estimate), " (", fmt_num(taubeta_a_row$conf.low), " to ", fmt_num(taubeta_a_row$conf.high), ")")
  ),
  `P value` = c(
    fmt_p(primary_row$p.value),
    fmt_p(reverse_row$p.value),
    fmt_p(ptaubeta_s_row$p.value),
    fmt_p(ptaubeta_p_row$p.value),
    fmt_p(ptaubeta_a_row$p.value),
    fmt_p(taubeta_s_row$p.value),
    fmt_p(taubeta_t_row$p.value),
    fmt_p(taubeta_a_row$p.value)
  ),
  stringsAsFactors = FALSE
)
names(table2) <- c("Model", "Outcome", "Key term", "Beta (95% CI)", "P value")

table3 <- data.frame(
  Analysis = c(
    "Global interaction test",
    "sTREM2 × MCI",
    "sTREM2 × AD",
    "Linear vs quadratic",
    "Linear vs spline"
  ),
  `Test statistic` = c(
    fmt_num(interaction_comp$interaction_f[1]),
    fmt_num(interaction_terms$statistic[interaction_terms$term == "exposure:diagnosisMCI"][1]),
    fmt_num(interaction_terms$statistic[interaction_terms$term == "exposure:diagnosisAD"][1]),
    fmt_num(nonlinear_tests$statistic[nonlinear_tests$test_name == "linear_vs_quadratic"][1]),
    fmt_num(nonlinear_tests$statistic[nonlinear_tests$test_name == "linear_vs_spline"][1])
  ),
  `P value` = c(
    fmt_p(interaction_comp$interaction_p_value[1]),
    fmt_p(interaction_terms$p.value[interaction_terms$term == "exposure:diagnosisMCI"][1]),
    fmt_p(interaction_terms$p.value[interaction_terms$term == "exposure:diagnosisAD"][1]),
    fmt_p(nonlinear_tests$p_value[nonlinear_tests$test_name == "linear_vs_quadratic"][1]),
    fmt_p(nonlinear_tests$p_value[nonlinear_tests$test_name == "linear_vs_spline"][1])
  ),
  Interpretation = c(
    "No significant stage-dependent interaction.",
    "No significant deviation from the CN slope.",
    "No significant deviation from the CN slope.",
    "Quadratic nonlinearity not supported.",
    "Spline model showed a trend but did not surpass the linear model."
  ),
  stringsAsFactors = FALSE
)
names(table3) <- c("Analysis", "Test statistic", "P value", "Interpretation")

table4 <- data.frame(
  `Cognitive construct` = cognition_fit$cognition_label_en,
  Indicators = cognition_fit$components,
  CFI = fmt_num(cognition_fit$cfi, 3),
  TLI = fmt_num(cognition_fit$tli, 3),
  RMSEA = fmt_num(cognition_fit$rmsea, 3),
  SRMR = fmt_num(cognition_fit$srmr, 3),
  Fit = cognition_fit$model_reasonable,
  `Primary SEM` = ifelse(cognition_fit$selected_for_main_sem, "Yes", "No"),
  stringsAsFactors = FALSE
)
names(table4) <- c("Cognitive construct", "Indicators", "CFI", "TLI", "RMSEA", "SRMR", "Fit", "Primary SEM")

model_label_map <- c(
  ChP_to_sTREM2_to_Cognition = "ChP/ICV -> sTREM2 -> Cognition",
  sTREM2_to_ChP_to_Cognition = "sTREM2 -> ChP/ICV -> Cognition",
  ChP_to_TAU_to_Cognition = "ChP/ICV -> Tau -> Cognition",
  TAU_to_ChP_to_Cognition = "Tau -> ChP/ICV -> Cognition",
  ChP_to_PTAU_to_Cognition = "ChP/ICV -> P-Tau -> Cognition",
  PTAU_to_ChP_to_Cognition = "P-Tau -> ChP/ICV -> Cognition",
  ChP_to_ABETA_to_Cognition = "ChP/ICV -> Aβ -> Cognition",
  ABETA_to_ChP_to_Cognition = "Aβ -> ChP/ICV -> Cognition"
)

pattern_label <- function(x) {
  ifelse(
    x == "consistent_mediation",
    "Facilitative mediation",
    ifelse(
      x == "inconsistent_mediation",
      "Suppression / inconsistent mediation",
      "No significant mediation"
    )
  )
}

overall_sem <- merge(
  sem_summary[sem_summary$group == "Overall", , drop = FALSE],
  sem_fit[sem_fit$group == "Overall", c("sem_model", "group", "cfi", "rmsea", "model_reasonable"), drop = FALSE],
  by = c("sem_model", "group"),
  all.x = TRUE
)
overall_sem <- overall_sem[match(names(model_label_map), overall_sem$sem_model), , drop = FALSE]

table5 <- data.frame(
  Model = unname(model_label_map[overall_sem$sem_model]),
  N = overall_sem$n,
  `Indirect β (p)` = mapply(fmt_effect, overall_sem$indirect, overall_sem$indirect_p, MoreArgs = list(digits = 3)),
  `Direct β (p)` = mapply(fmt_effect, overall_sem$direct, overall_sem$direct_p, MoreArgs = list(digits = 3)),
  `Total β (p)` = mapply(fmt_effect, overall_sem$total, overall_sem$total_p, MoreArgs = list(digits = 3)),
  Pattern = pattern_label(overall_sem$mediation_type),
  CFI = fmt_num(overall_sem$cfi, 3),
  RMSEA = fmt_num(overall_sem$rmsea, 3),
  stringsAsFactors = FALSE
)
names(table5) <- c("Model", "N", "Indirect Beta (p)", "Direct Beta (p)", "Total Beta (p)", "Pattern", "CFI", "RMSEA")

stage_sem <- merge(
  sem_summary[sem_summary$group != "Overall", , drop = FALSE],
  sem_fit[sem_fit$group != "Overall", c("sem_model", "group", "cfi", "rmsea", "model_reasonable"), drop = FALSE],
  by = c("sem_model", "group"),
  all.x = TRUE
)
stage_sem <- stage_sem[stage_sem$indirect_p < 0.05 | stage_sem$direct_p < 0.05 | stage_sem$total_p < 0.05, , drop = FALSE]
stage_sem <- stage_sem[match(stage_sem$sem_model, names(model_label_map)), , drop = FALSE]
stage_sem <- stage_sem[order(match(stage_sem$group, c("CN", "MCI", "AD")), match(stage_sem$sem_model, names(model_label_map))), , drop = FALSE]

table_s1 <- data.frame(
  Group = stage_sem$group,
  Model = unname(model_label_map[stage_sem$sem_model]),
  `Indirect β (p)` = mapply(fmt_effect, stage_sem$indirect, stage_sem$indirect_p, MoreArgs = list(digits = 3)),
  `Direct β (p)` = mapply(fmt_effect, stage_sem$direct, stage_sem$direct_p, MoreArgs = list(digits = 3)),
  `Total β (p)` = mapply(fmt_effect, stage_sem$total, stage_sem$total_p, MoreArgs = list(digits = 3)),
  Pattern = pattern_label(stage_sem$mediation_type),
  CFI = fmt_num(stage_sem$cfi, 3),
  RMSEA = fmt_num(stage_sem$rmsea, 3),
  stringsAsFactors = FALSE
)
table_s1 <- unique(table_s1)
names(table_s1) <- c("Group", "Model", "Indirect Beta (p)", "Direct Beta (p)", "Total Beta (p)", "Pattern", "CFI", "RMSEA")

significant_multigroup <- multigroup_tests[
  multigroup_tests$significant |
    (multigroup_tests$scope == "omnibus" & multigroup_tests$p_value < 0.10),
  ,
  drop = FALSE
]

table_s2 <- data.frame(
  Model = unname(model_label_map[significant_multigroup$sem_model]),
  Path = significant_multigroup$path,
  Scope = significant_multigroup$scope,
  Comparison = ifelse(is.na(significant_multigroup$group_pair), "CN vs MCI vs AD", significant_multigroup$group_pair),
  Statistic = fmt_num(significant_multigroup$statistic, 3),
  df = significant_multigroup$df,
  `P value` = vapply(significant_multigroup$p_value, fmt_p, character(1)),
  Significant = ifelse(significant_multigroup$significant, "Yes", "Trend"),
  stringsAsFactors = FALSE
)
table_s2 <- unique(table_s2)
names(table_s2) <- c("Model", "Path", "Scope", "Comparison", "Statistic", "df", "P value", "Significant")

key_tables_html <- file.path(tables_dir, "Key_three_line_tables.html")
write_multi_table_html(
  path = key_tables_html,
  title = "Key Three-Line Tables for ChP/ICV, sTREM2, Tau, P-Tau, Aβ, and Latent SEM",
  sections = list(
    list(
      title = "Table 1. Baseline characteristics by diagnosis",
      data = table1,
      note = "Values are shown as mean ± SD or n (%). P values represent global between-group comparisons."
    ),
    list(
      title = "Table 2. Primary and biomarker-adjusted regression models",
      data = table2,
      note = "All regression models were adjusted for sex, age, education, and APOE4 status."
    ),
    list(
      title = "Table 3. Stage interaction and nonlinearity tests",
      data = table3,
      note = "The stage interaction model tested sTREM2 × diagnosis; nonlinear models compared linear, quadratic, and spline specifications."
    ),
    list(
      title = "Table 4. Fit comparison of candidate cognition constructs",
      data = table4,
      note = "MMSE + MoCA was retained as the primary cognition construct because it yielded the most stable overall fit."
    ),
    list(
      title = "Table 5. Overall latent SEM decomposition",
      data = table5,
      note = "Pattern labels indicate whether the indirect pathway was facilitative or suppressive relative to the direct path."
    ),
    list(
      title = "Table S1. Stage-specific latent SEM results",
      data = table_s1,
      note = "Only stage-specific models with at least one significant indirect, direct, or total effect are displayed."
    ),
    list(
      title = "Table S2. Multigroup SEM path-difference tests",
      data = table_s2,
      note = "Significant rows and omnibus trends (P < 0.10) are displayed for concise presentation."
    )
  )
)

figure1_path <- file.path(figures_dir, "Figure_1_sTREM2_ChPICV_overall.png")
figure2_path <- file.path(figures_dir, "Figure_2_Tau_ChPICV_overall.png")
figure3_path <- file.path(figures_dir, "Figure_3_PTau_ChPICV_overall.png")
figure4_path <- file.path(figures_dir, "Figure_4_ABeta_ChPICV_overall.png")
figure5_path <- file.path(figures_dir, "Figure_5_combined_regression_panel.png")
figure5b_path <- file.path(figures_dir, "Figure_5b_stage_regression_panel.png")
figure6_path <- file.path(figures_dir, "Figure_6_adjusted_partial_heatmap.png")
scatter_panel_csv <- file.path(summary_dir, "combined_regression_panel_stats.csv")
scatter_stage_panel_csv <- file.path(summary_dir, "combined_regression_stage_panel_stats.csv")
heatmap_csv <- file.path(summary_dir, "association_heatmap_partial_correlations.csv")

save_scatter_with_labels(
  analysis_data,
  x = "MSD_STREM2CORRECTED",
  y = "ChPICV",
  path = figure1_path,
  title = "Overall association between sTREM2 and ChP/ICV",
  xlab = "sTREM2",
  ylab = "ChP/ICV"
)
save_scatter_with_labels(
  analysis_data,
  x = "TAU",
  y = "ChPICV",
  path = figure2_path,
  title = "Overall association between Tau and ChP/ICV",
  xlab = "Tau",
  ylab = "ChP/ICV"
)
save_scatter_with_labels(
  analysis_data,
  x = "PTAU",
  y = "ChPICV",
  path = figure3_path,
  title = "Overall association between P-Tau and ChP/ICV",
  xlab = "P-Tau",
  ylab = "ChP/ICV"
)
save_scatter_with_labels(
  analysis_data,
  x = "S_ABETA",
  y = "ChPICV",
  path = figure4_path,
  title = "Overall association between Aβ and ChP/ICV",
  xlab = "Aβ",
  ylab = "ChP/ICV"
)

scatter_pair_configs <- list(
  list(panel = "A", title = "sTREM2 vs ChP/ICV", exposure = "MSD_STREM2CORRECTED", outcome = "ChPICV", xlab = "sTREM2", ylab = "ChP/ICV"),
  list(panel = "B", title = "Tau vs ChP/ICV", exposure = "TAU", outcome = "ChPICV", xlab = "Tau", ylab = "ChP/ICV"),
  list(panel = "C", title = "P-Tau vs ChP/ICV", exposure = "PTAU", outcome = "ChPICV", xlab = "P-Tau", ylab = "ChP/ICV"),
  list(panel = "D", title = "Aβ vs ChP/ICV", exposure = "S_ABETA", outcome = "ChPICV", xlab = "Aβ", ylab = "ChP/ICV")
)

scatter_panel_stats <- save_combined_scatter_panel(
  data = analysis_data,
  pair_configs = scatter_pair_configs,
  transformation_table = transformation_table,
  covariates = covariates,
  factor_vars = factor_vars,
  path = figure5_path
)
write_csv_utf8(scatter_panel_stats, scatter_panel_csv, row.names = FALSE)

scatter_stage_panel_stats <- save_stage_regression_panel(
  data = analysis_data,
  pair_configs = scatter_pair_configs,
  transformation_table = transformation_table,
  covariates = covariates,
  factor_vars = factor_vars,
  path = figure5b_path
)
write_csv_utf8(scatter_stage_panel_stats, scatter_stage_panel_csv, row.names = FALSE)

heatmap_vars <- c("ChPICV", "MSD_STREM2CORRECTED", "TAU", "PTAU", "S_ABETA", "MMSE", "MOCA")
heatmap_labels <- c("ChP/ICV", "sTREM2", "Tau", "P-Tau", "Aβ", "MMSE", "MoCA")
heatmap_df <- build_partial_heatmap_table(
  data = analysis_data,
  vars = heatmap_vars,
  labels = heatmap_labels,
  transformation_table = transformation_table,
  covariates = covariates,
  factor_vars = factor_vars
)
write_csv_utf8(heatmap_df, heatmap_csv, row.names = FALSE)
save_partial_heatmap(heatmap_df, labels = heatmap_labels, path = figure6_path)

sem_combined_path <- normalizePath(file.path(figures_dir, "sem_combined_Cog_MMSE_MOCA.png"), winslash = "/", mustWork = FALSE)

abstract_results <- paste(
  "A total of 735 participants were included (CN=225, MCI=380, AD=130).",
  "ChP/ICV was inversely associated with sTREM2 in the overall sample (β=-0.081, P<0.001), and this inverse association remained evident in CN and MCI but not in AD.",
  "Tau and P-Tau both showed inverse associations with ChP/ICV, while sTREM2 remained positively coupled to Tau-related neuroinflammatory burden in complementary models.",
  "After additional adjustment for P-Tau/Aβ or Tau/Aβ, the sTREM2 term was attenuated to borderline significance, whereas P-Tau, Tau, and Aβ remained independently associated with ChP/ICV.",
  "Among candidate latent cognition structures, MMSE + MoCA provided the most stable fit and was therefore selected as the primary cognition construct.",
  "In latent SEM, sTREM2-, Tau-, and P-Tau-related pathways predominantly showed suppression/inconsistent mediation, whereas ChP/ICV -> Aβ -> cognition showed consistent mediation.",
  "Multigroup SEM indicated that stage heterogeneity was most evident in the sTREM2 -> ChP/ICV -> cognition and P-Tau -> ChP/ICV -> cognition pathways, particularly for MCI versus AD."
)

manuscript_lines <- c(
  "# Choroid Plexus Volume, sTREM2, P-Tau, Aβ, and Cognitive Impairment Across the Alzheimer Disease Spectrum",
  "",
  "## Structured Abstract",
  "",
  "### Background",
  "脉络丛（choroid plexus, ChP）可能同时连接神经免疫、tau/amyloid 病理和认知下降，但其与 sTREM2 及核心阿尔茨海默病（AD）生物学标志物之间的整体关系仍不清楚。",
  "",
  "### Methods",
  "纳入 735 名受试者（CN 225 例、MCI 380 例、AD 130 例）。以 ChP/ICV 作为脉络丛相对体积指标，以 sTREM2、P-Tau 和 Aβ 作为核心生物学标志物。先进行描述统计、组间比较、协变量调整线性回归、biomarker-adjusted 回归、交互和非线性检验；随后比较 4 套候选认知潜变量结构，并以拟合最优的 MMSE + MoCA 作为正式 latent SEM 认知结局，进一步评估总体与分阶段中介结构。",
  "",
  "### Results",
  abstract_results,
  "",
  "### Conclusions",
  "本研究支持 ChP/ICV 与 sTREM2 之间存在跨疾病谱的稳定负相关，但 sTREM2 更可能表现为部分抵消 ChP/ICV 不利认知影响的制衡成分，而非单纯病理放大器。相比之下，Aβ 通路显示出更一致的有害中介特征；P-Tau 通路则表现为阶段依赖的复杂网络效应。",
  "",
  "## Introduction",
  "",
  "脉络丛（ChP）位于脑脊液产生与免疫监视的关键界面，越来越多证据提示其结构改变并非单纯的伴随现象，而可能深度参与阿尔茨海默病连续谱中的神经炎症与认知损害。sTREM2 是反映小胶质细胞活化和髓系免疫应答的重要生物学标志物；P-Tau 与 Aβ 则分别代表 tau 病理与 amyloid 病理的核心轴线。将这些指标与 ChP/ICV 同时纳入分析，有助于回答三个临床与机制层面的关键问题：第一，ChP/ICV 与 sTREM2 是否存在独立且稳定的相关关系；第二，这一关系是否仍能在 P-Tau/Aβ 调整后保留；第三，ChP/ICV 是否通过不同病理通路以相同或相反方向影响认知功能。",
  "",
  "## Methods",
  "",
  "### Study design and participants",
  "本研究基于单中心影像-生物标志物数据集开展横断面统计分析。受试者按照临床诊断分为认知正常（CN）、轻度认知障碍（MCI）和 AD 三组。原始数据来自本项目固定数据文件，并在本次分析中保留 ID、诊断、协变量、ChP/ICV、sTREM2、P-Tau、Aβ、Tau 和认知变量。",
  "",
  "### Biological rationale for biomarker selection",
  "选择 sTREM2 的原因在于其代表神经免疫活化，并可能处于 ChP 结构改变与认知后果之间的关键桥梁。选择 P-Tau 和 Aβ 的原因在于 reviewer 几乎必然要求判断 ChP-sTREM2 关系是否只是经典 AD 病理的替代体现。P-Tau 用于刻画 tau 相关神经退行，Aβ 用于刻画 amyloid 负荷，从而评估 ChP/ICV 与认知下降是否更多经由经典病理轴传递。",
  "",
  "### Statistical analysis",
  "首先清理并限制变量范围，仅在本项目内构建分析数据集。连续变量进行正态性检查，对偏态分布变量执行对数变换或 log1p 变换。随后采用全样本与按诊断分组的描述统计和组间比较；对于年龄与性别失衡，采用 pairwise matching 作为敏感性分析。主回归模型为 ChP/ICV ~ sTREM2 + sex + age + education + APOE4，以及其反向模型 sTREM2 ~ ChP/ICV + 同一协变量。为检验 ChP-sTREM2 关系是否可由经典 AD 病理解释，进一步拟合 ChP/ICV ~ sTREM2 + P-Tau + Aβ + covariates 和 ChP/ICV ~ sTREM2 + Tau + Aβ + covariates。交互模型用于检验 sTREM2 × diagnosis，非线性模型比较 linear、quadratic 与 restricted spline 形式。对于认知潜变量，比较 MMSE + MoCA、mPACC + NegaADAS13、MMSE + MoCA + mPACC 和 mPACC 四种候选结构，并依据 CFI、TLI、RMSEA、SRMR 选择正式 SEM 认知结局。正式 latent SEM 检验 ChP/ICV 与 sTREM2、P-Tau、Aβ 的双向中介结构，并在 CN/MCI/AD 中开展 multigroup 路径差异检验。",
  "",
  "## Results",
  "",
  "### Participant characteristics and diagnostic group differences",
  paste0(
    "全样本共纳入 ", group_sizes["Overall"], " 例，CN/MCI/AD 分别为 ",
    group_sizes["CN"], "/", group_sizes["MCI"], "/", group_sizes["AD"],
    "。总体年龄为 ", table1$Overall[table1$Characteristic == "Age, years"],
    "，女性占 ", table1$Overall[table1$Characteristic == "Female sex, n (%)"], "。"
  ),
  "在诊断组比较中，ChP/ICV、Aβ、Tau 和 P-Tau 均显示显著组间差异，而 sTREM2 本身并无显著诊断组差异。年龄和性别也存在组间不平衡，因此分组结果需结合协变量调整和匹配敏感性分析进行解释。",
  "",
  "### Association between ChP/ICV and sTREM2",
  "在全样本协变量调整线性回归中，sTREM2 与 ChP/ICV 呈显著负相关（β=-0.081，95%CI -0.112 to -0.050，P<0.001）；反向模型同样提示 ChP/ICV 与 sTREM2 呈显著负相关（β=-0.430，95%CI -0.595 to -0.266，P<0.001）。按诊断分组后，该关系主要保留于 CN 和 MCI，在 AD 组中减弱并不再显著，提示 ChP-sTREM2 耦合更可能对应疾病早中期的神经免疫结构反应。",
  "",
  "### Additional analyses incorporating P-Tau and Aβ",
  "当在 ChP/ICV 模型中同时加入 P-Tau 和 Aβ 后，sTREM2 系数衰减至边缘显著（β=-0.033，P=0.072），而 P-Tau（β=-0.084，P<0.001）和 Aβ（β=-0.085，P<0.001）仍保持独立相关。同样，在 Tau + Aβ 调整模型中，sTREM2 亦衰减为边缘显著（β=-0.035，P=0.058），Tau（β=-0.087，P<0.001）和 Aβ（β=-0.077，P<0.001）仍显著。这说明 ChP-sTREM2 关系并非完全独立于核心 AD 病理，但也并不能被其完全替代。",
  "",
  "### Stage interaction and nonlinear analyses",
  "sTREM2 × diagnosis 交互并未达到显著（P=0.514），因此当前数据不足以用单一交互项模型正式证明 stage-dependent association。非线性分析中，spline 模型相对线性模型仅呈趋势性改进（P=0.088），尚不足以作为主结论。",
  "",
  "### Selection of the cognition latent construct",
  "在 4 套候选认知潜变量结构中，MMSE + MoCA 取得最稳定的整体拟合（CFI=0.987, TLI=0.942, RMSEA=0.056, SRMR=0.009），优于 mPACC + NegaADAS13、MMSE + MoCA + mPACC 与 mPACC 模型。其余模型的主要问题在于 RMSEA 偏高，提示测量结构不够理想，因此正式 SEM 采用 MMSE + MoCA 作为认知潜变量。",
  "",
  "### Overall latent SEM",
  "整体 latent SEM 显示，ChP/ICV -> sTREM2 -> cognition 与 sTREM2 -> ChP/ICV -> cognition 两条通路均呈 suppression / inconsistent mediation，即间接路径方向与直接路径方向相反。换言之，sTREM2 更像部分抵消 ChP/ICV 不利认知影响的制衡性成分，而不是单纯放大认知下降的病理中介。相比之下，ChP/ICV -> Aβ -> cognition 为 consistent mediation，提示 Aβ 更符合经典同向病理传递通路。P-Tau 相关通路在总体上亦多表现为 inconsistent mediation，提示 P-Tau 与 ChP/ICV 之间并非简单线性病理链，而可能反映更复杂的网络分解。",
  "",
  "### Stage-specific heterogeneity",
  "分阶段 latent SEM 显示，CN 期大多数中介通路并不显著；MCI 期则出现最清晰的机制性信号，尤其是 sTREM2 -> ChP/ICV -> cognition 与 P-Tau -> ChP/ICV -> cognition；AD 期则以直接效应或弱中介为主。多组路径差异检验进一步证实，sTREM2 -> ChP/ICV -> cognition 的 b 路径存在显著组间差异（omnibus P=0.002），且 MCI vs AD 的间接效应差异达到显著（P=0.040）；P-Tau -> ChP/ICV -> cognition 的 b 路径、c' 路径和间接效应均存在显著组间差异，其中 MCI vs AD 的间接效应差异最为突出（P=0.003）。这说明疾病阶段差异并非平均分布于所有路径，而主要集中在 ChP/ICV 与下游认知之间的耦合方式。",
  "",
  "## Discussion",
  "",
  "### Principal findings",
  "本研究的第一主发现，是 ChP/ICV 与 sTREM2 在全疾病谱中存在稳定负相关，且这一关系主要由 CN 和 MCI 阶段驱动。第二，P-Tau 和 Aβ 调整后 sTREM2 关联减弱，提示 ChP-sTREM2 关系部分嵌入核心 AD 病理网络之中。第三，从 latent SEM 看，sTREM2 相关路径主要表现为 suppression / inconsistent mediation，而 Aβ 路径则更符合 consistent mediation。第四，多组检验证实阶段差异主要集中在 MCI 与 AD 之间，说明中介结构存在实质性病程异质性。",
  "",
  "### Interpretation of sTREM2 relative to P-Tau and Aβ",
  "从机制层面看，sTREM2 不应被简单解释为“更高即更坏”的病理标志物。当前结果更支持 sTREM2 作为一种神经免疫应答或代偿性信号：它与 ChP/ICV 结构改变相关，但在进入 cognition 的路径分解后，多数情况下表现为与直接不利效应方向相反的间接成分。相反，Aβ 更像稳定且方向一致的有害通路；P-Tau 虽然总体上与更差认知相关，但其经由 ChP/ICV 的支路并不总是同向，这提示 P-Tau 与 ChP/ICV 之间可能同时包含结构损害、代偿重排和阶段依赖网络重构。",
  "",
  "### Why stage-specific findings matter",
  "之所以整体模型较清晰而分阶段模型更复杂，一方面是样本量与统计功效在分层后下降，另一方面是不同阶段的生物学关系确实并不相同。MCI 阶段最可能对应结构-炎症-认知耦合重新组织的窗口期，因此在该阶段最容易观测到 sTREM2 和 P-Tau 经由 ChP/ICV 影响 cognition 的路径差异；到了 AD 阶段，系统更可能进入结构破坏和多病理并存状态，导致部分路径信号减弱或转为直接效应主导。",
  "",
  "### Strengths and limitations",
  "本研究的优势在于：在同一分析框架中系统整合了 ChP/ICV、sTREM2、P-Tau、Aβ 和 cognition；同时开展了 biomarker-adjusted regression、interaction/nonlinearity 检验、candidate latent construct selection、overall/stage-specific SEM 和 multigroup path-difference testing。局限性在于：横断面设计限制了因果推断；Aβ 使用的是当前数据库中的单指标而非 Aβ42/40 ratio；部分分阶段模型，尤其 CN 和 AD 的 latent SEM，受样本量和阶段异质性影响而拟合较弱，因此阶段性结论仍应视为机制导向的统计证据，而非最终因果证明。",
  "",
  "## Conclusion",
  "综合来看，ChP/ICV 与 sTREM2 在 AD 连续谱中存在稳定负相关，但 sTREM2 更像部分缓冲或抵消 ChP/ICV 不利认知效应的神经免疫成分，而不是单纯病理放大器。Aβ 通路显示出最稳定的同向有害中介特征；P-Tau 通路则体现出更复杂的阶段依赖网络结构。对于后续高影响因子期刊投稿，最稳健的主线应当围绕“ChP/ICV-sTREM2 negative coupling plus differential mediation by Aβ versus P-Tau”展开。",
  "",
  "## Key Tables and Figures",
  "",
  paste0("- Key three-line tables (English): [open](", app_path(key_tables_html), ")"),
  paste0("- Figure 1. Overall association between sTREM2 and ChP/ICV: ![Figure 1](", app_path(figure1_path), ")"),
  paste0("- Figure 2. Overall association between Tau and ChP/ICV: ![Figure 2](", app_path(figure2_path), ")"),
  paste0("- Figure 3. Overall association between P-Tau and ChP/ICV: ![Figure 3](", app_path(figure3_path), ")"),
  paste0("- Figure 4. Overall association between Aβ and ChP/ICV: ![Figure 4](", app_path(figure4_path), ")"),
  paste0("- Figure 5. Combined regression panel: ![Figure 5](", app_path(figure5_path), ")"),
  paste0("- Figure 5B. Overall plus stage-stratified regression panel: ![Figure 5B](", app_path(figure5b_path), ")"),
  paste0("- Figure 6. Covariate-adjusted partial-correlation heatmap: ![Figure 6](", app_path(figure6_path), ")"),
  paste0("- Figure 7. Combined latent SEM overview (MMSE + MoCA): ![Figure 7](", app_path(sem_combined_path), ")")
)

manuscript_report_path <- file.path(report_dir, "High_Impact_Manuscript_Draft.md")
manuscript_document_path <- file.path(document_dir, "论文整合稿_高影响因子版.md")
assets_path <- file.path(report_dir, "Professor_presentation_assets.md")
japanese_summary_report_path <- file.path(report_dir, "Brief_Japanese_Summary.md")
japanese_summary_document_path <- file.path(document_dir, "指導教員向け_日本語簡略説明.md")

write_utf8_lines(manuscript_lines, manuscript_report_path)
write_utf8_lines(manuscript_lines, manuscript_document_path)

assets_lines <- c(
  "# Professor Presentation Assets",
  "",
  paste0("- Key three-line tables (English): [open](", app_path(key_tables_html), ")"),
  paste0("- Combined regression panel: [open](", app_path(figure5_path), ")"),
  paste0("- Adjusted partial-correlation heatmap: [open](", app_path(figure6_path), ")"),
  "",
  "## Core Figures",
  "",
  paste0("### Figure 1. sTREM2 vs ChP/ICV", "\n", "![Figure 1](", app_path(figure1_path), ")"),
  "",
  paste0("### Figure 2. Tau vs ChP/ICV", "\n", "![Figure 2](", app_path(figure2_path), ")"),
  "",
  paste0("### Figure 3. P-Tau vs ChP/ICV", "\n", "![Figure 3](", app_path(figure3_path), ")"),
  "",
  paste0("### Figure 4. Aβ vs ChP/ICV", "\n", "![Figure 4](", app_path(figure4_path), ")"),
  "",
  paste0("### Figure 5. Combined regression panel", "\n", "![Figure 5](", app_path(figure5_path), ")"),
  "",
  paste0("### Figure 5B. Overall plus stage-stratified regression panel", "\n", "![Figure 5B](", app_path(figure5b_path), ")"),
  "",
  paste0("### Figure 6. Adjusted partial-correlation heatmap", "\n", "![Figure 6](", app_path(figure6_path), ")"),
  "",
  paste0("### Figure 7. Combined latent SEM overview", "\n", "![Figure 7](", app_path(sem_combined_path), ")"),
  "",
  "## One-line take-home messages",
  "",
  "- ChP/ICV and sTREM2 are inversely associated in the overall sample, with the clearest signal in CN and MCI.",
  "- The ChP/ICV-sTREM2 association is attenuated after additional adjustment for P-Tau/Aβ or Tau/Aβ, suggesting partial embedding within the core AD pathological network.",
  "- Aβ provides the most consistent harmful mediation pathway, whereas sTREM2 more often behaves as a suppressive/counterbalancing pathway in latent SEM.",
  "- Stage heterogeneity is most evident for the sTREM2 -> ChP/ICV -> cognition and P-Tau -> ChP/ICV -> cognition pathways, particularly between MCI and AD."
)

write_utf8_lines(assets_lines, assets_path)

japanese_summary_lines <- c(
  "# 指導教員向け簡略説明",
  "",
  "## 主要用語",
  "",
  "- 抑制効果（suppression effect）: 直接効果と間接効果の方向が逆で、媒介経路が主効果を部分的に打ち消す状態。",
  "- 促進的媒介効果（facilitative mediation）: 直接効果と間接効果の方向が一致し、媒介経路が主効果を同方向に伝える状態。",
  "",
  "## 全体像",
  "",
  "- 全体集団では ChP/ICV と sTREM2 は有意な負の関連を示し、この関係は主に CN と MCI で明瞭であった。",
  "- Tau と P-Tau は ChP/ICV と負に関連し、sTREM2 とは正に関連した。したがって、Tau 系病理と ChP/ICV 変化は密接に結びついている。",
  "- Aβ を介する経路は、認知機能低下を一方向に伝える比較的一貫した媒介経路として観察された。",
  "- これに対し、sTREM2・Tau・P-Tau を介する経路では抑制効果（不一致媒介）が多く、病理的な直接経路とは逆向きの媒介経路が同時に存在することが示唆された。",
  "- 多群 SEM では、MCI と AD の間で一部の経路係数差が有意であり、病期によって ChP/ICV・炎症・Tau 病理・認知の結びつき方が変化する可能性が示された。",
  "",
  "## 解釈",
  "",
  "本研究の結果は、ChP/ICV・sTREM2・Tau 病理・Aβ 病理・認知機能の関係が単一の直線的な病理経路ではなく、複数の方向の作用を含むネットワークであることを示す。特に sTREM2 は、ChP/ICV と認知機能の間で一部代償的または制御的に働く可能性があり、Aβ はより一貫して認知低下を媒介する経路として解釈される。"
)

write_utf8_lines(japanese_summary_lines, japanese_summary_report_path)
write_utf8_lines(japanese_summary_lines, japanese_summary_document_path)
