source(file.path(getwd(), "00_setup.R"))

sem_group_estimates <- read_result_or_empty(file.path(result_summary_dir, "sem_multigroup_group_estimates.csv"))
sem_moderated_tests <- read_result_or_empty(file.path(result_summary_dir, "sem_moderated_mediation_summary.csv"))
sem_parallel_summary <- read_result_or_empty(file.path(result_summary_dir, "sem_parallel_mediation_summary.csv"))
sem_parallel_tests <- read_result_or_empty(file.path(result_summary_dir, "sem_parallel_multigroup_tests.csv"))
combined_panel_stats <- read_result_or_empty(file.path(result_summary_dir, "combined_regression_panel_stats.csv"))
association_heatmap <- read_result_or_empty(file.path(result_summary_dir, "association_heatmap_partial_correlations.csv"))

model_label_map <- c(
  ChP_to_sTREM2_to_Cognition = "ChP/ICV -> sTREM2 -> Cognition",
  sTREM2_to_ChP_to_Cognition = "sTREM2 -> ChP/ICV -> Cognition",
  ChP_to_TAU_to_Cognition = "ChP/ICV -> Tau -> Cognition",
  TAU_to_ChP_to_Cognition = "Tau -> ChP/ICV -> Cognition",
  ChP_to_PTAU_to_Cognition = "ChP/ICV -> P-Tau -> Cognition",
  PTAU_to_ChP_to_Cognition = "P-Tau -> ChP/ICV -> Cognition",
  ChP_to_ABETA_to_Cognition = "ChP/ICV -> Abeta -> Cognition",
  ABETA_to_ChP_to_Cognition = "Abeta -> ChP/ICV -> Cognition"
)

parallel_model_label_map <- c(
  ChP_to_sTREM2_PTAU_ABETA_parallel = "ChP/ICV -> sTREM2 + P-Tau + Abeta -> Cognition",
  ChP_to_sTREM2_TAU_ABETA_parallel = "ChP/ICV -> sTREM2 + Tau + Abeta -> Cognition"
)

parallel_short_label_map <- c(
  ChP_to_sTREM2_PTAU_ABETA_parallel = "P-Tau pathway",
  ChP_to_sTREM2_TAU_ABETA_parallel = "Tau pathway"
)

mediator_label_map <- c(
  MSD_STREM2CORRECTED = "sTREM2",
  PTAU = "P-Tau",
  TAU = "Tau",
  S_ABETA = "Abeta"
)

format_p_compact <- function(p_value) {
  if (is.na(p_value)) {
    return("NA")
  }
  if (p_value < 0.001) {
    return("<0.001**")
  }
  suffix <- if (p_value < 0.05) "*" else ""
  paste0(sprintf("%.3f", p_value), suffix)
}

format_beta_compact <- function(beta, p_value) {
  if (is.na(beta)) {
    return("NA")
  }
  suffix <- if (!is.na(p_value) && p_value < 0.001) {
    "**"
  } else if (!is.na(p_value) && p_value < 0.05) {
    "*"
  } else {
    ""
  }
  paste0(sprintf("%.3f", beta), suffix)
}

draw_pvalue_heatmap <- function(mat, row_labels, col_labels, title, path, subtitle = NULL) {
  finite_values <- mat[is.finite(mat)]
  max_scale <- if (length(finite_values) == 0) 3 else max(-log10(pmax(finite_values, 1e-6)), na.rm = TRUE)
  max_scale <- max(max_scale, 1)
  palette_fun <- grDevices::colorRampPalette(c("#4EA3D8", "#F7F7F7", "#F2A65A"))
  palette_cols <- palette_fun(101)

  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(5.2, 0.9))
    graphics::par(mar = c(5.8, 23.5, 4.8, 1.2), xpd = NA)

    nr <- nrow(mat)
    nc <- ncol(mat)
    graphics::plot(
      NA,
      xlim = c(0, nc),
      ylim = c(0, nr),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      bty = "n",
      main = ""
    )

    for (r in seq_len(nr)) {
      for (c in seq_len(nc)) {
        p_value <- mat[r, c]
        fill <- "#f2f2f2"
        if (is.finite(p_value)) {
          scaled <- min(max(-log10(max(p_value, 1e-6)) / max_scale, 0), 1)
          idx <- max(1, min(length(palette_cols), floor(scaled * (length(palette_cols) - 1)) + 1))
          fill <- palette_cols[[idx]]
        }
        x_left <- c - 1
        x_right <- c
        y_bottom <- nr - r
        y_top <- nr - r + 1
        graphics::rect(x_left, y_bottom, x_right, y_top, col = fill, border = "white", lwd = 1.5)
        label <- if (is.finite(p_value)) format_p_compact(p_value) else ""
        graphics::text(c - 0.5, nr - r + 0.5, labels = label, cex = 1.02)
      }
    }

    graphics::axis(1, at = seq_len(nc) - 0.5, labels = col_labels, tick = FALSE, las = 1, cex.axis = 1.02)
    graphics::axis(2, at = rev(seq_len(nr)) - 0.5, labels = row_labels, tick = FALSE, las = 1, cex.axis = 0.98)
    graphics::mtext("P value; * p<0.05, ** p<0.001", side = 1, line = 4.2, adj = 1, cex = 0.92)

    graphics::par(mar = c(5.8, 1.5, 4.8, 4.2), mgp = c(3.1, 0.9, 0))
    legend_vals <- seq(0, max_scale, length.out = length(palette_cols))
    graphics::image(
      x = c(0, 1),
      y = legend_vals,
      z = matrix(rep(legend_vals, each = 2), nrow = 2),
      col = palette_cols,
      xaxt = "n",
      xlab = "",
      ylab = ""
    )
    legend_tick_p <- c(1, 0.10, 0.05, 0.01, 0.001)
    legend_tick_p <- legend_tick_p[legend_tick_p >= 10^(-max_scale)]
    legend_tick_scale <- -log10(legend_tick_p)
    legend_tick_labels <- vapply(
      legend_tick_p,
      function(x) {
        if (is.na(x)) {
          return("NA")
        }
        if (x < 0.001) {
          return("<0.001")
        }
        formatC(x, format = "f", digits = if (x < 0.1) 2 else 1)
      },
      FUN.VALUE = character(1)
    )
    graphics::axis(4, at = legend_tick_scale, labels = legend_tick_labels, las = 2, cex.axis = 0.85, lwd = 0, lwd.ticks = 1)
    graphics::mtext("P value", side = 4, line = 2.2, cex = 0.9, font = 2)

  }

  save_plot_file(plot_fun, path = path, width = 15.8, height = max(7.4, 0.62 * nrow(mat) + 3.2), dpi = 320)
}

draw_effect_heatmap <- function(mat, p_mat, row_labels, col_labels, title, path, subtitle = NULL) {
  finite_values <- mat[is.finite(mat)]
  limit <- if (length(finite_values) == 0) 1 else max(abs(finite_values), na.rm = TRUE)
  limit <- max(limit, 0.05)
  palette_fun <- grDevices::colorRampPalette(c("#4EA3D8", "#F7F7F7", "#F2A65A"))
  palette_cols <- palette_fun(101)

  effect_to_color <- function(value) {
    if (!is.finite(value)) {
      return("#f2f2f2")
    }
    scaled <- (value + limit) / (2 * limit)
    idx <- max(1, min(length(palette_cols), floor(scaled * (length(palette_cols) - 1)) + 1))
    palette_cols[[idx]]
  }

  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(5.5, 0.7))
    graphics::par(mar = c(5.8, 18.5, 4.8, 1.2), xpd = NA)

    nr <- nrow(mat)
    nc <- ncol(mat)
    graphics::plot(
      NA,
      xlim = c(0, nc),
      ylim = c(0, nr),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      bty = "n",
      main = ""
    )

    for (r in seq_len(nr)) {
      for (c in seq_len(nc)) {
        value <- mat[r, c]
        p_value <- p_mat[r, c]
        x_left <- c - 1
        x_right <- c
        y_bottom <- nr - r
        y_top <- nr - r + 1
        graphics::rect(x_left, y_bottom, x_right, y_top, col = effect_to_color(value), border = "white", lwd = 1.5)
        label <- if (is.finite(value)) format_beta_compact(value, p_value) else ""
        graphics::text(c - 0.5, nr - r + 0.5, labels = label, cex = 1.02)
      }
    }

    graphics::axis(1, at = seq_len(nc) - 0.5, labels = col_labels, tick = FALSE, las = 1, cex.axis = 1.02)
    graphics::axis(2, at = rev(seq_len(nr)) - 0.5, labels = row_labels, tick = FALSE, las = 1, cex.axis = 1.00)
    graphics::mtext("Effect estimate; * p<0.05, ** p<0.001", side = 1, line = 4.2, adj = 1, cex = 0.92)

    graphics::par(mar = c(5.8, 1.5, 4.8, 4.2), mgp = c(3.1, 0.9, 0))
    legend_vals <- seq(-limit, limit, length.out = length(palette_cols))
    graphics::image(
      x = c(0, 1),
      y = legend_vals,
      z = matrix(rep(legend_vals, each = 2), nrow = 2),
      col = palette_cols,
      zlim = c(-limit, limit),
      xaxt = "n",
      xlab = "",
      ylab = ""
    )
    legend_ticks <- pretty(c(-limit, limit), n = 4)
    legend_ticks <- legend_ticks[legend_ticks >= (-limit - 1e-8) & legend_ticks <= (limit + 1e-8)]
    graphics::axis(4, at = legend_ticks, las = 2, cex.axis = 0.85, lwd = 0, lwd.ticks = 1)
    graphics::mtext("Effect estimate", side = 4, line = 2.2, cex = 0.9, font = 2)

  }

  save_plot_file(plot_fun, path = path, width = 15.8, height = max(7.4, 0.62 * nrow(mat) + 3.2), dpi = 320)
}

single_model_order <- names(model_label_map)
group_order <- c("CN", "MCI", "AD")
group_display <- c("CN" = "CN", "MCI" = "MCI", "AD" = "Dementia")

single_indirect <- sem_group_estimates[
  sem_group_estimates$cognition_model == "Cog_MMSE_MOCA" &
    sem_group_estimates$path == "indirect" &
    sem_group_estimates$sem_model %in% single_model_order,
  ,
  drop = FALSE
]

single_forest_path <- file.path(result_figures_dir, "Figure_7_single_mediator_stage_indirect_forest.png")
if (nrow(single_indirect) > 0) {
  y_positions <- c(CN = 3, MCI = 2, AD = 1)

  forest_plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(4, 2), mar = c(5.4, 5.1, 4.8, 3.2), oma = c(0.8, 0, 1.6, 0), mgp = c(2.5, 0.75, 0), xpd = NA)

    for (model_name in single_model_order) {
      panel_data <- single_indirect[single_indirect$sem_model == model_name, , drop = FALSE]
      panel_data <- panel_data[match(group_order, panel_data$group), , drop = FALSE]
      panel_limits <- range(c(panel_data$conf_low, panel_data$conf_high, panel_data$estimate, 0), na.rm = TRUE)
      tick_min <- if (panel_limits[1] < -1) floor(panel_limits[1]) else floor(panel_limits[1])
      tick_max <- ceiling(panel_limits[2])
      panel_ticks <- seq(tick_min, tick_max, by = 1)
      panel_ticks <- sort(unique(c(panel_ticks, 0)))
      panel_xlim <- c(min(panel_ticks), max(panel_ticks))
      panel_span <- diff(panel_xlim)
      if (!is.finite(panel_span) || panel_span <= 0) {
        panel_span <- 0.5
        panel_xlim <- c(-0.5, 0.5)
      }

      graphics::plot(
        NA,
        xlim = panel_xlim,
        ylim = c(0.5, 3.5),
        xaxt = "n",
        yaxt = "n",
        xaxs = "i",
        xlab = "Indirect effect (beta)",
        ylab = "",
        main = "",
        bty = "n",
        cex.lab = 1.14
      )
      graphics::title(main = unname(model_label_map[model_name]), cex.main = 1.18, font.main = 2, line = 1.1)
      graphics::abline(v = 0, col = "#9b9b9b", lty = 2, lwd = 1)
      graphics::axis(1, at = panel_ticks, labels = format(panel_ticks, trim = TRUE, scientific = FALSE), cex.axis = 1.08)
      graphics::axis(
        2,
        at = y_positions[group_order],
        labels = unname(group_display[group_order]),
        las = 1,
        cex.axis = 1.08
      )

      for (idx in seq_len(nrow(panel_data))) {
        row <- panel_data[idx, , drop = FALSE]
        group_name <- row$group[[1]]
        y <- y_positions[[group_name]]
        est <- row$estimate[[1]]
        p_value <- row$p_value[[1]]
        ci_low <- row$conf_low[[1]]
        ci_high <- row$conf_high[[1]]
        point_col <- if (is.na(est)) {
          "#bdbdbd"
        } else if (est >= 0) {
          "#b2182b"
        } else {
          "#2166ac"
        }

        graphics::segments(ci_low, y, ci_high, y, col = point_col, lwd = 2.2)
        graphics::points(est, y, pch = if (!is.na(p_value) && p_value < 0.05) 19 else 1, cex = 1.55, col = point_col)

        label_x <- panel_xlim[1] + 0.80 * panel_span
        label <- sprintf("beta=%s\np=%s", formatC(est, format = "f", digits = 3), format_p_compact(p_value))
        graphics::text(label_x, y, labels = label, adj = c(0, 0.5), cex = 1.02)
      }
    }

  }

  save_plot_file(forest_plot_fun, path = single_forest_path, width = 12.6, height = 16.6, dpi = 320)
}

single_heatmap_path <- file.path(result_figures_dir, "Figure_8_single_mediator_moderated_mediation_heatmap.png")
single_tests <- sem_moderated_tests[
  sem_moderated_tests$analysis_type == "single_mediator" &
    sem_moderated_tests$cognition_model == "Cog_MMSE_MOCA",
  ,
  drop = FALSE
]

if (nrow(single_tests) > 0) {
  single_cols <- c("omnibus" = "Omnibus", "CN_vs_MCI" = "CN vs MCI", "CN_vs_AD" = "CN vs Dementia", "MCI_vs_AD" = "MCI vs Dementia")
  single_mat <- matrix(NA_real_, nrow = length(single_model_order), ncol = length(single_cols))
  single_p_mat <- matrix(NA_real_, nrow = length(single_model_order), ncol = length(single_cols))
  rownames(single_mat) <- single_model_order
  rownames(single_p_mat) <- single_model_order
  colnames(single_mat) <- names(single_cols)
  colnames(single_p_mat) <- names(single_cols)

  for (model_name in single_model_order) {
    model_groups <- single_indirect[single_indirect$sem_model == model_name, , drop = FALSE]
    est_map <- setNames(model_groups$estimate, model_groups$group)
    if (all(group_order %in% names(est_map))) {
      single_mat[model_name, "omnibus"] <- max(est_map[group_order], na.rm = TRUE) - min(est_map[group_order], na.rm = TRUE)
      single_mat[model_name, "CN_vs_MCI"] <- est_map[["CN"]] - est_map[["MCI"]]
      single_mat[model_name, "CN_vs_AD"] <- est_map[["CN"]] - est_map[["AD"]]
      single_mat[model_name, "MCI_vs_AD"] <- est_map[["MCI"]] - est_map[["AD"]]
    }
  }

  for (i in seq_len(nrow(single_tests))) {
    row <- single_tests[i, , drop = FALSE]
    col_name <- if (row$scope[[1]] == "omnibus") "omnibus" else row$group_pair[[1]]
    if (row$model_name[[1]] %in% rownames(single_p_mat) && col_name %in% colnames(single_p_mat)) {
      single_p_mat[row$model_name[[1]], col_name] <- row$p_value[[1]]
    }
  }

  draw_effect_heatmap(
    mat = single_mat,
    p_mat = single_p_mat,
    row_labels = unname(model_label_map[rownames(single_mat)]),
    col_labels = unname(single_cols[colnames(single_mat)]),
    title = "Single-mediator stage-difference effect heatmap",
    subtitle = "Cell labels are effect differences; * p<0.05, ** p<0.001",
    path = single_heatmap_path
  )
}

parallel_stage_path <- file.path(result_figures_dir, "Figure_9_parallel_mediator_stage_heatmap.png")
parallel_groups <- sem_parallel_summary[
  sem_parallel_summary$cognition_model == "Cog_MMSE_MOCA" &
    sem_parallel_summary$group %in% group_order,
  ,
  drop = FALSE
]

if (nrow(parallel_groups) > 0) {
  row_ids <- character(0)
  row_labels <- character(0)
  for (model_name in names(parallel_model_label_map)) {
    mediators <- unique(parallel_groups$mediator_var[parallel_groups$parallel_model == model_name])
    mediators <- mediators[match(names(mediator_label_map), mediators, nomatch = 0)]
    for (mediator_name in mediators) {
      row_id <- paste(model_name, mediator_name, sep = "||")
      row_ids <- c(row_ids, row_id)
      row_labels <- c(row_labels, paste0(parallel_short_label_map[[model_name]], " | ", mediator_label_map[[mediator_name]]))
    }
  }
  if (length(row_ids) > 0) {
    effect_mat <- matrix(NA_real_, nrow = length(row_ids), ncol = length(group_order))
    p_mat <- matrix(NA_real_, nrow = length(row_ids), ncol = length(group_order))
    rownames(effect_mat) <- row_ids
    rownames(p_mat) <- row_ids
    colnames(effect_mat) <- group_order
    colnames(p_mat) <- group_order
    for (i in seq_len(nrow(parallel_groups))) {
      row <- parallel_groups[i, , drop = FALSE]
      row_id <- paste(row$parallel_model[[1]], row$mediator_var[[1]], sep = "||")
      if (row_id %in% rownames(effect_mat) && row$group[[1]] %in% group_order) {
        effect_mat[row_id, row$group[[1]]] <- row$indirect[[1]]
        p_mat[row_id, row$group[[1]]] <- row$indirect_p[[1]]
      }
    }
    draw_effect_heatmap(
      mat = effect_mat,
      p_mat = p_mat,
      row_labels = row_labels,
      col_labels = unname(group_display[group_order]),
      title = "Stage-specific indirect effects in parallel mediation SEM",
      subtitle = "Cell labels are indirect beta; * p<0.05, ** p<0.001",
      path = parallel_stage_path
    )
  }
}

parallel_test_path <- file.path(result_figures_dir, "Figure_10_parallel_mediator_moderated_mediation_heatmap.png")
parallel_tests <- sem_parallel_tests[
  sem_parallel_tests$cognition_model == "Cog_MMSE_MOCA",
  ,
  drop = FALSE
]

if (nrow(parallel_tests) > 0) {
  parallel_cols <- c("omnibus" = "Omnibus", "CN_vs_MCI" = "CN vs MCI", "CN_vs_AD" = "CN vs Dementia", "MCI_vs_AD" = "MCI vs Dementia")
  row_defs <- unique(parallel_tests[, c("sem_model", "path"), drop = FALSE])
  row_ids <- paste(row_defs$sem_model, row_defs$path, sep = "||")
  row_labels <- vapply(seq_len(nrow(row_defs)), function(i) {
    model_part <- parallel_short_label_map[[row_defs$sem_model[[i]]]]
    path_part <- row_defs$path[[i]]
    path_part <- sub("^indirect_", "", path_part)
    path_label <- if (path_part == "total_indirect") "Total indirect" else mediator_label_map[[path_part]] %||% path_part
    paste0(model_part, " | ", path_label)
  }, FUN.VALUE = character(1))

  parallel_mat <- matrix(NA_real_, nrow = length(row_ids), ncol = length(parallel_cols))
  parallel_p_mat <- matrix(NA_real_, nrow = length(row_ids), ncol = length(parallel_cols))
  rownames(parallel_mat) <- row_ids
  rownames(parallel_p_mat) <- row_ids
  colnames(parallel_mat) <- names(parallel_cols)
  colnames(parallel_p_mat) <- names(parallel_cols)

  for (row_id in row_ids) {
    parts <- strsplit(row_id, "\\|\\|")[[1]]
    model_name <- parts[1]
    path_name <- parts[2]
    if (path_name == "total_indirect") {
      group_rows <- unique(parallel_groups[parallel_groups$parallel_model == model_name, c("group", "total_indirect"), drop = FALSE])
      est_map <- setNames(group_rows$total_indirect, group_rows$group)
    } else {
      mediator_name <- sub("^indirect_", "", path_name)
      group_rows <- parallel_groups[parallel_groups$parallel_model == model_name & parallel_groups$mediator_var == mediator_name, , drop = FALSE]
      est_map <- setNames(group_rows$indirect, group_rows$group)
    }
    if (all(group_order %in% names(est_map))) {
      parallel_mat[row_id, "omnibus"] <- max(est_map[group_order], na.rm = TRUE) - min(est_map[group_order], na.rm = TRUE)
      parallel_mat[row_id, "CN_vs_MCI"] <- est_map[["CN"]] - est_map[["MCI"]]
      parallel_mat[row_id, "CN_vs_AD"] <- est_map[["CN"]] - est_map[["AD"]]
      parallel_mat[row_id, "MCI_vs_AD"] <- est_map[["MCI"]] - est_map[["AD"]]
    }
  }

  for (i in seq_len(nrow(parallel_tests))) {
    row <- parallel_tests[i, , drop = FALSE]
    row_id <- paste(row$sem_model[[1]], row$path[[1]], sep = "||")
    col_name <- if (row$scope[[1]] == "omnibus") "omnibus" else row$group_pair[[1]]
    if (row_id %in% rownames(parallel_p_mat) && col_name %in% colnames(parallel_p_mat)) {
      parallel_p_mat[row_id, col_name] <- row$p_value[[1]]
    }
  }

  draw_effect_heatmap(
    mat = parallel_mat,
    p_mat = parallel_p_mat,
    row_labels = row_labels,
    col_labels = unname(parallel_cols[colnames(parallel_mat)]),
    title = "Parallel-mediator stage-difference effect heatmap",
    subtitle = "Cell labels are effect differences; * p<0.05, ** p<0.001",
    path = parallel_test_path
  )
}

report_lines <- c(
  "# SEM Stage-Difference Figures",
  "",
  "- `Figure 7`: stage-specific indirect effects from single-mediator SEM, shown as forest panels with 95% confidence intervals.",
  "- `Figure 8`: formal moderated-mediation heatmap for single-mediator SEM, showing omnibus and pairwise Wald-test p-values.",
  "- `Figure 9`: stage-specific indirect-effect heatmap for parallel-mediator SEM (`sTREM2 + P-Tau + Abeta`, `sTREM2 + Tau + Abeta`).",
  "- `Figure 10`: formal moderated-mediation heatmap for parallel-mediator SEM.",
  "",
  paste0("- [Figure 7](../figures/", basename(single_forest_path), ")"),
  paste0("- [Figure 8](../figures/", basename(single_heatmap_path), ")"),
  paste0("- [Figure 9](../figures/", basename(parallel_stage_path), ")"),
  paste0("- [Figure 10](../figures/", basename(parallel_test_path), ")")
)

sem_figure_note_path <- file.path(result_report_dir, "SEM_stage_difference_figures.md")
writeLines(report_lines, con = sem_figure_note_path, useBytes = TRUE)

extract_panel_row <- function(x_name, y_name) {
  if (nrow(combined_panel_stats) == 0) {
    return(data.frame())
  }
  combined_panel_stats[combined_panel_stats$x == x_name & combined_panel_stats$y == y_name, , drop = FALSE]
}

extract_heatmap_row <- function(row_var, col_var) {
  if (nrow(association_heatmap) == 0) {
    return(data.frame())
  }
  association_heatmap[
    association_heatmap$row_var == row_var & association_heatmap$col_var == col_var,
    ,
    drop = FALSE
  ]
}

panel_strem2 <- extract_panel_row("MSD_STREM2CORRECTED", "ChPICV")
panel_tau <- extract_panel_row("TAU", "ChPICV")
panel_ptau <- extract_panel_row("PTAU", "ChPICV")
panel_abeta <- extract_panel_row("S_ABETA", "ChPICV")

heat_chp_strem2 <- extract_heatmap_row("ChPICV", "MSD_STREM2CORRECTED")
heat_chp_tau <- extract_heatmap_row("ChPICV", "TAU")
heat_chp_ptau <- extract_heatmap_row("ChPICV", "PTAU")
heat_chp_abeta <- extract_heatmap_row("ChPICV", "S_ABETA")
heat_mmse_strem2 <- extract_heatmap_row("MMSE", "MSD_STREM2CORRECTED")
heat_moca_strem2 <- extract_heatmap_row("MOCA", "MSD_STREM2CORRECTED")

fmt_sentence_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("NA")
  }
  formatC(x[[1]], format = "f", digits = digits)
}

fmt_sentence_p <- function(x) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("NA")
  }
  format_p_compact(x[[1]])
}

figure_guide_lines <- c(
  "# Figures 5-10 Talking Points",
  "",
  "## Figure 5. Combined regression panel",
  "",
  paste0(
    "- `sTREM2` and `ChP/ICV` show an inverse adjusted association in the overall sample ",
    "(beta=", fmt_sentence_num(panel_strem2$beta), ", p=", fmt_sentence_p(panel_strem2$p_value), ")."
  ),
  paste0(
    "- `Tau` and `P-Tau` are also inversely associated with `ChP/ICV` ",
    "(Tau: beta=", fmt_sentence_num(panel_tau$beta), ", p=", fmt_sentence_p(panel_tau$p_value),
    "; P-Tau: beta=", fmt_sentence_num(panel_ptau$beta), ", p=", fmt_sentence_p(panel_ptau$p_value), ")."
  ),
  paste0(
    "- `Abeta` is inversely associated with `ChP/ICV` as well ",
    "(beta=", fmt_sentence_num(panel_abeta$beta), ", p=", fmt_sentence_p(panel_abeta$p_value), ")."
  ),
  "- Take-home message: all four biomarkers are significantly linked to ChP/ICV in the same overall pathological network, but the direction and downstream role differ in SEM.",
  "",
  "## Figure 6. Adjusted partial-correlation heatmap",
  "",
  paste0(
    "- `ChP/ICV` retains significant negative partial correlations with `sTREM2`, `Tau`, and `P-Tau`, and a weaker negative correlation with `Abeta` ",
    "(sTREM2 r=", fmt_sentence_num(heat_chp_strem2$estimate), ", p=", fmt_sentence_p(heat_chp_strem2$p_value),
    "; Tau r=", fmt_sentence_num(heat_chp_tau$estimate), ", p=", fmt_sentence_p(heat_chp_tau$p_value),
    "; P-Tau r=", fmt_sentence_num(heat_chp_ptau$estimate), ", p=", fmt_sentence_p(heat_chp_ptau$p_value),
    "; Abeta r=", fmt_sentence_num(heat_chp_abeta$estimate), ", p=", fmt_sentence_p(heat_chp_abeta$p_value), ")."
  ),
  paste0(
    "- `sTREM2` shows weak inverse adjusted correlations with `MMSE` and `MoCA` ",
    "(MMSE r=", fmt_sentence_num(heat_mmse_strem2$estimate), ", p=", fmt_sentence_p(heat_mmse_strem2$p_value),
    "; MoCA r=", fmt_sentence_num(heat_moca_strem2$estimate), ", p=", fmt_sentence_p(heat_moca_strem2$p_value), ")."
  ),
  "- Take-home message: Figure 6 helps explain why SEM may show both direct and indirect paths, because these variables are not independent and share structured covariance.",
  "",
  "## Figure 7. Stage-specific indirect effects from single-mediator SEM",
  "",
  "- Use this figure to explain direction, magnitude, and uncertainty of the indirect effect in each stage.",
  "- The most important visual points are whether the confidence interval crosses 0 and whether the point estimate changes sign across CN, MCI, and AD.",
  "- This figure is best for saying which pathway is exploratory and which stage drives the effect pattern.",
  "",
  "## Figure 8. Single-mediator moderated mediation heatmap",
  "",
  "- This figure gives the formal p-values for stage difference testing.",
  "- In the current run, the strongest stage heterogeneity is seen in `Tau -> ChP/ICV -> Cognition`, `P-Tau -> ChP/ICV -> Cognition`, and `sTREM2 -> ChP/ICV -> Cognition`, especially for `MCI vs AD`.",
  "- Use Figure 8 when the professor asks: `Is this stage difference only visual, or statistically confirmed?`",
  "",
  "## Figure 9. Stage-specific indirect effects from parallel mediation SEM",
  "",
  "- This figure shows what remains after `sTREM2`, `Tau/P-Tau`, and `Abeta` are entered simultaneously.",
  "- In this stricter framework, many apparent single-mediator signals shrink, which means part of the earlier effect was shared across biomarkers.",
  "- The strongest residual stage pattern is concentrated in the `Abeta` branch of the `sTREM2 + Tau + Abeta` parallel model.",
  "",
  "## Figure 10. Parallel-mediator moderated mediation heatmap",
  "",
  "- This is the most rigorous stage-difference figure for the parallel models.",
  "- In the current results, `ChP/ICV -> sTREM2 + Tau + Abeta -> Cognition` shows a significant omnibus stage difference for the `Abeta` indirect path, and pairwise significance for `CN vs MCI` and `MCI vs AD`.",
  "- By contrast, the `sTREM2 + P-Tau + Abeta` parallel model does not show a stable mediator-specific stage difference.",
  "",
  "## Suggested 3-step explanation",
  "",
  "- Step 1: Figures 5-6 establish that ChP/ICV is significantly linked to sTREM2, Tau, P-Tau, and Abeta at the overall association level.",
  "- Step 2: Figures 7-8 show that stage heterogeneity exists in several single-mediator SEM pathways, especially around MCI vs AD.",
  "- Step 3: Figures 9-10 show that once biomarkers compete within the same model, the clearest remaining stage-dependent signal is the Abeta branch, while many sTREM2/Tau/P-Tau signals attenuate."
)

figure_guide_path <- file.path(result_report_dir, "Figures_5_to_10_talking_points.md")
writeLines(figure_guide_lines, con = figure_guide_path, useBytes = TRUE)

append_analysis_log(
  summary_dir = result_summary_dir,
  analysis_name = "08c_sem_difference_figures",
  output_files = c(single_forest_path, single_heatmap_path, parallel_stage_path, parallel_test_path, sem_figure_note_path, figure_guide_path),
  note = "Generated stage-difference figures for single-mediator and parallel-mediator SEM, including forest plots, moderated-mediation heatmaps, and a talking-points note for Figures 5-10."
)

