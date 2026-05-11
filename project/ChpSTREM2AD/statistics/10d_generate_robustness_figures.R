source(file.path(getwd(), "00_setup.R"))

fmt_p_short <- function(x) {
  if (is.na(x)) return("NA")
  if (x < 0.001) return("<0.001")
  sprintf("%.3f", x)
}

fmt_beta_star <- function(beta, p) {
  if (is.na(beta)) return("NA")
  stars <- if (!is.na(p) && p < 0.001) {
    "**"
  } else if (!is.na(p) && p < 0.05) {
    "*"
  } else {
    ""
  }
  paste0(sprintf("%.3f", beta), stars)
}

find_latest_complete_result_dir <- function(project_root) {
  result_root <- file.path(project_root, "result")
  dirs <- list.dirs(result_root, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[grepl("^\\d{8}_\\d{6}$", basename(dirs))]
  dirs <- sort(dirs, decreasing = TRUE)
  required <- c("matched_key_model_sensitivity.csv", "overlap_weighting_compare_unweighted.csv")
  for (d in dirs) {
    sdir <- file.path(d, "summary")
    if (all(file.exists(file.path(sdir, required)))) return(d)
  }
  stop("No complete result directory found for robustness figures.")
}

sig_stars <- function(p_value) {
  if (length(p_value) == 0 || is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  ""
}

stage_display <- c("CN" = "CN", "MCI" = "MCI", "AD" = "Dementia")
structure_display <- c(
  "ChP_SUM" = "Choroid plexus volume",
  "Hippocampus_SUM" = "Hippocampal volume",
  "Amygdala_SUM" = "Amygdalar volume",
  "LV_SUM" = "Lateral ventricle volume"
)
biomarker_display <- c(
  "MSD_STREM2CORRECTED" = "sTREM2",
  "TAU" = "Tau",
  "PTAU" = "P-Tau",
  "S_ABETA" = "Aβ"
)
alt_model_display <- c(
  "absolute_ChP_SUM" = "Absolute ChP volume\n(adjusted for ICV)",
  "ratio_ChPICV" = "ChP/ICV ratio",
  "right_ChP_ratio" = "Right ChP/ICV ratio"
)

simplify_pair <- function(x) {
  out <- gsub("\\.csv$", "", x)
  out <- gsub("_vs_", " vs ", out, fixed = TRUE)
  gsub("\\bAD\\b", "Dementia", out)
}

simplify_model <- function(x) {
  map <- c(
    sTREM2_on_ChPICV = "sTREM2",
    PTAU_on_ChPICV = "P-Tau",
    TAU_on_ChPICV = "Tau"
  )
  out <- unname(map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

simplify_overlap_model <- function(x) {
  map <- c(
    ChPICV_on_sTREM2 = "sTREM2",
    ChPICV_on_TAU = "Tau",
    ChPICV_on_PTAU = "P-Tau",
    ChPICV_on_ABETA = "Aβ"
  )
  out <- unname(map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

draw_simple_forest <- function(df, estimate_col, lower_col, upper_col, label_col, path, xlab = "Standardized beta", group_col = NULL, legend_title = NULL) {
  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mar = c(4.8, 26.5, 1.6, 1.6), mgp = c(2.7, 0.9, 0), xpd = NA)
    y_pos <- rev(seq_len(nrow(df)))
    x_range <- range(c(df[[estimate_col]], df[[lower_col]], df[[upper_col]], 0), na.rm = TRUE)
    pad <- max(diff(x_range) * 0.08, 0.02)
    color_vec <- rep("#1B4F72", nrow(df))
    legend_df <- NULL
    if (!is.null(group_col) && group_col %in% names(df)) {
      group_levels <- unique(as.character(df[[group_col]]))
      palette_map <- c(
        "Unadjusted" = "#D35400",
        "Primary adjusted" = "#1B4F72",
        "Overlap weighted" = "#117A65"
      )
      color_map <- palette_map[group_levels]
      missing_cols <- is.na(color_map)
      if (any(missing_cols)) {
        color_map[missing_cols] <- grDevices::rainbow(sum(missing_cols))
      }
      names(color_map) <- group_levels
      color_vec <- unname(color_map[as.character(df[[group_col]])])
      legend_df <- data.frame(group = group_levels, color = unname(color_map[group_levels]), stringsAsFactors = FALSE)
    }

    graphics::plot(
      x = df[[estimate_col]],
      y = y_pos,
      xlim = c(x_range[1] - pad, x_range[2] + pad),
      ylim = c(0.5, nrow(df) + 0.5),
      yaxt = "n",
      ylab = "",
      xlab = xlab,
      pch = 19,
      col = color_vec
    )
    graphics::axis(2, at = y_pos, labels = df[[label_col]], las = 1, tick = FALSE, cex.axis = 0.92)
    graphics::abline(v = 0, lty = 2, col = "grey55")
    graphics::segments(df[[lower_col]], y_pos, df[[upper_col]], y_pos, lwd = 2, col = color_vec)
    if (!is.null(legend_df) && nrow(legend_df) > 0) {
      graphics::legend(
        "topright",
        inset = c(0.05, 0.00),
        legend = legend_df$group,
        col = legend_df$color,
        pch = 19,
        lty = 1,
        lwd = 2,
        bty = "n",
        title = legend_title,
        cex = 0.86
      )
    }
  }
  save_plot_file(plot_fun, path = path, width = 12.2, height = max(5.4, 0.38 * nrow(df) + 2.0), dpi = 320)
}

draw_grouped_bar <- function(df, path, ylab, legend_pos = "top") {
  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mar = c(6.2, 6.8, 1.6, 1.6), mgp = c(3.0, 0.9, 0), xpd = NA)
    groups <- unique(df$group)
    series <- unique(df$series)
    mat <- sapply(series, function(series_name) {
      df$value[df$series == series_name][match(groups, df$group[df$series == series_name])]
    })
    if (is.null(dim(mat))) {
      mat <- matrix(mat, ncol = 1)
      colnames(mat) <- series
      rownames(mat) <- groups
    }
    col_vec <- c("#D6EAF8", "#2E86C1", "#1B4F72")[seq_len(ncol(mat))]
    y_vals <- as.numeric(mat)
    y_min <- min(c(0, y_vals), na.rm = TRUE)
    y_max <- max(c(0, y_vals), na.rm = TRUE)
    y_span <- max(y_max - y_min, 0.05)
    y_lim <- c(y_min - 0.20 * y_span, y_max + 0.32 * y_span)
    mids <- graphics::barplot(
      t(mat),
      beside = TRUE,
      col = col_vec,
      border = NA,
      names.arg = groups,
      las = 1,
      ylab = ylab,
      cex.names = 0.88,
      ylim = y_lim
    )
    graphics::abline(h = 0, lty = 2, col = "grey55")
    graphics::legend(
      legend_pos,
      inset = c(0, -0.02),
      legend = series,
      fill = col_vec,
      bty = "n",
      cex = 0.92,
      horiz = FALSE
    )
    label_vec <- sprintf("%.3f", as.numeric(t(mat)))
    graphics::text(
      mids,
      as.numeric(t(mat)),
      labels = label_vec,
      pos = ifelse(as.numeric(t(mat)) >= 0, 3, 1),
      offset = 0.35,
      cex = 0.84
    )
  }
  save_plot_file(plot_fun, path = path, width = 8.8, height = 5.8, dpi = 320)
}

compute_smd_numeric <- function(x, g) {
  keep <- stats::complete.cases(x, g)
  x <- x[keep]
  g <- as.character(g[keep])
  if (length(unique(g)) != 2) return(NA_real_)
  lev <- unique(g)
  x1 <- x[g == lev[1]]
  x2 <- x[g == lev[2]]
  if (length(x1) < 2 || length(x2) < 2) return(NA_real_)
  m1 <- mean(x1)
  m2 <- mean(x2)
  s1 <- stats::sd(x1)
  s2 <- stats::sd(x2)
  sp <- sqrt((s1^2 + s2^2) / 2)
  if (!is.finite(sp) || sp == 0) return(NA_real_)
  (m1 - m2) / sp
}

compute_smd_binary <- function(x, g) {
  keep <- stats::complete.cases(x, g)
  x <- as.numeric(x[keep])
  g <- as.character(g[keep])
  if (length(unique(g)) != 2) return(NA_real_)
  lev <- unique(g)
  p1 <- mean(x[g == lev[1]] == 1)
  p2 <- mean(x[g == lev[2]] == 1)
  p <- (p1 + p2) / 2
  denom <- sqrt(p * (1 - p))
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  (p1 - p2) / denom
}

draw_heatmap_matrix <- function(mat, row_labels, col_labels, path, legend_title = "", star_mat = NULL, value_digits = 2) {
  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mar = c(6.6, 12.8, 1.8, 5.2), mgp = c(3.0, 0.8, 0))
    nr <- nrow(mat)
    nc <- ncol(mat)
    graphics::plot(
      NA,
      xlim = c(0.5, nc + 1.8),
      ylim = c(0.5, nr + 0.5),
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = ""
    )
    zlim <- max(abs(mat), na.rm = TRUE)
    zlim <- max(zlim, 0.05)
    pal <- grDevices::colorRampPalette(c("#2E86C1", "#F7F9F9", "#CA6F1E"))(100)
    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        val <- mat[i, j]
        idx <- round((val + zlim) / (2 * zlim) * 99) + 1
        idx <- max(1, min(100, idx))
        y <- nr - i + 1
        graphics::rect(j - 0.5, y - 0.5, j + 0.5, y + 0.5, col = pal[idx], border = "white")
        star_lab <- ""
        if (!is.null(star_mat)) {
          star_lab <- star_mat[i, j]
          if (is.na(star_lab)) {
            star_lab <- ""
          }
        }
        value_lab <- sprintf(paste0("%.", value_digits, "f"), val)
        graphics::text(j, y, labels = paste0(value_lab, star_lab), cex = 0.78)
      }
    }
    graphics::axis(1, at = seq_len(nc), labels = col_labels, las = 1, tick = FALSE, cex.axis = 0.92)
    graphics::axis(2, at = seq_len(nr), labels = rev(row_labels), las = 1, tick = FALSE, cex.axis = 0.92)
    legend_x <- nc + 1.05
    legend_y <- seq(0.9, nr - 0.1, length.out = 100)
    for (k in seq_along(legend_y)[-length(legend_y)]) {
      graphics::rect(legend_x, legend_y[k], legend_x + 0.35, legend_y[k + 1], col = pal[k], border = pal[k])
    }
    if (nzchar(legend_title)) {
      graphics::text(legend_x + 0.55, nr - 0.55, legend_title, adj = c(0, 0.5), cex = 0.86, font = 2)
    }
    graphics::text(legend_x + 0.42, c(0.9, nr - 0.1), labels = sprintf("%.2f", c(-zlim, zlim)), pos = 4, cex = 0.76)
    if (!is.null(star_mat)) {
      graphics::mtext("* p<0.05   ** p<0.01   *** p<0.001", side = 1, line = 4.8, adj = 1, cex = 0.82)
    }
  }
  save_plot_file(plot_fun, path = path, width = 8.8, height = max(5.8, 0.52 * nrow(mat) + 2.2), dpi = 320)
}

result_dir_use <- dirname(result_summary_dir)
required_summary <- c("matched_key_model_sensitivity.csv", "overlap_weighting_compare_unweighted.csv")
if (!all(file.exists(file.path(result_summary_dir, required_summary)))) {
  result_dir_use <- find_latest_complete_result_dir(project_root)
}
summary_dir <- file.path(result_dir_use, "summary")
matched_path <- file.path(summary_dir, "matched_key_model_sensitivity.csv")
alt_chp_path <- file.path(summary_dir, "alternative_chp_definition_models.csv")
structure_path <- file.path(summary_dir, "structure_specificity_models.csv")
abeta_full_path <- file.path(summary_dir, "abeta_truncation_regression_sensitivity.csv")
abeta_censored_path <- file.path(summary_dir, "abeta_censored_sensitivity.csv")

result_figures_dir_use <- file.path(result_dir_use, "figures")
matched_fig_path <- file.path(result_figures_dir_use, "robustness_matched_vs_unmatched.png")
alt_chp_fig_path <- file.path(result_figures_dir_use, "robustness_alternative_chp_definitions.png")
structure_fig_path <- file.path(result_figures_dir_use, "robustness_structure_specificity_heatmap.png")
abeta_fig_path <- file.path(result_figures_dir_use, "robustness_abeta_sensitivity.png")
forest_fig_path <- file.path(result_figures_dir_use, "robustness_core_stage_forest.png")
love_fig_path <- file.path(result_figures_dir_use, "robustness_matching_love_plot.png")
love_csv_path <- file.path(summary_dir, "matching_smd_summary.csv")
ow_balance_path <- file.path(summary_dir, "overlap_weighting_balance.csv")
ow_compare_path <- file.path(summary_dir, "overlap_weighting_compare_unweighted.csv")
ow_love_fig_path <- file.path(result_figures_dir_use, "robustness_overlap_love_plot.png")
ow_compare_fig_path <- file.path(result_figures_dir_use, "robustness_overlap_weighted_vs_unweighted.png")

if (file.exists(matched_path)) {
  matched_df <- read_project_data(matched_path)
  matched_df <- matched_df[matched_df$model_name %in% c("sTREM2_on_ChPICV", "PTAU_on_ChPICV", "TAU_on_ChPICV"), , drop = FALSE]
  matched_df$label <- paste(
    simplify_pair(matched_df$pair_name),
    simplify_model(matched_df$model_name),
    ifelse(matched_df$sample_set == "matched_pair", "matched", "unmatched"),
    sep = " | "
  )
  matched_df <- matched_df[order(matched_df$model_name, matched_df$pair_name, matched_df$sample_set), , drop = FALSE]
  if (nrow(matched_df) > 0) {
    draw_simple_forest(
      df = matched_df,
      estimate_col = "std_beta",
      lower_col = "conf.low",
      upper_col = "conf.high",
      label_col = "label",
      path = matched_fig_path
    )
  }
}

if (exists("analysis_data_path") && file.exists(analysis_data_path)) {
  analysis_df <- read_project_data(analysis_data_path)
  analysis_df$dx3 <- factor(
    ifelse(analysis_df$S_DX == 0, "CN", ifelse(analysis_df$S_DX == 1, "MCI", "Dementia")),
    levels = c("CN", "MCI", "Dementia")
  )
  analysis_df$sex_female <- ifelse(analysis_df$S_PTGENDER == 2, 1, 0)
  pair_map <- list(
    "CN vs MCI" = c("CN", "MCI"),
    "MCI vs Dementia" = c("MCI", "Dementia"),
    "CN vs Dementia" = c("CN", "Dementia")
  )
  smd_rows <- list()
  for (pair_name in names(pair_map)) {
    groups <- pair_map[[pair_name]]
    pair_df <- analysis_df[analysis_df$dx3 %in% groups, , drop = FALSE]
    if (nrow(pair_df) == 0) next

    smd_rows[[length(smd_rows) + 1]] <- data.frame(
      pair = pair_name,
      sample = "Unmatched",
      variable = "Age",
      smd = compute_smd_numeric(pair_df$S_AGE, pair_df$dx3),
      stringsAsFactors = FALSE
    )
    smd_rows[[length(smd_rows) + 1]] <- data.frame(
      pair = pair_name,
      sample = "Unmatched",
      variable = "Female sex",
      smd = compute_smd_binary(pair_df$sex_female, pair_df$dx3),
      stringsAsFactors = FALSE
    )

    matched_file <- file.path(result_data_clean_dir, paste0("matched_", gsub(" vs ", "_vs_", gsub("Dementia", "AD", pair_name)), ".csv"))
    if (file.exists(matched_file)) {
      matched_df_local <- read_project_data(matched_file)
      if (".group" %in% names(matched_df_local)) {
        matched_df_local$dx3 <- factor(
          ifelse(matched_df_local$.group == "CN", "CN",
            ifelse(matched_df_local$.group == "MCI", "MCI", "Dementia")
          ),
          levels = c("CN", "MCI", "Dementia")
        )
        matched_df_local$sex_female <- ifelse(matched_df_local$S_PTGENDER == 2, 1, 0)
        smd_rows[[length(smd_rows) + 1]] <- data.frame(
          pair = pair_name,
          sample = "Matched",
          variable = "Age",
          smd = compute_smd_numeric(matched_df_local$S_AGE, matched_df_local$dx3),
          stringsAsFactors = FALSE
        )
        smd_rows[[length(smd_rows) + 1]] <- data.frame(
          pair = pair_name,
          sample = "Matched",
          variable = "Female sex",
          smd = compute_smd_binary(matched_df_local$sex_female, matched_df_local$dx3),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(smd_rows) > 0) {
    smd_df <- do.call(rbind, smd_rows)
    write_csv_utf8(smd_df, love_csv_path, row.names = FALSE)

    plot_fun <- function() {
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mar = c(4.8, 10.5, 2.1, 1.6), mgp = c(2.8, 0.9, 0), xpd = NA)
      smd_df$label <- paste(smd_df$pair, smd_df$variable, sep = " | ")
      label_levels <- rev(unique(smd_df$label))
      y_map <- setNames(seq_along(label_levels), label_levels)
      y_vals <- y_map[smd_df$label]
      x_lim <- range(c(-0.8, 0.8, smd_df$smd), na.rm = TRUE)
      graphics::plot(
        smd_df$smd,
        y_vals,
        xlim = x_lim,
        ylim = c(0.5, length(label_levels) + 0.5),
        yaxt = "n",
        ylab = "",
        xlab = "Standardized mean difference",
        pch = ifelse(smd_df$sample == "Matched", 17, 19),
        col = ifelse(smd_df$sample == "Matched", "#1B4F72", "#D35400"),
        cex = 1.1
      )
      graphics::axis(2, at = seq_along(label_levels), labels = label_levels, las = 1, tick = FALSE, cex.axis = 0.88)
      graphics::abline(v = 0, lty = 1, col = "grey40")
      graphics::abline(v = c(-0.1, 0.1), lty = 2, col = "grey60")
      graphics::legend(
        "topright",
        legend = c("Unmatched", "Matched"),
        pch = c(19, 17),
        col = c("#D35400", "#1B4F72"),
        bty = "n",
        cex = 0.92
      )
    }
    save_plot_file(plot_fun, path = love_fig_path, width = 10.8, height = 5.8, dpi = 320)
  }
}

if (file.exists(ow_balance_path)) {
  ow_balance_df <- read_project_data(ow_balance_path)
  if (nrow(ow_balance_df) > 0) {
    plot_fun <- function() {
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mar = c(4.8, 10.5, 2.1, 1.6), mgp = c(2.8, 0.9, 0), xpd = NA)
      ow_balance_df$label <- paste(gsub("\\bAD\\b", "Dementia", ow_balance_df$pair), ow_balance_df$variable, sep = " | ")
      label_levels <- rev(unique(ow_balance_df$label))
      y_map <- setNames(seq_along(label_levels), label_levels)
      y_vals <- y_map[ow_balance_df$label]
      x_lim <- range(c(-0.8, 0.8, ow_balance_df$smd), na.rm = TRUE)
      graphics::plot(
        ow_balance_df$smd,
        y_vals,
        xlim = x_lim,
        ylim = c(0.5, length(label_levels) + 0.5),
        yaxt = "n",
        ylab = "",
        xlab = "Standardized mean difference",
        pch = ifelse(ow_balance_df$sample == "Overlap weighted", 17, 19),
        col = ifelse(ow_balance_df$sample == "Overlap weighted", "#1B4F72", "#D35400"),
        cex = 1.1
      )
      graphics::axis(2, at = seq_along(label_levels), labels = label_levels, las = 1, tick = FALSE, cex.axis = 0.88)
      graphics::abline(v = 0, lty = 1, col = "grey40")
      graphics::abline(v = c(-0.1, 0.1), lty = 2, col = "grey60")
      graphics::legend(
        "topright",
        legend = c("Unweighted", "Overlap weighted"),
        pch = c(19, 17),
        col = c("#D35400", "#1B4F72"),
        bty = "n",
        cex = 0.92
      )
    }
    save_plot_file(plot_fun, path = ow_love_fig_path, width = 10.8, height = 5.8, dpi = 320)
  }
}

if (file.exists(ow_compare_path)) {
  ow_compare_df <- read_project_data(ow_compare_path)
  ow_compare_df <- ow_compare_df[ow_compare_df$term == "exposure", , drop = FALSE]
  if (nrow(ow_compare_df) > 0) {
    ow_compare_df$analysis_type <- factor(
      ow_compare_df$analysis_type,
      levels = c("Unadjusted", "Primary adjusted", "Overlap weighted")
    )
    ow_compare_df$label <- paste(
      simplify_overlap_model(ow_compare_df$model_name),
      ow_compare_df$analysis_type,
      sep = " | "
    )
    ow_compare_df <- ow_compare_df[order(ow_compare_df$model_name, ow_compare_df$analysis_type), , drop = FALSE]
    draw_simple_forest(
      df = ow_compare_df,
      estimate_col = "std_beta",
      lower_col = "std_conf.low",
      upper_col = "std_conf.high",
      label_col = "label",
      path = ow_compare_fig_path,
      group_col = "analysis_type",
      legend_title = "Model"
    )
  }
}

if (file.exists(alt_chp_path)) {
  alt_df <- read_project_data(alt_chp_path)
  alt_df <- alt_df[alt_df$group == "Overall", , drop = FALSE]
  if (nrow(alt_df) > 0) {
    alt_df$model_label <- unname(alt_model_display[alt_df$model_name])
    alt_df$model_label[is.na(alt_df$model_label)] <- alt_df$model_name[is.na(alt_df$model_label)]
    alt_plot_df <- data.frame(
      group = alt_df$model_label,
      series = "sTREM2 association",
      value = alt_df$std_beta,
      stringsAsFactors = FALSE
    )
    draw_grouped_bar(df = alt_plot_df, path = alt_chp_fig_path, ylab = "Standardized beta")
  }
}

if (file.exists(structure_path)) {
  structure_df <- read_project_data(structure_path)
  if (nrow(structure_df) > 0) {
    structure_df$structure_label <- unname(structure_display[structure_df$structure])
    structure_df$structure_label[is.na(structure_df$structure_label)] <- structure_df$structure[is.na(structure_df$structure_label)]
    structure_df$biomarker_label <- unname(biomarker_display[structure_df$biomarker])
    structure_df$biomarker_label[is.na(structure_df$biomarker_label)] <- structure_df$biomarker[is.na(structure_df$biomarker_label)]
    structures <- unique(structure_df$structure_label)
    biomarkers <- unique(structure_df$biomarker_label)
    mat <- outer(structures, biomarkers, Vectorize(function(s, b) {
      row <- structure_df[structure_df$structure_label == s & structure_df$biomarker_label == b, , drop = FALSE]
      if (nrow(row) == 0) return(NA_real_)
      row$std_beta[[1]]
    }))
    star_mat <- outer(structures, biomarkers, Vectorize(function(s, b) {
      row <- structure_df[structure_df$structure_label == s & structure_df$biomarker_label == b, , drop = FALSE]
      if (nrow(row) == 0) return("")
      sig_stars(row[["p.value"]][[1]])
    }))
    rownames(mat) <- structures
    colnames(mat) <- biomarkers
    draw_heatmap_matrix(
      mat = mat,
      row_labels = structures,
      col_labels = biomarkers,
      path = structure_fig_path,
      legend_title = "",
      star_mat = star_mat,
      value_digits = 2
    )
  }
}

if (file.exists(abeta_full_path) && file.exists(abeta_censored_path)) {
  full_df <- read_project_data(abeta_full_path)
  cens_df <- read_project_data(abeta_censored_path)
  full_pick <- full_df[full_df$model_name == "ABETA_on_ChPICV" & full_df$sample_set %in% c("full", "restricted_no_ge_1700") & full_df$term == "exposure" & full_df$group == "Overall", , drop = FALSE]
  cens_pick <- cens_df[cens_df$group == "Overall" & cens_df$term == "chp", , drop = FALSE]
  linear_vals <- setNames(rep(NA_real_, 2), c("Observed full sample", "Removed >=1700"))
  if (nrow(full_pick) >= 1) {
    linear_vals["Observed full sample"] <- full_pick$std_beta[match("full", full_pick$sample_set)]
    linear_vals["Removed >=1700"] <- full_pick$std_beta[match("restricted_no_ge_1700", full_pick$sample_set)]
  }
  cens_est <- if (nrow(cens_pick) > 0) cens_pick$estimate[[1]] / 100000 else NA_real_
  cens_p <- if (nrow(cens_pick) > 0) cens_pick$p_value[[1]] else NA_real_

  plot_fun <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(1, 2), mar = c(6.6, 8.4, 1.8, 1.8), oma = c(0.6, 0.2, 0.4, 0.2), mgp = c(3.4, 1.0, 0), xpd = NA)

    linear_span <- max(abs(linear_vals), na.rm = TRUE)
    if (!is.finite(linear_span) || linear_span <= 0) linear_span <- 0.2
    linear_ylim <- c(min(c(0, linear_vals), na.rm = TRUE) - 0.22 * linear_span, max(c(0, linear_vals), na.rm = TRUE) + 0.34 * linear_span)
    mids1 <- graphics::barplot(
      linear_vals,
      col = c("#D6EAF8", "#2E86C1"),
      border = NA,
      las = 1,
      ylim = linear_ylim,
      ylab = "Standardized beta",
      cex.names = 0.92,
      axes = FALSE,
      axisnames = FALSE
    )
    graphics::axis(2, las = 1)
    graphics::axis(1, at = mids1, labels = names(linear_vals), tick = FALSE, line = 0.5, cex.axis = 0.92)
    graphics::mtext("A", side = 3, line = -0.7, adj = 0, font = 2, cex = 1.1)
    graphics::legend("top", inset = c(0, -0.02), legend = c("Observed full sample", "Removed >=1700"), fill = c("#D6EAF8", "#2E86C1"), bty = "n", cex = 0.86)
    graphics::text(mids1, linear_vals, labels = sprintf("%.3f", linear_vals), pos = ifelse(linear_vals >= 0, 3, 1), offset = 0.4, cex = 0.84)

    cens_span <- max(abs(cens_est), na.rm = TRUE)
    if (!is.finite(cens_span) || cens_span <= 0) cens_span <- 1
    cens_ylim <- c(min(0, cens_est) - 0.28 * cens_span, max(0, cens_est) + 0.34 * cens_span)
    mids2 <- graphics::barplot(
      cens_est,
      names.arg = "Right-censored model",
      col = "#1B4F72",
      border = NA,
      las = 1,
      ylim = cens_ylim,
      ylab = expression("Slope (" %*% 10^-5 * ")"),
      cex.names = 0.92,
      axes = FALSE,
      axisnames = FALSE
    )
    graphics::axis(2, las = 1)
    graphics::axis(1, at = mids2, labels = "Right-censored model", tick = FALSE, line = 0.5, cex.axis = 0.92)
    graphics::mtext("B", side = 3, line = -0.7, adj = 0, font = 2, cex = 1.1)
    graphics::legend("top", inset = c(0, -0.02), legend = "Right-censored model", fill = "#1B4F72", bty = "n", cex = 0.86)
    graphics::text(mids2, cens_est, labels = sprintf("%.3f\np=%s", cens_est, fmt_p_short(cens_p)), pos = ifelse(cens_est >= 0, 3, 1), offset = 0.45, cex = 0.82)
  }
  save_plot_file(plot_fun, path = abeta_fig_path, width = 11.8, height = 6.0, dpi = 320)
}

core_forest_rows <- list()
for (path_name in c("chp_strem2_linear_by_group.csv", "tau_linear_by_group.csv", "ptau_linear_by_group.csv")) {
  full_path <- file.path(summary_dir, path_name)
  if (!file.exists(full_path)) next
  df <- read_project_data(full_path)
  df <- df[df$term == "exposure", , drop = FALSE]
  if (grepl("^chp_strem2", path_name)) {
    df$family <- "sTREM2"
  } else if (grepl("^tau_", path_name)) {
    df$family <- "Tau"
  } else {
    df$family <- "P-Tau"
  }
  df$group_label <- ifelse(df$group == "AD", "Dementia", df$group)
  df$label <- paste(df$family, df$group_label, sep = " | ")
  core_forest_rows[[path_name]] <- df
}
core_forest_df <- if (length(core_forest_rows) > 0) do.call(rbind, core_forest_rows) else data.frame()
if (nrow(core_forest_df) > 0) {
  draw_simple_forest(
    df = core_forest_df,
    estimate_col = "std_beta",
    lower_col = "conf.low",
    upper_col = "conf.high",
    label_col = "label",
    path = forest_fig_path
  )
}

report_path <- file.path(result_report_dir, "Robustness_analysis_figures.md")
lines <- c(
  "# Robustness analysis figures",
  "",
  "## Matched versus unmatched sensitivity",
  sprintf("![Matched sensitivity](%s)", matched_fig_path),
  "",
  "## Matching balance (SMD / Love plot)",
  sprintf("![Love plot](%s)", love_fig_path),
  "",
  "## Overlap weighting balance (SMD / Love plot)",
  sprintf("![Overlap love plot](%s)", ow_love_fig_path),
  "",
  "## Unadjusted, primary adjusted, and overlap-weighted models",
  sprintf("![Overlap comparison](%s)", ow_compare_fig_path),
  "",
  "## Alternative ChP definitions",
  sprintf("![Alternative ChP](%s)", alt_chp_fig_path),
  "",
  "## Structure specificity",
  sprintf("![Structure specificity](%s)", structure_fig_path),
  "",
  "## Aβ truncation handling",
  sprintf("![Aβ sensitivity](%s)", abeta_fig_path),
  "",
  "## Core stage-stratified forest plot",
  sprintf("![Core forest](%s)", forest_fig_path)
)
writeLines(lines, con = report_path, useBytes = TRUE)

append_analysis_log(
  project_root = project_root,
  analysis_name = "10d_generate_robustness_figures",
  output_files = c(matched_fig_path, love_fig_path, love_csv_path, ow_love_fig_path, ow_compare_fig_path, alt_chp_fig_path, structure_fig_path, abeta_fig_path, forest_fig_path, report_path),
  note = "Generated robustness figures, including matched-vs-unmatched comparison, SMD/Love plot, and title-free panels.",
  summary_dir = result_summary_dir
)



