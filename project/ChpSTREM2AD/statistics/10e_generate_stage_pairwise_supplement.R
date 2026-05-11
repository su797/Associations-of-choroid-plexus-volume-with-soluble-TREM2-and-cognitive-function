source(file.path(getwd(), "00_setup.R"))

suppressPackageStartupMessages({
  library(grid)
})

fmt_p_short <- function(x) {
  if (is.na(x)) return("")
  if (x < 0.001) return("<0.001")
  sprintf("%.3f", x)
}

sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  ""
}

fmt_p_star <- function(p) {
  if (is.na(p)) return("")
  paste0(fmt_p_short(p), sig_stars(p))
}

is_sig_p <- function(p) {
  is.finite(p) && !is.na(p) && p < 0.05
}

draw_three_line_table_png_local <- function(data, output_path, col_widths = NULL, font_size = 11, width_px = 3200, height_px = 980, left_align_cols = 1:2) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  n_rows <- nrow(data)
  n_cols <- ncol(data)

  if (is.null(col_widths)) {
    col_widths <- rep(1 / n_cols, n_cols)
  } else {
    col_widths <- col_widths / sum(col_widths)
  }

  png(filename = output_path, width = width_px, height = height_px, res = 220)
  grid.newpage()

  left <- 0.035
  right <- 0.965
  bottom <- 0.09
  table_top <- 0.84
  row_height <- min(0.12, max(0.065, (table_top - bottom) / (n_rows + 0.8)))

  x_edges <- c(left, left + cumsum((right - left) * col_widths))
  x_centers <- (x_edges[-length(x_edges)] + x_edges[-1]) / 2

  grid.lines(
    x = unit(c(left, right), "npc"),
    y = unit(c(table_top + row_height * 0.56, table_top + row_height * 0.56), "npc"),
    gp = gpar(lwd = 2)
  )
  grid.lines(
    x = unit(c(left, right), "npc"),
    y = unit(c(table_top - row_height * 0.56, table_top - row_height * 0.56), "npc"),
    gp = gpar(lwd = 1.4)
  )

  for (j in seq_len(n_cols)) {
    grid.text(
      names(data)[j],
      x = x_centers[j], y = table_top,
      gp = gpar(fontsize = font_size, fontface = "bold")
    )
  }

  for (i in seq_len(n_rows)) {
    y <- table_top - i * row_height
    for (j in seq_len(n_cols)) {
      left_aligned <- j %in% left_align_cols
      just <- if (left_aligned) "left" else "centre"
      x <- if (left_aligned) x_edges[j] + 0.006 else x_centers[j]
      grid.text(
        as.character(data[i, j]),
        x = x, y = y, just = just,
        gp = gpar(fontsize = font_size)
      )
    }
  }

  bottom_y <- max(bottom, table_top - n_rows * row_height - row_height * 0.70)
  grid.lines(
    x = unit(c(left, right), "npc"),
    y = unit(c(bottom_y, bottom_y), "npc"),
    gp = gpar(lwd = 2)
  )
  dev.off()
}

read_csv_utf8_local <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
}

find_latest_complete_result_dir_local <- function(project_root) {
  result_root <- file.path(project_root, "result")
  dirs <- list.dirs(result_root, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[grepl("^\\d{8}_\\d{6}$", basename(dirs))]
  dirs <- sort(dirs, decreasing = TRUE)
  required <- c("group_comparisons_pairwise.csv", "group_comparisons_summary_by_group.csv")
  for (d in dirs) {
    sdir <- file.path(d, "summary")
    if (all(file.exists(file.path(sdir, required)))) return(d)
  }
  stop("No complete result directory found for stage pairwise supplement.")
}

result_dir_use_local <- dirname(result_summary_dir)
required_local <- c("group_comparisons_pairwise.csv", "group_comparisons_summary_by_group.csv")
if (!all(file.exists(file.path(result_summary_dir, required_local)))) {
  result_dir_use_local <- find_latest_complete_result_dir_local(project_root)
}
result_summary_dir_use <- file.path(result_dir_use_local, "summary")
result_tables_dir_use <- file.path(result_dir_use_local, "tables")
result_figures_dir_use <- file.path(result_dir_use_local, "figures")
dir.create(result_tables_dir_use, recursive = TRUE, showWarnings = FALSE)
dir.create(result_figures_dir_use, recursive = TRUE, showWarnings = FALSE)

pairwise_path <- file.path(result_summary_dir_use, "group_comparisons_pairwise.csv")
summary_by_group_path <- file.path(result_summary_dir_use, "group_comparisons_summary_by_group.csv")
analysis_data_path_use <- analysis_data_path
if (!file.exists(analysis_data_path_use)) {
  analysis_data_path_use <- file.path(project_root, "data", "clean", "ChpSTREM2AD_analysis_dataset.csv")
}

pairwise_df <- read_csv_utf8_local(pairwise_path)
summary_by_group_df <- read_csv_utf8_local(summary_by_group_path)
analysis_df <- read_project_data(analysis_data_path_use)
analysis_df$S_DX_plot <- factor(analysis_df$S_DX_label, levels = c("CN", "MCI", "AD"), labels = c("CN", "MCI", "Dementia"))

get_pairwise_p <- function(variable, pair_label) {
  candidates <- switch(
    pair_label,
    "CN vs MCI" = c("CN vs MCI", "MCI vs CN", "MCI-CN", "CN-MCI"),
    "MCI vs Dementia" = c("MCI vs AD", "AD vs MCI", "MCI-AD", "AD-MCI", "MCI vs Dementia", "Dementia vs MCI"),
    "CN vs Dementia" = c("CN vs AD", "AD vs CN", "CN-AD", "AD-CN", "CN vs Dementia", "Dementia vs CN"),
    pair_label
  )
  row <- pairwise_df[pairwise_df$variable == variable & pairwise_df$comparison %in% candidates, , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  row$p_value[[1]]
}

get_summary_text <- function(variable, group) {
  row <- summary_by_group_df[summary_by_group_df$variable == variable & summary_by_group_df$group == group, , drop = FALSE]
  if (nrow(row) == 0) return("")
  row$summary[[1]]
}

female_summary <- function(group_label) {
  dx_value <- c("CN" = "CN", "MCI" = "MCI", "Dementia" = "AD")[[group_label]]
  sub <- analysis_df[analysis_df$S_DX_label == dx_value, , drop = FALSE]
  n <- nrow(sub)
  female_n <- sum(sub$S_PTGENDER_label == "Female", na.rm = TRUE)
  paste0(female_n, " (", sprintf("%.1f", female_n / n * 100), "%)")
}

pairwise_sex_p <- function(group_a, group_b) {
  map_dx <- c("CN" = "CN", "MCI" = "MCI", "Dementia" = "AD")
  sub <- analysis_df[analysis_df$S_DX_label %in% c(map_dx[[group_a]], map_dx[[group_b]]), , drop = FALSE]
  sub$grp <- factor(sub$S_DX_label, levels = c(map_dx[[group_a]], map_dx[[group_b]]), labels = c(group_a, group_b))
  tab <- table(sub$grp, sub$S_PTGENDER_label)
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  stats::fisher.test(tab)$p.value
}

table_s1a <- data.frame(
  Variable = c("Age", "Female, n (%)", "ChP/ICV", "sTREM2", "Aβ", "Tau", "P-Tau"),
  CN = c(
    get_summary_text("S_AGE", "CN"),
    female_summary("CN"),
    get_summary_text("ChPICV", "CN"),
    get_summary_text("MSD_STREM2CORRECTED", "CN"),
    get_summary_text("S_ABETA", "CN"),
    get_summary_text("TAU", "CN"),
    get_summary_text("PTAU", "CN")
  ),
  MCI = c(
    get_summary_text("S_AGE", "MCI"),
    female_summary("MCI"),
    get_summary_text("ChPICV", "MCI"),
    get_summary_text("MSD_STREM2CORRECTED", "MCI"),
    get_summary_text("S_ABETA", "MCI"),
    get_summary_text("TAU", "MCI"),
    get_summary_text("PTAU", "MCI")
  ),
  Dementia = c(
    get_summary_text("S_AGE", "AD"),
    female_summary("Dementia"),
    get_summary_text("ChPICV", "AD"),
    get_summary_text("MSD_STREM2CORRECTED", "AD"),
    get_summary_text("S_ABETA", "AD"),
    get_summary_text("TAU", "AD"),
    get_summary_text("PTAU", "AD")
  ),
  `CN vs MCI` = c(
    fmt_p_star(get_pairwise_p("S_AGE", "CN vs MCI")),
    fmt_p_star(pairwise_sex_p("CN", "MCI")),
    fmt_p_star(get_pairwise_p("ChPICV", "CN vs MCI")),
    fmt_p_star(get_pairwise_p("MSD_STREM2CORRECTED", "CN vs MCI")),
    fmt_p_star(get_pairwise_p("S_ABETA", "CN vs MCI")),
    fmt_p_star(get_pairwise_p("TAU", "CN vs MCI")),
    fmt_p_star(get_pairwise_p("PTAU", "CN vs MCI"))
  ),
  `MCI vs Dementia` = c(
    fmt_p_star(get_pairwise_p("S_AGE", "MCI vs Dementia")),
    fmt_p_star(pairwise_sex_p("MCI", "Dementia")),
    fmt_p_star(get_pairwise_p("ChPICV", "MCI vs Dementia")),
    fmt_p_star(get_pairwise_p("MSD_STREM2CORRECTED", "MCI vs Dementia")),
    fmt_p_star(get_pairwise_p("S_ABETA", "MCI vs Dementia")),
    fmt_p_star(get_pairwise_p("TAU", "MCI vs Dementia")),
    fmt_p_star(get_pairwise_p("PTAU", "MCI vs Dementia"))
  ),
  `CN vs Dementia` = c(
    fmt_p_star(get_pairwise_p("S_AGE", "CN vs Dementia")),
    fmt_p_star(pairwise_sex_p("CN", "Dementia")),
    fmt_p_star(get_pairwise_p("ChPICV", "CN vs Dementia")),
    fmt_p_star(get_pairwise_p("MSD_STREM2CORRECTED", "CN vs Dementia")),
    fmt_p_star(get_pairwise_p("S_ABETA", "CN vs Dementia")),
    fmt_p_star(get_pairwise_p("TAU", "CN vs Dementia")),
    fmt_p_star(get_pairwise_p("PTAU", "CN vs Dementia"))
  ),
  Method = c("Wilcoxon", "Fisher exact", "Wilcoxon", "Tukey HSD", "Wilcoxon", "Wilcoxon", "Wilcoxon"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

summary_csv <- file.path(result_summary_dir_use, "stage_pairwise_key_variables.csv")
write_csv_utf8(table_s1a, summary_csv, row.names = FALSE)

table_png <- file.path(result_tables_dir_use, "stage_pairwise_key_variables_three_line.png")
draw_three_line_table_png_local(
  table_s1a,
  table_png,
  col_widths = c(0.15, 0.13, 0.13, 0.13, 0.12, 0.14, 0.14, 0.10),
  font_size = 10.5,
  width_px = 3600,
  height_px = 920,
  left_align_cols = c(1)
)

plot_vars <- c("ChPICV", "MSD_STREM2CORRECTED", "S_ABETA", "TAU", "PTAU")
plot_labels <- c(
  "S_AGE" = "Age (years)",
  "ChPICV" = "ChP/ICV",
  "MSD_STREM2CORRECTED" = "sTREM2",
  "S_ABETA" = "Aβ",
  "TAU" = "Tau",
  "PTAU" = "P-Tau"
)
plot_table_rows <- c(
  "S_AGE" = "Age",
  "ChPICV" = "ChP/ICV",
  "MSD_STREM2CORRECTED" = "sTREM2",
  "S_ABETA" = "Aβ",
  "TAU" = "Tau",
  "PTAU" = "P-Tau"
)
plot_palette <- c("CN" = "#2C7FB8", "MCI" = "#F39C34", "Dementia" = "#C0392B")

draw_half_violin_panel <- function(vals, grp, main_label, row_name) {
  ok <- is.finite(vals) & !is.na(grp)
  vals <- vals[ok]
  grp <- droplevels(grp[ok])
  groups <- c("CN", "MCI", "Dementia")
  split_vals <- lapply(groups, function(g) vals[grp == g])
  names(split_vals) <- groups

  finite_vals <- vals[is.finite(vals)]
  y_rng <- range(finite_vals, na.rm = TRUE)
  y_span <- diff(y_rng)
  if (!is.finite(y_span) || y_span == 0) y_span <- max(abs(y_rng[1]), 1)
  ylim_use <- c(y_rng[1] - 0.08 * y_span, y_rng[2] + 0.28 * y_span)
  pretty_y <- pretty(y_rng, n = 3)
  pretty_y <- pretty_y[pretty_y >= ylim_use[1] & pretty_y <= ylim_use[2]]
  max_abs <- max(abs(pretty_y), na.rm = TRUE)
  y_digits <- if (max_abs < 0.01) 4 else if (max_abs < 1) 2 else 0
  y_labels <- formatC(pretty_y, format = "f", digits = y_digits)

  plot(
    NA, NA, xlim = c(-0.08, 4.05), ylim = ylim_use,
    xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = main_label,
    cex.main = 1.25, cex.axis = 1.00, frame.plot = FALSE
  )
  axis(1, at = 1:3, labels = groups, cex.axis = 1.0)
  axis(2, at = pretty_y, labels = FALSE, las = 1, cex.axis = 0.78)
  text(
    x = rep(-0.34, length(pretty_y)),
    y = pretty_y,
    labels = y_labels,
    adj = c(1, 0.5),
    cex = 0.76,
    xpd = NA
  )
  box(bty = "l")

  for (i in seq_along(split_vals)) {
    vec <- split_vals[[i]]
    if (length(vec) >= 2) {
      dens <- density(vec, na.rm = TRUE, adjust = 1.1)
      width <- dens$y / max(dens$y, na.rm = TRUE) * 0.30
      polygon(
        x = c(i, i + width, i),
        y = c(dens$x[1], dens$x, dens$x[length(dens$x)]),
        col = adjustcolor(plot_palette[names(split_vals)[i]], alpha.f = 0.42),
        border = plot_palette[names(split_vals)[i]],
        lwd = 1.2
      )
    }

    boxplot(
      vec,
      at = i, add = TRUE, boxwex = 0.10, outline = FALSE, axes = FALSE,
      col = adjustcolor("white", alpha.f = 0.92),
      border = plot_palette[names(split_vals)[i]],
      medcol = plot_palette[names(split_vals)[i]],
      whiskcol = plot_palette[names(split_vals)[i]],
      staplecol = plot_palette[names(split_vals)[i]],
      lwd = 1.2
    )

    stripchart(
      vec, method = "jitter", at = i + 0.08, vertical = TRUE, add = TRUE,
      pch = 16, cex = 0.34,
      col = adjustcolor(plot_palette[names(split_vals)[i]], alpha.f = 0.20),
      jitter = 0.06
    )
  }

  variable_name <- names(plot_table_rows)[plot_table_rows == row_name]
  p_cn_mci_num <- get_pairwise_p(variable_name, "CN vs MCI")
  p_mci_dem_num <- get_pairwise_p(variable_name, "MCI vs Dementia")
  p_cn_dem_num <- get_pairwise_p(variable_name, "CN vs Dementia")

  y_top <- ylim_use[2] - 0.04 * diff(ylim_use)
  line_gap <- 0.052 * diff(ylim_use)
  labels <- list(
    list(text = paste0("CN vs MCI: ", fmt_p_short(p_cn_mci_num), sig_stars(p_cn_mci_num)), sig = is_sig_p(p_cn_mci_num)),
    list(text = paste0("MCI vs Dem: ", fmt_p_short(p_mci_dem_num), sig_stars(p_mci_dem_num)), sig = is_sig_p(p_mci_dem_num)),
    list(text = paste0("CN vs Dem: ", fmt_p_short(p_cn_dem_num), sig_stars(p_cn_dem_num)), sig = is_sig_p(p_cn_dem_num))
  )
  for (k in seq_along(labels)) {
    text(
      4.00, y_top - (k - 1) * line_gap,
      labels = labels[[k]]$text,
      adj = c(1, 1),
      cex = 0.76,
      font = if (labels[[k]]$sig) 2 else 1,
      xpd = FALSE
    )
  }
}

fig_png <- file.path(result_figures_dir_use, "stage_group_distribution_panel.png")
png(fig_png, width = 2400, height = 1600, res = 220)
old_par <- par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(mfrow = c(2, 3), mar = c(4.5, 11.0, 3.0, 0.5), oma = c(0, 0, 0, 0))
for (v in plot_vars) {
  draw_half_violin_panel(
    vals = analysis_df[[v]],
    grp = analysis_df$S_DX_plot,
    main_label = plot_labels[[v]],
    row_name = plot_table_rows[[v]]
  )
}
plot.new()
dev.off()

age_fig_png <- file.path(result_figures_dir_use, "stage_age_distribution.png")
png(age_fig_png, width = 1200, height = 1100, res = 220)
old_par_age <- par(no.readonly = TRUE)
on.exit(par(old_par_age), add = TRUE)
par(mfrow = c(1, 1), mar = c(4.8, 11.0, 3.1, 0.5), oma = c(0, 0, 0, 0))
draw_half_violin_panel(
  vals = analysis_df$S_AGE,
  grp = analysis_df$S_DX_plot,
  main_label = plot_labels[["S_AGE"]],
  row_name = plot_table_rows[["S_AGE"]]
)
dev.off()

for (overleaf_root in c(
  file.path(project_root, "document", "ChpsTrem2"),
  file.path(project_root, "document", "Overleaf_CN"),
  file.path(project_root, "document", "Overleaf_EN"),
  file.path(project_root, "document", "Overleaf_JA")
)) {
  dir.create(file.path(overleaf_root, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(overleaf_root, "figures"), recursive = TRUE, showWarnings = FALSE)
  file.copy(table_png, file.path(overleaf_root, "tables", basename(table_png)), overwrite = TRUE)
  file.copy(fig_png, file.path(overleaf_root, "figures", basename(fig_png)), overwrite = TRUE)
  file.copy(age_fig_png, file.path(overleaf_root, "figures", basename(age_fig_png)), overwrite = TRUE)
}
