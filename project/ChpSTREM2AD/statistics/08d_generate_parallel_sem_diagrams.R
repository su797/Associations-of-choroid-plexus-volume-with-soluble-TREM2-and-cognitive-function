source(file.path(getwd(), "00_setup.R"))

suppressPackageStartupMessages({
  library(grid)
})

parallel_summary_path <- file.path(result_summary_dir, "sem_parallel_mediation_summary.csv")
find_latest_complete_result_dir_parallel <- function(project_root) {
  result_root <- file.path(project_root, "result")
  dirs <- list.dirs(result_root, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[grepl("^\\d{8}_\\d{6}$", basename(dirs))]
  dirs <- sort(dirs, decreasing = TRUE)
  for (d in dirs) {
    target <- file.path(d, "summary", "sem_parallel_mediation_summary.csv")
    if (file.exists(target)) return(d)
  }
  stop("No complete result directory found for parallel SEM diagrams.")
}

result_dir_use_parallel <- dirname(result_summary_dir)
if (!file.exists(parallel_summary_path)) {
  result_dir_use_parallel <- find_latest_complete_result_dir_parallel(project_root)
  parallel_summary_path <- file.path(result_dir_use_parallel, "summary", "sem_parallel_mediation_summary.csv")
}
if (!file.exists(parallel_summary_path)) {
  stop("Parallel SEM summary file not found: ", parallel_summary_path)
}
dir.create(file.path(result_dir_use_parallel, "figures"), recursive = TRUE, showWarnings = FALSE)

parallel_df <- utils::read.csv(parallel_summary_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
parallel_df <- parallel_df[parallel_df$cognition_model == "Cog_MMSE_MOCA", , drop = FALSE]
if (nrow(parallel_df) == 0) {
  stop("No parallel SEM rows found for Cog_MMSE_MOCA.")
}

group_order <- c("Overall", "CN", "MCI", "AD")
group_label_map <- c("Overall" = "Overall", "CN" = "CN", "MCI" = "MCI", "AD" = "Dementia")
parallel_label_map <- c(
  "ChP_to_sTREM2_PTAU_ABETA_parallel" = "ChP/ICV -> sTREM2 + P-Tau + Aβ -> Cognition",
  "ChP_to_sTREM2_TAU_ABETA_parallel" = "ChP/ICV -> sTREM2 + Tau + Aβ -> Cognition"
)
mediators_map <- list(
  "ChP_to_sTREM2_PTAU_ABETA_parallel" = c("MSD_STREM2CORRECTED", "PTAU", "S_ABETA"),
  "ChP_to_sTREM2_TAU_ABETA_parallel" = c("MSD_STREM2CORRECTED", "TAU", "S_ABETA")
)
mediator_label_map <- c(
  "MSD_STREM2CORRECTED" = "sTREM2",
  "PTAU" = "P-Tau",
  "TAU" = "Tau",
  "S_ABETA" = "Aβ"
)

fmt_p_parallel <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  if (x < 0.001) return("<0.001")
  sprintf("%.3f", x)
}

fmt_beta_parallel <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  sprintf("%+.3f", round(x, digits))
}

sig_font <- function(p) {
  ifelse(!is.na(p) && p < 0.05, 2, 1)
}

draw_parallel_diagram <- function(model_rows, model_name, group_name, panel_cex = 2, margin_cex = 2, configure_par = TRUE) {
  mediators <- mediators_map[[model_name]]
  if (is.null(mediators)) stop("Unknown parallel model: ", model_name)

  if (configure_par) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(1.0, 0.8, 4.2, 0.6), xpd = NA)
  }

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  par(xpd = NA)

  title(
    main = paste0(parallel_label_map[[model_name]] %||% model_name, " | ", group_label_map[[group_name]] %||% group_name),
    font.main = 2,
    cex.main = 1.05 * margin_cex,
    line = 0.8
  )

  x_box <- c(xleft = 0.04, ybottom = 0.42, xright = 0.22, ytop = 0.62)
  y_box <- c(xleft = 0.80, ybottom = 0.42, xright = 0.98, ytop = 0.62)
  m_boxes <- list(
    c(xleft = 0.37, ybottom = 0.72, xright = 0.59, ytop = 0.88),
    c(xleft = 0.37, ybottom = 0.47, xright = 0.59, ytop = 0.63),
    c(xleft = 0.37, ybottom = 0.22, xright = 0.59, ytop = 0.38)
  )

  rect(x_box["xleft"], x_box["ybottom"], x_box["xright"], x_box["ytop"], col = "#FFFFFF", border = "#222222", lwd = 5.8)
  rect(y_box["xleft"], y_box["ybottom"], y_box["xright"], y_box["ytop"], col = "#FFFFFF", border = "#222222", lwd = 5.8)
  text(mean(c(x_box["xleft"], x_box["xright"])), mean(c(x_box["ybottom"], x_box["ytop"])), "ChP/ICV", cex = 1.05 * panel_cex)
  text(mean(c(y_box["xleft"], y_box["xright"])), mean(c(y_box["ybottom"], y_box["ytop"])), "Cognition", cex = 1.05 * panel_cex)

  for (i in seq_along(mediators)) {
    m_var <- mediators[[i]]
    row <- model_rows[model_rows$mediator_var == m_var, , drop = FALSE]
    if (nrow(row) == 0) next
    m_box <- m_boxes[[i]]
    rect(m_box["xleft"], m_box["ybottom"], m_box["xright"], m_box["ytop"], col = "#FFFFFF", border = "#222222", lwd = 5.8)
    text(mean(c(m_box["xleft"], m_box["xright"])), mean(c(m_box["ybottom"], m_box["ytop"])), mediator_label_map[[m_var]], cex = 1.00 * panel_cex)

    arrows(x_box["xright"], mean(c(x_box["ybottom"], x_box["ytop"])), m_box["xleft"], mean(c(m_box["ybottom"], m_box["ytop"])), length = 0.07, lwd = 5.8)
    arrows(m_box["xright"], mean(c(m_box["ybottom"], m_box["ytop"])), y_box["xleft"], mean(c(y_box["ybottom"], y_box["ytop"])), length = 0.07, lwd = 5.8)

    text(
      x = 0.285,
      y = mean(c(m_box["ybottom"], m_box["ytop"])) + 0.03,
      labels = paste0("a=", fmt_beta_parallel(row$a[[1]]), "\np=", fmt_p_parallel(row$a_p[[1]])),
      cex = 0.74 * panel_cex,
      font = sig_font(row$a_p[[1]])
    )
    text(
      x = 0.675,
      y = mean(c(m_box["ybottom"], m_box["ytop"])) + 0.03,
      labels = paste0("b=", fmt_beta_parallel(row$b[[1]]), "\np=", fmt_p_parallel(row$b_p[[1]])),
      cex = 0.74 * panel_cex,
      font = sig_font(row$b_p[[1]])
    )
  }

  direct_row <- model_rows[1, , drop = FALSE]
  direct_y <- 0.12
  total_indirect_y <- 0.05
  arrows(x_box["xright"], direct_y, y_box["xleft"], direct_y, length = 0.07, lwd = 6.0)
  text(
    x = 0.50, y = direct_y + 0.05,
    labels = paste0("Direct (c')=", fmt_beta_parallel(direct_row$direct[[1]]), ", p=", fmt_p_parallel(direct_row$direct_p[[1]])),
    cex = 0.78 * panel_cex,
    font = sig_font(direct_row$direct_p[[1]])
  )
  text(
    x = 0.50, y = total_indirect_y,
    labels = paste0("Total indirect=", fmt_beta_parallel(direct_row$total_indirect[[1]]), ", p=", fmt_p_parallel(direct_row$total_indirect_p[[1]])),
    cex = 0.78 * panel_cex,
    font = sig_font(direct_row$total_indirect_p[[1]])
  )
}

plot_parallel_diagram <- function(model_rows, model_name, group_name, path) {
  png(path, width = 3000, height = 2100, res = 220)
  draw_parallel_diagram(model_rows, model_name, group_name, panel_cex = 2.8, margin_cex = 2.65, configure_par = TRUE)
  dev.off()
}

plot_parallel_combined <- function(specs, path) {
  png(path, width = 6600, height = 3900, res = 240)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(2, 4), mar = c(0.9, 0.7, 2.8, 0.5), oma = c(0.4, 0.4, 1.8, 0.4))
  for (spec in specs) {
    par(mar = c(1.0, 0.8, 3.4, 0.6), xpd = NA)
    draw_parallel_diagram(spec$model_rows, spec$model_name, spec$group_name, panel_cex = 2.30, margin_cex = 2.20, configure_par = FALSE)
  }
  mtext("Combined parallel-mediator SEM diagrams", side = 3, outer = TRUE, line = 0.2, cex = 2.0, font = 2)
  dev.off()
}

single_paths <- character(0)
combined_specs <- list()

for (model_name in names(parallel_label_map)) {
  for (group_name in group_order) {
    model_rows <- parallel_df[parallel_df$parallel_model == model_name & parallel_df$group == group_name, , drop = FALSE]
    if (nrow(model_rows) == 0) next
    plot_path <- file.path(result_dir_use_parallel, "figures", paste0("sem_parallel_", group_name, "_", model_name, "_Cog_MMSE_MOCA.png"))
    plot_parallel_diagram(model_rows, model_name, group_name, plot_path)
    single_paths <- c(single_paths, plot_path)
    combined_specs[[length(combined_specs) + 1]] <- list(model_rows = model_rows, model_name = model_name, group_name = group_name)
  }
}

combined_path <- file.path(result_dir_use_parallel, "figures", "sem_parallel_combined_Cog_MMSE_MOCA.png")
plot_parallel_combined(combined_specs, combined_path)

for (overleaf_root in c(
  file.path(project_root, "document", "ChpsTrem2"),
  file.path(project_root, "document", "Overleaf_CN"),
  file.path(project_root, "document", "Overleaf_EN"),
  file.path(project_root, "document", "Overleaf_JA")
)) {
  dir.create(file.path(overleaf_root, "figures"), recursive = TRUE, showWarnings = FALSE)
  for (src in c(single_paths, combined_path)) {
    file.copy(src, file.path(overleaf_root, "figures", basename(src)), overwrite = TRUE)
  }
}

if (exists("record_analysis_step", mode = "function")) {
  record_analysis_step(
    step = "parallel_sem_diagrams",
    output_files = c(single_paths, combined_path),
    note = "Generated parallel-mediator SEM structure diagrams (single-panel and combined-panel) for supplement.",
    summary_dir = result_figures_dir
  )
}
