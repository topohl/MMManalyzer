#' Sleep Analysis with Scoped I/O and Phase/Run Slicing
#'
#' This script performs a comprehensive analysis of sleep behavior based on activity index data.
#' It handles data loading, preprocessing, exclusion of specific animals, and calculation of
#' sleep metrics (total sleep, bouts, percentages, etc.). The analysis is performed at two levels:
#' 1. **Overall Aggregation:** Averages sleep metrics per animal across phases and sexes.
#' 2. **Per-Run (Consecutive) Analysis:** Breaks down sleep metrics by consecutive active/inactive
#'    phases (runs) to track changes over time.
#'
#' @section Key Features:
#' - **Dependency Management:** Automatically installs and loads required packages (e.g., `ggplot2`, `dplyr`, `rstatix`).
#' - **Data Cleaning:** Normalizes animal IDs and filters based on exclusion lists and specific experimental "Changes" (e.g., CC4, SocP).
#' - **Metric Calculation:** Computes `SleepCount`, `PercentageSleep`, `TotalSleepingTime`, `SleepBouts`, and `AvgSleepBoutDuration`.
#' - **Directory Management:** Dynamically creates a structured output hierarchy based on Change, Phase, and Sex to segregate results.
#' - **Statistical Testing:**
#'   - Uses `testAndPlotVariable` (from `MMM_functions.R`) for standard group comparisons.
#'   - Implements custom ANOVA and Tukey HSD post-hoc tests via `rstatix` for per-run analysis.
#' - **Visualization:** Generates and saves SVG plots for overall comparisons and per-run trajectories.
#'
#' @section Input Requirements:
#' - **`data_filtered`:** A dataframe containing `AnimalNum`, `ActivityIndex`, `DateTime`, `Phase`, `Group`, `Batch`, and optionally `Sex`.
#'   *Note: If not present in the environment, the script expects a CSV at `sleep_directory/preprocessed_input.csv`.*
#' - **Exclusion Files:** Reads animal exclusion lists and metadata from defined paths.
#' - **Custom Functions:** Requires `MMM_functions.R` to be sourced for plotting and statistical wrappers.
#'
#' @section Outputs:
#' - **Tables:** CSV files containing calculated metrics, statistical test results (ANOVA/KW), and post-hoc comparisons.
#' - **Plots:** SVG files showing distribution of sleep metrics across groups, faceted by Sex or Phase where applicable.
#' - **Logs:** Directory structure prepared for logging (though specific log writing implementation depends on helper functions).
#'
#' @author TobiAS Pohl
#' @seealso `MMM_functions.R` for the definition of `testAndPlotVariable`.
# ==============================
# Sleep analysis with scoped I/O
# ==============================

# -------- Required packages --------
required_packages <- c(
  "ggplot2", "gridExtra", "cowplot", "tidyr", "dplyr",
  "rstatix", "readxl", "svglite", "stringr", "openxlsx", "rlang", "ggpubr"
)

install_and_load <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}
install_and_load(required_packages)

# -------- Nature-Style Aesthetics --------
theme_nature <- function() {
  ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "black", family = "sans"),
      axis.text = ggplot2::element_text(color = "black", size = 9),
      axis.title = ggplot2::element_text(face = "bold", size = 10),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.3),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9),
      legend.key.size = ggplot2::unit(0.8, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid = ggplot2::element_blank()
    )
}

# -------- Constants and paths --------
sleep_directory  <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Sleep"
graphs_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Sleep"

# Source custom functions (must define testAndPlotVariable, savePlotsInDir, incldeFactorExist)
source("C:/Users/topohl/Documents/GitHub/MMManalyzer/Analysis/MMM_functions.R")

# -------- Single explicit selector for Change --------
# options: "CC4" (SIS), "SocP" (post SP), or "All" for no slicing
changeToAnalyze <- "All"

# -------- Inputs: metadata lists --------
sus_animals <- readLines("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")
excluded_animals <- readLines("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/excludedAnimals.csv")

# Helpers to normalize IDs
clean_id <- function(x) x %>% as.character() %>% stringr::str_trim() %>% toupper()
pad4 <- function(x) ifelse(grepl("^[0-9]+$", x), stringr::str_pad(x, 4, pad = "0"), x)

# -------- Data pre-filtering --------
# Expect data_filtered exists upstream; if not, load precomputed input
# data_filtered <- read.csv(file.path(sleep_directory, "preprocessed_input.csv"))

# Normalize and exclude animals
excluded_animals <- clean_id(pad4(excluded_animals))
if (!("AnimalNum" %in% names(data_filtered))) stop("data_filtered must have AnimalNum")
data_filtered <- data_filtered %>% dplyr::mutate(AnimalNum = clean_id(pad4(AnimalNum)))

# Ensure Change is NA-safe and aligned with selector
data_filtered <- data_filtered %>% dplyr::mutate(Change = tidyr::replace_na(Change, "All"))

# Derive analyzeSingleChange from the selector
analyzeSingleChange <- (changeToAnalyze != "All")

# Apply exclusion and optional change slicing
data_filtered <- data_filtered %>% dplyr::filter(!AnimalNum %in% excluded_animals)
if (analyzeSingleChange) {
  data_filtered <- data_filtered %>% dplyr::filter(!is.na(Change) & Change == changeToAnalyze)
}

# -------- Group colors and analysis flags --------
groupColors <- c("#3d3c6f", "#C6C3BB", "#E63946")

# Determine factor availability
include_phase <- incldeFactorExist("Phase", data_filtered, TRUE)
include_sex   <- incldeFactorExist("Sex", data_filtered, TRUE)

# -------- Helpers: safe names and directory building --------
safe_name <- function(x) {
  x <- ifelse(is.null(x) || is.na(x) || x == "" || x == " ", "All", x)
  x <- gsub("[^A-Za-z0-9_\\-\\.]+", "_", x)
  x
}

build_dirs <- function(base_dir, change, phase, sex) {
  ch <- safe_name(change); ph <- safe_name(phase); sx <- safe_name(sex)
  dir_out <- file.path(base_dir, paste0("Change=", ch), paste0("Phase=", ph), paste0("Sex=", sx))
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  dir_out
}

get_save_paths <- function(base_root, change, phase, sex) {
  dir_root <- build_dirs(base_root, change, phase, sex)
  tables <- file.path(dir_root, "tables")
  plots <- file.path(dir_root, "plots")
  logs <- file.path(dir_root, "logs")
  
  if (!dir.exists(tables)) dir.create(tables, recursive = TRUE)
  if (!dir.exists(plots))  dir.create(plots,  recursive = TRUE)
  if (!dir.exists(logs))   dir.create(logs,   recursive = TRUE)
  
  list(root = dir_root, tables = tables, plots = plots, logs = logs)
}

save_plot_by_slice <- function(plot, paths, var, ext = ".svg", width = 3, height = 4) {
  fname <- paste0("plot_", safe_name(var), ext)
  ggsave(filename = file.path(paths$plots, fname), plot = plot, width = width, height = height, device = sub("^\\.", "", ext))
}

save_table_by_slice <- function(df, paths, stem) {
  fp <- file.path(paths$tables, paste0(safe_name(stem), ".csv"))
  write.csv(df, fp, row.names = FALSE)
  fp
}

# -------- Metric calculations --------
calculate_sleep_metrics <- function(data) {
  grouping_cols <- intersect(c("AnimalNum", "Batch", "Change", "Group", "Phase", "Sex"), names(data))
  
  data %>%
    dplyr::mutate(Change = tidyr::replace_na(Change, "All")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
    dplyr::summarize(
      SleepCount = sum(ActivityIndex == 0, na.rm = TRUE),
      TotalCount = dplyr::n(),
      PercentageSleep = ifelse(TotalCount == 0, NA_real_, (SleepCount / TotalCount) * 100),
      TotalSleepingTime = sum(ActivityIndex == 0, na.rm = TRUE),
      SleepBouts = sum(ActivityIndex == 0 & dplyr::lag(ActivityIndex, default = 1) != 0, na.rm = TRUE),
      AvgSleepBoutDuration = dplyr::if_else(SleepBouts == 0, 0, TotalSleepingTime / SleepBouts),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup()
}

total_sleep_info_per_change <- calculate_sleep_metrics(data_filtered)
if (analyzeSingleChange) {
  total_sleep_info_per_change <- total_sleep_info_per_change %>% dplyr::filter(!is.na(Change) & Change == changeToAnalyze)
}

summarize_data <- function(data) {
  grouping_cols <- intersect(c("AnimalNum", "Phase", "Sex"), names(data))
  
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
    dplyr::summarize(
      Batch = dplyr::first(Batch),
      Group = dplyr::first(Group),
      Change = dplyr::first(Change),
      SleepBouts = sum(SleepBouts, na.rm = TRUE),
      TotalSleepingTime = sum(TotalSleepingTime, na.rm = TRUE),
      PercentageSleep = mean(PercentageSleep, na.rm = TRUE),
      AvgSleepBoutDuration = mean(AvgSleepBoutDuration, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup()
}
overall_data <- summarize_data(total_sleep_info_per_change)

# -------- CV calculation (optional) --------
calculate_cv <- function(data) {
  data %>%
    dplyr::group_by(AnimalNum, Batch, Group, Phase) %>%
    dplyr::summarize(
      CV_SleepCount = stats::sd(SleepCount, na.rm = TRUE) / mean(SleepCount, na.rm = TRUE),
      CV_TotalCount = stats::sd(TotalCount, na.rm = TRUE) / mean(TotalCount, na.rm = TRUE),
      CV_PercentageSleep = stats::sd(PercentageSleep, na.rm = TRUE) / mean(PercentageSleep, na.rm = TRUE),
      CV_TotalSleepingTime = stats::sd(TotalSleepingTime, na.rm = TRUE) / mean(TotalSleepingTime, na.rm = TRUE),
      CV_SleepBouts = stats::sd(SleepBouts, na.rm = TRUE) / mean(SleepBouts, na.rm = TRUE),
      CV_AvgSleepBoutDuration = stats::sd(AvgSleepBoutDuration, na.rm = TRUE) / mean(AvgSleepBoutDuration, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup()
}

compute_cv <- FALSE
if (compute_cv) {
  cv_data <- calculate_cv(total_sleep_info_per_change)
  overall_data <- merge(overall_data, cv_data, by = c("AnimalNum", "Batch", "Group", "Phase"), all.x = TRUE)
}

# -------- Variables and factors for general analysis --------
columns_to_plot <- setdiff(names(overall_data), c("AnimalNum", "Group", "Phase", "Batch", "Sex", "Change"))
phases <- if (isTRUE(include_phase)) c("Active", "Inactive") else " "
sexes  <- if (isTRUE(include_sex))   c("m", "f")            else " "
current_change <- if (analyzeSingleChange) changeToAnalyze else "All"

# -------- NA-safe slicer --------
filter_slice <- function(df, phase, sex, change) {
  out <- df
  if (!identical(phase, " ") && !identical(phase, "All")) out <- dplyr::filter(out, !is.na(Phase) & Phase == phase)
  if (!identical(sex, " ") && !identical(sex, "All"))     out <- dplyr::filter(out, !is.na(Sex) & Sex == sex)
  if (!identical(change, " ") && !identical(change, "All")) out <- dplyr::filter(out, !is.na(Change) & Change == change)
  out
}

# -------- General slice analysis (uses MMM test function) --------
all_test_results <- list()
all_posthoc_results <- list()
all_plots <- list()

for (variable in columns_to_plot) {
  for (phase in phases) {
    for (sex in sexes) {
      subset_df <- filter_slice(overall_data, phase, sex, current_change)
      if (nrow(subset_df) == 0) next
      
      result <- testAndPlotVariable(overall_data, variable, phase, sex)
      if (is.null(result)) next
      
      paths <- get_save_paths(sleep_directory, current_change, phase, sex)
      
      if (!is.null(result$testResults)) {
        tr <- result$testResults; tr$Variable <- variable
        save_table_by_slice(as.data.frame(tr), paths, paste0("tests_", variable))
        all_test_results <- c(all_test_results, list(as.data.frame(tr)))
      }
      
      if (!is.null(result$posthocResults)) {
        save_table_by_slice(result$posthocResults, paths, paste0("posthoc_", variable))
        all_posthoc_results <- c(all_posthoc_results, list(result$posthocResults))
      }
      
      if (!is.null(result$plot)) {
        # Note: We keep the MMM plot output unchanged as it's defined externally
        save_plot_by_slice(result$plot, paths, variable, ext = ".svg", width = 7, height = 5)
        all_plots <- c(all_plots, list(result$plot))
      }
    }
  }
}

global_paths <- get_save_paths(sleep_directory, current_change, "All", "All")
if (length(all_test_results) > 0) write.csv(dplyr::bind_rows(all_test_results), file = file.path(global_paths$tables, "test_results_Sleep.csv"), row.names = FALSE)
if (length(all_posthoc_results) > 0) write.csv(dplyr::bind_rows(all_posthoc_results), file = file.path(global_paths$tables, "posthoc_results_Sleep.csv"), row.names = FALSE)
write.csv(overall_data, file = file.path(global_paths$tables, "overallData.csv"), row.names = FALSE)
write.csv(total_sleep_info_per_change, file = file.path(global_paths$tables, "total_sleep_info_per_change.csv"), row.names = FALSE)
if (exists("cv_data")) write.csv(cv_data, file = file.path(global_paths$tables, "cv.csv"), row.names = FALSE)

if (length(all_plots) > 0) {
  grid_plots <- gridExtra::grid.arrange(grobs = all_plots, ncol = 4)
  ggsave(file = file.path(global_paths$plots, "allPlots.svg"), plot = grid_plots, width = 6, height = 9, device = "svg")
}

# =========================
# Per-run (consecutive) stats + rstatix + MMM tests
# =========================

byrun_root <- file.path(sleep_directory, "ByRun")
if (!dir.exists(byrun_root)) dir.create(byrun_root, recursive = TRUE)

build_run_dirs <- function(base_dir, phase, run_id) {
  dir_root <- file.path(base_dir, paste0("Phase=", safe_name(phase)), paste0("Run=", safe_name(run_id)))
  tables <- file.path(dir_root, "tables"); plots <- file.path(dir_root, "plots"); logs <- file.path(dir_root, "logs")
  if (!dir.exists(tables)) dir.create(tables, recursive = TRUE)
  if (!dir.exists(plots))  dir.create(plots,  recursive = TRUE)
  if (!dir.exists(logs))   dir.create(logs,   recursive = TRUE)
  list(root = dir_root, tables = tables, plots = plots, logs = logs)
}
has_sex <- "Sex" %in% names(data_filtered)

# Phase-conditional run filtering
data_runs <- data_filtered %>%
  dplyr::mutate(
    RunID = dplyr::case_when(
      Phase == "Active"   ~ ConsecActive,
      Phase == "Inactive" ~ ConsecInactive,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::filter(
    dplyr::case_when(
      Phase == "Active"   ~ ConsecActive   > 0,
      Phase == "Inactive" ~ ConsecInactive > 0,
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::filter(!is.na(RunID)) %>%
  dplyr::mutate(Change = tidyr::replace_na(Change, "All")) %>%
  tidyr::drop_na(DateTime)

if (exists("analyzeSingleChange") && isTRUE(analyzeSingleChange)) {
  data_runs <- data_runs %>% dplyr::filter(Change == changeToAnalyze)
}

group_vars <- c("AnimalNum", "Batch", "Change", "Group", "Phase", "RunID")
if (has_sex) group_vars <- c("AnimalNum", "Batch", "Change", "Group", "Sex", "Phase", "RunID")

required_cols <- c(group_vars, "ActivityIndex", "DateTime")
missing <- setdiff(required_cols, names(data_runs))
if (length(missing)) stop("Missing columns in data_runs: ", paste(missing, collapse = ", "))

per_run_stats <- data_runs %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
  dplyr::summarize(
    SleepCount = sum(ActivityIndex == 0, na.rm = TRUE),
    TotalCount = dplyr::n(),
    PercentageSleep = ifelse(TotalCount == 0, NA_real_, (SleepCount / TotalCount) * 100),
    TotalSleepingTime = sum(ActivityIndex == 0, na.rm = TRUE),
    SleepBouts = sum(ActivityIndex == 0 & dplyr::lag(ActivityIndex, default = 1) != 0, na.rm = TRUE),
    AvgSleepBoutDuration = dplyr::if_else(SleepBouts == 0, 0, TotalSleepingTime / SleepBouts),
    StartTime = suppressWarnings(min(DateTime, na.rm = TRUE)),
    EndTime   = suppressWarnings(max(DateTime, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    StartTime = dplyr::if_else(is.infinite(StartTime), as.POSIXct(NA), StartTime),
    EndTime   = dplyr::if_else(is.infinite(EndTime),   as.POSIXct(NA), EndTime)
  )

unique_runs <- per_run_stats %>% dplyr::arrange(Phase, RunID) %>% dplyr::distinct(Phase, RunID)

# rstatix helpers
get_run_formula <- function(has_sex) { if (isTRUE(has_sex)) PercentageSleep ~ Group * Sex else PercentageSleep ~ Group }
run_anova <- function(df, has_sex) { fm <- get_run_formula(has_sex); df %>% rstatix::anova_test(formula = fm, effect.size = "ges") }
run_tukey <- function(df, has_sex) {
  if (isTRUE(has_sex)) {
    tuk_group <- rstatix::tukey_hsd(df, PercentageSleep ~ Group)
    tuk_within_sex <- df %>% dplyr::group_by(Sex) %>% rstatix::tukey_hsd(PercentageSleep ~ Group) %>% dplyr::ungroup()
    aov_fit <- aov(PercentageSleep ~ Group * Sex, data = df); tuk_inter <- rstatix::tukey_hsd(aov_fit)
    list(main_group = tuk_group, within_sex = tuk_within_sex, interaction = tuk_inter)
  } else {
    list(main_group = rstatix::tukey_hsd(df, PercentageSleep ~ Group))
  }
}

# Per-run loop: save per-run tables, plots, rstatix stats, and MMM stats
for (i in seq_len(nrow(unique_runs))) {
  ph <- unique_runs$Phase[i]
  rn <- unique_runs$RunID[i]

  run_df <- per_run_stats %>%
    dplyr::filter(Phase == ph, RunID == rn) %>%
    dplyr::mutate(
      Group = factor(Group, levels = c("CON", "RES", "SUS")),
      Sex = if ("Sex" %in% names(.)) factor(Sex, levels = c("m", "f")) else Sex
    )

  paths <- build_run_dirs(byrun_root, ph, rn)
  save_table_by_slice(run_df, paths, paste0("per_run_stats_", ph, "_run", rn))

  if (nrow(run_df) == 0) next

  # Nature-style palette consistent with sex comparison plot
  group_palette <- c("CON" = "#3d3c6f", "RES" = "#C6C3BB", "SUS" = "#E63946")

  # Compute raw_slice per-animal PercentageSleep for statistical tests
  raw_slice <- data_runs %>%
    dplyr::filter(Phase == ph) %>%
    dplyr::filter(
      dplyr::case_when(
        ph == "Active"   ~ ConsecActive == rn,
        ph == "Inactive" ~ ConsecInactive == rn,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::mutate(
      Group = factor(Group, levels = c("CON", "RES", "SUS")),
      Sex = if ("Sex" %in% names(.)) factor(Sex, levels = c("m", "f")) else Sex
    )

  group_cols <- c("AnimalNum", "Group", "Phase")
  if (has_sex && "Sex" %in% names(raw_slice)) group_cols <- c(group_cols, "Sex")

  raw_slice_stats <- raw_slice %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarize(
      SleepCount = sum(ActivityIndex == 0, na.rm = TRUE),
      TotalCount = dplyr::n(),
      PercentageSleep = ifelse(TotalCount == 0, NA_real_, (SleepCount / TotalCount) * 100),
      .groups = "drop"
    )

  # Plot styled to match sex comparison plot (single panel, shape by sex if available)
  if (has_sex && "Sex" %in% names(run_df)) {
    p <- ggplot2::ggplot(run_df, ggplot2::aes(x = Group, y = PercentageSleep)) +
      ggplot2::geom_point(
        ggplot2::aes(color = Group, shape = Sex),
        position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
        size = 2, stroke = 0.5, alpha = 0.7
      ) +
      ggplot2::scale_color_manual(values = group_palette) +
      ggplot2::scale_shape_manual(values = c("m" = 1, "f" = 16)) +
      ggplot2::stat_summary(
        ggplot2::aes(group = Sex),
        fun.data = function(y) {
          m <- stats::median(y, na.rm = TRUE)
          s <- stats::sd(y, na.rm = TRUE)
          if (!is.finite(s)) s <- 0
          data.frame(y = m, ymin = m - s, ymax = m + s)
        },
        geom = "errorbar", color = "black", width = 0.2, linewidth = 0.4,
        position = ggplot2::position_dodge(width = 0.8)
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(group = Sex),
        fun = function(y) stats::median(y, na.rm = TRUE),
        geom = "crossbar", color = "black", width = 0.4, linewidth = 0.6,
        position = ggplot2::position_dodge(width = 0.8)
      )
  } else {
    p <- ggplot2::ggplot(run_df, ggplot2::aes(x = Group, y = PercentageSleep, color = Group)) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.15),
        size = 2, stroke = 0.5, alpha = 0.7
      ) +
      ggplot2::scale_color_manual(values = group_palette) +
      ggplot2::stat_summary(
        fun.data = function(y) {
          m <- stats::median(y, na.rm = TRUE)
          s <- stats::sd(y, na.rm = TRUE)
          if (!is.finite(s)) s <- 0
          data.frame(y = m, ymin = m - s, ymax = m + s)
        },
        geom = "errorbar", color = "black", width = 0.2, linewidth = 0.4
      ) +
      ggplot2::stat_summary(
        fun = function(y) stats::median(y, na.rm = TRUE),
        geom = "crossbar", color = "black", width = 0.4, linewidth = 0.6
      )
  }

  p <- p +
    ggplot2::labs(
      title = paste0(ph, " Phase - Run ", rn),
      x = "Group",
      y = "Percentage Sleep (%)"
    ) +
    theme_nature() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15)))

  # 1) rstatix ANOVA + Tukey per run AND plot annotation
  has_enough_groups <- raw_slice_stats %>%
    dplyr::filter(!is.na(PercentageSleep)) %>%
    dplyr::distinct(Group) %>%
    nrow() >= 2

  if (nrow(raw_slice_stats) > 0 && has_enough_groups) {
    anova_tbl <- run_anova(raw_slice_stats, has_sex)
    save_table_by_slice(anova_tbl, paths, paste0("ANOVA_", ph, "_run", rn))

    tuk <- run_tukey(raw_slice_stats, has_sex)
    if (!is.null(tuk$main_group))  save_table_by_slice(tuk$main_group,  paths, paste0("Tukey_Group_", ph, "_run", rn))
    if (!is.null(tuk$within_sex))  save_table_by_slice(tuk$within_sex,  paths, paste0("Tukey_Group_withinSex_", ph, "_run", rn))
    if (!is.null(tuk$interaction)) save_table_by_slice(tuk$interaction, paths, paste0("Tukey_Interaction_", ph, "_run", rn))

    # Overlay significant group effects on plot
    if (!is.null(tuk$main_group)) {
      stat_df <- tuk$main_group %>% dplyr::filter(p.adj < 0.05)
      if (nrow(stat_df) > 0) {
        stat_df <- stat_df %>%
          rstatix::add_y_position(
            data = raw_slice_stats,
            formula = PercentageSleep ~ Group,
            step.increase = 0.1
          )
        p <- p + ggpubr::stat_pvalue_manual(
          stat_df,
          label = "p.adj.signif",
          tip.length = 0.01,
          bracket.size = 0.3,
          label.size = 4,
          hide.ns = TRUE,
          step.increase = 0.05
        )
      }
    }
  }

  # Single-plot export size requested: 3 x 4
  ggsave(
    filename = file.path(paths$plots, paste0("plot_PercentageSleep_", ph, "_run", rn, ".svg")),
    plot = p, width = 3, height = 4, device = "svg"
  )

  # 2) MMM testAndPlotVariable per run (reusing unified stats pipeline)
  sexes_in_run <- if ("Sex" %in% names(raw_slice_stats)) unique(na.omit(raw_slice_stats$Sex)) else " "
  if (length(sexes_in_run) == 0) sexes_in_run <- " "

  for (sx in sexes_in_run) {
    res_mmm <- testAndPlotVariable(
      data = raw_slice_stats,
      variableName = "PercentageSleep",
      phase = ph,
      sex = if (sx == " ") " " else sx
    )

    if (!is.null(res_mmm)) {
      if (!is.null(res_mmm$testResults)) {
        tr_df <- as.data.frame(res_mmm$testResults)
        save_table_by_slice(tr_df, paths, paste0("MMM_tests_PercentageSleep_", ph, "_run", rn, "_sex_", safe_name(sx)))
      }
      if (!is.null(res_mmm$posthocResults)) {
        save_table_by_slice(res_mmm$posthocResults, paths, paste0("MMM_posthoc_PercentageSleep_", ph, "_run", rn, "_sex_", safe_name(sx)))
      }
      if (!is.null(res_mmm$plot)) {
        ggsave(
          filename = file.path(paths$plots, paste0("MMM_plot_PercentageSleep_", ph, "_run", rn, "_sex_", safe_name(sx), ".svg")),
          plot = res_mmm$plot, width = 3, height = 4, device = "svg"
        )
      }
    }
  }
}

# Export all per-run summaries
if (!dir.exists(byrun_root)) dir.create(byrun_root, recursive = TRUE)
write.csv(per_run_stats, file = file.path(byrun_root, "per_run_stats_all.csv"), row.names = FALSE)

# =========================
# Sex Comparison Analysis (Males vs Females within Groups)
# =========================

sex_comparison_root <- file.path(sleep_directory, "SexComparison")
if (!dir.exists(sex_comparison_root)) dir.create(sex_comparison_root, recursive = TRUE)

# Ensure data has Sex column and it contains both m and f
if (isTRUE(include_sex) && all(c("m", "f") %in% unique(na.omit(overall_data$Sex)))) {
  
  group_colors <- c("CON" = "#3d3c6f", "RES" = "#C6C3BB", "SUS" = "#E63946")

  for (variable in columns_to_plot) {
    for (phase in phases) {
      
      # 1. Filter Data
      subset_sex_comp <- overall_data
      if (phase != " " && phase != "All") {
        subset_sex_comp <- subset_sex_comp %>% dplyr::filter(Phase == phase)
      }
      
      subset_sex_comp <- subset_sex_comp %>% 
        dplyr::filter(Sex %in% c("m", "f")) %>%
        dplyr::mutate(
          Sex = factor(Sex, levels = c("m", "f")),
          Group = factor(Group, levels = c("CON", "RES", "SUS")) # Ensure factor order
        ) %>%
        tidyr::drop_na(dplyr::all_of(variable))
      
      # Skip if insufficient data
      if (nrow(subset_sex_comp) < 6) next
      
      # Prepare output paths
      safe_var <- safe_name(variable)
      safe_ph  <- safe_name(phase)
      
      stats_dir <- file.path(sex_comparison_root, "Statistics")
      plots_dir <- file.path(sex_comparison_root, "Plots")
      if (!dir.exists(stats_dir)) dir.create(stats_dir, recursive = TRUE)
      if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
      
      # 2. Normality Check (Shapiro-Wilk)
      normality_check <- tryCatch({
        subset_sex_comp %>%
          dplyr::group_by(Group, Sex) %>%
          rstatix::shapiro_test(vars = variable) %>%
          as.data.frame()
      }, error = function(e) NULL)
      
      is_normal <- TRUE
      if (!is.null(normality_check)) {
        if (any(normality_check$p < 0.05)) is_normal <- FALSE
      }
      
      # 3. Statistical Testing
      workbook_sheets <- list()
      plot_subtitle <- ""
      
      desc_stats <- subset_sex_comp %>%
        dplyr::group_by(Group, Sex) %>%
        rstatix::get_summary_stats(type = "common")
      workbook_sheets[["Descriptive_Stats"]] <- desc_stats
      
      pairwise_res <- NULL
      
      if (is_normal) {
        formula_str <- as.formula(paste(variable, "~ Group * Sex"))
        res_aov <- subset_sex_comp %>% rstatix::anova_test(formula_str, effect.size = "ges")
        res_aov <- as.data.frame(res_aov)
        class(res_aov) <- "data.frame"
        workbook_sheets[["ANOVA_Results"]] <- res_aov
        
        inter_row <- res_aov %>% dplyr::filter(Effect == "Group:Sex")
        p_inter <- if(nrow(inter_row) > 0) inter_row$p else NA
        plot_subtitle <- paste0("2-Way ANOVA\nInteraction p = ", signif(p_inter, 3))
        
        pairwise_res <- subset_sex_comp %>%
          dplyr::group_by(Group) %>%
          rstatix::t_test(as.formula(paste(variable, "~ Sex"))) %>%
          rstatix::adjust_pvalue(method = "bonferroni") %>%
          rstatix::add_significance() %>%
          as.data.frame()
        
        workbook_sheets[["PostHoc_Sex_within_Group"]] <- pairwise_res
        
      } else {
        subset_sex_comp <- subset_sex_comp %>% dplyr::mutate(Group_Sex = paste(Group, Sex, sep = "_"))
        res_kruskal <- subset_sex_comp %>% rstatix::kruskal_test(as.formula(paste(variable, "~ Group_Sex"))) %>% as.data.frame()
        workbook_sheets[["Kruskal_Global"]] <- res_kruskal
        
        plot_subtitle <- paste0("Non-Parametric (Kruskal Global p = ", signif(res_kruskal$p, 3), ")")
        
        pairwise_res <- subset_sex_comp %>%
          dplyr::group_by(Group) %>%
          rstatix::wilcox_test(as.formula(paste(variable, "~ Sex"))) %>%
          rstatix::adjust_pvalue(method = "bonferroni") %>%
          rstatix::add_significance() %>%
          as.data.frame()
        
        workbook_sheets[["PostHoc_Wilcoxon_SexInGroup"]] <- pairwise_res
      }
      
      # Save Statistics to Excel
      wb <- openxlsx::createWorkbook()
      if (!is.null(normality_check)) {
        openxlsx::addWorksheet(wb, "Normality_Check")
        openxlsx::writeData(wb, "Normality_Check", normality_check)
      }
      for (sheet_name in names(workbook_sheets)) {
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, workbook_sheets[[sheet_name]])
      }
      
      stat_fname <- paste0("SleepStats_", safe_var, "_", safe_ph, ".xlsx")
      openxlsx::saveWorkbook(wb, file = file.path(stats_dir, stat_fname), overwrite = TRUE)
      
      # 4. Plotting with Nature aesthetics + Stats OVERLAY
      p_sex <- ggplot2::ggplot(subset_sex_comp, ggplot2::aes(x = Group, y = !!rlang::sym(variable))) +
        ggplot2::geom_point(
          ggplot2::aes(color = Group, shape = Sex), 
          position = ggplot2::position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
          size = 2, stroke = 0.5, alpha = 0.7
        ) +
        ggplot2::scale_color_manual(values = group_colors) +
        ggplot2::scale_shape_manual(values = c("m" = 1, "f" = 16)) +
        ggplot2::stat_summary(
          ggplot2::aes(group = Sex),
          fun.data = function(y) {
            m <- stats::median(y, na.rm = TRUE)
            s <- stats::sd(y, na.rm = TRUE)
            if (is.na(s)) s <- 0
            data.frame(y = m, ymin = m - s, ymax = m + s)
          },
          geom = "errorbar", color = "black", width = 0.2, linewidth = 0.4, position = ggplot2::position_dodge(width = 0.8)
        ) +
        ggplot2::stat_summary(
          ggplot2::aes(group = Sex),
          fun = function(y) stats::median(y, na.rm = TRUE),
          geom = "crossbar", color = "black", width = 0.4, linewidth = 0.6, position = ggplot2::position_dodge(width = 0.8)
        ) +
        ggplot2::labs(
          title = paste0(variable, " (", phase, ")"),
          subtitle = plot_subtitle,
          y = variable, x = "Group"
        ) +
        theme_nature() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15)))
      
      # Add robust ggpubr brackets
      if (!is.null(pairwise_res) && nrow(pairwise_res) > 0) {
        stat_df <- pairwise_res %>% dplyr::filter(p.adj < 0.05)
        if (nrow(stat_df) > 0) {
          stat_df <- stat_df %>% rstatix::add_xy_position(
            x = "Group", dodge = 0.8, 
            data = subset_sex_comp, formula = as.formula(paste(variable, "~ Sex"))
          )
          p_sex <- p_sex + ggpubr::stat_pvalue_manual(
            stat_df, label = "p.adj.signif", tip.length = 0.01, 
            bracket.size = 0.3, label.size = 4, hide.ns = TRUE
          )
        }
      }
      
      plot_fname <- paste0("SleepPlot_", safe_var, "_", safe_ph, ".svg")
      ggsave(file = file.path(plots_dir, plot_fname), plot = p_sex, width = 3, height = 4, device = "svg")
    }
  }
}