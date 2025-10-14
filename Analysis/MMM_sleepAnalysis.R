# ==============================
# Sleep analysis with scoped I/O
# ==============================

# -------- Required packages --------
required_packages <- c(
  "ggplot2", "gridExtra", "cowplot", "tidyr", "dplyr",
  "rstatix", "readxl", "svglite", "stringr", "openxlsx"
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
# data_filtered <- read.csv(file.path(sleep_directory, "preprocessed_input.csv")

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
groupColors <- c("#457B9D", "#C6C3BB", "#E63946")

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
  tables <- file.path(dir_root, "tables"); plots <- file.path(dir_root, "plots"); logs <- file.path(dir_root, "logs")
  if (!dir.exists(tables)) dir.create(tables, recursive = TRUE)
  if (!dir.exists(plots))  dir.create(plots,  recursive = TRUE)
  if (!dir.exists(logs))   dir.create(logs,   recursive = TRUE)
  list(root = dir_root, tables = tables, plots = plots, logs = logs)
}
save_plot_by_slice <- function(plot, paths, var, ext = ".svg", width = 7, height = 5) {
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
  data %>%
    dplyr::mutate(Change = tidyr::replace_na(Change, "All")) %>%
    dplyr::group_by_at(intersect(c("AnimalNum", "Batch", "Change", "Group", "Phase", "Sex"), names(.))) %>%
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
  data %>%
    dplyr::group_by_at(intersect(c("AnimalNum", "Phase", "Sex"), names(data))) %>%
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
    dplyr::ungroup()  # overall per-animal [web:240]
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
  if (!identical(change, " ") && !identical(change, "All"))out <- dplyr::filter(out, !is.na(Change) & Change == change)
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
        save_table_by_slice(as.data.frame(tr), paths, paste0("tests_", variable)); all_test_results <- c(all_test_results, list(as.data.frame(tr)))
      }
      if (!is.null(result$posthocResults)) {
        save_table_by_slice(result$posthocResults, paths, paste0("posthoc_", variable)); all_posthoc_results <- c(all_posthoc_results, list(result$posthocResults))
      }
      if (!is.null(result$plot)) {
        save_plot_by_slice(result$plot, paths, variable, ext = ".svg", width = 7, height = 5); all_plots <- c(all_plots, list(result$plot))
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
  ggsave(file = file.path(global_paths$plots, "allPlots.svg"), plot = grid_plots, width = 30, height = 20, device = "svg")
}

# =========================
# Per-run (consecutive) stats + rstatix + MMM tests
# =========================

byrun_root <- file.path(sleep_directory, "ByRun"); if (!dir.exists(byrun_root)) dir.create(byrun_root, recursive = TRUE)
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
  tidyr::drop_na(DateTime)  # valid DateTime [web:240]
if (exists("analyzeSingleChange") && isTRUE(analyzeSingleChange)) data_runs <- data_runs %>% dplyr::filter(Change == changeToAnalyze)

group_vars <- c("AnimalNum", "Batch", "Change", "Group", "Phase", "RunID"); if (has_sex) group_vars <- c("AnimalNum", "Batch", "Change", "Group", "Sex", "Phase", "RunID")
required_cols <- c(group_vars, "ActivityIndex", "DateTime"); missing <- setdiff(required_cols, names(data_runs)); if (length(missing)) stop("Missing columns in data_runs: ", paste(missing, collapse = ", "))

per_run_stats <- data_runs %>%
  dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
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
  ph <- unique_runs$Phase[i]; rn <- unique_runs$RunID[i]

  run_df <- per_run_stats %>%
    dplyr::filter(Phase == ph, RunID == rn) %>%
    dplyr::mutate(Group = factor(Group, levels = c("CON", "RES", "SUS")))

  paths <- build_run_dirs(byrun_root, ph, rn)
  save_table_by_slice(run_df, paths, paste0("per_run_stats_", ph, "_run", rn))

  if (nrow(run_df) > 0) {
    # Plot per-run medians +/- SD
    group_palette <- c(CON = "#457B9D", RES = "#C6C3BB", SUS = "#E63946")
    p <- ggplot2::ggplot(run_df, ggplot2::aes(x = Group, y = PercentageSleep, color = Group)) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.15, height = 0), alpha = 0.8, size = 5) +
      ggplot2::scale_color_manual(values = group_palette) +
      ggplot2::stat_summary(
        fun.data = function(y) { m <- stats::median(y, na.rm = TRUE); s <- stats::sd(y, na.rm = TRUE); if (!is.finite(s)) s <- 0; data.frame(y = m, ymin = m - s, ymax = m + s) },
        geom = "linerange", color = "black", size = 0.8
      ) +
      ggplot2::stat_summary(fun = function(y) stats::median(y, na.rm = TRUE), geom = "point", color = "black", size = 3) +
      ggplot2::labs(title = paste0("PercentageSleep - ", ph, " Run ", rn), x = "Group", y = "Percentage Sleep (%)") +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
        strip.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 14),
        axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.6),
        axis.ticks.x = ggplot2::element_line(color = "black"),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
    if (has_sex) p <- p + ggplot2::facet_wrap(~ Sex, nrow = 1)
    ggsave(filename = file.path(paths$plots, paste0("plot_PercentageSleep_", ph, "_run", rn, ".svg")), plot = p, width = 6, height = 3, device = "svg")

    # Build raw_slice and compute per-animal PercentageSleep for current run
    raw_slice <- data_runs %>%
      dplyr::filter(Phase == ph) %>%
      dplyr::filter(dplyr::case_when(ph == "Active" ~ ConsecActive == rn, ph == "Inactive" ~ ConsecInactive == rn, TRUE ~ FALSE)) %>%
      dplyr::mutate(Group = factor(Group, levels = c("CON", "RES", "SUS")))

    raw_slice_stats <- raw_slice %>%
      dplyr::group_by(AnimalNum, Group, Phase, Sex) %>%
      dplyr::summarize(
        SleepCount = sum(ActivityIndex == 0, na.rm = TRUE),
        TotalCount = dplyr::n(),
        PercentageSleep = ifelse(TotalCount == 0, NA_real_, (SleepCount / TotalCount) * 100),
        .groups = "drop"
      )

    # 1) rstatix ANOVA + Tukey per run
    has_enough_groups <- raw_slice_stats %>% dplyr::filter(!is.na(PercentageSleep)) %>% dplyr::distinct(Group) %>% nrow() >= 2
    if (nrow(raw_slice_stats) > 0 && has_enough_groups) {
      anova_tbl <- run_anova(raw_slice_stats, has_sex)
      save_table_by_slice(anova_tbl, paths, paste0("ANOVA_", ph, "_run", rn))  # export [web:288]
      tuk <- run_tukey(raw_slice_stats, has_sex)
      if (!is.null(tuk$main_group))  save_table_by_slice(tuk$main_group,  paths, paste0("Tukey_Group_", ph, "_run", rn))
      if (!is.null(tuk$within_sex))  save_table_by_slice(tuk$within_sex,  paths, paste0("Tukey_Group_withinSex_", ph, "_run", rn))
      if (!is.null(tuk$interaction)) save_table_by_slice(tuk$interaction, paths, paste0("Tukey_Interaction_", ph, "_run", rn))
    }

    # 2) MMM testAndPlotVariable per run (reusing unified stats pipeline)
    # You can analyze within each sex separately (recommended), or pass " " to skip Sex filtering if your function supports it.
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
            plot = res_mmm$plot, width = 6, height = 3, device = "svg"
          )
        }
      }
    }
  }
}

# Export all per-run summaries
if (!dir.exists(byrun_root)) dir.create(byrun_root, recursive = TRUE)
write.csv(per_run_stats, file = file.path(byrun_root, "per_run_stats_all.csv"), row.names = FALSE)
