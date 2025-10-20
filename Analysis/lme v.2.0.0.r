###############################################
# Mixed models analysis with improved I/O
# - Organized results folder structure
# - Scoped aggregate XLSX outputs
# - Optional per-combination CSV exports
# - Reproducible logging
###############################################

# Packages
packages <- c(
  "ggplot2","dplyr","tidyr","gridExtra","lme4",
  "lmerTest","cowplot","lsmeans","emmeans","Matrix",
  "tcltk","openxlsx","readr","stringr"
)

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# -------------------------------------------------------------------
# Base results directory
# -------------------------------------------------------------------
if (!dir.exists("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/lme_sis_activity")) {
  dir.create("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/lme_sis_activity", recursive = TRUE)
}
results_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/lme_sis_activity/"

# Safe directory creation
dir_create_safe <- function(path) { 
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE) 
}

# Standard subdirectories
dirs <- list(
  logs       = file.path(results_dir, "logs"),
  models     = file.path(results_dir, "models"),
  tables_sum = file.path(results_dir, "tables", "summary"),
  tables_emm = file.path(results_dir, "tables", "emmeans"),
  plots_line = file.path(results_dir, "plots", "line"),
  plots_heat = file.path(results_dir, "plots", "heatmap")
)
invisible(lapply(dirs, dir_create_safe))

# Scope helpers
scope_label <- function(Change, Sex, Phase) {
  paste0("CC-", Change, "_Sex-", Sex, "_Phase-", Phase)
}
scope_all <- function(includeChange, includeSex, includePhase) {
  paste0(
    if (includeChange) "subsetChanges" else "allChanges", "_",
    if (includeSex)    "subsetSexes"  else "allSexes",  "_",
    if (includePhase)  "subsetPhases" else "allPhases"
  )
}

# Plot directory builders
plot_dir_line <- function(Change, Sex, includeSex) {
  sub <- file.path(dirs$plots_line, paste0("CC-", Change))
  dir_create_safe(sub)
  if (isTRUE(includeSex)) {
    sub <- file.path(sub, paste0("Sex-", Sex))
    dir_create_safe(sub)
  }
  sub
}
plot_dir_heat <- function(Change, Sex, includeSex) {
  sub <- file.path(dirs$plots_heat, paste0("CC-", Change))
  dir_create_safe(sub)
  if (isTRUE(includeSex)) {
    sub <- file.path(sub, paste0("Sex-", Sex))
    dir_create_safe(sub)
  }
  sub
}

# Optional granular table directories
tables_cell_dir <- function(kind, Change, Sex, Phase) {
  base <- if (kind == "summary") dirs$tables_sum else dirs$tables_emm
  sub <- file.path(base, scope_label(Change, Sex, Phase))
  dir_create_safe(sub)
  sub
}

# -------------------------------------------------------------------
# User GUI input
# -------------------------------------------------------------------
get_user_input <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "Analysis Settings")
  tkconfigure(tt, bg = "#F7F9FC", padx = 10, pady = 20)

  includeChange_var <- tclVar(0)
  includeSex_var    <- tclVar(0)
  includePhase_var  <- tclVar(0)

  frame <- tkframe(tt, bg = "#F7F9FC")
  tkgrid(frame, padx = 10, pady = 10)

  tkgrid(tklabel(frame, text = "Set Inclusion", bg = "#F7F9FC", font = c("Helvetica", 14)), padx = 10, pady = 0)
  tkgrid(tklabel(frame, text = "Parameters", bg = "#F7F9FC", font = c("Helvetica", 14)), padx = 5, pady = 5)

  tkgrid(tklabel(frame, text = "Change", bg = "#F7F9FC", font = c("Helvetica", 12)),
         tkcheckbutton(frame, variable = includeChange_var, bg = "#F7F9FC", activebackground = "#E1E6EA"),
         padx = 5, pady = 5)
  tkgrid(tklabel(frame, text = "Sex", bg = "#F7F9FC", font = c("Helvetica", 12)),
         tkcheckbutton(frame, variable = includeSex_var, bg = "#F7F9FC", activebackground = "#E1E6EA"),
         padx = 5, pady = 5)
  tkgrid(tklabel(frame, text = "Phase", bg = "#F7F9FC", font = c("Helvetica", 12)),
         tkcheckbutton(frame, variable = includePhase_var, bg = "#F7F9FC", activebackground = "#E1E6EA"),
         padx = 5, pady = 5)

  onOK <- function() tkdestroy(tt)
  ok_button <- tkbutton(tt, text = "OK", command = onOK, bg = "#007AFF", fg = "white", relief = "flat", font = c("Helvetica", 12))
  tkgrid(ok_button, padx = 10, pady = 10)
  tkbind(ok_button, "<Enter>", function() tkconfigure(ok_button, bg = "#005A9E"))
  tkbind(ok_button, "<Leave>", function() tkconfigure(ok_button, bg = "#0075CE"))

  tkwait.window(tt)

  list(
    includeChange = as.logical(as.numeric(tclvalue(includeChange_var))),
    includeSex    = as.logical(as.numeric(tclvalue(includeSex_var))),
    includePhase  = as.logical(as.numeric(tclvalue(includePhase_var)))
  )
}

user_input   <- get_user_input()
includeChange <- user_input$includeChange
includeSex    <- user_input$includeSex
includePhase  <- user_input$includePhase

# -------------------------------------------------------------------
# Data requirements
# - Expects 'data_filtered_agg' with columns:
#   Change, Sex, Phase, Group, HalfHourElapsed, ActivityIndex, AnimalNum
# -------------------------------------------------------------------
# Ensure factor levels are stable
# data_filtered_agg <- data_filtered_agg %>% mutate(
#   Change = as.factor(Change),
#   Sex    = as.factor(Sex),
#   Phase  = as.factor(Phase),
#   Group  = factor(Group, levels = c("CON","RES","SUS"))
# )

# -------------------------------------------------------------------
# Plotting
# -------------------------------------------------------------------
plot_list <- list()
heatmap_list <- list()

data_filtered_agg_Change <- data_filtered_agg %>% split(.$Change)

# Line plots per Change and Sex
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  df_change <- data_filtered_agg_Change[[Change]]
  out_sex <- list()

  for (SexValue in c("m","f")) {
    df_sex <- if (includeSex) dplyr::filter(df_change, Sex == SexValue) else df_change

    color_palette <- c("#457B9D", "#C6C3BB", "#E63946")

    p <- ggplot(df_sex, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
      geom_path(stat = "summary", fun = mean, linewidth = 0.8) +
      stat_summary(aes(fill = Group), fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "ribbon", alpha = 0.3, colour = NA) +
      scale_color_manual(name = "Group", values = color_palette) +
      scale_fill_manual(name = "Group", values = color_palette) +
      scale_x_continuous(breaks = seq(0, max(df_sex$HalfHourElapsed), by = 24),
                         labels = seq(0, max(df_sex$HalfHourElapsed), by = 24) / 2,
                         expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      labs(title = "Circadian Activity Index",
           subtitle = "Activity Levels Across Groups",
           y = "Activity [a.u.]", x = "Time Elapsed [h]",
           caption = "Shaded regions represent ±1 SD of the mean") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.ticks.x = element_line(size = 0.5),
        legend.position = "top",
        panel.background = element_blank()
      )

    out_dir <- plot_dir_line(Change, SexValue, includeSex)
    file_name <- paste0("line_", Change, "_", if (includeSex) SexValue else "allSexes", ".svg")
    ggsave(filename = file.path(out_dir, file_name), plot = p, device = "svg", width = 7, height = 4)

    out_sex[[SexValue]] <- p
  }
  out_sex
})

# Heatmaps per Change and Sex
heatmap_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  df_change <- data_filtered_agg_Change[[Change]]
  out_sex <- list()
  for (SexValue in c("m","f")) {
    df_sex <- if (includeSex) dplyr::filter(df_change, Sex == SexValue) else df_change
    df_sex <- df_sex %>% mutate(Group = factor(Group, levels = c("CON","RES","SUS")))

    p <- ggplot(df_sex, aes(x = HalfHourElapsed, y = reorder(Group, -as.numeric(Group)), fill = ActivityIndex)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkblue", limits = c(0, max(data_filtered_agg$ActivityIndex))) +
      scale_x_continuous(breaks = seq(0, max(df_sex$HalfHourElapsed), by = 24),
                         labels = seq(0, max(df_sex$HalfHourElapsed), by = 24)/2) +
      facet_grid(Sex ~ ., scales = "free") +
      cowplot::theme_minimal_hgrid(12, rel_small = 1) +
      labs(title = bquote(~bold(.(paste("Activity Index")))),
           subtitle = paste("Cage Change:", Change),
           y = "Group", x = "Time elapsed [h]") +
      theme(plot.title = element_text(hjust = 0.5, face = "plain"),
            plot.subtitle = element_text(hjust = 0.5, face = "plain"),
            legend.position = "top",
            legend.justification = "right",
            legend.text = element_text(size = 9),
            legend.box.spacing = unit(1, "pt"),
            axis.ticks = element_blank(),
            axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5),
            strip.text = element_text(size = 9))

    out_dir <- plot_dir_heat(Change, SexValue, includeSex)
    file_name <- paste0("heatmap_", Change, "_", if (includeSex) SexValue else "allSexes", ".svg")
    ggsave(filename = file.path(out_dir, file_name), plot = p, device = "svg", width = 5, height = 5)

    out_sex[[SexValue]] <- p
  }
  out_sex
})

# -------------------------------------------------------------------
# Modeling
# -------------------------------------------------------------------
results_list <- list()

changes <- if (includeChange) unique(data_filtered_agg$Change) else "allChanges"
sexes   <- if (includeSex)    unique(data_filtered_agg$Sex)    else "allSexes"
phases  <- if (includePhase)  unique(data_filtered_agg$Phase)  else "allPhases"

summary_results <- data.frame(
  Change = character(), Sex = character(), Phase = character(),
  Fixed_effect = character(), Estimate = numeric(), Std.Error = numeric(),
  df = numeric(), t.value = numeric(), p.value = numeric(),
  p.round = numeric(), p.sign = character(), stringsAsFactors = FALSE
)

emmeans_results <- data.frame(
  Change = character(), Sex = character(), Phase = character(),
  contrast = character(), estimate = numeric(), SE = numeric(),
  df = numeric(), t.ratio = numeric(), p.value = numeric(),
  p.adjust = numeric(), p.round = numeric(), stringsAsFactors = FALSE
)

total_iterations <- length(changes) * length(sexes) * length(phases)
progress_bar <- tkProgressBar(title = "Model Fitting Progress", min = 0, max = total_iterations, width = 300)
iteration_counter <- 0

for (Change in changes) {
  for (Sex in sexes) {
    for (Phase in phases) {
      iteration_counter <- iteration_counter + 1
      setTkProgressBar(progress_bar, iteration_counter, label = paste("Progress:", round(iteration_counter / total_iterations * 100, 2), "%"))

      data_subset <- data_filtered_agg %>%
        dplyr::filter(
          (!includeChange | Change == !!Change),
          (!includeSex    | Sex    == !!Sex),
          (!includePhase  | Phase  == !!Phase)
        )

      if (nrow(data_subset) == 0) {
        cat("No data available for Change:", Change, "Sex:", Sex, "Phase:", Phase, "\n")
        next
      }

      emm_options(pbkrtest.limit = 100000)

      model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_subset)
      model_summary <- summary(model)

      fixed_effects <- data.frame(
        Fixed_effect = rownames(model_summary$coefficients),
        Estimate     = model_summary$coefficients[, "Estimate"],
        Std.Error    = model_summary$coefficients[, "Std. Error"],
        df           = model_summary$coefficients[, "df"],
        t.value      = model_summary$coefficients[, "t value"],
        p.value      = model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))],
        p.round      = round(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))], digits = 3),
        p.sign       = ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.001, "***",
                         ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.01, "**",
                         ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.05, "*",
                         ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.1, "T", "ns")))),
        Change = Change, Phase = Phase, Sex = Sex, stringsAsFactors = FALSE
      )

      summary_results <- dplyr::bind_rows(summary_results, fixed_effects)

      posthoc <- emmeans::emmeans(model, pairwise ~ Group)
      posthoc_summary <- summary(posthoc, infer = TRUE, adjust = "holm")
      posthoc_df <- as.data.frame(posthoc_summary$contrasts)
      posthoc_df$Change <- Change
      posthoc_df$Phase  <- Phase
      posthoc_df$Sex    <- Sex
      posthoc_df <- posthoc_df[!is.na(posthoc_df$p.value), ]
      posthoc_df$p.adjust <- p.adjust(posthoc_df$p.value, method = "holm")

      if (nrow(posthoc_df) > 0) {
        emmeans_results <- dplyr::bind_rows(emmeans_results, posthoc_df)
      }

      # Optional: save model objects per cell
      # model_path <- file.path(dirs$models, paste0("model_", scope_label(Change, Sex, Phase), ".rds"))
      # saveRDS(model, model_path)

      # Optional: per-combination CSVs (granular exports)
      # cell_dir_sum <- tables_cell_dir("summary", Change, Sex, Phase)
      # cell_dir_emm <- tables_cell_dir("emmeans", Change, Sex, Phase)
      # readr::write_csv(fixed_effects, file.path(cell_dir_sum, "fixed_effects.csv"))
      # if (nrow(posthoc_df) > 0) readr::write_csv(posthoc_df, file.path(cell_dir_emm, "contrasts.csv"))
    }
  }
}
close(progress_bar)

# -------------------------------------------------------------------
# Aggregated table outputs (scoped filenames)
# -------------------------------------------------------------------
run_scope <- scope_all(includeChange, includeSex, includePhase)
summary_xlsx <- file.path(dirs$tables_sum, paste0("lme_summary_results_", run_scope, ".xlsx"))
emmeans_xlsx <- file.path(dirs$tables_emm, paste0("emmeans_results_", run_scope, ".xlsx"))

openxlsx::write.xlsx(summary_results, file = summary_xlsx, row.names = FALSE)
cat("Summary results saved to:", summary_xlsx, "\n")

openxlsx::write.xlsx(emmeans_results, file = emmeans_xlsx, row.names = FALSE)
cat("EMMeans results saved to:", emmeans_xlsx, "\n")

# -------------------------------------------------------------------
# Logging for reproducibility
# -------------------------------------------------------------------
ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
logfile <- file.path(dirs$logs, paste0("run_", ts, ".txt"))
cat(
  "Run timestamp: ", ts, "\n",
  "includeChange: ", includeChange, "\n",
  "includeSex: ", includeSex, "\n",
  "includePhase: ", includePhase, "\n",
  "Scope label: ", run_scope, "\n\n",
  capture.output(sessionInfo()), sep = "", file = logfile
)

cat("Run log written to:", logfile, "\n")



# -------------------------------------------------
# Fixed effects bar plots with error bars
# -------------------------------------------------

# Ensure destination dir exists
dir_fixed <- file.path(results_dir, "plots", "fixed_effects")
if (!dir.exists(dir_fixed)) dir.create(dir_fixed, recursive = TRUE, showWarnings = FALSE)

# Choose which aggregated file to read:
# Use the same run_scope used for writing the XLSX earlier
# If running independently, set run_scope manually, e.g.:
# run_scope <- "subsetChanges_subsetSexes_subsetPhases"  # or "allChanges_allSexes_allPhases"
summary_xlsx <- file.path(results_dir, "tables", "summary", paste0("lme_summary_results_", run_scope, ".xlsx"))

# Load results
data <- openxlsx::read.xlsx(summary_xlsx)

# Clean factors for plotting
data <- data %>%
  mutate(
    Fixed_effect = factor(Fixed_effect, levels = unique(Fixed_effect)),
    Sex   = factor(Sex, levels = c("m","f")),
    Phase = factor(Phase, levels = c("Active","Inactive")),
    Change = factor(Change, levels = sort(unique(Change))),
    effect_type = ifelse(Fixed_effect == "(Intercept)", "Intercept (Control)", "Other Effect")
  )

# Helper: build per-plot directory consistent with new structure
plot_dir_fixed <- function(Change, Sex) {
  sub <- file.path(dir_fixed, paste0("CC-", Change))
  if (!dir.exists(sub)) dir.create(sub, recursive = TRUE, showWarnings = FALSE)
  sub2 <- file.path(sub, paste0("Sex-", as.character(Sex)))
  if (!dir.exists(sub2)) dir.create(sub2, recursive = TRUE, showWarnings = FALSE)
  sub2
}

unique_change <- unique(data$Change)
unique_sex    <- unique(data$Sex)
unique_phase  <- unique(data$Phase)

# Palette choices (pick one; the second block from your snippet kept)
palette1 <- c("(Intercept)"="#457B9D","GroupRES"="#C6C3BB","GroupSUS"="#E63946")
palette2 <- c("(Intercept)"="#1e3791","GroupRES"="#8aacdb","GroupSUS"="#f49620")
use_palette <- palette2

for (c in unique_change) {
  for (s in unique_sex) {
    for (p in unique_phase) {

      plot_data <- data %>%
        filter(Change == c, Sex == s, Phase == p) %>%
        filter(Fixed_effect != "HalfHourElapsed")

      if (nrow(plot_data) == 0) next

      # Intercept for this cell
      int_val <- data %>%
        filter(Fixed_effect == "(Intercept)", Change == c, Sex == s, Phase == p) %>%
        pull(Estimate)

      # Adjusted estimates: add intercept to Group contrasts
      plot_data <- plot_data %>%
        mutate(
          Adjusted_Estimate = dplyr::case_when(
            Fixed_effect == "(Intercept)" ~ Estimate,
            grepl("^Group", Fixed_effect) ~ as.numeric(int_val) + Estimate,
            TRUE ~ Estimate
          )
        )

      # Keep only target effects in palette
      plot_data <- plot_data %>%
        mutate(Fixed_effect = as.character(Fixed_effect)) %>%
        filter(Fixed_effect %in% names(use_palette)) %>%
        mutate(Fixed_effect = factor(Fixed_effect, levels = names(use_palette)))

      if (nrow(plot_data) == 0) next

      p_fixed <- ggplot(plot_data, aes(x = Fixed_effect, y = Adjusted_Estimate, fill = Fixed_effect)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = NA) +
        geom_errorbar(aes(ymin = Adjusted_Estimate - Std.Error, ymax = Adjusted_Estimate + Std.Error),
                      position = position_dodge(width = 0.7), width = 0.2, color = "grey40") +
        geom_text(aes(label = p.sign), position = position_dodge(width = 0.7),
                  vjust = -0.8, size = 5, color = "grey20") +
        scale_fill_manual(values = use_palette) +
        labs(
          title = paste("Fixed Effects for", s, "(", p, "Phase) - Change:", c),
          x = "Fixed Effect",
          y = "Adjusted Estimate ± SE",
          fill = "Effect Type"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          text = element_text(family = "sans"),
          axis.text.x = element_text(angle = 45, hjust = 1, color = "grey20"),
          axis.text.y = element_text(color = "grey20"),
          axis.title = element_text(face = "bold", color = "grey20"),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "grey10"),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey85", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "white", color = NA)
        )

      out_dir <- plot_dir_fixed(c, s)
      out_file <- file.path(out_dir, paste0("fixed_effects_", c, "_", s, "_", p, ".svg"))
      ggsave(filename = out_file, plot = p_fixed, device = "svg", width = 4, height = 5)
      cat("Saved:", out_file, "\n")
    }
  }
}


# -------------------------------------------------
# Publication figure panels with guards
# -------------------------------------------------

# Palettes and theme
pal_groups <- c(CON = "#457B9D", RES = "#C6C3BB", SUS = "#E63946")
theme_pub_small <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

dir_pub <- dirs$plots_pub

# Panel A: Forest plot
emm_df <- read.xlsx(emmeans_xlsx) %>%
  dplyr::mutate(
    contrast_clean = gsub("\\s+", "", contrast),
    pair = dplyr::case_when(
      contrast_clean %in% c("SUS-CON") ~ "SUS-CON",
      contrast_clean %in% c("RES-CON") ~ "RES-CON",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(pair), !is.na(Sex), !is.na(Change))

file_panelA <- file.path(dir_pub, paste0("panelA_forest_", run_scope, ".svg"))

if (nrow(emm_df) > 0) {
  emm_df <- emm_df %>%
    dplyr::mutate(
      tcrit = qt(0.975, df = df),
      ymin = estimate - tcrit * SE,
      ymax = estimate + tcrit * SE,
      Sex = droplevels(factor(Sex)),
      Change = droplevels(factor(Change))
    )

  forest_plot <- ggplot(
    data = emm_df,
    aes(x = factor(Change, levels = sort(unique(as.character(Change)))), y = estimate,
        ymin = ymin, ymax = ymax, color = pair, shape = pair)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(position = position_dodge(width = 0.5), linewidth = 0.6) +
    geom_text(aes(label = ifelse(pair == "SUS-CON" & p.adjust < 0.05, "★", "")),
              position = position_dodge(width = 0.5), vjust = -1.0, size = 4, color = "#333333") +
    scale_color_manual(values = c("RES-CON" = "grey70", "SUS-CON" = "#E63946")) +
    scale_shape_manual(values = c("RES-CON" = 16, "SUS-CON" = 17)) +
    labs(title = "Panel A • EMM contrasts vs CON", x = "Cage change", y = "Estimate (difference vs CON)") +
    facet_wrap(~ Sex, nrow = 1, scales = "free_x") +
    theme_pub_small

  ggsave(file_panelA, forest_plot, width = 8, height = 3.2)
  cat("Saved Panel A:", file_panelA, "\n")
} else {
  warning("Panel A skipped: no SUS-CON/RES-CON contrasts available after filtering")
}

# Panel B: Collapsed time-course ribbons
tc_df <- data_filtered_agg %>%
  dplyr::mutate(time_h = (HalfHourElapsed %% 48)/2) %>%
  dplyr::group_by(Change, Sex, Group, time_h) %>%
  dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE),
                   sd_act = sd(ActivityIndex, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(Group = factor(Group, levels = c("CON","RES","SUS")),
                Sex = droplevels(factor(Sex)),
                Change = droplevels(factor(Change)))

file_panelB <- file.path(dir_pub, paste0("panelB_ribbons_", run_scope, ".svg"))

if (nrow(tc_df) > 0 && length(unique(tc_df$Sex)) > 0 && length(unique(tc_df$Change)) > 0) {
  ribbons_plot <- ggplot(tc_df, aes(x = time_h, y = mean_act, color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = mean_act - sd_act, ymax = mean_act + sd_act), alpha = 0.25, colour = NA) +
    geom_line(size = 0.7) +
    scale_color_manual(values = pal_groups) +
    scale_fill_manual(values = pal_groups) +
    scale_x_continuous(breaks = seq(0,24,6), limits = c(0,24)) +
    labs(title = "Panel B • 24h activity (mean ± SD)", x = "Zeit [h]", y = "Activity [a.u.]") +
    facet_grid(Sex ~ Change, scales = "free_y") +
    theme_pub_small

  ggsave(file_panelB, ribbons_plot, width = 9.5, height = 4.8)
  cat("Saved Panel B:", file_panelB, "\n")
} else {
  warning("Panel B skipped: no data for faceting")
}

# Panel C: SUS−CON difference heatmap with visual emphasis
diff_df <- data_filtered_agg %>%
  dplyr::mutate(time_h = (HalfHourElapsed %% 48)/2) %>%
  dplyr::filter(Group %in% c("CON","SUS")) %>%
  dplyr::group_by(Change, Sex, Group, time_h) %>%
  dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Group, values_from = mean_act) %>%
  dplyr::mutate(diff_SC = SUS - CON) %>%
  dplyr::mutate(
    Sex = droplevels(factor(Sex)),
    Change = droplevels(factor(Change))
  ) %>%
  dplyr::filter(!is.na(diff_SC))

file_panelC <- file.path(dir_pub, paste0("panelC_heat_", run_scope, ".svg"))

if (nrow(diff_df) > 0 && length(unique(diff_df$Sex)) > 0 && length(unique(diff_df$Change)) > 0) {
  thresh <- stats::quantile(abs(diff_df$diff_SC), probs = 0.95, na.rm = TRUE)
  diff_df <- diff_df %>% dplyr::mutate(sig_bin = abs(diff_SC) >= as.numeric(thresh))

  heat_plot <- ggplot(diff_df, aes(x = time_h, y = factor(Change, levels = sort(unique(as.character(Change)))), fill = diff_SC)) +
    geom_tile() +
    scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0) +
    geom_point(data = subset(diff_df, sig_bin),
               aes(x = time_h, y = factor(Change, levels = sort(unique(as.character(Change))))),
               shape = 4, size = 0.8, color = "black", inherit.aes = FALSE) +
    labs(title = "Panel C • SUS−CON difference", x = "Zeit [h]", y = "Cage change", fill = "Δ Activity") +
    facet_wrap(~ Sex, nrow = 1) +
    theme_pub_small

  ggsave(file_panelC, heat_plot, width = 9.5, height = 3.2)
  cat("Saved Panel C:", file_panelC, "\n")
} else {
  warning("Panel C skipped: no data for heatmap faceting")
}

# Composite assembly (stack available panels)
file_composite <- file.path(dir_pub, paste0("MainFigure_ABC_", run_scope, ".svg"))

plots_list <- list()
if (exists("forest_plot"))  plots_list <- c(plots_list, list(forest_plot))
if (exists("ribbons_plot")) plots_list <- c(plots_list, list(ribbons_plot))
if (exists("heat_plot"))    plots_list <- c(plots_list, list(heat_plot))

if (length(plots_list) > 0) {
  # Set heights proportional to typical sizes A,B,C
  heights <- c(3.2, 4.8, 3.2)[seq_along(plots_list)]
  composite <- Reduce(`/`, plots_list) + plot_layout(guides = "collect") & theme(legend.position = "top")
  ggsave(file_composite, composite, width = 9.5, height = sum(heights))
  cat("Saved Composite:", file_composite, "\n")
} else {
  warning("Composite skipped: no panels available")
}

# -------------------------------------------------
# Diagnostics for empty facets (optional)
# -------------------------------------------------
# emmeans contrasts by Sex, Change
# print(emmeans_results %>% dplyr::filter(grepl("SUS\\s*-\\s*CON|RES\\s*-\\s*CON", contrast, ignore.case = TRUE)) %>% dplyr::count(Sex, Change))
# time-course counts
# print(data_filtered_agg %>% dplyr::mutate(time_h = (HalfHourElapsed %% 48)/2) %>% dplyr::count(Sex, Change))
# SUS/CON diff counts
# print(data_filtered_agg %>% dplyr::filter(Group %in% c("CON","SUS")) %>% dplyr::count(Sex, Change))