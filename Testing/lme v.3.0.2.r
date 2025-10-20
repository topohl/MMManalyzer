###############################################
# Mixed models analysis with improved I/O and figures
# + Oversight tables and adjusted folder structure
# - Organized results folder structure
# - Scoped aggregate XLSX outputs
# - Optional per-combination CSV exports (commented)
# - Fixed-effects plots
# - Publication panels (forest, ribbons, diff heatmap) with guards
# - Composite figure assembly
# - NEW: manifest, slice inventory, contrasts export, phase-averaged tables, figure index
###############################################

# Packages
packages <- c(
  "ggplot2","dplyr","tidyr","gridExtra","lme4",
  "lmerTest","cowplot","lsmeans","emmeans","Matrix",
  "tcltk","openxlsx","readr","stringr","patchwork","scales",
  "broom.mixed","pbkrtest"
)
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) install.packages(package, dependencies = TRUE)
  library(package, character.only = TRUE)
}

# -------------------------------------------------
# Base results directory
# -------------------------------------------------
results_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/lme_sis_activity/"
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
dir_create_safe <- function(path) { if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE) }

dirs <- list(
  logs        = file.path(results_dir, "logs"),
  models      = file.path(results_dir, "models"),
  tables_sum  = file.path(results_dir, "tables", "summary"),
  tables_emm  = file.path(results_dir, "tables", "emmeans"),
  plots_line  = file.path(results_dir, "plots", "line"),
  plots_heat  = file.path(results_dir, "plots", "heatmap"),
  plots_fixed = file.path(results_dir, "plots", "fixed_effects"),
  plots_pub   = file.path(results_dir, "plots", "publication")
)
invisible(lapply(dirs, dir_create_safe))

# NEW folders for oversight tables
dirs$tables_contr <- file.path(results_dir, "tables", "contrasts")
dirs$tables_inv   <- file.path(results_dir, "tables", "inventory")
dirs$tables_man   <- file.path(results_dir, "tables", "manifest")
dirs$tables_phase <- file.path(results_dir, "tables", "phase_average")
invisible(lapply(dirs[c("tables_contr","tables_inv","tables_man","tables_phase")], dir_create_safe))

# -------------------------------------------------
# Scope helpers
# -------------------------------------------------
scope_label <- function(Change, Sex, Phase) paste0("CC-", Change, "_Sex-", Sex, "_Phase-", Phase)
scope_all <- function(includeChange, includeSex, includePhase) paste0(
  if (includeChange) "subsetChanges" else "allChanges", "_",
  if (includeSex)    "subsetSexes"  else "allSexes",  "_",
  if (includePhase)  "subsetPhases" else "allPhases"
)

plot_dir_line <- function(Change, Sex, includeSex) {
  sub <- file.path(dirs$plots_line, paste0("CC-", Change)); dir_create_safe(sub)
  if (isTRUE(includeSex)) { sub <- file.path(sub, paste0("Sex-", Sex)); dir_create_safe(sub) }
  sub
}
plot_dir_heat <- function(Change, Sex, includeSex) {
  sub <- file.path(dirs$plots_heat, paste0("CC-", Change)); dir_create_safe(sub)
  if (isTRUE(includeSex)) { sub <- file.path(sub, paste0("Sex-", Sex)); dir_create_safe(sub) }
  sub
}
plot_dir_fixed <- function(Change, Sex) {
  sub <- file.path(dirs$plots_fixed, paste0("CC-", Change)); dir_create_safe(sub)
  sub2 <- file.path(sub, paste0("Sex-", as.character(Sex))); dir_create_safe(sub2)
  sub2
}

# -------------------------------------------------
# GUI to select inclusion factors
# -------------------------------------------------
get_user_input <- function() {
  tt <- tktoplevel(); tkwm.title(tt, "Analysis Settings"); tkconfigure(tt, bg = "#F7F9FC", padx = 10, pady = 20)
  includeChange_var <- tclVar(0); includeSex_var <- tclVar(0); includePhase_var <- tclVar(0)
  frame <- tkframe(tt, bg = "#F7F9FC"); tkgrid(frame, padx = 10, pady = 10)
  tkgrid(tklabel(frame, text = "Set Inclusion", bg = "#F7F9FC", font = c("Helvetica", 14)))
  tkgrid(tklabel(frame, text = "Parameters",   bg = "#F7F9FC", font = c("Helvetica", 14)))
  tkgrid(tklabel(frame, text="Change", bg="#F7F9FC", font=c("Helvetica",12)),
         tkcheckbutton(frame, variable = includeChange_var, bg="#F7F9FC"), padx=5, pady=5)
  tkgrid(tklabel(frame, text="Sex", bg="#F7F9FC", font=c("Helvetica",12)),
         tkcheckbutton(frame, variable = includeSex_var, bg="#F7F9FC"), padx=5, pady=5)
  tkgrid(tklabel(frame, text="Phase", bg="#F7F9FC", font=c("Helvetica",12)),
         tkcheckbutton(frame, variable = includePhase_var, bg="#F7F9FC"), padx=5, pady=5)
  onOK <- function() tkdestroy(tt)
  ok_button <- tkbutton(tt, text="OK", command=onOK, bg="#007AFF", fg="white", relief="flat", font=c("Helvetica",12))
  tkgrid(ok_button, padx=10, pady=10); tkwait.window(tt)
  list(
    includeChange = as.logical(as.numeric(tclvalue(includeChange_var))),
    includeSex    = as.logical(as.numeric(tclvalue(includeSex_var))),
    includePhase  = as.logical(as.numeric(tclvalue(includePhase_var)))
  )
}
user_input    <- get_user_input()
includeChange <- user_input$includeChange
includeSex    <- user_input$includeSex
includePhase  <- user_input$includePhase

# -------------------------------------------------
# Data preparation
# -------------------------------------------------
stopifnot(all(c("Change","Sex","Phase","Group","HalfHourElapsed","ActivityIndex","AnimalNum") %in% names(data_filtered_agg)))
data_filtered_agg <- data_filtered_agg %>%
  mutate(
    Change = as.factor(Change),
    Sex    = as.factor(Sex),
    Phase  = as.factor(Phase),
    Group  = factor(Group, levels = c("CON","RES","SUS"))
  )

# -------------------------------------------------
# Plotting: Line and Heatmaps (unchanged)
# -------------------------------------------------
plot_list <- list(); heatmap_list <- list()
data_filtered_agg_Change <- data_filtered_agg %>% split(.$Change)

# Line plots
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
                         labels = seq(0, max(df_sex$HalfHourElapsed), by = 24) / 2, expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      labs(title = "Circadian Activity Index", subtitle = "Activity Levels Across Groups",
           y = "Activity [a.u.]", x = "Time Elapsed [h]",
           caption = "Shaded regions represent ±1 SD of the mean") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14, face = "bold"),
            axis.ticks.x = element_line(linewidth = 0.5),
            legend.position = "top",
            panel.background = element_blank())
    out_dir <- plot_dir_line(Change, SexValue, includeSex)
    file_name <- paste0("line_", Change, "_", if (includeSex) SexValue else "allSexes", ".svg")
    ggsave(filename = file.path(out_dir, file_name), plot = p, device = "svg", width = 7, height = 4)
    out_sex[[SexValue]] <- p
  }
  out_sex
})

# Heatmaps
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

# -------------------------------------------------
# Modeling
# -------------------------------------------------
changes <- if (includeChange) unique(data_filtered_agg$Change) else "allChanges"
sexes   <- if (includeSex)    unique(data_filtered_agg$Sex)    else "allSexes"
phases  <- if (includePhase)  unique(data_filtered_agg$Phase)  else "allPhases"

summary_results <- data.frame(Change=character(), Sex=character(), Phase=character(),
  Fixed_effect=character(), Estimate=numeric(), Std.Error=numeric(),
  df=numeric(), t.value=numeric(), p.value=numeric(),
  p.round=numeric(), p.sign=character(), stringsAsFactors=FALSE)

emmeans_results <- data.frame(Change=character(), Sex=character(), Phase=character(),
  contrast=character(), estimate=numeric(), SE=numeric(),
  df=numeric(), t.ratio=numeric(), p.value=numeric(),
  p.adjust=numeric(), p.round=numeric(), stringsAsFactors=FALSE)

total_iterations <- length(changes) * length(sexes) * length(phases)
progress_bar <- tkProgressBar(title = "Model Fitting Progress", min = 0, max = total_iterations, width = 300)
iteration_counter <- 0

for (Change in changes) {
  for (Sex in sexes) {
    for (Phase in phases) {
      iteration_counter <- iteration_counter + 1
      setTkProgressBar(progress_bar, iteration_counter,
                       label = paste("Progress:", round(iteration_counter / total_iterations * 100, 2), "%"))

      data_subset <- data_filtered_agg %>%
        dplyr::filter(
          (!includeChange | Change == !!Change),
          (!includeSex    | Sex    == !!Sex),
          (!includePhase  | Phase  == !!Phase)
        )
      if (nrow(data_subset) == 0) { cat("No data for Change:", Change, "Sex:", Sex, "Phase:", Phase, "\n"); next }

      emm_options(pbkrtest.limit = 100000)
      model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_subset)
      ms <- summary(model)

      fixed_effects <- data.frame(
        Fixed_effect = rownames(ms$coefficients),
        Estimate     = ms$coefficients[, "Estimate"],
        Std.Error    = ms$coefficients[, "Std. Error"],
        df           = ms$coefficients[, "df"],
        t.value      = ms$coefficients[, "t value"],
        p.value      = ms$coefficients[, grep("Pr\\(>|t\\)", colnames(ms$coefficients))],
        p.round      = round(ms$coefficients[, grep("Pr\\(>|t\\)", colnames(ms$coefficients))], 3),
        p.sign       = ifelse(ms$coefficients[, grep("Pr\\(>|t\\)", colnames(ms$coefficients))] < 0.001, "***",
                         ifelse(ms$coefficients[, grep("Pr\\(>|t\\)", colnames(ms$coefficients))] < 0.01, "**",
                         ifelse(ms$coefficients[, grep("Pr\\(>|t\\)", colnames(ms$coefficients))] < 0.05, "*",
                         ifelse(ms$coefficients[, grep("Pr\\(>|t\\)", colnames(ms$coefficients))] < 0.1, "T", "ns")))),
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
      if (!includeChange) posthoc_df$Change <- "allChanges"
      if (nrow(posthoc_df) > 0) emmeans_results <- dplyr::bind_rows(emmeans_results, posthoc_df)
    }
  }
}
close(progress_bar)

run_scope <- scope_all(includeChange, includeSex, includePhase)
summary_xlsx <- file.path(dirs$tables_sum, paste0("lme_summary_results_", run_scope, ".xlsx"))
emmeans_xlsx <- file.path(dirs$tables_emm, paste0("emmeans_results_", run_scope, ".xlsx"))
openxlsx::write.xlsx(summary_results, file = summary_xlsx, rowNames = FALSE)
cat("Summary results saved to:", summary_xlsx, "\n")
openxlsx::write.xlsx(emmeans_results, file = emmeans_xlsx, rowNames = FALSE)
cat("EMMeans results saved to:", emmeans_xlsx, "\n")

# -------------------------------------------------
# Fixed effects bar plots (unchanged)
# -------------------------------------------------
data_sum <- openxlsx::read.xlsx(summary_xlsx)
data_sum <- data_sum %>%
  mutate(Fixed_effect = factor(Fixed_effect, levels = unique(Fixed_effect)),
         Sex   = factor(Sex, levels = c("m","f")),
         Phase = factor(Phase, levels = c("Active","Inactive")),
         Change = factor(Change, levels = sort(unique(Change))),
         effect_type = ifelse(Fixed_effect == "(Intercept)", "Intercept (Control)", "Other Effect"))

palette1 <- c("(Intercept)" = "#457B9D", "GroupRES" = "#C6C3BB","GroupSUS" = "#E63946")
palette2 <- c("(Intercept)" = "#1e3791", "GroupRES" = "#8aacdb","GroupSUS" = "#f49620")
use_palette <- palette2

for (c in unique(data_sum$Change)) for (s in unique(data_sum$Sex)) for (p in unique(data_sum$Phase)) {
  plot_data <- data_sum %>% filter(Change == c, Sex == s, Phase == p) %>% filter(Fixed_effect != "HalfHourElapsed")
  if (nrow(plot_data) == 0) next
  int_val <- data_sum %>% filter(Fixed_effect == "(Intercept)", Change == c, Sex == s, Phase == p) %>% pull(Estimate)
  plot_data <- plot_data %>%
    mutate(Adjusted_Estimate = dplyr::case_when(
      Fixed_effect == "(Intercept)" ~ Estimate,
      grepl("^Group", Fixed_effect) ~ as.numeric(int_val) + Estimate,
      TRUE ~ Estimate)) %>%
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
    labs(title = paste("Fixed Effects for", s, "(", p, "Phase) - Change:", c),
         x = "Fixed Effect", y = "Adjusted Estimate ± SE", fill = "Effect Type") +
    theme_minimal(base_size = 14) +
    theme(text = element_text(family = "sans"),
          axis.text.x = element_text(angle = 45, hjust = 1, color = "grey20"),
          axis.text.y = element_text(color = "grey20"),
          axis.title = element_text(face = "bold", color = "grey20"),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "grey10"),
          legend.position = "none",
          panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "white", color = NA))
  out_dir <- plot_dir_fixed(c, s)
  out_file <- file.path(out_dir, paste0("fixed_effects_", c, "_", s, "_", p, ".svg"))
  ggsave(filename = out_file, plot = p_fixed, device = "svg", width = 4, height = 5)
  cat("Saved:", out_file, "\n")
}

# -------------------------------------------------
# LME-based contrasts (SUS−CON and RES−CON), with Change collapse
# -------------------------------------------------
contrast_lmer <- function(model, L, df = NULL) {
  beta <- fixef(model); V <- as.matrix(vcov(model))
  est <- as.numeric(t(L) %*% beta); se <- sqrt(as.numeric(t(L) %*% V %*% L))
  if (is.null(df)) {
    df <- broom.mixed::glance(model)$df.residual
    if (is.na(df) || is.null(df)) df <- getME(model, "devcomp")$dims["n"] - length(beta)
  }
  tval <- est / se; pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
  ci95 <- est + c(-1,1) * qt(0.975, df) * se
  list(estimate = est, SE = se, df = df, t = tval, p = pval, lwr = ci95[1], upr = ci95[2])
}

slices <- expand.grid(
  Change = if (includeChange) unique(data_filtered_agg$Change) else unique(data_filtered_agg$Change),
  Sex    = if (includeSex)    unique(data_filtered_agg$Sex)    else unique(data_filtered_agg$Sex),
  Phase  = if (includePhase)  unique(data_filtered_agg$Phase)  else unique(data_filtered_agg$Phase),
  stringsAsFactors = FALSE
)

lme_contrasts <- list()
for (i in seq_len(nrow(slices))) {
  ch <- slices$Change[i]; sx <- slices$Sex[i]; ph <- slices$Phase[i]
  dsub <- data_filtered_agg %>%
    dplyr::filter(
      (!includeChange | Change == ch),
      (!includeSex    | Sex    == sx),
      (!includePhase  | Phase  == ph)
    )
  if (nrow(dsub) == 0) next
  m  <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1|AnimalNum), data = dsub)
  cf <- names(fixef(m))
  add_row <- function(pair, coef_name, ch_label) {
    if (coef_name %in% cf) {
      L <- rep(0, length(cf)); names(L) <- cf; L[coef_name] <- 1
      rs <- contrast_lmer(m, L)
      data.frame(
        Change = ch_label, Sex = sx, Phase = ph, pair = pair,
        estimate = rs$estimate, SE = rs$SE, df = rs$df,
        t.value = rs$t, p.value = rs$p, lwr = rs$lwr, upr = rs$upr
      )
    } else NULL
  }
  ch_label <- if (!includeChange) "allChanges" else ch
  lme_contrasts[[length(lme_contrasts)+1]] <- add_row("SUS-CON","GroupSUS", ch_label)
  lme_contrasts[[length(lme_contrasts)+1]] <- add_row("RES-CON","GroupRES", ch_label)
}
lme_contrasts <- dplyr::bind_rows(lme_contrasts)
if (nrow(lme_contrasts) > 0) {
  lme_contrasts <- lme_contrasts %>%
    dplyr::group_by(Change, Sex, Phase) %>%
    dplyr::mutate(p.adjust = p.adjust(p.value, method = "holm")) %>%
    dplyr::ungroup()
}

# -------------------------------------------------
# Publication figure panels with guards
# -------------------------------------------------
theme_pub_small <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
dir_pub <- dirs$plots_pub

# EMMeans forest (Panel A) with dynamic x
emm_df <- openxlsx::read.xlsx(emmeans_xlsx)
parse_contrast <- function(x) {
  x <- gsub("\\|.*$", "", x); x <- gsub("\\s+", "", x); x <- tolower(x)
  parts <- strsplit(x, "-", fixed = TRUE)[[1]]; if (length(parts) != 2) return(c(NA_character_, NA_character_)); parts
}
pc <- t(vapply(emm_df$contrast, parse_contrast, FUN.VALUE = character(2)))
colnames(pc) <- c("lhs","rhs")
emm_df <- cbind(emm_df, pc) |>
  as.data.frame() |>
  dplyr::mutate(
    pair = dplyr::case_when(
      (lhs == "sus" & rhs == "con") | (lhs == "con" & rhs == "sus") ~ "SUS-CON",
      (lhs == "res" & rhs == "con") | (lhs == "con" & rhs == "res") ~ "RES-CON",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(pair), !is.na(Sex), !is.na(Change), !is.na(Phase)) |>
  dplyr::mutate(
    tcrit = qt(0.975, df = df),
    ymin = estimate - tcrit * SE,
    ymax = estimate + tcrit * SE,
    Sex = droplevels(factor(Sex)),
    ChangeCollapsed = if (!includeChange) "allChanges" else as.character(Change),
    Phase = factor(Phase, levels = c("Active","Inactive"))
  )

x_var <- if (includeChange) "Change" else "ChangeCollapsed"

file_panelA <- file.path(dir_pub, paste0("panelA_forest_phase_", run_scope, ".svg"))
if (nrow(emm_df) > 0 && length(unique(emm_df$Sex)) > 0) {
  forest_plot <- ggplot(
    data = emm_df,
    aes(x = .data[[x_var]], y = estimate, ymin = ymin, ymax = ymax, color = pair, shape = pair)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(position = position_dodge(width = 0.5), linewidth = 0.6) +
    geom_text(aes(label = ifelse(pair == "SUS-CON" & p.adjust < 0.05, "★", "")),
              position = position_dodge(width = 0.5), vjust = -1.0, size = 3.8, color = "#333333") +
    scale_color_manual(values = c("RES-CON" = "grey70", "SUS-CON" = "#E63946")) +
    scale_shape_manual(values = c("RES-CON" = 16, "SUS-CON" = 17)) +
    labs(title = "EMM contrasts vs CON by Phase", x = if (includeChange) "Cage change" else "allChanges", y = "Estimate (difference vs CON)") +
    facet_grid(Phase ~ Sex, scales = "free_x") +
    theme_pub_small
  ggsave(file_panelA, forest_plot, width = 8.5, height = 4.8)
  cat("Saved Panel A (EMM):", file_panelA, "\n")
} else {
  warning("Panel A (EMM) skipped: no data")
}

# LME forest (Panel A-LME) with dynamic x
lme_plot_df <- lme_contrasts %>%
  dplyr::mutate(ChangeCollapsed = if (!includeChange) "allChanges" else as.character(Change))
file_panelA_lme <- file.path(dirs$plots_pub, paste0("panelA_forest_LME_phase_", run_scope, ".svg"))
if (nrow(lme_plot_df) > 0) {
  forest_plot_lme <- ggplot(
    data = lme_plot_df,
    aes(x = .data[[x_var]], y = estimate, ymin = lwr, ymax = upr, color = pair, shape = pair)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(position = position_dodge(width = 0.5), linewidth = 0.6) +
    geom_text(aes(label = ifelse(p.adjust < 0.05 & pair == "SUS-CON", "★", "")),
              position = position_dodge(width = 0.5), vjust = -1.0, size = 3.8, color = "#333333") +
    scale_color_manual(values = c("RES-CON" = "grey60", "SUS-CON" = "#E63946")) +
    scale_shape_manual(values = c("RES-CON" = 16, "SUS-CON" = 17)) +
    labs(title = "LME contrasts vs CON by Phase", x = if (includeChange) "Cage change" else "allChanges", y = "Estimate (difference vs CON)") +
    facet_grid(Phase ~ Sex, scales = "free_x") +
    theme_pub_small
  ggsave(file_panelA_lme, forest_plot_lme, width = 8.5, height = 4.8)
  cat("Saved Panel A (LME):", file_panelA_lme, "\n")
}

# ============================================
# CC start times for alignment (single definition)
# ============================================
cc_starts <- data_filtered_agg %>%
  dplyr::group_by(Change) %>%
  dplyr::summarise(t0 = min(HalfHourElapsed, na.rm = TRUE), .groups = "drop")

# ============================
# Panel B: CC-aligned ribbons (phases collapsed)
# ============================
df_plot_B <- data_filtered_agg %>%
  dplyr::filter(
    (!includeChange | Change %in% unique(Change)),
    (!includeSex    | Sex    %in% unique(Sex)),
    (!includePhase  | Phase  %in% unique(Phase))
  )
tc_df <- df_plot_B %>%
  dplyr::left_join(cc_starts, by = "Change") %>%
  dplyr::mutate(rel_time_h = (HalfHourElapsed - t0) / 2) %>%
  dplyr::group_by(Change, Sex, Group, rel_time_h) %>%
  dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE),
                   sd_act   = sd(ActivityIndex, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(Group = factor(Group, levels = c("CON","RES","SUS")),
                Sex = droplevels(factor(Sex)),
                Change = droplevels(factor(Change)))

file_panelB <- file.path(dir_pub, paste0("panelB_ribbons_rel_phaseCollapsed_", run_scope, ".svg"))
if (nrow(tc_df) > 0) {
  max_rel <- max(tc_df$rel_time_h, na.rm = TRUE)
  ribbons_plot <- ggplot(tc_df, aes(x = rel_time_h, y = mean_act, color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = mean_act - sd_act, ymax = mean_act + sd_act), alpha = 0.22, colour = NA) +
    geom_line(size = 0.8) +
    scale_color_manual(values = c("CON"="#C6C3BB", "RES"="#457B9D", "SUS"="#E63946")) +
    scale_fill_manual(values = c("CON"="#C6C3BB", "RES"="#457B9D", "SUS"="#E63946")) +
    scale_x_continuous(breaks = scales::breaks_pretty(), limits = c(0, max_rel)) +
    labs(title = "Activity over CC-relative time (mean ± SD)", x = "Time since CC start [h]", y = "Activity [a.u.]") +
    facet_grid(Sex ~ Change, scales = "free_y") +
    theme_pub_small
  ggsave(file_panelB, ribbons_plot, width = 10.5, height = 4.6)
  cat("Saved Panel B (rel, phases collapsed):", file_panelB, "\n")
} else { warning("Panel B skipped: no data") }

# ============================
# Panel C: Phase-aligned SUS−CON difference heatmap
# ============================
df_plot_C <- data_filtered_agg %>%
  dplyr::filter(
    Group %in% c("CON","SUS"),
    (!includeChange | Change %in% unique(Change)),
    (!includeSex    | Sex    %in% unique(Sex)),
    (!includePhase  | Phase  %in% unique(Phase))
  )
phase_starts <- df_plot_C %>%
  dplyr::group_by(Change, Sex, Phase) %>%
  dplyr::summarise(t0_phase = min(HalfHourElapsed, na.rm = TRUE), .groups = "drop")

diff_df <- df_plot_C %>%
  dplyr::left_join(phase_starts, by = c("Change","Sex","Phase")) %>%
  dplyr::mutate(rel_phase_h = (HalfHourElapsed - t0_phase) / 2) %>%
  dplyr::group_by(Change, Sex, Phase, Group, rel_phase_h) %>%
  dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Group, values_from = mean_act) %>%
  dplyr::mutate(diff_SC = SUS - CON,
                Sex = droplevels(factor(Sex)),
                Change = droplevels(factor(Change)),
                Phase = factor(Phase, levels = c("Active","Inactive"))) %>%
  dplyr::filter(!is.na(diff_SC))

file_panelC <- file.path(dir_pub, paste0("panelC_heat_phaseAligned_", run_scope, ".svg"))
if (nrow(diff_df) > 0) {
  thresh <- stats::quantile(abs(diff_df$diff_SC), probs = 0.95, na.rm = TRUE)
  diff_df <- diff_df %>% dplyr::mutate(sig_bin = abs(diff_SC) >= as.numeric(thresh))
  heat_plot <- ggplot(diff_df, aes(x = rel_phase_h, y = factor(Change, levels = sort(unique(as.character(Change)))), fill = diff_SC)) +
    geom_tile() +
    scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0) +
    geom_point(data = subset(diff_df, sig_bin),
               aes(x = rel_phase_h, y = factor(Change, levels = sort(unique(as.character(Change))))),
               shape = 4, size = 0.8, color = "black", inherit.aes = FALSE) +
    labs(title = "SUS−CON difference (Phase-aligned)", x = "Time since phase start [h]", y = "Cage change", fill = "Δ Activity") +
    facet_grid(Phase ~ Sex, scales = "free_y") +
    theme_pub_small
  ggsave(file_panelC, heat_plot, width = 10.5, height = 4.8)
  cat("Saved Panel C (phase-aligned):", file_panelC, "\n")
} else { warning("Panel C skipped: no data") }

# ============================
# Additional combined figures: EMM-ABC and LME-ABC
# ============================

# EMM-based composite (A-EMM + B + C if available)
if (exists("forest_plot")) {
  if (exists("ribbons_plot") && exists("heat_plot")) {
    combined_emm_abc <- (forest_plot / ribbons_plot / heat_plot) +
      patchwork::plot_layout(heights = c(4.8, 4.6, 4.8), guides = "collect") &
      theme(legend.position = "top")
    file_combined_emm_abc <- file.path(dir_pub, paste0("Combined_EMM_ABC_", run_scope, ".svg"))
    ggsave(file_combined_emm_abc, combined_emm_abc, width = 10.5, height = 4.8 + 4.6 + 4.8)
    cat("Saved Combined EMM ABC:", file_combined_emm_abc, "\n")
  } else if (exists("ribbons_plot")) {
    combined_emm_ab <- (forest_plot / ribbons_plot) +
      patchwork::plot_layout(heights = c(4.8, 4.6), guides = "collect") &
      theme(legend.position = "top")
    file_combined_emm_ab <- file.path(dir_pub, paste0("Combined_EMM_AB_", run_scope, ".svg"))
    ggsave(file_combined_emm_ab, combined_emm_ab, width = 10.5, height = 4.8 + 4.6)
    cat("Saved Combined EMM AB:", file_combined_emm_ab, "\n")
  }
}

# LME-based composite (A-LME + B + C if available)
if (exists("forest_plot_lme")) {
  if (exists("ribbons_plot") && exists("heat_plot")) {
    combined_lme_abc <- (forest_plot_lme / ribbons_plot / heat_plot) +
      patchwork::plot_layout(heights = c(4.8, 4.6, 4.8), guides = "collect") &
      theme(legend.position = "top")
    file_combined_lme_abc <- file.path(dir_pub, paste0("Combined_LME_ABC_", run_scope, ".svg"))
    ggsave(file_combined_lme_abc, combined_lme_abc, width = 10.5, height = 4.8 + 4.6 + 4.8)
    cat("Saved Combined LME ABC:", file_combined_lme_abc, "\n")
  } else if (exists("ribbons_plot")) {
    combined_lme_ab <- (forest_plot_lme / ribbons_plot) +
      patchwork::plot_layout(heights = c(4.8, 4.6), guides = "collect") &
      theme(legend.position = "top")
    file_combined_lme_ab <- file.path(dir_pub, paste0("Combined_LME_AB_", run_scope, ".svg"))
    ggsave(file_combined_lme_ab, combined_lme_ab, width = 10.5, height = 4.8 + 4.6)
    cat("Saved Combined LME AB:", file_combined_lme_ab, "\n")
  }
}

# ============================
# NEW OVERSIGHT TABLES
# ============================

# 1) Slice inventory table
slice_inventory <- data_filtered_agg %>%
  dplyr::filter(
    (!includeChange | Change %in% unique(Change)),
    (!includeSex    | Sex    %in% unique(Sex)),
    (!includePhase  | Phase  %in% unique(Phase))
  ) %>%
  dplyr::group_by(Change, Sex, Phase) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_animals = dplyr::n_distinct(AnimalNum),
    time_min = min(HalfHourElapsed, na.rm = TRUE),
    time_max = max(HalfHourElapsed, na.rm = TRUE),
    time_mean = mean(HalfHourElapsed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    lme_contrasts %>% dplyr::count(Change, Sex, Phase, name = "has_lme_rows"),
    by = c("Change", "Sex", "Phase")
  ) %>%
  dplyr::left_join(
    emmeans_results %>% dplyr::count(Change, Sex, Phase, name = "has_emm_rows"),
    by = c("Change", "Sex", "Phase")
  ) %>%
  dplyr::mutate(
    Has_LME = !is.na(has_lme_rows) & has_lme_rows > 0,
    Has_EMM = !is.na(has_emm_rows) & has_emm_rows > 0
  ) %>%
  dplyr::select(-has_lme_rows, -has_emm_rows)

openxlsx::write.xlsx(
  slice_inventory,
  file = file.path(dirs$tables_inv, paste0("slices_", run_scope, ".xlsx")),
  rowNames = FALSE
)

# 2) Export tidy contrasts with metadata
if (nrow(lme_contrasts) > 0) {
  lme_export <- lme_contrasts %>%
    dplyr::mutate(
      effect_type = "LME coefficient at reference time",
      reference_time = "HalfHourElapsed = 0 (uncentered)"
    )
  openxlsx::write.xlsx(
    lme_export,
    file = file.path(dirs$tables_contr, paste0("lme_contrasts_", run_scope, ".xlsx")),
    rowNames = FALSE
  )
}

if (nrow(emmeans_results) > 0) {
  emm_export <- emmeans_results %>%
    dplyr::rename(pair = contrast) %>%
    dplyr::mutate(
      effect_type = "EMMeans at specified time",
      reference_time = "as set in analysis (see Methods)"
    )
  openxlsx::write.xlsx(
    emm_export,
    file = file.path(dirs$tables_contr, paste0("emm_contrasts_", run_scope, ".xlsx")),
    rowNames = FALSE
  )
}

# 3) Phase-averaged observational differences (CON vs SUS)
phase_avg <- data_filtered_agg %>%
  dplyr::filter(Group %in% c("CON", "SUS")) %>%
  dplyr::filter(
    (!includeChange | Change %in% unique(Change)),
    (!includeSex    | Sex    %in% unique(Sex)),
    (!includePhase  | Phase  %in% unique(Phase))
  ) %>%
  dplyr::group_by(Change, Sex, Phase, Group) %>%
  dplyr::summarise(mean_activity = mean(ActivityIndex, na.rm = TRUE),
                   sd_activity = sd(ActivityIndex, na.rm = TRUE),
                   n_obs = dplyr::n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Group, values_from = c(mean_activity, sd_activity, n_obs)) %>%
  dplyr::mutate(diff_SC_mean = mean_activity_SUS - mean_activity_CON)

openxlsx::write.xlsx(
  phase_avg,
  file = file.path(dirs$tables_phase, paste0("phase_avg_diff_", run_scope, ".xlsx")),
  rowNames = FALSE
)

# 4) Figure index table
figure_rows <- list(
  data.frame(kind="PanelA_EMM",  file=file.path(dir_pub, paste0("panelA_forest_phase_", run_scope, ".svg"))),
  data.frame(kind="PanelA_LME",  file=file.path(dir_pub, paste0("panelA_forest_LME_phase_", run_scope, ".svg"))),
  data.frame(kind="PanelB",       file=file.path(dir_pub, paste0("panelB_ribbons_rel_phaseCollapsed_", run_scope, ".svg"))),
  data.frame(kind="PanelC",       file=file.path(dir_pub, paste0("panelC_heat_phaseAligned_", run_scope, ".svg"))),
  data.frame(kind="Combined_EMM_ABC", file=file.path(dir_pub, paste0("Combined_EMM_ABC_", run_scope, ".svg"))),
  data.frame(kind="Combined_LME_ABC", file=file.path(dir_pub, paste0("Combined_LME_ABC_", run_scope, ".svg")))
)
fig_idx <- do.call(rbind, figure_rows) %>%
  dplyr::mutate(
    scope = run_scope,
    exists = file.exists(file),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
openxlsx::write.xlsx(
  fig_idx,
  file = file.path(dirs$tables_man, paste0("figures_", run_scope, ".xlsx")),
  rowNames = FALSE
)

# 5) Run manifest (append)
manifest_file <- file.path(dirs$tables_man, "manifest_runs.xlsx")
manifest_row <- data.frame(
  timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  includeChange = includeChange,
  includeSex    = includeSex,
  includePhase  = includePhase,
  scope         = run_scope,
  n_slices      = nrow(slice_inventory),
  n_emm_rows    = nrow(emmeans_results),
  n_lme_rows    = nrow(lme_contrasts),
  summary_file  = summary_xlsx,
  emmeans_file  = emmeans_xlsx,
  stringsAsFactors = FALSE
)
if (file.exists(manifest_file)) {
  old <- openxlsx::read.xlsx(manifest_file)
  openxlsx::write.xlsx(dplyr::bind_rows(old, manifest_row), file = manifest_file, rowNames = FALSE)
} else {
  openxlsx::write.xlsx(manifest_row, file = manifest_file, rowNames = FALSE)
}
