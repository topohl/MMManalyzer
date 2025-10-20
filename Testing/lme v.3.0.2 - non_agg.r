###############################################
# Mixed models with random time slopes + fixed-grid phase-edge windows
###############################################

suppressPackageStartupMessages({
  packages <- c(
    "ggplot2","dplyr","tidyr","gridExtra","lme4",
    "lmerTest","cowplot","lsmeans","emmeans","Matrix",
    "tcltk","openxlsx","readr","stringr","patchwork","scales",
    "broom.mixed","pbkrtest","rlang","tibble"
  )
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
})
options(contrasts = c("contr.sum","contr.poly"))

# -------------------------------------------------
# Options and thresholds
# -------------------------------------------------
use_group_time_interaction <- TRUE
min_animals <- 2
min_groups  <- 2
min_obs     <- 8
min_time_lv <- 3  # distinct TH values required for slopes/interaction in main model

# -------------------------------------------------
# Paths
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
dirs$tables_contr <- file.path(results_dir, "tables", "contrasts")
dirs$tables_inv   <- file.path(results_dir, "tables", "inventory")
dirs$tables_man   <- file.path(results_dir, "tables", "manifest")
dirs$tables_phase <- file.path(results_dir, "tables", "phase_average")
invisible(lapply(dirs[c("tables_contr","tables_inv","tables_man","tables_phase")], dir_create_safe))

dirs$edgeAggAll <- file.path(results_dir, "plots", "edge_emm")
dir_create_safe(dirs$edgeAggAll)

# Convenience alias used later
edgeAggAll_dir <- dirs$edgeAggAll

# -------------------------------------------------
# Helpers
# -------------------------------------------------
scope_all <- function(includeChange, includeSex, includePhase) paste0(
  if (includeChange) "subsetChanges" else "allChanges", "_",
  if (includeSex)    "subsetSexes"  else "allSexes",  "_",
  if (includePhase)  "subsetPhases" else "allPhases"
)
plot_dir_line  <- function(Change, Sex, includeSex) { sub <- file.path(dirs$plots_line, paste0("CC-", Change)); dir_create_safe(sub); if (isTRUE(includeSex)) { sub <- file.path(sub, paste0("Sex-", Sex)); dir_create_safe(sub) }; sub }
plot_dir_heat  <- function(Change, Sex, includeSex) { sub <- file.path(dirs$plots_heat, paste0("CC-", Change)); dir_create_safe(sub); if (isTRUE(includeSex)) { sub <- file.path(sub, paste0("Sex-", Sex)); dir_create_safe(sub) }; sub }
plot_dir_fixed <- function(Change, Sex) { sub <- file.path(dirs$plots_fixed, paste0("CC-", Change)); dir_create_safe(sub); sub2 <- file.path(sub, paste0("Sex-", as.character(Sex))); dir_create_safe(sub2); sub2 }

quiet_singular <- function(expr) {
  withCallingHandlers(expr,
    warning = function(w) {
      if (grepl("boundary \\(singular\\) fit", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
}
fit_lmer_adaptive <- function(fixed_rhs, df, time_var = "TH", animal = "AnimalNum", allow_slope = TRUE) {
  ctrl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  if (!allow_slope) {
    f <- as.formula(paste0("ActivityIndex ~ ", fixed_rhs, " + (1 | ", animal, ")"))
    return(lmerTest::lmer(f, data = df, control = ctrl))
  }
  f1 <- as.formula(paste0("ActivityIndex ~ ", fixed_rhs, " + (1 + ", time_var, " | ", animal, ")"))
  fit1 <- try(lmerTest::lmer(f1, data = df, control = ctrl), silent = TRUE)
  if (!inherits(fit1, "try-error") && !lme4::isSingular(fit1, tol = 1e-5)) return(fit1)
  f2 <- as.formula(paste0("ActivityIndex ~ ", fixed_rhs, " + (1 | ", animal, ") + (0 + ", time_var, " | ", animal, ")"))
  fit2 <- try(lmerTest::lmer(f2, data = df, control = ctrl), silent = TRUE)
  if (!inherits(fit2, "try-error") && !lme4::isSingular(fit2, tol = 1e-5)) return(fit2)
  f3 <- as.formula(paste0("ActivityIndex ~ ", fixed_rhs, " + (1 | ", animal, ")"))
  lmerTest::lmer(f3, data = df, control = ctrl)
}
has_cols <- function(x, cols) all(cols %in% names(x))

# -------------------------------------------------
# GUI
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
ui <- get_user_input()
includeChange <- ui$includeChange
includeSex    <- ui$includeSex
includePhase  <- ui$includePhase

# -------------------------------------------------
# Data prep
# -------------------------------------------------
stopifnot(all(c("Change","Sex","Phase","Group","HalfHourElapsed","ActivityIndex","AnimalNum") %in% names(data_filtered_agg)))
data_filtered_agg <- data_filtered_agg %>%
  mutate(
    Change = factor(Change),
    Sex    = factor(Sex),
    Phase  = factor(Phase, levels = c("Active","Inactive")),
    Group  = factor(Group, levels = c("CON","RES","SUS")),
    HalfHourElapsed = as.integer(HalfHourElapsed)
  )

# -------------------------------------------------
# Line and heatmap plots
# -------------------------------------------------
data_by_change <- split(data_filtered_agg, data_filtered_agg$Change)

for (Change in sort(unique(data_filtered_agg$Change))) {
  df_change <- data_by_change[[as.character(Change)]]
  for (SexValue in c("m","f")) {
    df_sex <- if (includeSex) dplyr::filter(df_change, Sex == SexValue) else df_change
    pal <- c("#457B9D", "#C6C3BB", "#E63946")
    p <- ggplot(df_sex, aes(HalfHourElapsed, ActivityIndex, color = Group, group = Group)) +
      geom_path(stat = "summary", fun = mean, linewidth = 0.8) +
      stat_summary(aes(fill = Group), fun = mean,
                   fun.min = function(x) mean(x) - sd(x),
                   fun.max = function(x) mean(x) + sd(x),
                   geom = "ribbon", alpha = 0.3, colour = NA) +
      scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
      scale_x_continuous(labels = function(x) x/2, breaks = scales::pretty_breaks(6)) +
      labs(title = "Circadian Activity Index", x = "Time Elapsed [h]", y = "Activity [a.u.]") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(), legend.position = "top", plot.title = element_text(face="bold", hjust=0.5))
    out_dir <- plot_dir_line(as.character(Change), SexValue, includeSex)
    ggsave(file.path(out_dir, paste0("line_", Change, "_", if (includeSex) SexValue else "allSexes", ".svg")), p, width = 7, height = 4)
  }
}

for (Change in sort(unique(data_filtered_agg$Change))) {
  df_change <- data_by_change[[as.character(Change)]]
  for (SexValue in c("m","f")) {
    df_sex <- if (includeSex) dplyr::filter(df_change, Sex == SexValue) else df_change
    p <- ggplot(df_sex, aes(HalfHourElapsed, reorder(Group, -as.numeric(Group)), fill = ActivityIndex)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkblue", limits = c(0, max(data_filtered_agg$ActivityIndex))) +
      scale_x_continuous(labels = function(x) x/2, breaks = scales::pretty_breaks(6)) +
      facet_grid(Sex ~ ., scales = "free") +
      cowplot::theme_minimal_hgrid(12, rel_small = 1) +
      labs(title = bquote(~bold(.(paste("Activity Index")))),
           subtitle = paste("Cage Change:", Change), x = "Time elapsed [h]", y = "Group") +
      theme(legend.position = "top", plot.title = element_text(hjust = 0.5))
    out_dir <- plot_dir_heat(as.character(Change), SexValue, includeSex)
    ggsave(file.path(out_dir, paste0("heatmap_", Change, "_", if (includeSex) SexValue else "allSexes", ".svg")), p, width = 5, height = 5)
  }
}

# -------------------------------------------------
# Modeling (main slices)
# -------------------------------------------------
changes <- if (includeChange) unique(data_filtered_agg$Change) else "allChanges"
sexes   <- if (includeSex)    unique(data_filtered_agg$Sex)    else "allSexes"
phases  <- if (includePhase)  unique(data_filtered_agg$Phase)  else "allPhases"

summary_results <- tibble::tibble()
emmeans_results <- tibble::tibble()
log_file <- file.path(dirs$logs, paste0("fit_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

total_iterations <- length(changes) * length(sexes) * length(phases)
pb <- tkProgressBar(title = "Model Fitting Progress", min = 0, max = total_iterations, width = 300)
i <- 0

for (Change in changes) {
  for (Sex in sexes) {
    for (Phase in phases) {
      i <- i + 1
      setTkProgressBar(pb, i, label = paste("Progress:", round(i/total_iterations*100,2), "%"))
      dsub <- data_filtered_agg %>%
        dplyr::filter(
          (!includeChange | Change == !!Change),
          (!includeSex    | Sex    == !!Sex),
          (!includePhase  | Phase  == !!Phase)
        )
      if (nrow(dsub) == 0) next

      n_anim <- dplyr::n_distinct(dsub$AnimalNum)
      n_group <- dplyr::n_distinct(dsub$Group)
      if (n_anim < min_animals || n_group < min_groups || nrow(dsub) < min_obs) {
        cat(paste(Sys.time(), Change, Sex, Phase, "SKIP main: few_animals/groups/obs\n"), file = log_file, append = TRUE)
        next
      }

      t0 <- min(dsub$HalfHourElapsed, na.rm = TRUE)
      dsub <- dsub %>% dplyr::mutate(TH = (HalfHourElapsed - t0)/2)
      use_int <- use_group_time_interaction && (dplyr::n_distinct(dsub$TH) >= min_time_lv)
      fixed_rhs <- if (use_int) "Group * TH" else "Group + TH"
      per_animal <- dsub %>% dplyr::count(AnimalNum, name = "n_i")
      allow_slope <- all(per_animal$n_i >= 3) && (dplyr::n_distinct(dsub$TH) >= min_time_lv)

      model <- quiet_singular(fit_lmer_adaptive(fixed_rhs, dsub, time_var = "TH", allow_slope = allow_slope))
      ms <- summary(model)
      cat(paste(Sys.time(), Change, Sex, Phase, "MAIN form:", deparse(formula(model)),
                "singular:", lme4::isSingular(model), "\n"), file = log_file, append = TRUE)

      fx <- tibble::tibble(
        Fixed_effect = rownames(ms$coefficients),
        Estimate     = unname(ms$coefficients[, "Estimate"]),
        Std.Error    = unname(ms$coefficients[, "Std. Error"]),
        df           = unname(ms$coefficients[, "df"]),
        t.value      = unname(ms$coefficients[, "t value"]),
        p.value      = unname(ms$coefficients[, grep("Pr\\(>|t\\)", colnames(ms$coefficients))]),
        p.round      = round(p.value, 3),
        p.sign       = dplyr::case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          p.value < 0.1   ~ "T",
          TRUE ~ "ns"
        ),
        Change = as.character(Change), Phase = as.character(Phase), Sex = as.character(Sex)
      )
      summary_results <- dplyr::bind_rows(summary_results, fx)

      if (!any(grepl("^Group", names(fixef(model))))) next

      emm_lvl <- emmeans::emmeans(model, pairwise ~ Group, at = list(TH = 0))
      emm_lvl_df <- as.data.frame(summary(emm_lvl$contrasts, infer = TRUE, adjust = "holm")) %>%
        dplyr::mutate(contrast_type = "level@start",
                      Change = as.character(Change), Phase = as.character(Phase), Sex = as.character(Sex))
      emm_lvl_df <- emm_lvl_df[!is.na(emm_lvl_df$p.value), ]
      emm_lvl_df$p.adjust <- p.adjust(emm_lvl_df$p.value, "holm")
      if (nrow(emm_lvl_df) > 0) emmeans_results <- dplyr::bind_rows(emmeans_results, emm_lvl_df)

      if (use_int && any(grepl("Group:TH", names(fixef(model))))) {
        emm_tr <- emmeans::emtrends(model, pairwise ~ Group, var = "TH")
        emm_tr_df <- as.data.frame(summary(emm_tr$contrasts, infer = TRUE, adjust = "holm")) %>%
          dplyr::mutate(contrast_type = "slope",
                        Change = as.character(Change), Phase = as.character(Phase), Sex = as.character(Sex))
        emm_tr_df <- emm_tr_df[!is.na(emm_tr_df$p.value), ]
        emm_tr_df$p.adjust <- p.adjust(emm_tr_df$p.value, "holm")
        if (nrow(emm_tr_df) > 0) emmeans_results <- dplyr::bind_rows(emmeans_results, emm_tr_df)
      }
    }
  }
}
close(pb)

run_scope <- scope_all(includeChange, includeSex, includePhase)
summary_xlsx <- file.path(dirs$tables_sum, paste0("lme_summary_results_", run_scope, ".xlsx"))
emmeans_xlsx <- file.path(dirs$tables_emm, paste0("emmeans_results_", run_scope, ".xlsx"))
openxlsx::write.xlsx(summary_results, summary_xlsx, rowNames = FALSE)
openxlsx::write.xlsx(emmeans_results, emmeans_xlsx, rowNames = FALSE)

# -------------------------------------------------
# Fixed effects bar plots
# -------------------------------------------------
data_sum <- openxlsx::read.xlsx(summary_xlsx)
data_sum <- data_sum %>%
  mutate(Fixed_effect = factor(Fixed_effect, levels = unique(Fixed_effect)),
         Sex   = factor(Sex, levels = c("m","f")),
         Phase = factor(Phase, levels = c("Active","Inactive")),
         Change = factor(Change, levels = sort(unique(Change))))
pal <- c("(Intercept)"="#457B9D","GroupRES"="#C6C3BB","GroupSUS"="#E63946")

for (c in unique(data_sum$Change)) for (s in unique(data_sum$Sex)) for (p in unique(data_sum$Phase)) {
  plot_data <- data_sum %>%
    filter(Change == c, Sex == s, Phase == p) %>%
    filter(!Fixed_effect %in% c("HalfHourElapsed","TH")) %>%
    filter(!grepl("^Group:TH", Fixed_effect))
  if (nrow(plot_data) == 0) next
  int_val <- data_sum %>% filter(Fixed_effect=="(Intercept)", Change==c, Sex==s, Phase==p) %>% pull(Estimate)
  plot_data <- plot_data %>%
    mutate(Adjusted_Estimate = dplyr::case_when(
      Fixed_effect == "(Intercept)" ~ Estimate,
      grepl("^Group", Fixed_effect) ~ as.numeric(int_val) + Estimate,
      TRUE ~ Estimate)) %>%
    mutate(Fixed_effect = factor(as.character(Fixed_effect), levels = names(pal))) %>%
    filter(Fixed_effect %in% names(pal))
  if (nrow(plot_data)==0) next
  pf <- ggplot(plot_data, aes(Fixed_effect, Adjusted_Estimate, fill = Fixed_effect)) +
    geom_bar(stat="identity", width = 0.6) +
    geom_errorbar(aes(ymin = Adjusted_Estimate - Std.Error, ymax = Adjusted_Estimate + Std.Error), width = 0.2, color = "grey40") +
    geom_text(aes(label = p.sign), vjust = -0.8, size = 5, color = "grey20") +
    scale_fill_manual(values = pal) +
    labs(title = paste("Fixed Effects for", s, "(", p, ") - Change:", c), x = "Fixed Effect", y = "Adjusted Estimate ± SE") +
    theme_minimal(base_size = 14) + theme(legend.position = "none", plot.title = element_text(face="bold", hjust=0.5))
  out_dir <- plot_dir_fixed(as.character(c), as.character(s))
  ggsave(file.path(out_dir, paste0("fixed_effects_", c, "_", s, "_", p, ".svg")), pf, width = 4, height = 5)
}

# -------------------------------------------------
# Publication panels A/B/C and combined
# -------------------------------------------------
theme_pub_small <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
dir_pub <- dirs$plots_pub

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
}

# Panel B (ribbons) and C (heatmap)
cc_starts <- data_filtered_agg %>% dplyr::group_by(Change) %>% dplyr::summarise(t0 = min(HalfHourElapsed, na.rm = TRUE), .groups = "drop")
df_plot_B <- data_filtered_agg %>% dplyr::filter((!includeChange | Change %in% unique(Change)), (!includeSex | Sex %in% unique(Sex)), (!includePhase | Phase %in% unique(Phase)))
tc_df <- df_plot_B %>% dplyr::left_join(cc_starts, by = "Change") %>% dplyr::mutate(rel_time_h = (HalfHourElapsed - t0) / 2) %>%
  dplyr::group_by(Change, Sex, Group, rel_time_h) %>% dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE), sd_act = sd(ActivityIndex, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(Group = factor(Group, levels = c("CON","RES","SUS")), Sex = droplevels(factor(Sex)), Change = droplevels(factor(Change)))
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
}

df_plot_C <- data_filtered_agg %>% dplyr::filter(Group %in% c("CON","SUS"), (!includeChange | Change %in% unique(Change)), (!includeSex | Sex %in% unique(Sex)), (!includePhase | Phase %in% unique(Phase)))
phase_starts <- df_plot_C %>% dplyr::group_by(Change, Sex, Phase) %>% dplyr::summarise(t0_phase = min(HalfHourElapsed, na.rm = TRUE), .groups = "drop")
diff_df <- df_plot_C %>% dplyr::left_join(phase_starts, by = c("Change","Sex","Phase")) %>% dplyr::mutate(rel_phase_h = (HalfHourElapsed - t0_phase) / 2) %>%
  dplyr::group_by(Change, Sex, Phase, Group, rel_phase_h) %>% dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Group, values_from = mean_act) %>% dplyr::mutate(diff_SC = SUS - CON, Sex = droplevels(factor(Sex)), Change = droplevels(factor(Change)), Phase = factor(Phase, levels = c("Active","Inactive"))) %>%
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
}

# Combined
if (exists("forest_plot")) {
  if (exists("ribbons_plot") && exists("heat_plot")) {
    combined_emm_abc <- (forest_plot / ribbons_plot / heat_plot) +
      patchwork::plot_layout(heights = c(4.8, 4.6, 4.8), guides = "collect") &
      theme(legend.position = "top")
    ggsave(file.path(dirs$plots_pub, paste0("Combined_EMM_ABC_", run_scope, ".svg")), combined_emm_abc, width = 10.5, height = 14.2)
  } else if (exists("ribbons_plot")) {
    combined_emm_ab <- (forest_plot / ribbons_plot) +
      patchwork::plot_layout(heights = c(4.8, 4.6), guides = "collect") &
      theme(legend.position = "top")
    ggsave(file.path(dirs$plots_pub, paste0("Combined_EMM_AB_", run_scope, ".svg")), combined_emm_ab, width = 10.5, height = 9.4)
  }
}

# ==== Cleanup to avoid binding against stale objects on sourced runs ====
for (obj in c("df_emm_start","df_emm_end","df_emm_all",
              "plot_start_all","plot_end_all","plot_start_traj","plot_end_traj")) {
  if (exists(obj, envir = .GlobalEnv)) rm(list = obj, envir = .GlobalEnv)
}

# ==== REQUIRED: Build edge_animal_means_collapsed (unconditional) ====
edges_all_segments <- data_filtered_agg %>%
  dplyr::mutate(
    seg_id = dplyr::case_when(
      Phase == "Active"   ~ as.integer(ConsecActive),
      Phase == "Inactive" ~ as.integer(ConsecInactive),
      TRUE ~ NA_integer_
    )
  ) %>% dplyr::filter(!is.na(seg_id), seg_id >= 1L)

seg_bounds <- edges_all_segments %>%
  dplyr::group_by(Change, Sex, Phase, AnimalNum, seg_id) %>%
  dplyr::summarise(
    seg_h0 = min(HalfHourElapsed, na.rm = TRUE),
    seg_h1 = max(HalfHourElapsed, na.rm = TRUE),
    .groups = "drop"
  )

edges_labeled_all <- edges_all_segments %>%
  dplyr::left_join(seg_bounds, by = c("Change","Sex","Phase","AnimalNum","seg_id")) %>%
  dplyr::mutate(
    edge_bin = dplyr::case_when(
      HalfHourElapsed >= seg_h0 & HalfHourElapsed <= seg_h0 + 3L ~ "start4",
      HalfHourElapsed >= seg_h1 - 3L & HalfHourElapsed <= seg_h1   ~ "end4",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(edge_bin)) %>%
  dplyr::select(Change, Sex, Phase, Group, AnimalNum, seg_id, HalfHourElapsed, ActivityIndex, edge_bin)

edge_animal_means_all <- edges_labeled_all %>%
  dplyr::group_by(Change, Sex, Phase, Group, AnimalNum, seg_id, edge_bin) %>%
  dplyr::summarise(ActivityEdge = mean(ActivityIndex, na.rm = TRUE), .groups = "drop")

edge_animal_means_collapsed <- edge_animal_means_all %>%
  dplyr::group_by(Change, Sex, Phase, Group, AnimalNum, edge_bin) %>%
  dplyr::summarise(ActivityEdge = mean(ActivityEdge, na.rm = TRUE), .groups = "drop")

# =========================
# 8) EMMeans: START/END (hardcoded separation by Change, Phase, Sex)
# =========================

# Mixed-model df provider (Kenward–Roger for df/SE stability)
emm_options(pbkrtest.limit = 100000, lmerTest.limit = 100000, lmer.df = "kenward-roger")

# Canonical levels
lvl_Change <- sort(unique(as.character(edge_animal_means_collapsed$Change)))
lvl_Phase  <- c("Active","Inactive")
lvl_Group  <- c("CON","RES","SUS")
lvl_Sex    <- sort(unique(as.character(edge_animal_means_collapsed$Sex)))
lvl_edge   <- c("start4","end4")

# Inputs: factor copies for fitting (…_f), character copies for display
edge_dat <- edge_animal_means_collapsed %>%
  dplyr::transmute(
    ActivityEdge = as.numeric(ActivityEdge),
    Change_f = factor(as.character(Change), levels = lvl_Change),
    Change   = as.character(Change),
    Sex_f    = factor(as.character(Sex), levels = lvl_Sex),
    Sex      = as.character(Sex),
    Phase_f  = factor(as.character(Phase), levels = lvl_Phase),
    Phase    = as.character(Phase),
    Group_f  = factor(as.character(Group), levels = lvl_Group),
    Group    = as.character(Group),
    AnimalNum = as.factor(AnimalNum),
    edge_bin  = factor(as.character(edge_bin), levels = lvl_edge)
  )

edge_start <- edge_dat %>% dplyr::filter(edge_bin == "start4")
edge_end   <- edge_dat %>% dplyr::filter(edge_bin == "end4")

# Fit per-sex with safe optional interactions (Group:Phase + Group:Change when levels allow)
fit_emm_subset <- function(df) {
  if (dplyr::n_distinct(df$Group_f) < 2 || dplyr::n_distinct(df$AnimalNum) < 2) return(NULL)
  ok_change <- dplyr::n_distinct(df$Change_f) >= 2
  ok_phase  <- dplyr::n_distinct(df$Phase_f)  >= 2

  form_try <- if (ok_change && ok_phase) {
    ActivityEdge ~ Group_f + Phase_f + Change_f + Group_f:Phase_f + Group_f:Change_f + (1 | AnimalNum)
  } else {
    ActivityEdge ~ Group_f + Phase_f + Change_f + (1 | AnimalNum)
  }

  m <- quiet_singular(lmerTest::lmer(form_try, data = df,
                                     control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
  emmeans::emmeans(m, specs = ~ Group_f | Phase_f + Change_f)
}


# Converter: map model-term factor names back to display names; normalize columns
dbg_hdr <- function(title) {
  cat("\n", paste(rep("=", 70), collapse=""), "\n", sep = "")
  cat("DEBUG:", title, "\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
}
dbg_str <- function(obj, name) {
  cat(sprintf("- %s: class=%s | is.data.frame=%s | is.list=%s | is.atomic=%s | length=%d | nrow=%s | names=%s\n",
              name,
              paste(class(obj), collapse=","),
              is.data.frame(obj),
              is.list(obj),
              is.atomic(obj),
              length(obj),
              tryCatch(NROW(obj), error=function(e) NA_integer_),
              paste(names(obj), collapse=", ")))
}
dbg_cols <- function(df, needed) {
  miss <- setdiff(needed, names(df))
  if (length(miss)) cat("  missing cols:", paste(miss, collapse=", "), "\n")
  else cat("  all needed columns present\n")
}

# Converter: map model-term factor names back to display names; normalize columns
emm_to_df <- function(emm_obj, sex_label, edge_lab) {
  dbg_hdr("emm_to_df: entry")
  dbg_str(emm_obj, "emm_obj")

  # Try to obtain a summary with intervals; otherwise fall back to emm_obj
  smry <- try(summary(emm_obj, infer = TRUE), silent = TRUE)
  if (inherits(smry, "try-error")) {
    cat("summary(emm_obj, infer=TRUE) failed; using emm_obj directly\n")
    smry <- emm_obj
  }
  dbg_str(smry, "smry (post-summary)")

  # Coerce to a data.frame-like object while guarding against atomic output
  df_emm <- NULL
  if (is.data.frame(smry)) {
    df_emm <- smry
    cat("smry is already a data.frame; using as-is\n")
  } else if (is.list(smry)) {
    cat("smry is a list; attempting as.data.frame(list)\n")
    df_emm <- try(as.data.frame(smry, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df_emm, "try-error") || !is.data.frame(df_emm)) {
      cat("as.data.frame(list) failed; creating placeholder tibble\n")
      df_emm <- tibble::tibble(emmean = NA_real_)
    }
  } else {
    cat("smry is atomic or unsupported; creating placeholder tibble\n")
    df_emm <- tibble::tibble(emmean = NA_real_)
  }
  dbg_str(df_emm, "df (after coercion)")
  dbg_cols(df_emm, c("emmean","SE","df","lower.CL","upper.CL","Phase_f","Change_f","Group_f","Phase","Change","Group"))

  # Harmonize possible column names across emmeans versions
  if (!("emmean"   %in% names(df_emm)) && ("lsmean"     %in% names(df_emm))) df_emm$emmean   <- df_emm$lsmean
  if (!("lower.CL" %in% names(df_emm)) && ("asymp.LCL"  %in% names(df_emm))) df_emm$lower.CL <- df_emm$asymp.LCL
  if (!("upper.CL" %in% names(df_emm)) && ("asymp.UCL"  %in% names(df_emm))) df_emm$upper.CL <- df_emm$asymp.UCL

  # Map model-term names to display names when present
  if ("Phase_f"  %in% names(df_emm) && !("Phase"  %in% names(df_emm)))  df_emm$Phase  <- as.character(df_emm$Phase_f)
  if ("Change_f" %in% names(df_emm) && !("Change" %in% names(df_emm)))  df_emm$Change <- as.character(df_emm$Change_f)
  if ("Group_f"  %in% names(df_emm) && !("Group"  %in% names(df_emm)))  df_emm$Group  <- as.character(df_emm$Group_f)

  # Ensure keys exist even if not provided by the grid
  if (!("Phase"  %in% names(df_emm)))  df_emm$Phase  <- NA_character_
  if (!("Change" %in% names(df_emm)))  df_emm$Change <- NA_character_
  if (!("Group"  %in% names(df_emm)))  df_emm$Group  <- NA_character_

  # Attach Sex label
  df_emm$Sex <- sex_label

  # Guarantee numeric columns exist
  if (!("emmean"   %in% names(df_emm))) df_emm$emmean   <- NA_real_
  if (!("SE"       %in% names(df_emm))) df_emm$SE       <- NA_real_
  if (!("df"       %in% names(df_emm))) df_emm$df       <- NA_real_
  if (!("lower.CL" %in% names(df_emm))) df_emm$lower.CL <- NA_real_
  if (!("upper.CL" %in% names(df_emm))) df_emm$upper.CL <- NA_real_

  # Assert state and sample rows
  cat("ASSERT before column access: class(df)=", paste(class(df_emm), collapse=","),
      " | is.data.frame=", is.data.frame(df_emm),
      " | is.atomic=", is.atomic(df_emm),
      " | names=", paste(names(df_emm), collapse=", "), "\n", sep = "")
  print(utils::head(df_emm, 3))

  # Final guard before accessing columns
  cat("GUARD: forcing data.frame and re-checking structure\n")
  df_emm <- as.data.frame(df_emm, stringsAsFactors = FALSE)
  cat("GUARD class(df)=", paste(class(df_emm), collapse=","),
      " is.atomic=", is.atomic(df_emm),
      " is.data.frame=", is.data.frame(df_emm),
      " names=", paste(names(df_emm), collapse=", "), "\n", sep = "")

  # Extract with [[ ]] to avoid $ on non-frames
  em_vec  <- suppressWarnings(as.numeric(df_emm[["emmean"]]))
  se_vec  <- suppressWarnings(as.numeric(df_emm[["SE"]]))
  df_vec  <- suppressWarnings(as.numeric(df_emm[["df"]]))
  lwr_vec <- suppressWarnings(as.numeric(df_emm[["lower.CL"]]))
  upr_vec <- suppressWarnings(as.numeric(df_emm[["upper.CL"]]))

  # Build standardized tibble
  out <- tibble::tibble(
    Change   = factor(as.character(df_emm[["Change"]]), levels = lvl_Change),
    Phase    = factor(as.character(df_emm[["Phase"]]),  levels = lvl_Phase),
    Group    = factor(as.character(df_emm[["Group"]]),  levels = lvl_Group),
    Sex      = factor(as.character(df_emm[["Sex"]]),    levels = lvl_Sex),
    emmean   = em_vec,
    SE       = se_vec,
    df       = df_vec,
    lwr      = lwr_vec,
    upr      = upr_vec,
    edge_bin = factor(edge_lab, levels = lvl_edge)
  )
  dbg_str(out, "out (standardized tibble)")
  out
}





# Build EMMeans per sex (always split by Sex)
emm_list_start <- list()
emm_list_end   <- list()
for (sx in levels(edge_dat$Sex_f)) {
  cat("\n---- Processing Sex:", sx, "----\n")
  ds <- edge_start %>% dplyr::filter(Sex == !!sx)
  de <- edge_end   %>% dplyr::filter(Sex == !!sx)
  cat("nrow(ds)=", nrow(ds), " nrow(de)=", nrow(de), "\n")

  es <- fit_emm_subset(ds)
  cat("fit_emm_subset(ds) class:", if (is.null(es)) "NULL" else paste(class(es), collapse=","), "\n")
  ee <- fit_emm_subset(de)
  cat("fit_emm_subset(de) class:", if (is.null(ee)) "NULL" else paste(class(ee), collapse=","), "\n")

  if (!is.null(es)) {
    cat("Calling emm_to_df on START\n")
    emm_list_start[[sx]] <- emm_to_df(es, sx, "start4")
  }
  if (!is.null(ee)) {
    cat("Calling emm_to_df on END\n")
    emm_list_end[[sx]] <- emm_to_df(ee, sx, "end4")
  }
}


df_emm_start <- if (length(emm_list_start)) dplyr::bind_rows(emm_list_start) else tibble::tibble(
  Change=factor(character(), levels=lvl_Change),
  Phase=factor(character(),  levels=lvl_Phase),
  Group=factor(character(),  levels=lvl_Group),
  Sex=factor(character(),    levels=lvl_Sex),
  emmean=double(), SE=double(), df=double(), lwr=double(), upr=double(),
  edge_bin=factor(character(), levels=lvl_edge)
)
df_emm_end   <- if (length(emm_list_end)) dplyr::bind_rows(emm_list_end) else tibble::tibble(
  Change=factor(character(), levels=lvl_Change),
  Phase=factor(character(),  levels=lvl_Phase),
  Group=factor(character(),  levels=lvl_Group),
  Sex=factor(character(),    levels=lvl_Sex),
  emmean=double(), SE=double(), df=double(), lwr=double(), upr=double(),
  edge_bin=factor(character(), levels=lvl_edge)
)
df_emm_all <- dplyr::bind_rows(df_emm_start, df_emm_end)

# Save table
openxlsx::write.xlsx(df_emm_all, file = file.path(dirs$tables_emm, "edge_EMMeans_linearPred_subsetChanges_subsetSexes_subsetPhases.xlsx"), rowNames = FALSE)

# Facet plots: always Phase × Sex by Change
facet_formula_all <- vars(Phase, Sex) ~ vars(Change)

# START
plot_start_all <- ggplot(df_emm_start, aes(x = Group, y = emmean, color = Group)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25, alpha = 0.9) +
  geom_point(size = 2.8) + geom_line(aes(group = Group), linewidth = 0.7, alpha = 0.6) +
  scale_color_manual(values = c("CON"="#39568CFF","RES"="#2BB07FFF","SUS"="#FDE725FF")) +
  facet_grid(facet_formula_all, scales = "free_y") +
  labs(title = "Edge START (first 2h) — EMMeans linear predictions",
       x = "Group", y = "Linear Prediction (EMM)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave(file.path(edgeAggAll_dir, "EMM_edge_START_all_subsetChanges_subsetSexes_subsetPhases.svg"),
       plot_start_all, width = 12, height = 5.2)

# END
plot_end_all <- ggplot(df_emm_end, aes(x = Group, y = emmean, color = Group)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25, alpha = 0.9) +
  geom_point(size = 2.8) + geom_line(aes(group = Group), linewidth = 0.7, alpha = 0.6) +
  scale_color_manual(values = c("CON"="#39568CFF","RES"="#2BB07FFF","SUS"="#FDE725FF")) +
  facet_grid(facet_formula_all, scales = "free_y") +
  labs(title = "Edge END (last 2h) — EMMeans linear predictions",
       x = "Group", y = "Linear Prediction (EMM)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave(file.path(edgeAggAll_dir, "EMM_edge_END_all_subsetChanges_subsetSexes_subsetPhases.svg"),
       plot_end_all, width = 12, height = 5.2)

# Trajectories across CCs
make_traj <- function(df_emm, title) {
  p <- ggplot(df_emm, aes(x = Change, y = emmean, color = Group, group = Group)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr),
                  width = 0.15, alpha = 0.9,
                  position = position_dodge2(width = 0.25, padding = 0.15)) +
    geom_point(size = 2.6, position = position_dodge2(width = 0.25, padding = 0.15)) +
    geom_line(linewidth = 0.8, position = position_dodge2(width = 0.25, padding = 0.15)) +
    scale_color_manual(values = c("CON"="#39568CFF","RES"="#2BB07FFF","SUS"="#FDE725FF")) +
    facet_grid(vars(Phase), vars(Sex), scales = "free_y") +
    labs(title = title, x = "Cage Change", y = "Linear Prediction (EMM)") +
    theme_minimal(base_size = 12) + theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
  p
}

plot_start_traj <- make_traj(df_emm_start, "START (first 2h) across Cage Changes — EMMeans")
ggsave(file.path(edgeAggAll_dir, "EMM_edge_START_trajectory_subsetChanges_subsetSexes_subsetPhases.svg"),
       plot_start_traj, width = 12, height = 5.5)

plot_end_traj <- make_traj(df_emm_end, "END (last 2h) across Cage Changes — EMMeans")
ggsave(file.path(edgeAggAll_dir, "EMM_edge_END_trajectory_subsetChanges_subsetSexes_subsetPhases.svg"),
       plot_end_traj, width = 12, height = 5.5)


# -------------------------------------------------
# Oversight tables and manifest (guarded joins)
# -------------------------------------------------
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
  )

if (exists("emmeans_results") && nrow(emmeans_results) > 0 && has_cols(emmeans_results, c("Change","Sex","Phase"))) {
  emm_counts <- emmeans_results %>% dplyr::count(Change, Sex, Phase, name = "has_emm_rows")
  slice_inventory <- dplyr::left_join(slice_inventory, emm_counts, by = c("Change","Sex","Phase"))
} else {
  slice_inventory$has_emm_rows <- NA_integer_
}
openxlsx::write.xlsx(slice_inventory, file = file.path(dirs$tables_inv, paste0("slices_", run_scope, ".xlsx")), rowNames = FALSE)

manifest_file <- file.path(dirs$tables_man, "manifest_runs.xlsx")
manifest_row <- data.frame(
  timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  includeChange = includeChange,
  includeSex    = includeSex,
  includePhase  = includePhase,
  scope         = run_scope,
  n_slices      = nrow(slice_inventory),
  n_emm_rows    = nrow(emmeans_results),
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
