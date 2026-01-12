###############################################
# Mixed models with random time slopes + fixed-grid phase-edge windows
###############################################

suppressPackageStartupMessages({
  packages <- c(
    "ggplot2", "dplyr", "tidyr", "gridExtra", "lme4",
    "lmerTest", "cowplot", "lsmeans", "emmeans", "Matrix",
    "tcltk", "openxlsx", "readr", "stringr", "patchwork", "scales",
    "broom.mixed", "pbkrtest", "rlang", "tibble"
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

# load functions
source("C:/Users/topohl/Documents/GitHub/MMManalyzer/Analysis/MMM_functions.R")

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

# NEW: edge EMM plot directory
dirs$edgeAggAll <- file.path(results_dir, "plots", "edge_emm")
dir_create_safe(dirs$edgeAggAll)
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

compute_phase_avg_for_model <- function(model, Change, Sex, Phase, adjust_method = "holm") {
  th_step <- 0.5
  th_grid <- seq(0, 12, by = th_step)

  # EMMs across TH grid
  emm_grid <- emmeans::emmeans(model, ~ Group, at = list(TH = th_grid))
  emm_grid_df <- as.data.frame(emm_grid)

  # Average per Group
  avg_df <- emm_grid_df %>%
    dplyr::group_by(Group) %>%
    dplyr::summarise(
      emmean_avg = mean(emmean, na.rm = TRUE),
      SE_avg     = sqrt(mean(SE^2, na.rm = TRUE) / length(th_grid)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(Change = as.character(Change),
                  Sex    = as.character(Sex),
                  Phase  = as.character(Phase))

  # Pairwise contrasts from averaged EMMs (design-based approximation)
  avg_df$Group <- factor(avg_df$Group, levels = c("CON","RES","SUS"))
  get_contrast <- function(L) {
    est <- as.numeric(L %*% avg_df$emmean_avg)
    V   <- diag(avg_df$SE_avg^2, nrow = length(avg_df$SE_avg))
    se  <- sqrt(as.numeric(L %*% V %*% t(L)))
    c(estimate = est, SE = se)
  }
  L_list <- list(
    "RES - CON" = c(-1, 1, 0),
    "SUS - CON" = c(-1, 0, 1),
    "RES - SUS" = c( 0, 1, -1)
  )
  contr_rows <- lapply(names(L_list), function(nm) {
    L <- matrix(L_list[[nm]], nrow = 1)
    est <- get_contrast(L)
    data.frame(
      contrast = nm, estimate = est["estimate"], SE = est["SE"],
      Change = as.character(Change), Sex = as.character(Sex), Phase = as.character(Phase),
      stringsAsFactors = FALSE
    )
  })
  contr_df <- dplyr::bind_rows(contr_rows)

  # df and CIs
  df_rep <- tryCatch({
    sm <- summary(model)
    as.numeric(sm$coefficients[,"df"][1])
  }, error = function(e) NA_real_)
  if (!is.finite(df_rep)) df_rep <- 200

  tcrit <- qt(0.975, df = df_rep)
  avg_df <- avg_df %>%
    dplyr::mutate(
      lwr = emmean_avg - tcrit * SE_avg,
      upr = emmean_avg + tcrit * SE_avg
    )
  contr_df <- contr_df %>%
    dplyr::mutate(
      t.ratio = estimate / SE,
      df = df_rep,
      p.value = 2 * pt(-abs(t.ratio), df = df_rep),
      p.adjust = p.adjust(p.value, method = adjust_method),
      lwr = estimate - tcrit * SE,
      upr = estimate + tcrit * SE
    )

  list(means = avg_df, contrasts = contr_df)
}

# -------------------------------------------------
# GUI
# -------------------------------------------------
get_user_input <- function() {
  tt <- tktoplevel(); tkwm.title(tt, "Analysis Settings"); tkconfigure(tt, bg = "#F7F9FC", padx = 10, pady = 20)
  includeChange_var <- tclVar(0); includeSex_var <- tclVar(0); includePhase_var <- tclVar(0)
  frame <- tkframe(tt, bg = "#F7F9FC"); tkgrid(frame, padx = 10, pady = 10)
  tkgrid(tklabel(frame, text = "Set Inclusion", bg = "#F7F9FC", font = c("Helvetica", 14)))
  tkgrid(tklabel(frame, text = "Parameters",   bg = "#F7F9FC", font = c("Helvetica", 14)))
  tkgrid(tklabel(frame, text = "Change", bg = "#F7F9FC", font = c("Helvetica", 12)),
         tkcheckbutton(frame, variable = includeChange_var, bg="#F7F9FC"), padx=5, pady=5)
  tkgrid(tklabel(frame, text = "Sex", bg = "#F7F9FC", font = c("Helvetica", 12)),
         tkcheckbutton(frame, variable = includeSex_var, bg="#F7F9FC"), padx=5, pady=5)
  tkgrid(tklabel(frame, text = "Phase", bg = "#F7F9FC", font = c("Helvetica", 12)),
         tkcheckbutton(frame, variable = includePhase_var, bg = "#F7F9FC"), padx = 5, pady = 5)
  onOK <- function() tkdestroy(tt)
  ok_button <- tkbutton(tt, text = "OK", command = onOK,bg = "#007AFF", fg = "white", relief = "flat", font = c("Helvetica", 12))
  tkgrid(ok_button, padx = 10, pady = 10); tkwait.window(tt)
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
stopifnot(all(c("Change", "Sex", "Phase", "Group", "HalfHourElapsed", "ActivityIndex", "AnimalNum") %in% names(data_filtered_agg)))
data_filtered_agg <- data_filtered_agg %>%
  mutate(
    Change = factor(Change),
    Sex    = factor(Sex),
    Phase  = factor(Phase, levels = c("Active", "Inactive")),
    Group  = factor(Group, levels = c("CON", "RES", "SUS")),
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

# Collectors
phase_avg_means_all <- list()
phase_avg_contr_all <- list()

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

      n_anim  <- dplyr::n_distinct(dsub$AnimalNum)
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
      if (exists("model") && inherits(model, "lmerMod")) {
        stat_k <- compute_phase_avg_for_model(model, Change, Sex, Phase, adjust_method = "holm")
        if (!is.null(stat_k$means)     && nrow(stat_k$means))     phase_avg_means_all[[length(phase_avg_means_all)+1]]   <- stat_k$means
        if (!is.null(stat_k$contrasts) && nrow(stat_k$contrasts)) phase_avg_contr_all[[length(phase_avg_contr_all)+1]] <- stat_k$contrasts
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

phase_avg_means_tbl <- if (length(phase_avg_means_all)) dplyr::bind_rows(phase_avg_means_all) else tibble::tibble()
phase_avg_contr_tbl <- if (length(phase_avg_contr_all)) dplyr::bind_rows(phase_avg_contr_all) else tibble::tibble()

if (nrow(phase_avg_means_tbl) == 0 && nrow(phase_avg_contr_tbl) == 0) {
  message("No phase-average results were generated for the current selection.")
} else {
  if (nrow(phase_avg_means_tbl)) {
    phase_avg_means_tbl <- phase_avg_means_tbl %>%
      dplyr::mutate(
        Change = factor(Change, levels = unique(Change)),
        Phase  = factor(Phase,  levels = c("Active","Inactive")),
        Sex    = factor(Sex,    levels = unique(Sex)),
        Group  = factor(Group,  levels = c("CON","RES","SUS"))
      ) %>%
      dplyr::distinct(Change, Sex, Phase, Group, .keep_all = TRUE)
  }
  if (nrow(phase_avg_contr_tbl)) {
    phase_avg_contr_tbl <- phase_avg_contr_tbl %>%
      dplyr::mutate(
        Change = factor(Change, levels = unique(Change)),
        Phase  = factor(Phase,  levels = c("Active","Inactive")),
        Sex    = factor(Sex,    levels = unique(Sex))
      )
  }

  # Save tables
  openxlsx::write.xlsx(phase_avg_means_tbl, file.path(dirs$tables_emm, paste0("phaseAvg_emmeans_", run_scope, ".xlsx")), rowNames = FALSE)
  openxlsx::write.xlsx(phase_avg_contr_tbl, file.path(dirs$tables_emm, paste0("phaseAvg_contrasts_", run_scope, ".xlsx")), rowNames = FALSE)

  # Canonicalize, keep three pairs, and deduplicate
  canonicalize_contrast <- function(df, keep_pairs = c("RES-CON","SUS-CON","RES-SUS")) {
    if (!nrow(df)) return(df)
    parts <- strsplit(df$contrast, "\\s*-\\s*")
    lhs <- vapply(parts, `[`, character(1), 1)
    rhs <- vapply(parts, `[`, character(1), 2)
    lhs <- trimws(lhs); rhs <- trimws(rhs)

    flip_vs_con <- lhs == "CON" & rhs %in% c("RES","SUS")
    if (any(flip_vs_con)) {
      df$estimate[flip_vs_con] <- -df$estimate[flip_vs_con]
      if (all(c("lwr","upr") %in% names(df))) {
        lwr_new <- -df$upr[flip_vs_con]
        upr_new <- -df$lwr[flip_vs_con]
        df$lwr[flip_vs_con] <- pmin(lwr_new, upr_new, na.rm = TRUE)
        df$upr[flip_vs_con] <- pmax(lwr_new, upr_new, na.rm = TRUE)
      }
      df$contrast[flip_vs_con] <- paste(rhs[flip_vs_con], "- CON")
    }

    df$pair <- dplyr::case_when(
      grepl("^RES\\s*-\\s*CON$", df$contrast) ~ "RES-CON",
      grepl("^SUS\\s*-\\s*CON$", df$contrast) ~ "SUS-CON",
      grepl("^RES\\s*-\\s*SUS$", df$contrast) ~ "RES-SUS",
      TRUE ~ NA_character_
    )

    df <- df %>% dplyr::filter(pair %in% keep_pairs)

    if (!all(c("lwr","upr") %in% names(df)) && all(c("SE","df") %in% names(df))) {
      df$tcrit <- qt(0.975, df = df$df)
      df$lwr   <- df$estimate - df$tcrit * df$SE
      df$upr   <- df$estimate + df$tcrit * df$SE
    }

    df %>% dplyr::distinct(Change, Sex, Phase, pair, .keep_all = TRUE)
  }

  phase_avg_contr_tbl <- canonicalize_contrast(phase_avg_contr_tbl, keep_pairs = c("RES-CON","SUS-CON","RES-SUS"))

  # Forest plot (RES-CON, SUS-CON, RES-SUS)
  if (nrow(phase_avg_contr_tbl) > 0) {
    forest_phaseavg <- ggplot(
      phase_avg_contr_tbl,
      aes(x = Change, y = estimate, ymin = lwr, ymax = upr, color = pair, shape = pair)
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_pointrange(position = position_dodge(width = 0.55), linewidth = 0.6) +
      geom_text(
        aes(label = ifelse(p.adjust < 0.001, "***",
          ifelse(p.adjust < 0.01, "**",
            ifelse(p.adjust < 0.05, "*", "")
          )
        )
        ),
        position = position_dodge(width = 0.55),
        vjust = -1.0,
        size = 3.6,
        color = "#333333"
      ) +
      scale_color_manual(values = c("RES-CON" = "grey60", "SUS-CON" = "#E63946", "RES-SUS" = "#457B9D")) +
      scale_shape_manual(values = c("RES-CON" = 16, "SUS-CON" = 16, "RES-SUS" = 16)) +
      labs(title = "Phase-averaged contrasts (RES-CON, SUS-CON, RES-SUS)",
           x = "Cage Change", y = "Estimate (difference)") +
      facet_grid(Phase ~ Sex, scales = "free_x") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", hjust = 0.5)) +
      coord_flip()

    ggsave(file.path(dirs$plots_pub, paste0("panelA_forest_phaseAvg_with_RES-SUS_", run_scope, ".svg")),
           forest_phaseavg, width = 5, height = 9)
  }
}

# ============================================
# 1. Load phase-average result tables
# ============================================
phase_avg_contr_tbl <- openxlsx::read.xlsx(file.path(dirs$tables_emm, paste0("phaseAvg_contrasts_", run_scope, ".xlsx")))
phase_avg_means_tbl <- openxlsx::read.xlsx(file.path(dirs$tables_emm, paste0("phaseAvg_emmeans_", run_scope, ".xlsx")))

# Convert relevant columns to factors with preferred order
phase_avg_contr_tbl <- phase_avg_contr_tbl %>%
  mutate(
    contrast = factor(contrast, levels = c("RES - CON", "SUS - CON", "RES - SUS")),
    Change = factor(Change),
    Sex = factor(Sex),
    Phase = factor(Phase, levels = c("Active", "Inactive"))
  )

# ----------- Plot functions -----------
plot_volcano_phaseavg <- function(df, n_labels = 10, pval_thresh = 0.05, effect_thresh = 1) {
  df <- df %>%
    mutate(
      sig = p.adjust < pval_thresh,
      large_effect = abs(estimate) > effect_thresh,
      label_flag = sig & large_effect,
      contrast = factor(contrast, levels = c("RES - CON",
                                             "SUS - CON",
                                             "RES - SUS")),
      Phase = factor(Phase, levels = c("Active", "Inactive")),
      Sex = factor(Sex)
    )
  # Optional: top labels per Sex (phases are overlaid)
  label_points <- df %>%
    filter(label_flag) %>%
    group_by(Sex) %>%
    arrange(p.adjust, desc(abs(estimate))) %>%
    slice_head(n = n_labels) %>%
    ungroup()

  ggplot(
    df,
    aes(
      x = estimate,
      y = -log10(p.adjust),
      color = contrast,
      shape = Phase
    )
  ) +
    geom_vline(
      xintercept = c(-effect_thresh, effect_thresh),
      linetype = "dotted",
      color = "gray50"
    ) +
    geom_hline(
      yintercept = -log10(0.05),
      linetype = "dashed",
      color = "gray70"
    ) +
    geom_point(size = 5, alpha = 0.8
    ) +
    # geom_text_repel(
    #   data = label_points,
    #   aes(label = contrast),
    #   size = 3,
    #   max.overlaps = 15,
    #   nudge_y = 0.5
    # ) +
    scale_color_manual(values = c(
      "RES - CON" = "#457B9D",
      "SUS - CON" = "#E63946",
      "RES - SUS" = "#C6C3BB"
    )) +
scale_shape_manual(values = c("Active" = 16, "Inactive" = 1), name = "Phase") +
    facet_grid(. ~ Sex) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_line(linewidth = 0.2),
      panel.grid.major.y = element_line(linewidth = 0.2),
      legend.position = "top"
    ) +
scale_x_continuous(breaks = seq(-20, 30, by = 10), minor_breaks = NULL) +
    labs(
      x = "Effect size (estimate)",
      y = expression(-log[10](adjusted ~ p ~ value)),
      color = "Contrast",
      title = "Volcano Plot by Sex (phases overlaid with shape)"
   )
}

plot_forest_phaseavg <- function(df, contrast_levels = levels(df$contrast)) {
  sub <- df %>% filter(contrast %in% contrast_levels)
  ggplot(sub, aes(y = reorder(contrast, estimate),
                  x = estimate,
                  xmin = lwr,
                  xmax = upr,
                  color = contrast)
                  ) +
geom_point(size = 3) +
geom_errorbarh(height = 0.2) +
facet_grid(Phase ~ Change) +
theme_minimal(base_size = 14) +
    labs(x = "Estimate (phase avg, with 95% CI)", y = "Contrast", title = "Forest Plot (Phase Average)")
}

# For dot / estimation plot you use phase_avg_means_tbl; then:
# Convert Group, Phase, Change, Sex to factor as needed (similar approach)
phase_avg_means_tbl <- phase_avg_means_tbl %>%
  mutate(
    Group = factor(Group, levels = c("CON", "RES", "SUS")),
    Phase = factor(Phase, levels = c("Active", "Inactive")),
    Change = factor(Change),
    Sex = factor(Sex)
  )

plot_dot_estimation_phaseavg <- function(df) {
  ggplot(df, aes(x = Group, y = emmean_avg, color = Sex)) +
    geom_point(position = position_jitter(width = 0.2), size = 3) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
    facet_grid(Phase ~ Change) +
    theme_minimal(base_size = 14) +
    labs(x = "Group", y = "Estimated Phase Average", title = "Dot / Estimation Plot (Phase Average)")
}

# ----------- Create and save plots -----------

volcano_plot_phaseavg <- plot_volcano_phaseavg(phase_avg_contr_tbl)
ggsave(file.path(dirs$plots_pub, paste0("volcano_phaseAvg_", run_scope, ".svg")), volcano_plot_phaseavg, width = 8, height = 6)

forest_plot_phaseavg <- plot_forest_phaseavg(phase_avg_contr_tbl)
ggsave(file.path(dirs$plots_pub, paste0("forest_phaseAvg_", run_scope, ".svg")), forest_plot_phaseavg, width = 9, height = 7)

dot_estimation_plot_phaseavg <- plot_dot_estimation_phaseavg(phase_avg_means_tbl)
ggsave(file.path(dirs$plots_pub, paste0("dotEstimation_phaseAvg_", run_scope, ".svg")), dot_estimation_plot_phaseavg, width = 8, height = 6)

# -------------------------------------------------
# Fixed effects bar plots
# -------------------------------------------------
data_sum <- openxlsx::read.xlsx(summary_xlsx)
data_sum <- data_sum %>%
  mutate(Fixed_effect = factor(Fixed_effect, levels = unique(Fixed_effect)),
         Sex   = factor(Sex, levels = c("m","f")),
         Phase = factor(Phase, levels = c("Active","Inactive")),
         Change = factor(Change, levels = sort(unique(Change))))
pal <- c("(Intercept)" = "#457B9D", "GroupRES" = "#C6C3BB", "GroupSUS" = "#E63946")

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

# NEW: materialize XFacet to avoid quosure/non-atomic mapping
emm_df <- emm_df %>%
  dplyr::mutate(
    XFacet = if (includeChange)
      factor(as.character(Change), levels = sort(unique(as.character(Change))))
    else
      factor("allChanges", levels = "allChanges")
  )

file_panelA <- file.path(dir_pub, paste0("panelA_forest_phase_", run_scope, ".svg"))
if (nrow(emm_df) > 0 && length(unique(emm_df$Sex)) > 0) {
  forest_plot <- ggplot(
    data = emm_df,
    aes(x = XFacet, y = estimate, ymin = ymin, ymax = ymax, color = pair, shape = pair)
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
cc_starts <- data_filtered_agg %>%
  dplyr::group_by(Change) %>%
  dplyr::summarise(t0 = min(HalfHourElapsed, na.rm = TRUE), .groups = "drop")
df_plot_B <- data_filtered_agg %>%
  dplyr::filter((!includeChange | Change %in% unique(Change)),
                (!includeSex | Sex %in% unique(Sex)),
                (!includePhase | Phase %in% unique(Phase)))
tc_df <- df_plot_B %>%
  dplyr::left_join(cc_starts, by = "Change") %>%
  dplyr::mutate(rel_time_h = (HalfHourElapsed - t0) / 2) %>%
  dplyr::group_by(Change, Sex, Group, rel_time_h) %>%
  dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE),
                   sd_act = sd(ActivityIndex, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(Group = factor(Group, levels = c("CON", "RES", "SUS")),
                Sex = droplevels(factor(Sex)),
                Change = droplevels(factor(Change)))
file_panelB <- file.path(dir_pub, paste0("panelB_ribbons_rel_phaseCollapsed_", run_scope, ".svg"))
if (nrow(tc_df) > 0) {
  max_rel <- max(tc_df$rel_time_h, na.rm = TRUE)
  ribbons_plot <- ggplot(tc_df, aes(x = rel_time_h, y = mean_act, color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = mean_act - sd_act, ymax = mean_act + sd_act), alpha = 0.22, colour = NA) +
    geom_line(size = 0.8) +
    scale_color_manual(values = c("CON" = "#457B9D",
                                  "RES" = "#C6C3BB",
                                  "SUS" = "#E63946")) +
    scale_fill_manual(values = c("CON" = "#457B9D",
                                 "RES" = "#C6C3BB",
                                 "SUS" = "#E63946")) +
    scale_x_continuous(breaks = scales::breaks_pretty(),
                       limits = c(0, max_rel)) +
    labs(title = "Activity over CC-relative time (mean ± SD)",
         x = "Time since CC start [h]",
         y = "Activity [a.u.]") +
    facet_grid(Sex ~ Change, scales = "free_y") +
    theme_pub_small
  ggsave(file_panelB, ribbons_plot, width = 10.5, height = 4.6)
}

df_plot_C <- data_filtered_agg %>%
  dplyr::filter(Group %in% c("CON", "SUS"),
               (!includeChange | Change %in% unique(Change)),
               (!includeSex | Sex %in% unique(Sex)),
               (!includePhase | Phase %in% unique(Phase)))
phase_starts <- df_plot_C %>%
  dplyr::group_by(Change, Sex, Phase) %>%
  dplyr::summarise(t0_phase = min(HalfHourElapsed, na.rm = TRUE),
                   groups = "drop")
diff_df <- df_plot_C %>%
  dplyr::left_join(phase_starts, by = c("Change", "Sex", "Phase")) %>%
  dplyr::mutate(rel_phase_h = (HalfHourElapsed - t0_phase) / 2) %>%
  dplyr::group_by(Change, Sex, Phase, Group, rel_phase_h) %>%
  dplyr::summarise(mean_act = mean(ActivityIndex, na.rm = TRUE),
  .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Group, values_from = mean_act) %>%
  dplyr::mutate(diff_SC = SUS - CON, Sex = droplevels(factor(Sex)),
                Change = droplevels(factor(Change)),
                Phase = factor(Phase,
                levels = c("Active", "Inactive"))) %>%
  dplyr::filter(!is.na(diff_SC))
file_panelC <- file.path(dir_pub, paste0("panelC_heat_phaseAligned_", run_scope, ".svg"))
if (nrow(diff_df) > 0) {
  thresh <- stats::quantile(abs(diff_df$diff_SC), probs = 0.95, na.rm = TRUE)
  diff_df <- diff_df %>%
    dplyr::mutate(sig_bin = abs(diff_SC) >= as.numeric(thresh))
  heat_plot <- ggplot(diff_df, aes(x = rel_phase_h,
                                   y = factor(Change,
                                              levels = sort(unique(as.character(Change)))),
                                   fill = diff_SC)) +
    geom_tile() +
    scale_fill_gradient2(low = "#2c7bb6",
                         mid = "white",
                         high = "#d7191c",
                         midpoint = 0) +
    geom_point(data = subset(diff_df, sig_bin),
               aes(x = rel_phase_h, 
                   y = factor(Change,
                              levels = sort(unique(as.character(Change))))),
               shape = 4, size = 0.8, color = "black", inherit.aes = FALSE) +
    labs(title = "SUS−CON difference (Phase-aligned)",
         x = "Time since phase start [h]",
         y = "Cage change",
         fill = "Δ Activity") +
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
# The original code expected ConsecActive/ConsecInactive; compute contiguous segment ids
# per AnimalNum / Change / Sex / Phase based on gaps in HalfHourElapsed.
edges_all_segments <- data_filtered_agg %>%
  dplyr::arrange(AnimalNum, Change, Sex, Phase, HalfHourElapsed) %>%
  dplyr::group_by(AnimalNum, Change, Sex, Phase) %>%
  dplyr::mutate(
    gap = HalfHourElapsed - dplyr::lag(HalfHourElapsed),
    new_seg = ifelse(is.na(gap) | gap > 1L, 1L, 0L),
    seg_id = cumsum(new_seg)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(seg_id), seg_id >= 1L) %>%
  dplyr::select(-gap, -new_seg)

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
# 8) EMMeans: START/END (hardcoded separation by Change,
# Phase, Sex)
# =========================
emm_options(
  pbkrtest.limit = 100000,
  lmerTest.limit = 100000,
  lmer.df = "kenward-roger"
)

# Canonical levels
lvl_change <- sort(unique(as.character(edge_animal_means_collapsed$Change)))
lvl_phase  <- c("Active", "Inactive")
lvl_group  <- c("CON", "RES", "SUS")
lvl_sex    <- sort(unique(as.character(edge_animal_means_collapsed$Sex)))
lvl_edge   <- c("start4", "end4")

# Inputs: factor copies for fitting (…_f), character copies for display
edge_dat <- edge_animal_means_collapsed %>%
  dplyr::transmute(
    ActivityEdge = as.numeric(ActivityEdge),
    Change_f = factor(as.character(Change), levels = lvl_change),
    Change   = as.character(Change),
    Sex_f    = factor(as.character(Sex), levels = lvl_sex),
    Sex      = as.character(Sex),
    Phase_f  = factor(as.character(Phase), levels = lvl_phase),
    Phase    = as.character(Phase),
    Group_f  = factor(as.character(Group), levels = lvl_group),
    Group    = as.character(Group),
    AnimalNum = as.factor(AnimalNum),
    edge_bin  = factor(as.character(edge_bin), levels = lvl_edge)
  )

edge_start <- edge_dat %>% dplyr::filter(edge_bin == "start4")
edge_end   <- edge_dat %>% dplyr::filter(edge_bin == "end4")

# Mixed-model df provider (Kenward–Roger for df/SE stability)
fit_emm_subset <- function(df) {
  if (dplyr::n_distinct(df$Group_f) < 2 ||
      dplyr::n_distinct(df$AnimalNum) < 2) {
    return(NULL)
  }
  ok_change <- dplyr::n_distinct(df$Change_f) >= 2
  ok_phase  <- dplyr::n_distinct(df$Phase_f)  >= 2
  form_try <- if (ok_change && ok_phase) {
    ActivityEdge ~ Group_f + Phase_f + Change_f +
      Group_f:Phase_f + Group_f:Change_f + (1 | AnimalNum)
  } else {
    ActivityEdge ~ Group_f + Phase_f + Change_f + (1 | AnimalNum)
  }
  m <- quiet_singular(
    lmerTest::lmer(
      form_try,
      data = df,
      control = lme4::lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    )
  )
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

# Converter:
# map model-term factor names back to display names; normalize columns
emm_to_df <- function(emm_obj, sex_label, edge_lab) {
#  dbg_hdr("emm_to_df: entry")
#  dbg_str(emm_obj, "emm_obj")

  smry <- try(summary(emm_obj, infer = TRUE), silent = TRUE)
  if (inherits(smry, "try-error")) {
    cat("summary(emm_obj, infer=TRUE) failed; using emm_obj directly\n")
    smry <- emm_obj
  }
  dbg_str(smry, "smry (post-summary)")

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
  dbg_cols(
    df_emm,
    c(
      "emmean", "SE", "df", "lower.CL", "upper.CL",
      "Phase_f", "Change_f", "Group_f", "Phase", "Change", "Group"
    )
  )

  if (!("emmean" %in% names(df_emm)) &&
      ("lsmean" %in% names(df_emm))) {
    df_emm$emmean <- df_emm$lsmean
  }
  if (!("lower.CL" %in% names(df_emm)) &&
      ("asymp.LCL" %in% names(df_emm))) {
    df_emm$lower.CL <- df_emm$asymp.LCL
  }
  if (!("upper.CL" %in% names(df_emm)) &&
      ("asymp.UCL" %in% names(df_emm))) {
    df_emm$upper.CL <- df_emm$asymp.UCL
  }

  if ("Phase_f" %in% names(df_emm) && !("Phase" %in% names(df_emm))) {
    df_emm$Phase <- as.character(df_emm$Phase_f)
  }
  if ("Change_f" %in% names(df_emm) && !("Change" %in% names(df_emm))) {
    df_emm$Change <- as.character(df_emm$Change_f)
  }
  if ("Group_f" %in% names(df_emm) && !("Group" %in% names(df_emm))) {
    df_emm$Group <- as.character(df_emm$Group_f)
  }

  if (!("Phase" %in% names(df_emm)))  df_emm$Phase  <- NA_character_
  if (!("Change" %in% names(df_emm))) df_emm$Change <- NA_character_
  if (!("Group" %in% names(df_emm)))  df_emm$Group  <- NA_character_

  df_emm$Sex <- sex_label

  if (!("emmean" %in% names(df_emm)))   df_emm$emmean   <- NA_real_
  if (!("SE" %in% names(df_emm)))       df_emm$SE       <- NA_real_
  if (!("df" %in% names(df_emm)))       df_emm$df       <- NA_real_
  if (!("lower.CL" %in% names(df_emm))) df_emm$lower.CL <- NA_real_
  if (!("upper.CL" %in% names(df_emm))) df_emm$upper.CL <- NA_real_

  em_vec  <- suppressWarnings(as.numeric(df_emm[["emmean"]]))
  se_vec  <- suppressWarnings(as.numeric(df_emm[["SE"]]))
  df_vec  <- suppressWarnings(as.numeric(df_emm[["df"]]))
  lwr_vec <- suppressWarnings(as.numeric(df_emm[["lower.CL"]]))
  upr_vec <- suppressWarnings(as.numeric(df_emm[["upper.CL"]]))

  out <- tibble::tibble(
    Change   = factor(as.character(df_emm[["Change"]]), levels = lvl_change),
    Phase    = factor(as.character(df_emm[["Phase"]]),  levels = lvl_phase),
    Group    = factor(as.character(df_emm[["Group"]]),  levels = lvl_group),
    Sex      = factor(as.character(df_emm[["Sex"]]),    levels = lvl_sex),
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
  cat(
    "fit_emm_subset(ds) class:",
    if (is.null(es)) "NULL" else paste(class(es), collapse = ","),
    "\n"
  )
  ee <- fit_emm_subset(de)
  cat(
    "fit_emm_subset(de) class:",
    if (is.null(ee)) "NULL" else paste(class(ee), collapse = ","),
    "\n"
  )

  if (!is.null(es)) {
    cat("Calling emm_to_df on START\n")
    emm_list_start[[sx]] <- emm_to_df(es, sx, "start4")
  }
  if (!is.null(ee)) {
    cat("Calling emm_to_df on END\n")
    emm_list_end[[sx]] <- emm_to_df(ee, sx, "end4")
  }
}

df_emm_start <- if (length(emm_list_start)) {
  dplyr::bind_rows(emm_list_start)
} else {
  tibble::tibble(
    Change   = factor(character(), levels = lvl_change),
    Phase    = factor(character(), levels = lvl_phase),
    Group    = factor(character(), levels = lvl_group),
    Sex      = factor(character(), levels = lvl_sex),
    emmean   = double(),
    SE       = double(),
    df       = double(),
    lwr      = double(),
    upr      = double(),
    edge_bin = factor(character(), levels = lvl_edge)
  )
}
df_emm_end <- if (length(emm_list_end)) {
  dplyr::bind_rows(emm_list_end)
} else {
  tibble::tibble(
    Change   = factor(character(), levels = lvl_change),
    Phase    = factor(character(), levels = lvl_phase),
    Group    = factor(character(), levels = lvl_group),
    Sex      = factor(character(), levels = lvl_sex),
    emmean   = double(),
    SE       = double(),
    df       = double(),
    lwr      = double(),
    upr      = double(),
    edge_bin = factor(character(), levels = lvl_edge)
  )
}

# Normalize factor/numeric types consistently
normalize_emm_df <- function(x) {
  dplyr::mutate(
    x,
    Change   = factor(as.character(.data$Change), levels = lvl_change),
    Phase    = factor(as.character(.data$Phase),  levels = lvl_phase),
    Group    = factor(as.character(.data$Group),  levels = lvl_group),
    Sex      = factor(as.character(.data$Sex),    levels = lvl_sex),
    edge_bin = factor(as.character(.data$edge_bin), levels = lvl_edge),
    emmean   = as.numeric(.data$emmean),
    SE       = as.numeric(.data$SE),
    df       = as.numeric(.data$df),
    lwr      = as.numeric(.data$lwr),
    upr      = as.numeric(.data$upr)
  )
}

# Re-assert normalization before every plot
df_emm_start <- normalize_emm_df(df_emm_start)
df_emm_end   <- normalize_emm_df(df_emm_end)
df_emm_all   <- normalize_emm_df(dplyr::bind_rows(df_emm_start, df_emm_end))

# Save table
openxlsx::write.xlsx(
  df_emm_all,
  file = file.path(
    dirs$tables_emm,
    "edge_EMMeans_linearPred_subsetChanges_subsetSexes_subsetPhases.xlsx"
  ),
  rowNames = FALSE
)

# Facet formula (avoid vars() quosures to prevent comparison errors)
facet_formula_all <- Phase + Sex ~ Change

# START plot (modernized, keep colors)
df_emm_start <- normalize_emm_df(df_emm_start)
df_emm_start$Group <- factor(df_emm_start$Group, levels = c("CON","RES","SUS"))
pal_grp <- c("CON"="#457B9D","RES"="#C6C3BB","SUS"="#E63946")

plot_start_all <- ggplot(df_emm_start, aes(x = .data$Group, y = .data$emmean)) +
    geom_pointrange(
        aes(ymin = .data$lwr, ymax = .data$upr, color = .data$Group),
        linewidth = 0.9, fatten = 1.8
    ) +
    geom_point(
        aes(fill = .data$Group),
        shape = 21, color = "white", size = 3.4, stroke = 1.0
    ) +
    scale_color_manual(values = pal_grp, guide = "none") +
    scale_fill_manual(values = pal_grp, name = "Group") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    facet_grid(facet_formula_all, scales = "free_y") +
    labs(
        title = "Edge START (first 2h) — EMMeans linear predictions",
        x = "Group", y = "Linear Prediction (EMM)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

ggsave(
    file.path(
        edgeAggAll_dir,
        "EMM_edge_START_all_subsetChanges_subsetSexes_subsetPhases.svg"
    ),
    plot_start_all,
    width = 8,
    height = 5.2
)

# END plot (modernized, keep colors)
df_emm_end <- normalize_emm_df(df_emm_end)
df_emm_end$Group <- factor(df_emm_end$Group, levels = c("CON","RES","SUS"))

plot_end_all <- ggplot(df_emm_end, aes(x = .data$Group, y = .data$emmean)) +
    geom_pointrange(
        aes(ymin = .data$lwr, ymax = .data$upr, color = .data$Group),
        linewidth = 0.9, fatten = 1.8
    ) +
    geom_point(
        aes(fill = .data$Group),
        shape = 21, color = "white", size = 3.4, stroke = 1.0
    ) +
    scale_color_manual(values = pal_grp, guide = "none") +
    scale_fill_manual(values = pal_grp, name = "Group") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    facet_grid(facet_formula_all, scales = "free_y") +
    labs(
        title = "Edge END (last 2h) — EMMeans linear predictions",
        x = "Group", y = "Linear Prediction (EMM)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

ggsave(
    file.path(
        edgeAggAll_dir,
        "EMM_edge_END_all_subsetChanges_subsetSexes_subsetPhases.svg"
    ),
    plot_end_all,
    width = 8,
    height = 5.2
)

# Trajectory helper ensures normalization and discrete x
make_traj <- function(df_emm, title) {
  df_emm <- normalize_emm_df(df_emm)
  df_emm$Change <- factor(as.character(df_emm$Change), levels = lvl_change)
  ggplot(
    df_emm,
    aes(x = .data$Change, y = .data$emmean, color = .data$Group, group = .data$Group)
  ) +
    geom_errorbar(
      aes(ymin = .data$lwr, ymax = .data$upr),
      width = 0.15,
      alpha = 0.9,
      position = position_dodge2(width = 0.25, padding = 0.15)
    ) +
    geom_point(
      size = 2.6,
      position = position_dodge2(width = 0.25, padding = 0.15)
    ) +
    geom_line(
      linewidth = 0.8,
      position = position_dodge2(width = 0.25, padding = 0.15)
    ) +
    scale_color_manual(
      values = c(
        "CON" = "#457B9D",
        "RES" = "#C6C3BB",
        "SUS" = "#E63946"
      )
    ) +
    facet_grid(Phase ~ Sex, scales = "free_y") +
    labs(title = title, x = "Cage Change", y = "Linear Prediction (EMM)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
}

plot_start_traj <- make_traj(
  df_emm_start,
  "START (first 2h) across Cage Changes — EMMeans"
)
ggsave(
  file.path(
    edgeAggAll_dir,
    "EMM_edge_START_trajectory_subsetChanges_subsetSexes_subsetPhases.svg"
  ),
  plot_start_traj,
  width = 8,
  height = 5.5
)

plot_end_traj <- make_traj(
  df_emm_end,
  "END (last 2h) across Cage Changes — EMMeans"
)
ggsave(
  file.path(
    edgeAggAll_dir,
    "EMM_edge_END_trajectory_subsetChanges_subsetSexes_subsetPhases.svg"
  ),
  plot_end_traj,
  width = 8,
  height = 5.5
)

# =========================
# Edge statistics export
# =========================

# Helper to compute EMMs and contrasts per sex and edge_bin
compute_edge_stats <- function(df_edge, edge_lab) {
  out_means <- list()
  out_contr <- list()

  for (sx in levels(df_edge$Sex_f)) {
    d_sx <- df_edge %>% dplyr::filter(Sex == !!sx)
    if (nrow(d_sx) < 1) next

    emm_obj <- fit_emm_subset(d_sx)
    if (is.null(emm_obj)) next

    # EMM table (Group_f by Phase_f + Change_f)
    emm_tbl <- try(
      summary(emm_obj, infer = TRUE),
      silent = TRUE
    )
    if (!inherits(emm_tbl, "try-error")) {
      emm_df <- as.data.frame(emm_tbl)
      # Harmonize names and add labels
      names(emm_df) <- gsub("^lsmean$", "emmean", names(emm_df))
      names(emm_df) <- sub("^asymp\\.LCL$", "lower.CL", names(emm_df))
      names(emm_df) <- sub("^asymp\\.UCL$", "upper.CL", names(emm_df))

      emm_df$Sex      <- sx
      emm_df$edge_bin <- edge_lab
      # Map model-term factors to string columns if present
      if ("Group_f" %in% names(emm_df)) emm_df$Group <- as.character(emm_df$Group_f)
      if ("Phase_f" %in% names(emm_df)) emm_df$Phase <- as.character(emm_df$Phase_f)
      if ("Change_f" %in% names(emm_df)) emm_df$Change <- as.character(emm_df$Change_f)

      out_means[[paste(edge_lab, sx, sep="_")]] <- emm_df %>%
        dplyr::select(Change, Phase, Sex, Group, emmean, SE, df, lower.CL, upper.CL, edge_bin)
    }

    # Pairwise contrasts among Group within each Phase × Change
    contr_pairs <- try(
      summary(pairs(emm_obj, by = c("Phase_f","Change_f")), infer = TRUE, adjust = "holm"),
      silent = TRUE
    )
    if (!inherits(contr_pairs, "try-error")) {
      contr_df <- as.data.frame(contr_pairs)
      # Expand factor aliases
      if ("Phase_f" %in% names(contr_df)) contr_df$Phase  <- as.character(contr_df$Phase_f)
      if ("Change_f" %in% names(contr_df)) contr_df$Change <- as.character(contr_df$Change_f)

      contr_df$Sex      <- sx
      contr_df$edge_bin <- edge_lab
      # Add BH alongside Holm
      if (!("p.value" %in% names(contr_df))) {
        # When using confint() only, p may be missing; derive from t and df if present
        if (all(c("t.ratio","df") %in% names(contr_df))) {
          contr_df$p.value <- 2 * pt(-abs(contr_df$t.ratio), df = contr_df$df)
        } else {
          contr_df$p.value <- NA_real_
        }
      }
      contr_df$p.holm <- p.adjust(contr_df$p.value, "holm")
      contr_df$p.BH   <- p.adjust(contr_df$p.value, "BH")

      out_contr[[paste(edge_lab, sx, sep="_")]] <- contr_df %>%
        dplyr::select(Change, Phase, Sex, edge_bin, contrast, estimate, SE, df, t.ratio, p.value, p.holm, p.BH, lower.CL, upper.CL)
    }
  }

  list(
    means = dplyr::bind_rows(out_means),
    contr = dplyr::bind_rows(out_contr)
  )
}

# Compute for START and END using existing edge_start/edge_end data
stats_start <- compute_edge_stats(edge_start, "start4")
stats_end   <- compute_edge_stats(edge_end,   "end4")

edge_emm_means    <- dplyr::bind_rows(stats_start$means, stats_end$means) %>%
  dplyr::mutate(
    Change = factor(Change, levels = lvl_change),
    Phase  = factor(Phase,  levels = lvl_phase),
    Sex    = factor(Sex,    levels = lvl_sex),
    Group  = factor(Group,  levels = lvl_group),
    edge_bin = factor(edge_bin, levels = lvl_edge)
  )

edge_emm_contrasts <- dplyr::bind_rows(stats_start$contr, stats_end$contr) %>%
  dplyr::mutate(
    Change = factor(Change, levels = lvl_change),
    Phase  = factor(Phase,  levels = lvl_phase),
    Sex    = factor(Sex,    levels = lvl_sex),
    edge_bin = factor(edge_bin, levels = lvl_edge)
  )

# Inventory-style long table for quick filtering
edge_stats_inventory <- edge_emm_contrasts %>%
  dplyr::mutate(sig_holm = p.holm < 0.05, sig_BH = p.BH < 0.05)

# Create output filenames
edge_means_xlsx <- file.path(dirs$tables_emm, paste0("edge_emm_means_", run_scope, ".xlsx"))
edge_contr_xlsx <- file.path(dirs$tables_emm, paste0("edge_emm_contrasts_", run_scope, ".xlsx"))
edge_contr_csv  <- file.path(dirs$tables_emm, paste0("edge_stats_inventory_", run_scope, ".csv"))

# Save to disk
openxlsx::write.xlsx(edge_emm_means, edge_means_xlsx, rowNames = FALSE)
openxlsx::write.xlsx(edge_emm_contrasts, edge_contr_xlsx, rowNames = FALSE)
readr::write_csv(edge_stats_inventory, edge_contr_csv)

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

# -------------------------------------------------
# Sex Difference Computations (Phase Average Contrasts & Within-Group)
# -------------------------------------------------

# (1) Sex Differences in Phase-Averaged Contrasts (e.g. [RES-CON in Females] - [RES-CON in Males])
if (exists("phase_avg_contr_tbl") && nrow(phase_avg_contr_tbl) > 0) {
  sexes_avail <- unique(as.character(phase_avg_contr_tbl$Sex))
  
  if (length(sexes_avail) == 2) {
    s1 <- sort(sexes_avail)[1]
    s2 <- sort(sexes_avail)[2]
    
    # Pivot wider by Sex to get pairs
    # Ensure contrast format is consistent before pivoting using 'pair' if available
    # phase_avg_contr_tbl likely has columns: contrast, estimate, SE, df, Change, Sex, Phase, pair...
    
    # We use 'pair' column for matching if available, otherwise 'contrast'
    join_cols <- c("Change", "Phase", "contrast")
    if ("pair" %in% names(phase_avg_contr_tbl)) join_cols <- c("Change", "Phase", "pair")
    
    df_sex_diff <- phase_avg_contr_tbl %>%
      dplyr::select(any_of(c("Change", "Phase", "pair", "contrast", "estimate", "SE", "df", "Sex"))) %>%
      tidyr::pivot_wider(
        names_from = Sex, 
        values_from = c(estimate, SE, df),
        names_sep = "__"
      )
    
    est1 <- paste0("estimate__", s1); est2 <- paste0("estimate__", s2)
    se1  <- paste0("SE__", s1);       se2  <- paste0("SE__", s2)
    df1  <- paste0("df__", s1);       df2  <- paste0("df__", s2)
    
    df_sex_diff <- df_sex_diff %>%
      dplyr::mutate(
        estimate_diff = .data[[est2]] - .data[[est1]], # s2 - s1
        SE_diff = sqrt(.data[[se1]]^2 + .data[[se2]]^2),
        # Welch-Satterthwaite approx
        df_diff = (.data[[se1]]^2 + .data[[se2]]^2)^2 / 
                  ((.data[[se1]]^4 / .data[[df1]]) + (.data[[se2]]^4 / .data[[df2]])),
        # If df is NA (fixed dummy), fallback to large number
        df_diff = ifelse(is.na(df_diff), 200, df_diff),
        t_ratio = estimate_diff / SE_diff,
        p_value = 2 * pt(-abs(t_ratio), df = df_diff),
        sex_comp = paste0(s2, " - ", s1)
      ) %>%
      dplyr::mutate(
        p_holm = p.adjust(p_value, method = "holm"),
        lower_CL = estimate_diff - qt(0.975, df_diff) * SE_diff,
        upper_CL = estimate_diff + qt(0.975, df_diff) * SE_diff,
        annot = ifelse(p_holm < 0.001, "***", ifelse(p_holm < 0.01, "**", ifelse(p_holm < 0.05, "*", ""))),
        # Explicit direction for easy reading
        Direction = dplyr::case_when(
          estimate_diff > 0 ~ paste(s2, ">", s1),
          estimate_diff < 0 ~ paste(s1, ">", s2),
          TRUE ~ "Equal"
        ),
        # Convert Change to factor for ordered plotting
        Change = factor(Change, levels = sort(unique(as.character(Change))))
      )
    
    # Save table
    openxlsx::write.xlsx(df_sex_diff, file.path(dirs$tables_contr, paste0("phaseAvg_contrasts_sexDiff_", run_scope, ".xlsx")))
    readr::write_csv(df_sex_diff, file.path(dirs$tables_contr, paste0("phaseAvg_contrasts_sexDiff_", run_scope, ".csv")))
    
    # Optional plot: Forest plot of sex differences (Vertical)
    if (nrow(df_sex_diff) > 0) {
      if (!"pair" %in% names(df_sex_diff)) df_sex_diff$pair <- df_sex_diff$contrast

      p_sex_diff <- ggplot(df_sex_diff, aes(x = estimate_diff, y = Change, color = pair)) +
        geom_pointrange(aes(xmin = lower_CL, xmax = upper_CL), position = position_dodge(width = 0.6)) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_text(aes(label = annot, group = pair), 
                  position = position_dodge(width = 0.6), 
                  vjust = -0.8, size = 3.5, show.legend = FALSE) +
        facet_grid(Phase ~ ., scales = "free_x") +
        labs(title = paste0("Sex Difference in Contrasts (", s2, " - ", s1, ")"),
             subtitle = paste("Positive values:", s2, "higher effect | Negative values:", s1, "higher effect"),
             x = "Difference Estimate", y = "Cage Change") +
        scale_color_manual(values = c("RES-CON" = "grey60", "SUS-CON" = "#E63946", "RES-SUS" = "#457B9D")) +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank(), legend.position = "top")

      ggsave(file.path(dirs$plots_pub, paste0("forest_phaseAvg_sexDiff_", run_scope, ".svg")), p_sex_diff, width = 8, height = 6)
    }
  }
}

# (2) Within-Group Sex Contrasts (e.g. CON Male vs CON Female)
# This requires querying the phase_avg_means_tbl which contains Group means per Sex/Phase/Change
if (exists("phase_avg_means_tbl") && nrow(phase_avg_means_tbl) > 0) {
  sexes_avail <- unique(as.character(phase_avg_means_tbl$Sex))
  
  if (length(sexes_avail) == 2) {
    s1 <- sort(sexes_avail)[1]
    s2 <- sort(sexes_avail)[2]
    
    # Pivot to align Sexes per Group
    df_group_sex <- phase_avg_means_tbl %>%
      dplyr::select(Change, Phase, Group, Sex, emmean_avg, SE_avg) %>% # Assuming df is generic/large if not carried, roughly approx
      tidyr::pivot_wider(
        names_from = Sex,
        values_from = c(emmean_avg, SE_avg),
        names_sep = "__"
      )
    
    m1 <- paste0("emmean_avg__", s1); m2 <- paste0("emmean_avg__", s2)
    se1 <- paste0("SE_avg__", s1);    se2 <- paste0("SE_avg__", s2)
    
    # For DF, we might need to look up original models, but here we use a standard approximation (e.g. 200 or infinite)
    # as phase_avg_means_tbl table often loses specific df per point. 
    # Or rely on the fact that SEs are populated.
    
    df_group_sex <- df_group_sex %>%
      dplyr::mutate(
        estimate_diff = .data[[m2]] - .data[[m1]],
        SE_diff = sqrt(.data[[se1]]^2 + .data[[se2]]^2),
        df_approx = 200, # Fallback
        t_ratio = estimate_diff / SE_diff,
        p_value = 2 * pt(-abs(t_ratio), df = df_approx),
        p_holm = p.adjust(p_value, method = "holm"),
        lower_CL = estimate_diff - qt(0.975, df_approx) * SE_diff,
        upper_CL = estimate_diff + qt(0.975, df_approx) * SE_diff,
        contrast = paste0(s2, " - ", s1),
        annot = ifelse(p_holm < 0.001, "***", ifelse(p_holm < 0.01, "**", ifelse(p_holm < 0.05, "*", ""))),
        # Explicit direction for easy reading
        Direction = dplyr::case_when(
          estimate_diff > 0 ~ paste(s2, ">", s1),
          estimate_diff < 0 ~ paste(s1, ">", s2),
          TRUE ~ "Equal"
        ),
        Change = factor(Change, levels = sort(unique(as.character(Change)))),
        # Reverse level order to c("SUS", "RES", "CON") so that 'CON' appears at the top 
        # (highest Y position) within the dodge group on the plot.
        Group = factor(Group, levels = c("SUS", "RES", "CON"))
      )
    
    # Save
    openxlsx::write.xlsx(df_group_sex, file.path(dirs$tables_contr, paste0("phaseAvg_withinGroup_sexDiff_", run_scope, ".xlsx")))
    readr::write_csv(df_group_sex, file.path(dirs$tables_contr, paste0("phaseAvg_withinGroup_sexDiff_", run_scope, ".csv")))
    
    # Plot: Forest plot of Sex Diff within Group (Horizontal orientation requested)
    if(nrow(df_group_sex) > 0) {
      
      # Define palette for groups
      grp_pal <- c("CON" = "#457B9D", "RES" = "#C6C3BB", "SUS" = "#E63946")

      p_within_grp <- ggplot(df_group_sex, aes(x = estimate_diff, y = Change, color = Group)) +
      geom_pointrange(aes(xmin = lower_CL, xmax = upper_CL), 
              position = position_dodge(width = 0.6), 
              size = 1, linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_text(aes(label = annot, group = Group), 
            position = position_dodge(width = 0.6), 
            vjust = -0.8, size = 5, show.legend = FALSE) + 
      scale_color_manual(values = grp_pal, breaks = c("CON", "RES", "SUS")) +
      facet_grid(Phase ~ ., scales = "free_x") +
      labs(title = paste0("Sex Difference Within Group (", s2, " - ", s1, ")"),
         subtitle = paste("Phases Averaged | Positive:", s2, "higher | Negative:", s1, "higher"),
         x = "Difference Estimate", y = "Cage Change") +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
      ggsave(file.path(dirs$plots_pub, paste0("forest_phaseAvg_withinGroup_sexDiff_", run_scope, ".svg")), p_within_grp, width = 5, height = 5)
    }
  }
}