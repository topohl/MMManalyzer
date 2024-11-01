## Mixed models analysis to analyze both the active and inactive phases
# Compares susceptible and resilient groups to controls
# Creates results text files in the defined folder

# List of packages to check and load
packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "lme4", "lmerTest", "cowplot", "lsmeans", "emmeans", "Matrix", "tcltk", "openxlsx")

# Check if each package is installed, install if not, and load it
for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

# Define the folder to store the result files
results_dir <- "C:/Users/topohl/Documents/test/"

# Function to create popup window for user input
get_user_input <- function() {
    # Create a top-level window
    tt <- tktoplevel()
    tkwm.title(tt, "Analysis Settings")

    # Set the background color
    tkconfigure(tt, bg = "#F7F9FC")  # Light background color

    # Add some padding to the window
    tkconfigure(tt, padx = 10, pady = 20)

    # Initialize variables for user input
    includeChange_var <- tclVar(0)  # Default to FALSE
    includeSex_var <- tclVar(0)     # Default to FALSE
    includePhase_var <- tclVar(0)   # Default to FALSE

    # Create a frame for better organization
    frame <- tkframe(tt, bg = "#F7F9FC")  # Match frame background color
    tkgrid(frame, padx = 10, pady = 10)  # Use grid for frame layout

    tkgrid(tklabel(frame, text = "Set Inclusion", bg = "#F7F9FC", font = c("Helvetica", 14)), padx = 10, pady = 0)
    tkgrid(tklabel(frame, text = "Parameters", bg = "#F7F9FC", font = c("Helvetica", 14)), padx = 5, pady = 5)

    # Create checkboxes for each setting with modern styling
    tkgrid(tklabel(frame, text = "Change", bg = "#F7F9FC", font = c("Helvetica", 12)), tkcheckbutton(frame, variable = includeChange_var, bg = "#F7F9FC", activebackground = "#E1E6EA"), padx = 5, pady = 5)

    tkgrid(tklabel(frame, text = "Sex", bg = "#F7F9FC", font = c("Helvetica", 12)), tkcheckbutton(frame, variable = includeSex_var, bg = "#F7F9FC", activebackground = "#E1E6EA"), padx = 5, pady = 5)

    tkgrid(tklabel(frame, text = "Phase", bg = "#F7F9FC", font = c("Helvetica", 12)), tkcheckbutton(frame, variable = includePhase_var, bg = "#F7F9FC", activebackground = "#E1E6EA"), padx = 5, pady = 5)

    # Function to handle OK button click
    onOK <- function() {
        tkdestroy(tt)
    }

    # Create an OK button with custom styling
    ok_button <- tkbutton(tt, text = "OK", command = onOK, bg = "#007AFF", fg = "white", relief = "flat", font = c("Helvetica", 12))
    tkgrid(ok_button, padx = 10, pady = 10)

    # Bind hover effects to the OK button
    tkbind(ok_button, "<Enter>", function() tkconfigure(ok_button, bg = "#005A9E"))  # Darker blue on hover
    tkbind(ok_button, "<Leave>", function() tkconfigure(ok_button, bg = "#0075CE"))  # Reset to original

    # Wait for the window to close
    tkwait.window(tt)

    # Return the user input as logical values
    list(
        includeChange = as.logical(as.numeric(tclvalue(includeChange_var))),
        includeSex = as.logical(as.numeric(tclvalue(includeSex_var))),
        includePhase = as.logical(as.numeric(tclvalue(includePhase_var)))
    )
}

# Get user input
user_input <- get_user_input()
includeChange <- user_input$includeChange
includeSex <- user_input$includeSex
includePhase <- user_input$includePhase

# Define the list to store plots
plot_list <- list()
heatmap_list <- list()

# Split data into separate data frames for each cage change (CC)
data_filtered_agg_Change <- data_filtered_agg %>% 
    split(data_filtered_agg$Change)

# Create plots for each CC and Sex
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
    # Subset data for the current CC
    data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
    
    # Create an empty list to store plots for each Sex
    plot_list_sex <- list()
    
    # Loop through each Sex
    for (SexValue in c("m", "f")) {
        # Filter data by Sex if includeSex is TRUE
        if (includeSex) {
            data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset %>% 
                filter(Sex == SexValue)
        } else {
            data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset
        }
        
        # Define a color palette for the groups
        color_palette <- c("#1e3791", "#8aacdb", "#f49620")

        # Create the line plot
        p <- ggplot(data_filtered_agg_Change_subset_sex, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
            geom_path(stat = "summary", fun = mean, linewidth = 0.8) +  # Adjust line thickness for visibility
            stat_summary(aes(fill = Group), fun = mean,
                         fun.min = function(x) mean(x) - sd(x), 
                         fun.max = function(x) mean(x) + sd(x), 
                         geom = "ribbon", 
                         alpha = 0.3, colour = NA) +  # Shaded area representing ±1 SD
            
            # Adjust color palette for groups
            scale_color_manual(name = "Group", values = color_palette) + 
            scale_fill_manual(name = "Group", values = color_palette) +
            
            # Adjust axis labels and breaks
            scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24), 
                               labels = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24) / 2,
                               expand = c(0, 0)) +  # Remove padding on the x-axis
            scale_y_continuous(expand = c(0, 0)) +  # Remove padding on the y-axis
            
            # Add labels and titles
            labs(title = "Circadian Activity Index",  # More concise title
                 subtitle = "Activity Levels Across Groups", 
                 y = "Activity [a.u.]", x = "Time Elapsed [h]", 
                 caption = "Shaded regions represent ±1 SD of the mean") +
            
            # Theme adjustments for clarity and aesthetics
            theme_minimal(base_size = 14) +  # Increase base font size for readability
            theme(
                panel.grid.major = element_blank(),  # Remove major gridlines
                panel.grid.minor = element_blank(),  # Remove minor gridlines
                plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center title
                plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),  # Italic for subtitle
                axis.text.x = element_text(size = 12),  # Adjust text sizes for clarity
                axis.text.y = element_text(size = 12),
                axis.title.y = element_text(size = 14, face = "bold"),
                axis.ticks.x = element_line(size = 0.5),
                legend.position = "top",  # Place legend inside the plot area
                panel.background = element_blank()  # Clean panel background
            )

        # Save each plot as an SVG file with the Sex in the filename
        save_file <- paste0(results_dir, "line_plot_", Change, "_", SexValue, ".svg")
        ggsave(filename = save_file, plot = p, device = "svg", width = 7, height = 4)
        
        # Add the plot to the list
        plot_list_sex[[SexValue]] <- p
    }
    
    # Return the list of plots for each Sex
    return(plot_list_sex)
})

# Create the heatmap for each CC with sexes not overlapping
heatmap_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
    # Subset data for the current CC
    data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
    
    # Create an empty list to store heatmaps for each Sex
    heatmap_list_sex <- list()
    
    # Loop through each Sex
    for (SexValue in c("m", "f")) {
        # Filter data by Sex if includeSex is TRUE
        if (includeSex) {
            data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset %>% 
                filter(Sex == SexValue)
        } else {
            data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset
        }
        
        # Reorder levels of Group factor
        data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset_sex %>% 
            mutate(Group = factor(Group, levels = c("CON", "RES", "SUS")))
        
        # Create the heatmap
        p <- ggplot(data_filtered_agg_Change_subset_sex, aes(x = HalfHourElapsed, y = reorder(Group, -as.numeric(Group)), fill = ActivityIndex)) +
            geom_tile() +
            scale_fill_gradient(low = "white", high = "darkblue", limits = c(0, max(data_filtered_agg$ActivityIndex))) +
            scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24), 
                               labels = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24)/2) +
            facet_grid(Sex ~ ., scales = "free") +  
            theme_minimal_hgrid(12, rel_small = 1) +
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
        
        # Save each heatmap as an SVG file with the Sex in the filename
        save_file <- paste0(results_dir, "heatmap_", Change, "_", SexValue, ".svg")
        ggsave(filename = save_file, plot = p, device = "svg", width = 5, height = 5)
        
        # Add the heatmap to the list
        heatmap_list_sex[[SexValue]] <- p
    }
    
    # Add the list of heatmaps for each Sex to the main heatmap list
    heatmap_list[[Change]] <- heatmap_list_sex
    
    # Return the main heatmap list
    return(heatmap_list_sex)
})

# Prepare an empty list to store results
results_list <- list()

# Define values for each factor or "All" if excluded
changes <- if (includeChange) unique(data_filtered_agg$Change) else "allChanges"
sexes <- if (includeSex) unique(data_filtered_agg$Sex) else "allSexes"
phases <- if (includePhase) unique(data_filtered_agg$Phase) else "allPhases"

# Prepare data frames to store summary results
summary_results <- data.frame(
    Change = character(),
    Sex = character(),
    Phase = character(),
    Fixed_effect = character(),
    Estimate = numeric(),
    Std.Error = numeric(),
    df = numeric(),
    t.value = numeric(),
    p.value = numeric(),
    p.round = numeric(),
    p.sign = character(),
    stringsAsFactors = FALSE
)

# Prepare data frame to store emmeans results
emmeans_results <- data.frame(
    Change = character(),
    Sex = character(),
    Phase = character(),
    contrast = character(),
    estimate = numeric(),
    SE = numeric(),
    df = numeric(),
    t.ratio = numeric(),
    p.value = numeric(),
    p.adjust = numeric(),
    p.round = numeric(),
    stringsAsFactors = FALSE
)

# Initialize the progress bar
total_iterations <- length(changes) * length(sexes) * length(phases)
progress_bar <- tkProgressBar(title = "Model Fitting Progress", min = 0, max = total_iterations, width = 300)

# Initialize a counter for progress tracking
iteration_counter <- 0

# Loop through each combination based on inclusion settings
for (Change in changes) {
    for (Sex in sexes) {
        for (Phase in phases) {
            # Increment the counter and update the progress bar
            iteration_counter <- iteration_counter + 1
            setTkProgressBar(progress_bar, iteration_counter, label = paste("Progress:", round(iteration_counter / total_iterations * 100, 2), "%"))
            
            # Create a subset based on currently included factors
            data_subset <- data_filtered_agg %>%
                filter((!includeChange | Change == !!Change),
                       (!includeSex | Sex == !!Sex),
                       (!includePhase | Phase == !!Phase))

            # Skip empty subsets
            if (nrow(data_subset) == 0) {
                cat("No data available for Change:", Change, "Sex:", Sex, "Phase:", Phase, "\n")
                next
            }

            # Set options for pbkrtest
            emm_options(pbkrtest.limit = 100000)

            # Fit the lmer model
            model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_subset)

            # Get the fixed effects from the lmer model
            model_summary <- summary(model)

            # Extract fixed effects coefficients
            fixed_effects <- data.frame(
                Fixed_effect = rownames(model_summary$coefficients),
                Estimate = model_summary$coefficients[, "Estimate"],
                Std.Error = model_summary$coefficients[, "Std. Error"],
                df = model_summary$coefficients[, "df"],
                t.value = model_summary$coefficients[, "t value"],
                p.value = model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))],
                p.round = round(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))], digits = 3),
                p.sign = ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.001, "***",
                         ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.01, "**",
                         ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.05, "*",
                         ifelse(model_summary$coefficients[, grep("Pr\\(>|t\\)", colnames(model_summary$coefficients))] < 0.1, "T", "ns")))),
                Change = Change,
                Phase = Phase,
                Sex = Sex,
                stringsAsFactors = FALSE
            )

            # Append each fixed effect to summary_results
            summary_results <- dplyr::bind_rows(summary_results, fixed_effects)

            # Conduct post-hoc tests and extract EMMs with specified adjustment
            posthoc <- emmeans::emmeans(model, pairwise ~ Group)
            posthoc_summary <- summary(posthoc, infer = TRUE, adjust = "holm")

            # Extract contrasts and append to emmeans_results data frame
            posthoc_df <- as.data.frame(posthoc_summary$contrasts)
            posthoc_df$Change <- Change
            posthoc_df$Phase <- Phase
            posthoc_df$Sex <- Sex

            # Filter for valid contrasts and p-values
            posthoc_df <- posthoc_df[!is.na(posthoc_df$p.value), ]

            # Adjust p-values using Holm method
            posthoc_df$p.adjust <- p.adjust(posthoc_df$p.value, method = "holm")

            # Append valid post-hoc results to emmeans_results
            if (nrow(posthoc_df) > 0) {
                emmeans_results <- dplyr::bind_rows(emmeans_results, posthoc_df)
            }
            
        }
    }
}

# Close the progress bar
close(progress_bar)

# Save the summary results and emmeans results to separate .xlsx files
write.xlsx(summary_results, file = paste0(results_dir, "lme_summary_results", 
                                         ifelse(includeChange, "_change", ""), 
                                         ifelse(includeSex, "_sex", ""), 
                                         ifelse(includePhase, "_phase", ""), 
                                         ".xlsx"), row.names = FALSE)
cat("Summary results saved to:", paste0(results_dir, "lme_summary_results", 
                                        ifelse(includeChange, "_change", ""), 
                                        ifelse(includeSex, "_sex", ""), 
                                        ifelse(includePhase, "_phase", ""), 
                                        ".xlsx"), "\n")

write.xlsx(emmeans_results, file = paste0(results_dir, "emmeans_results", 
                                         ifelse(includeChange, "_change", ""), 
                                         ifelse(includeSex, "_sex", ""), 
                                         ifelse(includePhase, "_phase", ""), 
                                         ".xlsx"), row.names = FALSE)
cat("EMMeans results saved to:", paste0(results_dir, "emmeans_results", 
                                        ifelse(includeChange, "_change", ""), 
                                        ifelse(includeSex, "_sex", ""), 
                                        ifelse(includePhase, "_phase", ""), 
                                        ".xlsx"), "\n")
