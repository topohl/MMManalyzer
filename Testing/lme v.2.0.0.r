## Mixed models analysis to analyze both the active and inactive phases
# Compares susceptible and resilient groups to controls
# Creates results text files in the defined folder

# List of packages to check and load
packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "lme4", "lmerTest", "cowplot", "lsmeans", "emmeans", "Matrix", "tcltk")

# Check if each package is installed, install if not, and load it
for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

# Define the folder to store the result files
results_dir <- "C:/Users/topohl/Documents/test/"

# Function to create a modern popup window for user input
get_user_input <- function() {
    # Create a top-level window
    tt <- tktoplevel()
    tkwm.title(tt, "Analysis Settings")
    
    # Add some padding to the window
    tkconfigure(tt, padx = 60, pady = 10)

    # Variables to store user input
    includeChange_var <- tclVar(1)  # Default to TRUE
    includeSex_var <- tclVar(1)     # Default to TRUE
    includePhase_var <- tclVar(1)   # Default to TRUE

    # Create a frame for better organization
    frame <- tkframe(tt)
    tkpack(frame)

    # Create checkboxes for each setting with modern styling
    tkgrid(tklabel(frame, text = "Include Change in analysis?"), padx = 5, pady = 5)
    tkgrid(tkcheckbutton(frame, variable = includeChange_var), padx = 5, pady = 5)

    tkgrid(tklabel(frame, text = "Include Sex in analysis?"), padx = 5, pady = 5)
    tkgrid(tkcheckbutton(frame, variable = includeSex_var), padx = 5, pady = 5)

    tkgrid(tklabel(frame, text = "Include Phase in analysis?"), padx = 5, pady = 5)
    tkgrid(tkcheckbutton(frame, variable = includePhase_var), padx = 5, pady = 5)

    # Function to handle OK button click
    onOK <- function() {
        tkdestroy(tt)
    }

    # Create OK button with improved styling
    ok_button <- tkbutton(tt, text = "OK", command = onOK)
    tkgrid(ok_button, padx = 10, pady = 10)

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
        
        # Define a modern color palette
        color_palette <- c("#1e3791", "#8aacdb", "#f49620")

        # Create the plot
        p <- ggplot(data_filtered_agg_Change_subset_sex, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
            geom_path(stat = "summary", fun = mean, linewidth = 0.8) +  # Adjust line thickness for visibility
            stat_summary(aes(fill = Group), fun = mean,
                         fun.min = function(x) mean(x) - sd(x), 
                         fun.max = function(x) mean(x) + sd(x), 
                         geom = "ribbon", 
                         alpha = 0.3, colour = NA) +  # Shaded area representing ±1 SD
            
            # Use the defined color palette
            scale_color_manual(name = "Group", values = color_palette) + 
            scale_fill_manual(name = "Group", values = color_palette) +
            
            # Adjust the x-axis
            scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24), 
                               labels = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24) / 2,
                               expand = c(0, 0)) +  # Remove padding on the x-axis
            scale_y_continuous(expand = c(0, 0)) +  # Remove padding on the y-axis
            
            # Refined labels and captions
            labs(title = "Circadian Activity Index",  # More concise title
                 subtitle = "Activity Levels Across Groups", 
                 y = "Activity [a.u.]", x = "Time Elapsed [h]", 
                 caption = "Shaded regions represent ±1 SD of the mean") +
            
            # Modern theme adjustments
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

# Prepare a data frame to store summary results
summary_results <- data.frame(
    Change = character(),
    Sex = character(),
    Phase = character(),
    p.value = numeric(),
    test.name = character(),
    comparison = character(),
    stringsAsFactors = FALSE
)

# Loop through each combination based on inclusion settings
for (Change in changes) {
    for (Sex in sexes) {
        for (Phase in phases) {
            # Create a subset based on currently included factors
            data_subset <- data_filtered_agg %>%
                filter((!includeChange | Change == !!Change),
                       (!includeSex | Sex == !!Sex),
                       (!includePhase | Phase == !!Phase))
            
            # Skip empty subsets
            if (nrow(data_subset) == 0) next
            
            # Set options for pbkrtest
            emm_options(pbkrtest.limit = 100000)   

            # Fit the model
            model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_subset)

            # Generate unique key
            key <- paste(ifelse(includeChange, Change, "allChanges"),
                         ifelse(includeSex, Sex, "allSexes"),
                         ifelse(includePhase, Phase, "allPhases"), sep = "_")
            
            # Store and save results
            results_list[[key]] <- summary(model)
            write.csv(summary(model)$coefficients, file = paste0(results_dir, "results_", key, ".csv"))
            
            # Calculate estimated marginal means
            emmeans_results <- emmeans(model, ~ Group | HalfHourElapsed)
            
            # Perform pairwise comparisons
            comparisons <- pairs(emmeans_results)
            
            # Extract results and store in summary_results
            comparison_summary <- summary(comparisons)
            comparison_summary$Change <- Change
            comparison_summary$Sex <- Sex
            comparison_summary$Phase <- Phase
            
            # Append to the summary_results data frame
            summary_results <- rbind(summary_results, comparison_summary[, c("Change", "Sex", "Phase", "p.value", "contrast", "adjust", "name")])
        }
    }
}

# Save the summary results to a CSV file
write.csv(summary_results, file = paste0(results_dir, "emmeans_summary_results.csv"), row.names = FALSE)

# Save the plot and heatmap lists to CSV files if needed
# Done!
