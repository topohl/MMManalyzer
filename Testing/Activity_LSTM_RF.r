## Mixed models analysis to analyse both the active and inactive phase
#  compares susceptible and resilient groups to controls
#  will create results text files in defined folder

# Define the required packages
packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "lme4", "lmerTest", "emmeans", "randomForest", "keras", "tensorflow")

# Check if each package is installed, install if not, and load it
for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

# Define the folder to store the result files
results_folder <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/"

# Split data into separate data frames for each CC
data_filtered_agg_Change <- data_filtered_agg %>% 
  split(data_filtered_agg$Change)

# Create plots for each CC
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Split "SIS" group into "RES" and "SUS" based on "SUS" column
  data_filtered_agg_Change_subset <- data_filtered_agg_Change_subset %>%
    mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                          ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
  
  # Create the plot
  ggplot(data_filtered_agg_Change_subset, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
    geom_path(stat = "summary") +
    stat_summary(aes(fill = Group), fun = mean,
                 fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "ribbon", 
                 alpha = 0.2, colour = NA) +
    scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
    scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
    scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset$HalfHourElapsed), by = 24), 
                       labels = seq(0, max(data_filtered_agg_Change_subset$HalfHourElapsed), by = 24)/2) +
    theme_minimal_hgrid(12, rel_small = 1) +
    labs(title = bquote(~bold(.(paste("Activity Index")))),
         y = "Activity [a.u.]", x = "Time elapsed [h]", caption = Change) +
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          legend.position = "top",
          legend.justification = "right",
          legend.text = element_text(size = 9),
          legend.box.spacing = unit(1, "pt"),
          axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5))
})

# Perform mixed-effects models
model_results <- list()

library(emmeans)

for (Change in sort(unique(data_filtered_agg$Change))) {
    for (PhaseValue in c("Active", "Inactive")) {
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
            filter(Phase == PhaseValue)
        
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change_Phase_subset %>%
            mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                                                        ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
        
        model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed (1 | AnimalNum), data = data_filtered_agg_Change_Phase_subset)
        model_results[[paste0(Change, "_", PhaseValue)]] <- model
        
        mixed_model_results_file <- paste0(results_folder, "NEW_mixed_model_results_", Change, "_", PhaseValue, ".txt")
        model_summary <- capture.output(summary(model))
        writeLines(model_summary, con = mixed_model_results_file)
        
        cat("Mixed-effects model results for", Change, "-", PhaseValue, ":\n")
        print(summary(model))
        
        # Perform post-hoc tests
        posthoc_results <- emmeans(model, pairwise ~ Group, adjust = "tukey")
        posthoc_results_file <- paste0(results_folder, "NEW_posthoc_results_", Change, "_", PhaseValue, ".txt")
        writeLines(as.character(posthoc_results), con = posthoc_results_file)
        
        cat("Post-hoc test results for", Change, "-", PhaseValue, ":\n")
        print(posthoc_results)

        #save plots into results folder
        ggsave(paste0(results_folder, "NEW_Activity_Index_by_Cage_Change_", Change, "_", PhaseValue, ".svg"), width = 6, height = 6)
    }
}

grid.arrange(grobs = plot_list, ncol = 2, top = "Activity Index by Cage Change")

# save each plot from plot_list to a file
for (i in 1:length(plot_list)) {
    ggsave(paste0(results_folder, "Activity_Index_by_Cage_Change_", i, ".svg"), plot_list[[i]], width = 6, height = 6)
}


# non-linear mixed-effects model
model_results_nl <- list()

for (Change in sort(unique(data_filtered_agg$Change))) {
    for (PhaseValue in c("Active", "Inactive")) {
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
            filter(Phase == PhaseValue)
        
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change_Phase_subset %>%
            mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                                                        ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
        
        model_nl <- nlmer(ActivityIndex ~ Group + (1 | AnimalNum) + (1 | HalfHourElapsed), 
                  data = data_filtered_agg_Change_Phase_subset,
                  start = c(fixed = c(0, 0), random = c(0, 0)))
        model_results_nl[[paste0(Change, "_", PhaseValue)]] <- model_nl
        
        mixed_model_results_file_nl <- paste0(results_folder, "NEW_mixed_model_results_nl_", Change, "_", PhaseValue, ".txt")
        model_summary_nl <- capture.output(summary(model_nl))
        writeLines(model_summary_nl, con = mixed_model_results_file_nl)
        
        cat("Non-linear mixed-effects model results for", Change, "-", PhaseValue, ":\n")
        print(summary(model_nl))
        
        # Perform post-hoc tests
        posthoc_results_nl <- emmeans(model_nl, pairwise ~ Group, adjust = "tukey")
        posthoc_results_file_nl <- paste0(results_folder, "NEW_posthoc_results_nl_", Change, "_", PhaseValue, ".txt")
        writeLines(as.character(posthoc_results_nl), con = posthoc_results_file_nl)
        
        cat("Post-hoc test results for", Change, "-", PhaseValue, ":\n")
        print(posthoc_results_nl)
    }
}

# random forest analysis
install.packages("randomForest")
library(randomForest)

for (Change in sort(unique(data_filtered_agg$Change))) {
    for (PhaseValue in c("Active", "Inactive")) {
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
            filter(Phase == PhaseValue)
        
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change_Phase_subset %>%
            mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                                                        ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
        
        rf_model <- randomForest(ActivityIndex ~ Group + HalfHourElapsed, data = data_filtered_agg_Change_Phase_subset, ntree = 1000)
        
        rf_model_results_file <- paste0(results_folder, "NEW_rf_model_results_", Change, "_", PhaseValue, ".txt")
        writeLines(as.character(rf_model), con = rf_model_results_file)
        
        cat("Random forest model results for", Change, "-", PhaseValue, ":\n")
        print(rf_model)
    }
}

for (Change in sort(unique(data_filtered_agg$Change))) {
    for (PhaseValue in c("Active", "Inactive")) {
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
            filter(Phase == PhaseValue) %>%
            mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                                   ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
        
        rf_model <- randomForest(ActivityIndex ~ Group + HalfHourElapsed, 
                                  data = data_filtered_agg_Change_Phase_subset, 
                                  ntree = 1000)
        
        rf_model_results_file <- paste0(results_folder, "NEW_rf_model_results_", Change, "_", PhaseValue, ".txt")
        
        # Extract important information from the model object and write to file
        rf_summary <- capture.output(summary(rf_model))
        writeLines(rf_summary, con = rf_model_results_file)
        
        # Print summary information
        cat("Random forest model results for", Change, "-", PhaseValue, ":\n")
        print(summary(rf_model))
    }
}

for (Change in sort(unique(data_filtered_agg$Change))) {
    for (PhaseValue in c("Active", "Inactive")) {
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
            filter(Phase == PhaseValue)
        
        data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change_Phase_subset %>%
            mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                                  ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
        
        rf_model <- randomForest(ActivityIndex ~ Group + HalfHourElapsed, 
                                 data = data_filtered_agg_Change_Phase_subset, 
                                 ntree = 1000)
        
        rf_model_results_file <- paste0(results_folder, "rf_model_results_", Change, "_", PhaseValue, ".txt")
        model_summary <- capture.output(print(rf_model))
        writeLines(model_summary, con = rf_model_results_file)
        
        cat("Random forest model results for", Change, "-", PhaseValue, ":\n")
        print(rf_model)
    }
}

# LSTM analysis
install.packages("keras")
install.packages("tensorflow")
library(keras)
library(tensorflow)

# Split data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(data_filtered_agg), 0.8 * nrow(data_filtered_agg))
train_data <- data_filtered_agg[train_indices, ]
test_data <- data_filtered_agg[-train_indices, ]

# Create LSTM model
lstm_model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(1, 1)) %>%
    layer_dense(units = 1)

# Create a list to store the models
lstm_models <- list()

# Loop through each unique group, phase, and cage change
for (Group in unique(data_filtered_agg$Group)) {
    for (PhaseValue in c("Active", "Inactive")) {
        for (Change in sort(unique(data_filtered_agg$Change))) {
            # Filter data for the specific group, phase, and cage change
            data_filtered_agg_subset <- data_filtered_agg %>%
                filter(Group == Group, Phase == PhaseValue, Change == Change)
            
            # Prepare the data for LSTM input
            x_train <- array_reshape(train_data$ActivityIndex, c(length(train_data$ActivityIndex), 1, 1))
            y_train <- train_data$ActivityIndex
            x_test <- array_reshape(test_data$ActivityIndex, c(length(test_data$ActivityIndex), 1, 1))
            y_test <- test_data$ActivityIndex
            
            # Train the LSTM model with modified parameters
            lstm_model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')
            lstm_model %>% fit(x_train, y_train, epochs = 50, batch_size = 16, validation_split = 0.2)
            
            # Store the trained model in the list
            model_name <- paste0("lstm_model_", Group, "_", PhaseValue, "_", Change)
            lstm_models[[model_name]] <- lstm_model
            
            # Save the trained model to a file
            model_file <- paste0(results_folder, model_name, ".h5")
            save_model_hdf5(lstm_model, model_file)
            
            # Evaluate the LSTM model
            evaluation <- lstm_model %>% evaluate(x_test, y_test)
            cat("Evaluation for Group:", Group, "Phase:", PhaseValue, "Change:", Change, "\n")
            print(evaluation)
        }
    }
}



# evaluate the created models and store the results in a text file
lstm_model_results_file <- paste0(results_folder, "lstm_model_results.txt")
model_summary <- capture.output(print(evaluation))
writeLines(model_summary, con = lstm_model_results_file)

# Define lists to store evaluation, performance metrics, ANOVA, and pairwise comparison results
performance_metrics <- list()
anova_results <- list()
pairwise_tukey_results <- list()

# Create an empty data frame to store the evaluation results
performance_data <- data.frame(Group = character(), Phase = character(), CageChange = character(), loss = numeric())

# Loop through each unique group, phase, and cage change
for (Group in unique(data_filtered_agg$Group)) {
    for (PhaseValue in c("Active", "Inactive")) {
        for (Change in sort(unique(data_filtered_agg$Change))) {
            # Filter data for the specific group, phase, and cage change
            data_filtered_agg_subset <- data_filtered_agg %>%
                filter(Group == Group, Phase == PhaseValue, Change == Change)
            
            # Prepare the data for LSTM input
            x_train <- array_reshape(train_data$ActivityIndex, c(length(train_data$ActivityIndex), 1, 1))
            y_train <- train_data$ActivityIndex
            x_test <- array_reshape(test_data$ActivityIndex, c(length(test_data$ActivityIndex), 1, 1))
            y_test <- test_data$ActivityIndex
            
            # Create LSTM model
            lstm_model <- keras_model_sequential() %>%
                layer_lstm(units = 50, input_shape = c(1, 1)) %>%
                layer_dense(units = 1)
            
            # Train the LSTM model
            lstm_model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')
            history <- lstm_model %>% fit(x_train, y_train, epochs = 50, batch_size = 16, validation_split = 0.2)
            
            # Store the trained model in the list
            model_name <- paste0("lstm_model_", Group, "_", PhaseValue, "_", Change)
            lstm_models[[model_name]] <- lstm_model
            
            # Save the trained model to a file
            model_file <- paste0(results_folder, model_name, ".h5")
            save_model_hdf5(lstm_model, model_file)
            
            # Evaluate the LSTM model
            evaluation <- lstm_model %>% evaluate(x_test, y_test)
            performance_metrics[[model_name]] <- evaluation
            
            # Perform ANOVA or t-tests
            # Example ANOVA:
            evaluation <- as.data.frame(evaluation)  # Convert evaluation to a data frame
            anova_result <- aov(loss ~ Group + Phase + CageChange, data = evaluation)  # Use 'loss' column directly
            anova_results[[model_name]] <- summary(anova_result)
            
            # Perform post-hoc pairwise comparisons if needed
            # Example pairwise t-tests:
            pairwise_tukey <- TukeyHSD(anova_result)
            pairwise_tukey_results[[model_name]] <- pairwise_tukey
            
            # Save the evaluation results to a file
            evaluation_file <- paste0(results_folder, model_name, "_evaluation.txt")
            write.table(evaluation, evaluation_file)
            
            # Save the performance metrics to a file
            performance_metrics_file <- paste0(results_folder, model_name, "_performance_metrics.txt")
            write.table(performance_metrics[[model_name]], performance_metrics_file)
            
            # Save the ANOVA results to a file
            anova_results_file <- paste0(results_folder, model_name, "_anova_results.txt")
            write.table(anova_results[[model_name]], anova_results_file)
            
            # Save the pairwise comparison results to a file
            pairwise_tukey_file <- paste0(results_folder, model_name, "_pairwise_tukey_results.txt")
            write.table(pairwise_tukey_results[[model_name]], pairwise_tukey_file)
            
            cat("Evaluation for Group:", Group, "Phase:", PhaseValue, "Change:", Change, "\n")
            print(evaluation)
        }
    }
}




# Define lists to store evaluation, performance metrics, ANOVA, and pairwise comparison results
performance_metrics <- list()
anova_results <- list()
pairwise_tukey_results <- list()

# Create an empty data frame to store the evaluation results
performance_data <- data.frame(Group = character(), Phase = character(), CageChange = character(), loss = numeric())

# Loop through each unique group, phase, and cage change
for (Group in unique(data_filtered_agg$Group)) {
    for (PhaseValue in c("Active", "Inactive")) {
        for (Change in sort(unique(data_filtered_agg$Change))) {
            # Filter data for the specific group, phase, and cage change
            data_filtered_agg_subset <- data_filtered_agg %>%
                filter(Group == Group, Phase == PhaseValue, Change == Change)
            
            # Prepare the data for LSTM input
            x_train <- array_reshape(train_data$ActivityIndex, c(length(train_data$ActivityIndex), 1, 1))
            y_train <- train_data$ActivityIndex
            x_test <- array_reshape(test_data$ActivityIndex, c(length(test_data$ActivityIndex), 1, 1))
            y_test <- test_data$ActivityIndex
            
            # Create LSTM model
            lstm_model <- keras_model_sequential() %>%
                layer_lstm(units = 50, input_shape = c(1, 1)) %>%
                layer_dense(units = 1)
            
            # Train the LSTM model
            lstm_model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')
            history <- lstm_model %>% fit(x_train, y_train, epochs = 50, batch_size = 16, validation_split = 0.2)
            
            # Store the trained model in the list
            model_name <- paste0("lstm_model_", Group, "_", PhaseValue, "_", Change)
            lstm_models[[model_name]] <- lstm_model
            
            # Save the trained model to a file
            model_file <- paste0(results_folder, model_name, ".h5")
            save_model_hdf5(lstm_model, model_file)
            
            # Evaluate the LSTM model
            evaluation <- lstm_model %>% evaluate(x_test, y_test)
            performance_metrics[[model_name]] <- evaluation
            
            # Save the evaluation results to a file
            evaluation_file <- paste0(results_folder, model_name, "_evaluation.txt")
            write.table(evaluation, evaluation_file)
            
            # Perform ANOVA or t-tests
            # Example ANOVA:
            anova_result <- aov(loss ~ 1, data = evaluation)  # Use a constant term instead of grouping variables
            anova_results[[model_name]] <- summary(anova_result)
            
            # Perform post-hoc pairwise comparisons if needed
            # Example pairwise t-tests:
            pairwise_tukey <- TukeyHSD(anova_result)
            pairwise_tukey_results[[model_name]] <- pairwise_tukey
            
            # Save the performance metrics to a file
            performance_metrics_file <- paste0(results_folder, model_name, "_performance_metrics.txt")
            write.table(performance_metrics[[model_name]], performance_metrics_file)
            
            # Save the ANOVA results to a file
            anova_results_file <- paste0(results_folder, model_name, "_anova_results.txt")
            write.table(anova_results[[model_name]], anova_results_file)
            
            # Save the pairwise comparison results to a file
            pairwise_tukey_file <- paste0(results_folder, model_name, "_pairwise_tukey_results.txt")
            write.table(pairwise_tukey_results[[model_name]], pairwise_tukey_file)
            
            cat("Evaluation for Group:", Group, "Phase:", PhaseValue, "Change:", Change, "\n")
            print(evaluation)
        }
    }
}


# Define lists to store evaluation and ANOVA results
performance_metrics <- list()
anova_results <- list()

# Create an empty data frame to store the evaluation results
performance_data <- data.frame(Group = character(), Phase = character(), CageChange = character(), loss = numeric())

# Loop through each unique group, phase, and cage change
for (Group in unique(data_filtered_agg$Group)) {
    for (PhaseValue in c("Active", "Inactive")) {
        for (Change in sort(unique(data_filtered_agg$Change))) {
            # Filter data for the specific group, phase, and cage change
            data_filtered_agg_subset <- data_filtered_agg %>%
                filter(Group == Group, Phase == PhaseValue, Change == Change)
            
            # Prepare the data for LSTM input
            x_train <- array_reshape(train_data$ActivityIndex, c(length(train_data$ActivityIndex), 1, 1))
            y_train <- train_data$ActivityIndex
            x_test <- array_reshape(test_data$ActivityIndex, c(length(test_data$ActivityIndex), 1, 1))
            y_test <- test_data$ActivityIndex
            
            # Create LSTM model
            lstm_model <- keras_model_sequential() %>%
                layer_lstm(units = 50, input_shape = c(1, 1)) %>%
                layer_dense(units = 1)
            
            # Train the LSTM model
            lstm_model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')
            history <- lstm_model %>% fit(x_train, y_train, epochs = 50, batch_size = 16, validation_split = 0.2)
            
            # Store the trained model in the list
            model_name <- paste0("lstm_model_", Group, "_", PhaseValue, "_", Change)
            lstm_models[[model_name]] <- lstm_model
            
            # Save the trained model to a file
            model_file <- paste0(results_folder, model_name, ".h5")
            save_model_hdf5(lstm_model, model_file)
            
            # Evaluate the LSTM model
            evaluation <- lstm_model %>% evaluate(x_test, y_test)
            performance_metrics[[model_name]] <- evaluation
            
            # Perform ANOVA
            evaluation <- as.data.frame(evaluation)  # Convert evaluation to a data frame
            anova_result <- aov(loss ~ Group, data = evaluation)  # Use 'Group' column directly
            anova_results[[model_name]] <- summary(anova_result)
            
            # Save the evaluation results to a file
            evaluation_file <- paste0(results_folder, model_name, "_evaluation.txt")
            write.table(evaluation, evaluation_file)
            
            # Save the performance metrics to a file
            performance_metrics_file <- paste0(results_folder, model_name, "_performance_metrics.txt")
            write.table(performance_metrics[[model_name]], performance_metrics_file)
            
            # Save the ANOVA results to a file
            anova_results_file <- paste0(results_folder, model_name, "_anova_results.txt")
            write.table(anova_results[[model_name]], anova_results_file)
            
            cat("Evaluation for Group:", Group, "Phase:", PhaseValue, "Change:", Change, "\n")
            print(evaluation)
        }
    }
}

# Perform pairwise Tukey's HSD test
tukey_results <- TukeyHSD(anova_result)




# Create plots for each CC with median
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
    # Subset data for the current CC
    data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
    
    # Split "SIS" group into "RES" and "SUS" based on "SUS" column
    data_filtered_agg_Change_subset <- data_filtered_agg_Change_subset %>%
        mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                                                    ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
    
    # Create the plot
    p <- ggplot(data_filtered_agg_Change_subset, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
        geom_path(stat = "summary") +
        stat_summary(aes(fill = Group), fun = median,  # Use median instead of mean
                                 fun.min = function(x) median(x) - sd(x), 
                                 fun.max = function(x) median(x) + sd(x), 
                                 geom = "ribbon", 
                                 alpha = 0.2, colour = NA) +
        scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
        scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
        scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset$HalfHourElapsed), by = 24), 
                                             labels = seq(0, max(data_filtered_agg_Change_subset$HalfHourElapsed), by = 24)/2) +
        theme_minimal_hgrid(12, rel_small = 1) +
        labs(title = bquote(~bold(.(paste("Activity Index")))),
                 y = "Activity [a.u.]", x = "Time elapsed [h]", caption = Change) +
        theme(plot.title = element_text(hjust = 0.5, face = "plain"),
                    plot.subtitle = element_text(hjust = 0.5, face = "plain"),
                    legend.position = "top",
                    legend.justification = "right",
                    legend.text = element_text(size = 9),
                    legend.box.spacing = unit(1, "pt"),
                    axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5))
    
    # Add geom_vline if there is data for each 24 HalfHourElapsed
    if (max(data_filtered_agg_Change_subset$HalfHourElapsed) >= 24) {
        p <- p + geom_vline(xintercept = seq(24, max(data_filtered_agg_Change_subset$HalfHourElapsed), by = 24), color = "grey")
    }
})