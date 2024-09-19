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

# LSTM analysis
library(keras)
library(tensorflow)

# Split data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(data_filtered_agg), 0.8 * nrow(data_filtered_agg))
train_data <- data_filtered_agg[train_indices, ]
test_data <- data_filtered_agg[-train_indices, ]

# Create a list to store the models and results
lstm_models <- list()
performance_metrics <- data.frame(Group = character(), Phase = character(), Change = character(), Loss = numeric())

# Loop through each unique group, phase, and cage change
for (Group in unique(data_filtered_agg$Group)) {
    for (PhaseValue in c("Active", "Inactive")) {
        for (Change in sort(unique(data_filtered_agg$Change))) {
            # Filter data for the specific group, phase, and cage change
            data_filtered_agg_subset <- data_filtered_agg %>%
                filter(Group == Group, Phase == PhaseValue, Change == Change)
            
            # Prepare the data for LSTM input (ensure train and test sets are filtered similarly)
            x_train <- array_reshape(train_data$ActivityIndex, c(length(train_data$ActivityIndex), 1, 1))
            y_train <- train_data$ActivityIndex
            x_test <- array_reshape(test_data$ActivityIndex, c(length(test_data$ActivityIndex), 1, 1))
            y_test <- test_data$ActivityIndex
            
            # Create and compile the LSTM model
            lstm_model <- keras_model_sequential() %>%
                layer_lstm(units = 50, input_shape = c(1, 1)) %>%
                layer_dense(units = 1)
            
            lstm_model %>% compile(loss = 'mean_squared_error', optimizer = 'adam')
            
            # Train the model
            history <- lstm_model %>% fit(x_train, y_train, epochs = 50, batch_size = 16, validation_split = 0.2)
            
            # Store the trained model in the list
            model_name <- paste0("lstm_model_", Group, "_", PhaseValue, "_", Change)
            lstm_models[[model_name]] <- lstm_model
            
            # Save the trained model to a file
            model_file <- paste0(results_folder, model_name, ".h5")
            save_model_hdf5(lstm_model, model_file)
            
            # Evaluate the model
            evaluation <- lstm_model %>% evaluate(x_test, y_test)
            cat("Evaluation for Group:", Group, "Phase:", PhaseValue, "Change:", Change, "\n")
            print(evaluation)
            
            # Store the evaluation results
            performance_metrics <- rbind(performance_metrics, 
                                         data.frame(Group = Group, Phase = PhaseValue, Change = Change, Loss = evaluation))
        }
    }
}

# Save the performance metrics to a CSV file
performance_metrics_file <- paste0(results_folder, "lstm_performance_metrics.csv")
write.csv(performance_metrics, performance_metrics_file, row.names = FALSE)

# Statistical Analysis: Perform ANOVA to compare performance across groups
anova_results <- aov(Loss ~ Group + Phase + Change, data = performance_metrics)

# Save ANOVA summary to CSV file
anova_summary_file <- paste0(results_folder, "lstm_anova_results.csv")
anova_summary <- summary(anova_results)
write.csv(as.data.frame(anova_summary), anova_summary_file)

# Perform post-hoc pairwise Tukey HSD test
tukey_results <- TukeyHSD(anova_results)

# Save Tukey HSD results to a CSV file
tukey_results_file <- paste0(results_folder, "lstm_tukey_hsd_results.csv")
write.csv(as.data.frame(tukey_results$Group), tukey_results_file)

# Output summary to console
cat("LSTM Model Performance Metrics:\n")
print(performance_metrics)

cat("ANOVA Results:\n")
print(anova_summary)

cat("Tukey's HSD Test Results:\n")
print(tukey_results)
