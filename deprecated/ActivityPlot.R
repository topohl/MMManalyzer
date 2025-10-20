################################################################################
## Line plot of Active and Inactive Phase based on Group
#  This is used to compare activity patterns during the active and inactive phase

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(forcats)

group_cols<-c("#1e3791","#00ac8c")

# Define the phases to loop through
phases <- c("Active", "Inactive")

# Create an empty list to store the plots
plots <- list()

# Loop through each phase and create a plot
for (phase in phases) {
  
  # Filter the data for the current phase
  phase_data <- nightly_data %>% 
    filter(Phase == phase) %>% 
    filter(RecentChange == FALSE) # Only keep rows where RecentChange is FALSE
  
  # Convert ConsecActive to factor
  phase_data <- phase_data %>% 
    mutate(ConsecActive = as.factor(ConsecActive))
    
  # Create the plot
  plot <- ggplot(phase_data, aes(x = fct_inorder(ConsecActive), ActivityIndex, group = Group, colour = Group)) +
    geom_line(stat = "summary") +
    stat_summary(aes(fill = Group), fun = mean,
                 fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "ribbon", 
                 alpha = 0.2, colour = NA) +
    scale_color_manual(name = NULL, values = group_cols) + 
    scale_fill_manual(name = NULL, values = group_cols) +
    theme_minimal_hgrid(12, rel_small = 1) +
    labs(title = bquote(~bold(.(paste("Activity Index")))), subtitle = paste("", phase, "Phase"),
         y = "Activity [a.u.]", x = "Day/Night", caption = "") +
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          legend.position = "top",
          legend.justification = "right",
          legend.text = element_text(size = 9),
          legend.box.spacing = unit(1, "pt"),
          axis.text.x = element_text(angle = 45, vjust = 1, size = 11, hjust = 1),
          axis.title.x = element_blank())
  
  # Add the plot to the list of plots
  plots[[phase]] <- plot
}

# Arrange the plots using grid.arrange
grid.arrange(grobs = plots, nrow = 2)
