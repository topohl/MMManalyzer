################################################################################
## Create the cookie plot
## Compare Means between Groups during 2 hours preceding the Active Phase
#  This is used to quantify activity before the onset of the active phase
################################### L I N E  P L O T ###################################
# here, we plot the different groups on a minute bin basis and compute lme analysis on the data

# Load necessary libraries
library(cowplot)
library(ggsignif)
library(ggplot2)
library(colorspace)
library(dplyr)
library(openxlsx)
library(tidyr)
library(broom)
library(readxl)
library(rstatix)
library(ggpubr)
library(Hmisc)
library(lme4)

# Define the list of SUS animals
sus_animals <- c("0001", "0004", "OR750", "OR751", "OR755", "OR762", "OR764", "OR770", "OR771", "OR106", "OR111", "OR112", "OR113", "OR120", "OR134")

# Replace Group values with "SUS" for SUS animals, "RES" for SIS animals, and keep other groups as is
data <- data %>%
  mutate(Group = if_else(AnimalNum %in% sus_animals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")

cookie_data <- data %>% 
  filter(Cookie == 'TRUE')

# Calculate the time difference from the beginning of the DateTime dataset
cookie_data$ElapsedMinutes <- as.numeric(cookie_data$DateTime - min(cookie_data$DateTime)) / 60

# Now "ElapsedMinutes" column contains the number of elapsed minutes from the beginning of the DateTime dataset
# Calculate the means and standard deviations of ActivityIndex for each group
means_cookie <- cookie_data %>% 
  group_by(Group, AnimalNum) %>% 
  summarise(ActivityIndex = mean(ActivityIndex),
            sd_ActivityIndex = sd(ActivityIndex))

# Perform Shapiro-Wilk normality test for each group

# Normality test for CON group
con_norm_cookie <- shapiro.test(means_cookie$ActivityIndex[means_cookie$Group == "CON"])

# Normality test for RES group
res_norm_cookie <- shapiro.test(means_cookie$ActivityIndex[means_cookie$Group == "RES"])

# Normality test for SUS group
sus_norm_cookie <- shapiro.test(means_cookie$ActivityIndex[means_cookie$Group == "SUS"])

# Fit linear mixed-effects models for each group
lme_model <- lmer(ActivityIndex ~ ElapsedMinutes + (1 | Group), data = cookie_data)

# Get the fitted values from the LME model
cookie_data$fitted_values <- fitted(lme_model)

# Create plot for individual values
p_indiv_cookie <- ggplot(cookie_data, aes(x = Group, y = ActivityIndex, color = Group)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) + # Add jittered dots
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) + # Set color scheme
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "All Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Create plot for means
p_means_cookie <- ggplot(means_cookie, aes(x = Group, y = ActivityIndex, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = ActivityIndex - sd_ActivityIndex, ymax = ActivityIndex + sd_ActivityIndex), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Individual Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Arrange two plots side by side
plot_grid(p_indiv_cookie, p_means_cookie, ncol = 2, align = "v", labels = c("A", "B"))

cookie_plot <- ggplot(cookie_data, aes(x = DateTime, y = ActivityIndex, group = Group, color = Group)) +
  geom_path(stat = "summary") +
  stat_summary(aes(fill = Group), fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "ribbon", 
               alpha = 0.2, colour = NA) +
  scale_color_manual(name = NULL, values = group_cols) + 
  scale_fill_manual(name = NULL, values = group_cols) +
  theme_minimal_hgrid(12, rel_small = 1) +
  labs(title = bquote(~bold(.(paste("Cookie Challenge")))),
       y = "Activity [a.u.]", x = "Day/Night", caption = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        legend.position = "top",
        legend.justification = "right",
        legend.text = element_text(size = 9),
        legend.box.spacing = unit(1, "pt"),
        axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5),
        axis.title.x = element_blank())

cookie_plot
