################################################################################
## Compare Means between Groups during 2 hours preceding the Active Phase
#  This is used to quantify activity before the onset of the active phase

library(cowplot)
library(ggsignif)

## Compare Means between Groups during 2 hours preceding the Active Phase

# Filter the data for PriorActive == TRUE
prior_active_data <- nightly_data %>% 
  filter(PriorActive == TRUE)

# Calculate the means and standard deviations of ActivityIndex for each group
means <- prior_active_data %>% 
  group_by(Group, AnimalNum) %>% 
  summarise(ActivityIndex = mean(ActivityIndex),
            sd_ActivityIndex = sd(ActivityIndex))

# Normality test for CTRL group
con_norm <- shapiro.test(means$ActivityIndex[means$Group == "CON"])
con_norm

# Normality test for SIS group
sis_norm <- shapiro.test(means$ActivityIndex[means$Group == "SIS"])
sis_norm

# Determine which test to use based on normality of both groups
if (con_norm$p.value >= 0.05 & sis_norm$p.value >= 0.05) {
  # If normal, use t-test
  t_test <- t.test(ActivityIndex ~ Group, data = prior_active_data)
  test_name <- "t.test"
  test_pval <- t_test$p.value
  # Determine the significance level for t-test
  sig_levels <- sprintf("%.3f", test_pval)
} else {
  # If not normal, use Wilcoxon rank-sum test
  wilcox_test <- wilcox.test(ActivityIndex ~ Group, data = prior_active_data)
  test_name <- "wilcox.test"
  test_pval <- wilcox_test$p.value
  # Determine the significance level for Wilcoxon rank-sum test
  sig_levels <- sprintf("%.3f", p.adjust(test_pval, method = "BH"))
}

# Create plot for individual values
p_indiv <- ggplot(prior_active_data, aes(x = Group, y = ActivityIndex, color = Group)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) + # Add jittered dots
  scale_color_manual(values = group_cols) + # Set color scheme
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank()) # Use a white and black theme

# Create plot for means
p_means <- ggplot(means, aes(x = Group, y = ActivityIndex, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = ActivityIndex - sd_ActivityIndex, ymax = ActivityIndex + sd_ActivityIndex), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Mean Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank()) # Use a white and black theme

# Add significance asterisks
p_means <- p_means + geom_signif(comparisons = list(c("CON", "SIS")), 
                                 test = test_name,
                                 map_signif_level = TRUE, 
                                 textsize = 4,
                                 annotations = sig_levels,
                                 y_position = max(means$ActivityIndex)+5)

# Arrange two plots side by side
plot_grid(p_indiv, p_means, ncol = 2, align = "v", labels = c("A", "B"))
