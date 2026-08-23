#=============================================================================
# PUBLICATION FIGURE: Grouped Bar Chart of Bootstrap Validation Results
#=============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)

#-----------------------------------------------------------------------------
# RESULTS
#-----------------------------------------------------------------------------

results <- data.frame(
  Model = c("PSE EOT", "PSE FU", "PCS EOT", "PCS FU"),
  Timepoint = c("End of Treatment", "6-Month Follow-up", "End of Treatment", "6-Month Follow-up"),
  Outcome = c("PSE", "PSE", "PCS", "PCS"),
  n = c(2325, 1098, 2325, 1086),
  R2_Apparent = c(0.085, 0.036, 0.088, 0.080),
  R2_Corrected = c(0.063, -0.008, 0.067, 0.032),
  R2_Optimism = c(0.022, 0.047, 0.021, 0.048),
  CalSlope_Apparent = c(1.000, 1.000, 1.000, 1.000),
  CalSlope_Corrected = c(0.885, 0.644, 0.894, 0.770),
  CalSlope_Optimism = c(0.115, 0.356, 0.106, 0.230),
  CalInt_Corrected = c(1.506, 4.066, -1.068, -2.820),
  EO_Corrected = c(1.000, 1.000, 1.000, 1.000)
)

#-----------------------------------------------------------------------------
# FIGURE 1: R-squared (Apparent vs Corrected)
#-----------------------------------------------------------------------------

# Reshape for plotting
r2_long <- results %>%
  dplyr::select(Model, Timepoint, Outcome, R2_Apparent, R2_Corrected) %>%
  pivot_longer(cols = c(R2_Apparent, R2_Corrected),
               names_to = "Type",
               values_to = "R2") %>%
  mutate(Type = factor(Type, 
                       levels = c("R2_Apparent", "R2_Corrected"),
                       labels = c("Apparent", "Optimism-Corrected")),
         Timepoint = factor(Timepoint, levels = c("End of Treatment", "6-Month Follow-up")),
         Outcome = factor(Outcome))

fig_r2 <- ggplot(r2_long, aes(x = Outcome, y = R2, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.3) +
  facet_wrap(~Timepoint) +
  scale_fill_manual(values = c("Apparent" = "#4DAF4A", "Optimism-Corrected" = "#377EB8"),
                    name = "") +
  scale_y_continuous(limits = c(-0.02, 0.12), breaks = seq(0, 0.10, 0.02)) +
  labs(x = NULL,
       y = expression(R^2),
       title = "Discrimination Performance") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(fig_r2)
ggsave("Figure_R2_comparison.png", fig_r2, width = 8, height = 5, dpi = 300)
ggsave("Figure_R2_comparison.pdf", fig_r2, width = 8, height = 5)

#-----------------------------------------------------------------------------
# FIGURE 2: Calibration Slope (Apparent vs Corrected)
#-----------------------------------------------------------------------------

calslope_long <- results %>%
  dplyr::select(Model, Timepoint, Outcome, CalSlope_Apparent, CalSlope_Corrected) %>%
  pivot_longer(cols = c(CalSlope_Apparent, CalSlope_Corrected),
               names_to = "Type",
               values_to = "CalSlope") %>%
  mutate(Type = factor(Type, 
                       levels = c("CalSlope_Apparent", "CalSlope_Corrected"),
                       labels = c("Apparent", "Optimism-Corrected")),
         Timepoint = factor(Timepoint, levels = c("End of Treatment", "6-Month Follow-up")),
         Outcome = factor(Outcome))

fig_calslope <- ggplot(calslope_long, aes(x = Outcome, y = CalSlope, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  facet_wrap(~Timepoint) +
  scale_fill_manual(values = c("Apparent" = "#4DAF4A", "Optimism-Corrected" = "#377EB8"),
                    name = "") +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
  labs(x = NULL,
       y = "Calibration Slope",
       title = "Calibration Performance") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(fig_calslope)
ggsave("Figure_CalSlope_comparison.png", fig_calslope, width = 8, height = 5, dpi = 300)
ggsave("Figure_CalSlope_comparison.pdf", fig_calslope, width = 8, height = 5)

#-----------------------------------------------------------------------------
# FIGURE 3: Combined Panel (R2 and Calibration Slope side by side)
#-----------------------------------------------------------------------------

# Prepare R2 data
r2_plot <- results %>%
  dplyr::select(Timepoint, Outcome, R2_Apparent, R2_Corrected) %>%
  pivot_longer(cols = c(R2_Apparent, R2_Corrected),
               names_to = "Type", values_to = "Value") %>%
  mutate(Metric = "R²",
         Type = ifelse(Type == "R2_Apparent", "Apparent", "Optimism-Corrected"))

# Prepare CalSlope data
cal_plot <- results %>%
  dplyr::select(Timepoint, Outcome, CalSlope_Apparent, CalSlope_Corrected) %>%
  pivot_longer(cols = c(CalSlope_Apparent, CalSlope_Corrected),
               names_to = "Type", values_to = "Value") %>%
  mutate(Metric = "Calibration Slope",
         Type = ifelse(Type == "CalSlope_Apparent", "Apparent", "Optimism-Corrected"))

# Combine
combined_data <- bind_rows(r2_plot, cal_plot) %>%
  mutate(Timepoint = factor(Timepoint, levels = c("End of Treatment", "6-Month Follow-up")),
         Type = factor(Type, levels = c("Apparent", "Optimism-Corrected")),
         Metric = factor(Metric, levels = c("R²", "Calibration Slope")))

fig_combined <- ggplot(combined_data, aes(x = Outcome, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(data = data.frame(Metric = factor("Calibration Slope", 
                                               levels = c("R²", "Calibration Slope")), 
                               yint = 1),
             aes(yintercept = yint), linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_hline(data = data.frame(Metric = factor("R²", 
                                               levels = c("R²", "Calibration Slope")), 
                               yint = 0),
             aes(yintercept = yint), linetype = "solid", color = "black", linewidth = 0.3) +
  facet_grid(Metric ~ Timepoint, scales = "free_y") +
  scale_fill_manual(values = c("Apparent" = "#4DAF4A", "Optimism-Corrected" = "#377EB8"),
                    name = "") +
  labs(x = NULL, y = NULL,
       title = "Internal Validation: Apparent vs Optimism-Corrected Performance") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(fig_combined)
ggsave("Figure_Combined_Validation.png", fig_combined, width = 10, height = 7, dpi = 300)
ggsave("Figure_Combined_Validation.pdf", fig_combined, width = 10, height = 7)

#-----------------------------------------------------------------------------
# FIGURE 4: Summary Table as Figure (for supplementary material)
#-----------------------------------------------------------------------------

summary_for_table <- results %>%
  dplyr::select(Model, n, R2_Corrected, CalSlope_Corrected, CalInt_Corrected, EO_Corrected) %>%
  rename(
    `Sample Size` = n,
    `R² (corrected)` = R2_Corrected,
    `Cal. Slope` = CalSlope_Corrected,
    `Cal. Intercept` = CalInt_Corrected,
    `E/O Ratio` = EO_Corrected
  )

print(summary_for_table)

# Export as CSV for table formatting
write.csv(summary_for_table, "Validation_Summary_Table.csv", row.names = FALSE)

cat("\n=== Figures saved ===\n")
cat("- Figure_R2_comparison.png/pdf\n")
cat("- Figure_CalSlope_comparison.png/pdf\n")
cat("- Figure_Combined_Validation.png/pdf\n")
cat("- Validation_Summary_Table.csv\n")
