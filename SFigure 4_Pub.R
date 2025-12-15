library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)
library(patchwork)
Updated_Tumor_Velocites <- read_excel("~/Desktop/GBM/Updated Tumor Velocites.xlsx")


#filtered out cumulative insulin = 0 units datapoints
Updated_Tumor_Velocites <- subset(Updated_Tumor_Velocites, Updated_Tumor_Velocites$`Cumulative Insulin Dose Since Last Scan (units)` > 0)

#initializing lists
scan_date <- Updated_Tumor_Velocites$`Scan Date`
tumor_measurement <- as.numeric(Updated_Tumor_Velocites$`Tumor Measurement (mm^2)`)
tumor_velocity <- as.numeric(Updated_Tumor_Velocites$`Tumor Velocity Since Last Scan (mm^2/day)`)
FLAIR_velocity <- as.numeric(Updated_Tumor_Velocites$`FLAIR Velocity Since Last Scan (mm^2/day)`)
FLAIR_measurement <- as.numeric(Updated_Tumor_Velocites$`FLAIR Measurement (mm^2)`)
cumulative_insulin <- as.numeric(Updated_Tumor_Velocites$`Cumulative Insulin Dose Since Last Scan (units)`)
average_insulin <- as.numeric(Updated_Tumor_Velocites$`Avg Daily Ins Dose Since Last Scan (units/day)`)



###############Figures for SFigure 4A (potentially)
#average insulin vs. tumor velocity plot
test <- cor.test(average_insulin, tumor_velocity, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value) # Format p-value
if (test$p.value < 0.0001) {
  correlation_p <- "p < 0.0001"
}
correlation_label <- paste("PCC =", correlation_r, "     ", correlation_p)
linear_model <- lm(tumor_velocity ~ average_insulin, data = Updated_Tumor_Velocites)
intercept <- round(coef(linear_model)[1], 2)
slope <- round(coef(linear_model)[2], 2)
equation <- paste("y =", slope, "x +", intercept)
r_squared <- round(summary(linear_model)$r.squared, 4)
r_squared_label <- paste("R^2 =", r_squared)
label_text <- paste(equation, "\n", "R^2 =", r_squared, "\n", correlation_label)
label_df <- data.frame(
  average_insulin = max(average_insulin, na.rm = TRUE),
  tumor_velocity = max(tumor_velocity, na.rm = TRUE),
  label = label_text
)
avg_ins_vs_tum_vel <- ggplot(Updated_Tumor_Velocites, 
                             aes(x = average_insulin, y = tumor_velocity), 
                             ggtheme = theme(
                               axis.text = element_text(size = 7, color = "black"),
                               axis.title = element_text(size = 7, color = "black"),
                               title = element_text(size = 7, color = "black")
                             )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = average_insulin, y = tumor_velocity, label = label),
    hjust = 1,
    vjust = 1.3,
    size = 2,
    color = "black",                  # text color
    fill  = "white",                  # white background
    label.size = 0.8,
    label.padding = unit(0.6, "lines")) +
  labs(title = "Tumor Velocity vs. Average Insulin",
       x = "Average Insulin (units/day)",
       y = "Tumor Velocity (mm^2/day)") +
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black"),  # y-axis title
    plot.title   = element_text(size = 7, color = "black", hjust = 0.5) # main title
  )

print(avg_ins_vs_tum_vel)


###############Figures for SFigure 4B (potentially)
#average insulin vs. FLAIR velocity plot
test <- cor.test(average_insulin, FLAIR_velocity, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value) # Format p-value
if (test$p.value < 0.0001) {
  correlation_p <- "p < 0.0001"
}
correlation_label <- paste("PCC =", correlation_r, "     ", correlation_p)
linear_model <- lm(FLAIR_velocity ~ average_insulin, data = Updated_Tumor_Velocites)
intercept <- round(coef(linear_model)[1], 2)
slope <- round(coef(linear_model)[2], 2)
equation <- paste("y =", slope, "x +", intercept)
r_squared <- round(summary(linear_model)$r.squared, 4)
r_squared_label <- paste("R^2 =", r_squared)
label_text <- paste(equation, "\n", "R^2 =", r_squared, "\n", correlation_label)
label_df <- data.frame(
  average_insulin = max(average_insulin, na.rm = TRUE),
  FLAIR_velocity = max(FLAIR_velocity, na.rm = TRUE),
  label = label_text
)
avg_ins_vs_fla_vel <- ggplot(Updated_Tumor_Velocites, 
                             aes(x = average_insulin, y = FLAIR_velocity), 
                             ggtheme = theme(
                               axis.text = element_text(size = 7, color = "black"),
                               axis.title = element_text(size = 7, color = "black"),
                               title = element_text(size = 7, color = "black")
                             )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = average_insulin, y = FLAIR_velocity, label = label),
    hjust = 1,
    vjust = 1.3,
    size = 2,
    color = "black",                  # text color
    fill  = "white",                  # white background
    label.size = 0.8,
    label.padding = unit(0.6, "lines")) +
  labs(title = "FLAIR Velocity vs. Average Insulin",
       x = "Average Insulin (units/day)",
       y = "FLAIR Velocity (mm^2/day)") +
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black"),  # y-axis title
    plot.title   = element_text(size = 7, color = "black", hjust = 0.5) # main title
  )

print(avg_ins_vs_fla_vel)


###############Figures for SFigure 4C (potentially)
#cumulative insulin vs. tumor velocity plot
test <- cor.test(cumulative_insulin, tumor_velocity, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value) # Format p-value
if (test$p.value < 0.0001) {
  correlation_p <- "p < 0.0001"
}
correlation_label <- paste("PCC =", correlation_r, "     ", correlation_p)
linear_model <- lm(tumor_velocity ~ cumulative_insulin, data = Updated_Tumor_Velocites)
intercept <- round(coef(linear_model)[1], 2)
slope <- round(coef(linear_model)[2], 2)
equation <- paste("y =", slope, "x +", intercept)
r_squared <- round(summary(linear_model)$r.squared, 4)
r_squared_label <- paste("R^2 =", r_squared)
label_text <- paste(equation, "\n", "R^2 =", r_squared, "\n", correlation_label)
label_df <- data.frame(
  cumulative_insulin = max(cumulative_insulin, na.rm = TRUE),
  tumor_velocity = max(tumor_velocity, na.rm = TRUE),
  label = label_text
)
cum_ins_vs_tum_vel <- ggplot(Updated_Tumor_Velocites, 
                             aes(x = cumulative_insulin, y = tumor_velocity), 
                             ggtheme = theme(
                               axis.text = element_text(size = 7, color = "black"),
                               axis.title = element_text(size = 7, color = "black"),
                               title = element_text(size = 7, color = "black")
                             )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = cumulative_insulin, y = tumor_velocity, label = label),
    hjust = 1,
    vjust = 1.3,
    size = 2,
    color = "black",                  # text color
    fill  = "white",                  # white background
    label.size = 0.8,
    label.padding = unit(0.6, "lines")) +
  labs(title = "Tumor Velocity vs. Cumulative Insulin",
       x = "Cumulative Insulin (units)",
       y = "Tumor Velocity (mm^2/day)") +
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black"),  # y-axis title
    plot.title   = element_text(size = 7, color = "black", hjust = 0.5) # main title
  )

print(cum_ins_vs_tum_vel)


###############Figures for SFigure 4D (potentially)
#cumulative insulin vs. FLAIR velocity plot
test <- cor.test(cumulative_insulin, FLAIR_velocity, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value) # Format p-value
if (test$p.value < 0.0001) {
  correlation_p <- "p < 0.0001"
}
correlation_label <- paste("PCC =", correlation_r, "     ", correlation_p)
linear_model <- lm(FLAIR_velocity ~ cumulative_insulin, data = Updated_Tumor_Velocites)
intercept <- round(coef(linear_model)[1], 2)
slope <- round(coef(linear_model)[2], 2)
equation <- paste("y =", slope, "x +", intercept)
r_squared <- round(summary(linear_model)$r.squared, 4)
r_squared_label <- paste("R^2 =", r_squared)
label_text <- paste(equation, "\n", "R^2 =", r_squared, "\n", correlation_label)
label_df <- data.frame(
  cumulative_insulin = max(cumulative_insulin, na.rm = TRUE),
  FLAIR_velocity = max(FLAIR_velocity, na.rm = TRUE),
  label = label_text
)
cum_ins_vs_fla_vel <- ggplot(Updated_Tumor_Velocites, 
                             aes(x = cumulative_insulin, y = FLAIR_velocity), 
                             ggtheme = theme(
                               axis.text = element_text(size = 7, color = "black"),
                               axis.title = element_text(size = 7, color = "black"),
                               title = element_text(size = 7, color = "black")
                             )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = cumulative_insulin, y = FLAIR_velocity, label = label),
    hjust = 1,
    vjust = 1.3,
    size = 2,
    color = "black",                  # text color
    fill  = "white",                  # white background
    label.size = 0.8,
    label.padding = unit(0.6, "lines")) +
  labs(title = "FLAIR Velocity vs. Cumulative Insulin",
       x = "Cumulative Insulin (units)",
       y = "FLAIR Velocity (mm^2/day)") +
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black"),  # y-axis title
    plot.title   = element_text(size = 7, color = "black", hjust = 0.5) # main title
  )

print(cum_ins_vs_fla_vel)





layout <- "AB\nCD"   # <- exact, rectangular (4 chars per row)

combined <- (avg_ins_vs_tum_vel + avg_ins_vs_fla_vel + cum_ins_vs_tum_vel + cum_ins_vs_fla_vel) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')

print(combined)
