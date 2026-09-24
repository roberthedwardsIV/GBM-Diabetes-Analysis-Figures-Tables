library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)
library(scales)

df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
average_insulin <- df$Average.Daily.Insulin

#######Histogram
# Create a data frame (ggplot2 prefers data frames)
plot_df <- data.frame(Value = average_insulin)

# Basic histogram with ggplot2
ggplot(plot_df, aes(x = Value)) +
  geom_histogram()

# Customize with ggplot2
average_insulin_histogram <- ggplot(plot_df, aes(x = Value)) +
  geom_histogram(binwidth = 5, fill = "skyblue2", color = "black") +
  labs(title = "",
       x = "Average Daily Insulin (units)",
       y = "Count") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black"),
    axis.title.x = element_text(size = 7, color = "black"),
    axis.title.y = element_text(size = 7, color = "black")
  )+
  scale_x_continuous(
    breaks = seq(0, 100, 25)              # example: ticks every 0.1
  )



library(survival)
library(survminer)
library(dplyr)

df$Average.Daily.Insulin <- as.numeric(df$Average.Daily.Insulin)

# Remove NAs
df <- df %>% filter(!is.na(Average.Daily.Insulin), !is.na(Survival), !is.na(Vital.Status))

# Dichotomize using 50 units cutoff
df$Dose.Group <- ifelse(df$Average.Daily.Insulin <= 10, "Low", "High")
df$Dose.Group <- factor(df$Dose.Group, levels = c("Low", "High"))

# Fit Kaplan-Meier curves
km_fit <- survfit(Surv(Survival, Vital.Status) ~ Dose.Group, data = df)

# Compute log-rank p-value separately for formatting
logrank <- survdiff(Surv(Survival, Vital.Status) ~ Dose.Group, data = df)
pval <- 1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)
pval_text <- paste0("p = ", formatC(pval, format = "f", digits = 4))

# Plot Kaplan-Meier curves
avg_km_plot <- ggsurvplot(
  km_fit,
  data = df,
  risk.table = FALSE,          # remove risk table
  conf.int = TRUE,             # show confidence intervals
  palette = c("skyblue2", "red"),  # Low=blue, High=red
  legend.title = " ",
  legend.labs = c("Low Dose (n=58)", "High Dose (n=28)"),
  xlab = "Months",
  ylab = "Overall Survival Probability",
  ggtheme = theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # remove major grid
      panel.grid.minor = element_blank(),  # remove minor grid
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
)

# Add p-value in upper right corner
avg_km_plot$plot <- avg_km_plot$plot +
  annotate(
    "text",
    x = max(df$Survival, na.rm = TRUE)*0.8,
    y = 0.8,
    label = pval_text,
    hjust = 1,
    size = 3
  )

# Display plot




library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)
library(scales)

df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df$survival_days <- df$Survival * 30
df$cumulative_insulin <- df$survival_days * df$Average.Daily.Insulin

################Cumulative Insulin####################
# Create a data frame (ggplot2 prefers data frames)
library(ggplot2)
library(scales)
library(dplyr)

# Filter only values <= 500
plot_df <- data.frame(Value = cumulative_insulin) %>%
  filter(!is.na(Value), Value <= 500)

# Define bin width
binwidth <- 50  # adjust as needed to get readable bins

# Plot histogram
cum_insulin_histogram <- ggplot(plot_df, aes(x = Value)) +
  geom_histogram(
    binwidth = binwidth,
    fill = "skyblue2",
    color = "black",
    boundary = 0
  ) +
  scale_x_continuous(
    breaks = seq(0, 500, 100),   # x-axis ticks every 100 units
    labels = comma,
    limits = c(0, 500)
  ) +
  labs(
    x = "Cumulative Insulin (units)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color="black", fill=NA, linewidth=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size=7, color="black"),
    axis.text.y = element_text(size=7),
    axis.title.x = element_text(size = 7, color = "black"),
    axis.title.y = element_text(size = 7, color = "black")
  )



#############Kaplan meier curve
library(survival)
library(survminer)
library(dplyr)

# Ensure cumulative_insulin is numeric
df$cumulative_insulin <- as.numeric(df$cumulative_insulin)

# Remove NAs
df <- df %>% filter(!is.na(cumulative_insulin), !is.na(Survival), !is.na(Vital.Status))

# Dichotomize using 50 units cutoff
df$Dose.Group <- ifelse(df$cumulative_insulin <= 50, "Low", "High")
df$Dose.Group <- factor(df$Dose.Group, levels = c("Low", "High"))

# Fit Kaplan-Meier curves
km_fit <- survfit(Surv(Survival, Vital.Status) ~ Dose.Group, data = df)

# Compute log-rank p-value separately for formatting
logrank <- survdiff(Surv(Survival, Vital.Status) ~ Dose.Group, data = df)
pval <- 1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)
pval_text <- paste0("p = ", formatC(pval, format = "f", digits = 4))

# Plot Kaplan-Meier curves
km_plot <- ggsurvplot(
  km_fit,
  data = df,
  risk.table = FALSE,          # remove risk table
  conf.int = TRUE,             # show confidence intervals
  palette = c("skyblue2", "red"),  # Low=blue, High=red
  legend.title = " ",
  legend.labs = c("Low Dose (n=34)", "High Dose (n=52)"),
  xlab = "Months",
  ylab = "Overall Survival Probability",
  ggtheme = theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # remove major grid
      panel.grid.minor = element_blank(),  # remove minor grid
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 7),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
)

# Add p-value in upper right corner
km_plot$plot <- km_plot$plot +
  annotate(
    "text",
    x = max(df$Survival, na.rm = TRUE)*0.8,
    y = 0.8,
    label = pval_text,
    hjust = 1,
    size = 3
  )

# Display plot
km_plot













layout <- "AB\nCD"   # <- exact, rectangular (4 chars per row)

# use avg_km_plot$plot instead of avg_km_plot
combined <- (
  average_insulin_histogram +
    avg_km_plot$plot +
    cum_insulin_histogram +
    km_plot$plot
) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')

print(combined)
