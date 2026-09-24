library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)

df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df <- df %>%
  mutate(Diabetes = ifelse(Coded.MRN == 3, 1, Diabetes))

df <- df %>% filter(Coded.MRN != 28)

df$Diabetes <- factor(df$Diabetes, levels=c(0,1), labels=c("Non-Diabetic", "Diabetic"))



##SFigure 1A (Violin)
sfigure_oneA <- ggplot(df, aes(x=Diabetes, y=Age.at.Diagnosis, fill=Diabetes)) +
  geom_jitter(width=0.1, alpha=0.2, size=0.8) +          # individual patient points
  geom_violin(trim=FALSE, alpha=0.3, color="black") +  # one violin per Diabetes group
  
  labs(title="",
       x=" ", y="Age (years)") +
  theme_minimal(base_size = 7)+
  theme(
    legend.title = element_text(size = 7, face = "bold", color = "black"),
    legend.text  = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, face = "plain", color = "black"),
    axis.label = element_text(size = 7, face = "plain", color = "black")
    
  )





library(tidyverse)
library(readxl)

df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))

# Factorize variables
df$Diabetes <- factor(df$Diabetes, levels=c(0,1), labels=c("Non-Diabetic", "Diabetic"))
df$Age.Class <- factor(df$Age.Class,
                       levels = c(0, 1, 2, 3),
                       labels = c("<40 yrs", "40-60 yrs", "61-80 yrs", ">80 yrs"))

# Calculate Summary and Percentages
summary_within <- df %>%
  count(Diabetes, Age.Class) %>%
  group_by(Diabetes) %>%
  mutate(percent_within = n / sum(n) * 100) %>%
  ungroup()


label_data <- summary_within %>%
  # Reverse the levels of Age.Class for the *calculation*
  mutate(Age.Class = fct_rev(Age.Class)) %>%
  
  # Arrange by the newly reversed factor order
  arrange(Diabetes, Age.Class) %>%
  group_by(Diabetes) %>%
  
  # Calculate cumulative sum (top of each segment, now counting from the top of the bar down)
  mutate(csum = cumsum(percent_within)) %>%
  
  # Calculate the midpoint of each segment
  mutate(label_y_position = csum - (percent_within / 2)) %>%
  ungroup() %>%

  mutate(Age.Class = fct_rev(Age.Class))


# --- 3. Plotting the Stacked Bar Chart with Right-Aligned Labels ---
sfigure_oneB <- ggplot(summary_within, 
                       aes(x = Diabetes,
                           y = percent_within,
                           fill = Age.Class)) +
  
  # Main stacked bars
  geom_col(position = "stack", width = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  
  # Labels: Use the calculated segment midpoint for 'y'
  geom_text(data = label_data,
            aes(
              y = label_y_position, # Correctly ordered and centered vertical position
              label = paste0(round(percent_within, 0), "%")
            ),
            hjust = 0,    
            nudge_x = 0.4, 
            size = 2,
            colour = "black") +
  
  # Ensure the x-axis has enough space for the nudged labels
  scale_x_discrete(expand = expansion(add = 0.6)) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 110),
                     expand = c(0, 0)) +
  
  labs(title = "",
       x = " ",
       y = "Percentage within Group",
       fill = "Age Class") +
  
  theme_minimal(base_size = 7) +
  theme(
    legend.title = element_text(size = 7, face = "bold", color = "black"),
    legend.text = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, face = "plain", color = "black"),
    axis.label = element_text(size = 7, face = "plain", color = "black")
  )

print(sfigure_oneB)




##SFigure 1C
diabetic <- df[df['Diabetes'] == "Diabetic", ]
d_age <- diabetic$Age.at.Diagnosis
d_survival <- diabetic$Survival

test <- cor.test(d_age, d_survival, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value) # Format p-value
if (test$p.value < 0.0001) {
  correlation_p <- "p < 0.0001"
}
correlation_label <- paste("PCC =", correlation_r, "     ", correlation_p)
linear_model <- lm(d_survival ~ d_age, data = diabetic)
intercept <- round(coef(linear_model)[1], 2)
slope <- round(coef(linear_model)[2], 2)
equation <- paste("y =", slope, "x +", intercept)
r_squared <- round(summary(linear_model)$r.squared, 4)
r_squared_label <- paste("R^2 =", r_squared)
label_text <- paste(equation, "\n", "R^2 =", r_squared, "\n", correlation_label)
label_df <- data.frame(
  d_age = max(d_age, na.rm = TRUE),
  d_survival = max(d_survival, na.rm = TRUE),
  label = label_text
)
sfigure_oneC <- ggplot(diabetic, aes(x = d_age, y = d_survival)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = d_age, y = d_survival, label = label),
    hjust = 1,
    vjust = 1.2,
    size = 1.5,
    color = "black",                  # text color
    fill  = "white",                  # white background
    label.size = 0.8,
    label.padding = unit(0.6, "lines")) +
  labs(title = "",
       x = "Age at Diagnosis (years)",
       y = "Survival (months)") +
  theme_minimal(base_size = 7) + 
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black")  # y-axis title
  )+
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  )

print(sfigure_oneC)


##SFigure 1D
nondiabetic <- df[df['Diabetes'] == "Non-Diabetic", ]
nd_age <- nondiabetic$Age.at.Diagnosis
nd_survival <- nondiabetic$Survival

test <- cor.test(nd_age, nd_survival, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value) # Format p-value
if (test$p.value < 0.0001) {
  correlation_p <- "p < 0.0001"
}
correlation_label <- paste("PCC =", correlation_r, "     ", correlation_p)
linear_model <- lm(nd_survival ~ nd_age, data = nondiabetic)
intercept <- round(coef(linear_model)[1], 2)
slope <- round(coef(linear_model)[2], 2)
equation <- paste("y =", slope, "x +", intercept)
r_squared <- round(summary(linear_model)$r.squared, 4)
r_squared_label <- paste("R^2 =", r_squared)
label_text <- paste(equation, "\n", "R^2 =", r_squared, "\n", correlation_label)
label_df <- data.frame(
  nd_age = max(nd_age, na.rm = TRUE),
  nd_survival = max(nd_survival, na.rm = TRUE),
  label = label_text
)
sfigure_oneD <- ggplot(nondiabetic, aes(x = nd_age, y = nd_survival)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = nd_age, y = nd_survival, label = label),
    hjust = 1,
    vjust = 1.2,
    size = 1.5,
    color = "black",                  # text color
    fill  = "white",                  # white background
    label.size = 0.8,
    label.padding = unit(0.6, "lines")) +
  labs(title = "",
       x = "Age at Diagnosis (years)",
       y = "Survival (months)") +
  theme_minimal(base_size = 7) +
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black")  # y-axis title
  )+
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  )

print(sfigure_oneD)


layout <- "AB\nCD"

combined <- (sfigure_oneA + sfigure_oneB + sfigure_oneC + sfigure_oneD) +
  plot_layout(design = layout) +
  plot_annotation(
    tag_levels = 'A',
    # Explicitly set the tag size and font style for A, B, C, D
    theme = theme(plot.tag = element_text(size = 7, face = 'bold'))
  )

print(combined)