library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)
library(patchwork)



df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df <- df %>% filter(Coded.MRN != 28)
df <- df %>%
  mutate(Diabetes = ifelse(Coded.MRN == 3, 1, Diabetes))


df$Diabetes...Insulin.Status[df$Coded.MRN == 3] <- 1

diabetic <- df[df$Diabetes == 1, ]

diabetic <- diabetic %>%
  mutate(
    KI_Class = case_when(
      is.na(`Ki.67`) ~ 5,                              # Unknowns → 5
      `Ki.67` >= 0  & `Ki.67` <= 20  ~ 0,               # 0–20%
      `Ki.67` >= 21 & `Ki.67` <= 40  ~ 1,               # 21–40%
      `Ki.67` >= 41 & `Ki.67` <= 60  ~ 2,               # 41–60%
      `Ki.67` >= 61 & `Ki.67` <= 80  ~ 3,               # 61–80%
      `Ki.67` >= 81 & `Ki.67` <= 100 ~ 4,               # 81–100%
      TRUE ~ 5                                      # Any other unexpected values → 5
    )  )

diabetic <- diabetic %>%
  mutate(
    KPS_Class = case_when(
      `Baseline.KPS` > 0  & `Baseline.KPS` <= 60  ~ 0, 
      `Baseline.KPS` >= 61  & `Baseline.KPS` <= 80  ~ 1,               
      `Baseline.KPS` >= 81  & `Baseline.KPS` <= 100  ~ 2,                
    )  )

df <- df %>%
  mutate(
    Insulin.Status = case_when(
     is.na(Diabetes...Insulin.Status) ~ 0,
     
    )
  )

#Figure 2A (diabetics insulin status combined)
twoA_fit <- survfit(Surv(Survival, Vital.Status) ~ Diabetes...Insulin.Status , data = df)
twoA <- kruskal.test(Survival ~ Diabetes...Insulin.Status, data = df)
twoA_pval <- sprintf('p = %.4f', twoA$p.value)

figure_twoA <- ggsurvplot(twoA_fit,
                          data = df,
                          pval = twoA_pval,
                          pval.size = 3,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          xlim = c(0, 80),
                          break.time.by = 20,
                          font.main = c(12, "plain", "black"),
                          palette = c("red", "green3", "royalblue1"),
                          legend = c(.8, .8),
                          title = " ",
                          legend.title = "",
                          legend.labs = c("Non-Diabetic", "No Insulin Diabetic", "Insulin Diabetic"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))

figure_twoA$plot <- figure_twoA$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
#Figure 2B (Non-Diabetic vs. Insulin Diabetic)
nondiabetic <- df[df$Diabetes == 0, ]
insulindiabetic <- diabetic[diabetic$Insulin == 1, ]
twoB_df <- merge.data.frame(nondiabetic, insulindiabetic, all = TRUE)


twoB_fit <- survfit(Surv(Survival, Vital.Status) ~ Insulin , data = twoB_df)
twoB <- kruskal.test(Survival ~ Insulin, data = twoB_df)
twoB_pval <- sprintf('p = %.4f', twoB$p.value)

figure_twoB <- ggsurvplot(twoB_fit,
                          data = twoB_df,
                          pval = twoB_pval,
                          pval.size = 3,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          xlim = c(0, 80),
                          break.time.by = 20,
                          font.main = c(12, "plain", "black"),
                          palette = c("red", "royalblue1"),
                          legend = c(.6, .8),
                          title = " ",
                          legend.title = "",
                          legend.labs = c("Non-Diabetic", "Insulin Diabetic"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black") )


figure_twoB$plot <- figure_twoB$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
#Figure 2C (No Insulin Diabetic vs. Insulin Diabetic)
twoC_fit <- survfit(Surv(Survival, Vital.Status) ~ Insulin , data = diabetic)
twoC <- kruskal.test(Survival ~ Insulin, data = diabetic)
twoC_pval <- sprintf('p = %.4f', twoC$p.value)

figure_twoC <- ggsurvplot(twoC_fit,
                          data = diabetic,
                          pval = twoC_pval,
                          pval.size = 3,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          xlim = c(0, 80),
                          break.time.by = 20,
                          font.main = c(12, "plain", "black"),
                          palette = c("green3", "royalblue1"),
                          legend = c(.6, .8),
                          title = " ",
                          legend.title = "",
                          legend.labs = c("No Insulin Diabetic", "Insulin Diabetic"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black") )

figure_twoC$plot <- figure_twoC$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

#Figure 2D (Non-Diabetic vs. Non-Insulin Diabetic)
nondiabetic <- df[df$Diabetes == 0, ]
noinsulindiabetic <- diabetic[diabetic$Insulin == 0, ]
twoD_df <- merge.data.frame(nondiabetic, noinsulindiabetic, all = TRUE)


twoD_fit <- survfit(Surv(Survival, Vital.Status) ~ Diabetes , data = twoD_df)
twoD <- kruskal.test(Survival ~ Diabetes, data = twoD_df)
twoD_pval <- sprintf('p = %.4f', twoD$p.value)

figure_twoD <- ggsurvplot(twoD_fit,
                          data = twoD_df,
                          pval = twoD_pval,
                          pval.size = 3,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          xlim = c(0, 80),
                          break.time.by = 20,
                          font.main = c(12, "plain", "black"),
                          palette = c("red", "green3"),
                          legend = c(.6, .8),
                          title = " ",
                          legend.title = "",
                          legend.labs = c("Non-Diabetic", "No Insulin Diabetic"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black") )

figure_twoD$plot <- figure_twoD$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
















#Figure 2C
library(tidyverse)
library(readxl)
library(survival)
library(broom)
library(stringr)
library(patchwork)
library(ggplot2)
library(dplyr)


# --- 1. Data Loading and Preparation (with simplified factor labels) ---
df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df <- df %>% filter(Coded.MRN != 28)
df <- df %>%
  mutate(Diabetes = ifelse(Coded.MRN == 3, 1, Diabetes))

#### converting datatypes to factors as needed
df$Diabetes <- as.factor(df$Diabetes)
df$Resection.Status <- as.factor(df$Resection.Status)
df$MGMT.Status <- as.factor(df$MGMT.Status)
df$Age.Class <- as.factor(df$Age.Class)
df$Baseline.KPS[df$Baseline.KPS <= 60] = "0"
df$Baseline.KPS[df$Baseline.KPS >= 90 | df$Baseline.KPS == 100] = "2"
df$Baseline.KPS[df$Baseline.KPS >= 61 & df$Baseline.KPS <= 80] = "1"
df$Baseline.KPS <- as.factor(df$Baseline.KPS)
df$Ki.67[df$Ki.67 <= 20] = "0"
df$Ki.67[df$Ki.67 <= 40 & df$Ki.67 >= 21] = "1"
df$Ki.67[df$Ki.67 <= 60 & df$Ki.67 >= 41] = "2"
df$Ki.67[df$Ki.67 <= 80 & df$Ki.67 >= 61] = "3"
df$Ki.67[df$Ki.67 <= 100 & df$Ki.67 >= 81 | df$Ki.67 == 90 | df$Ki.67 == 95 | df$Ki.67 == 100] = "4"
df$Ki.67 <- as.factor(df$Ki.67)
df$Sex <- as.factor(df$Sex)
df$Treatment.Status <- as.factor(df$Treatment.Status)
df$IDH.Type <- as.factor(df$IDH.Type)
df$Insulin.Dose.Level <- as.factor(df$Insulin.Dose.Level)

#### extracting selected patients
df <- within(df, {
  Diabetes <- factor(Diabetes, labels = c("Non-Diabetic", "Diabetic"))
  Diabetes...Insulin.Status <- factor(Diabetes...Insulin.Status, labels = c("Non-diabetic", "No Insulin", "Insulin"))
  Age.Class <- factor(Age.Class, 
                      levels = c("0", "1", "2", "3"), 
                      labels = c("<40yrs", "40-60yrs", "61-80yrs", "81+yrs"))
  Baseline.KPS <- factor(Baseline.KPS, labels = c("0-60", "70-80", "90-100"))
  Resection.Status <- factor(Resection.Status, labels = c("None", "STR", "GTR"))
  Group <- factor(Group, labels = c("Non-diabetic", "Insulin", "NIDM(s)", "Both", "Neither"))
  Sex <- factor(Sex, labels = c("Male", "Female"))
  Treatment.Status <- factor (Treatment.Status, labels = c("None", "RT Only", "Chemo Only", "TMZ/RT Only", "TMZ/RT & 1+ Line" ))
  MGMT.Status <- factor(MGMT.Status, labels = c("UNM", "ME"))
  IDH.Type <- factor(IDH.Type, labels = c("WT", "Mutant"))
  Ki.67 <- factor(Ki.67, labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%"))
})

diabetic <- df[df$Diabetes == "Diabetic", ]
nondiabetic <- df[df$Diabetes == "Non-Diabetic", ]


# --- MODEL 1: Diabetes Status (Diabetic vs Non-Diabetic) ---
# Goal: Get one HR for "Diabetic" vs "Non-Diabetic (Ref)".
cox_model_1 <- coxph(
  Surv(Survival, Vital.Status) ~ Diabetes + Age.Class + Baseline.KPS + Resection.Status + MGMT.Status + IDH.Type,
  data = df # MUST use full data 'df'
)

res_1 <- tidy(cox_model_1, exponentiate = TRUE, conf.int = TRUE)

# Filter and process Model 1 results (only Diabetes is needed)
df_diabetes_status <- res_1 %>%
  filter(str_detect(term, "^Diabetes")) %>% # Filter for the single "Diabetic" row
  mutate(
    Variable = "Diabetes Status",
    # The term should be "DiabetesDiabetic" based on your factor labels
    Level = "Diabetics (All)",
    HR_label = sprintf("HR = %.2f (%.1f–%.1f)", estimate, conf.low, conf.high),
    p_label = ifelse(p.value < 0.0001, "p < 0.0001", sprintf("p = %.4f", p.value))
  ) %>%
  select(Variable, Level, estimate, conf.low, conf.high, HR_label, p_label)

# Add reference row for Model 1
ref_diabetes_status <- tibble(
  Variable = "Diabetes Status",
  Level = "Non-Diabetics",
  HR_label = "Reference",
  estimate = 1, conf.low = 1, conf.high = 1, p_label = ""
)

# --- MODEL 2: Insulin Status (Subtypes vs Non-Diabetic) ---

cox_model_2 <- coxph(
  Surv(Survival, Vital.Status) ~ `Diabetes...Insulin.Status` + Age.Class + Baseline.KPS + Resection.Status + MGMT.Status + IDH.Type,
  data = df # MUST use full data 'df'
)

res_2 <- tidy(cox_model_2, exponentiate = TRUE, conf.int = TRUE)

# Filter and process Model 2 results (only Insulin Status is needed)
df_insulin_status <- res_2 %>%
  filter(str_detect(term, "^Diabetes...Insulin.Status")) %>%
  mutate(
    Variable = "Insulin Status",
    # Robustly extract level name and map to desired labels
    Level = str_replace(term, "^Diabetes...Insulin\\.Status", ""),
    Level = case_when(
      Level == "No Insulin" ~ "No Insulin", 
      Level == "Insulin"    ~ "Insulin",
      TRUE ~ Level
    ),
    HR_label = sprintf("HR = %.2f (%.1f–%.1f)", estimate, conf.low, conf.high),
    p_label = ifelse(p.value < 0.0001, "p < 0.0001", sprintf("p = %.4f", p.value))
  ) %>%
  select(Variable, Level, estimate, conf.low, conf.high, HR_label, p_label)

# Add reference row for Model 2 (This is redundant but needed for the facet structure)
ref_insulin_status <- tibble(
  Variable = "Insulin Status",
  Level = "Non-diabetics (Ref)", # This row will share the y-axis with the Diabetes Status ref
  HR_label = "Reference",
  estimate = 1, conf.low = 1, conf.high = 1, p_label = ""
)


# --- COMBINE DATA FOR PLOTTING ---
plot_df_2c <- bind_rows(
  ref_diabetes_status, 
  df_diabetes_status, 
  ref_insulin_status,
  df_insulin_status
) %>%
  # Define the order of the final levels for plotting
  mutate(
    Level = factor(Level, levels = rev(c(
      "Non-Diabetics", 
      "Diabetics (All)",
      "No Insulin",
      "Insulin"
    ))),
    # Set the Variable factor levels to control facet order
    Variable = factor(Variable, levels = c("Diabetes Status", "Insulin Status")) 
  ) %>% 
  # Filter out the redundant reference row for the Insulin Status facet, 
  # keeping only the pooled reference line.
  filter(!(Variable == "Insulin Status" & Level == "Non-Diabetics")) %>%
  filter(!is.na(Level))


# --- FINAL PLOT CODE ---
table_2c <- ggplot(plot_df_2c, aes(y = Level, x = estimate), title = "Multivariate Analysis of Prognostic Factors (Diabetes Status)") +
  # CI bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "lightgray") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2) +
  # Points
  geom_point(aes(color = Level), size = 2) +
  scale_color_manual(
    values = c(
      "Non-Diabetics" = "red",
      "Diabetics (All)" = "purple",
      "No Insulin" = "green3",
      "Insulin" = "royalblue1"
    )
  ) +
  # Add HR + CI text
  geom_text(aes(label = HR_label), vjust = 2, size = 1.8) +
  # Add p-value
  geom_text(aes(label = p_label), size = 1.8,
            vjust = 4, color = "black", fontface = "italic") +
  
  scale_x_log10(limits = c(0.1, 6)) +
  labs(x = "Hazard Ratio (log-scale)", y = NULL) +
  # Facet the two groups
  facet_grid(Variable ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme_minimal(base_size = 7) +
  theme(
    legend.position = "none",
    strip.text.y.left = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.spacing.y = unit(0.02, "cm"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) # <--- ADD THIS
    
  )





layout <- "AAB\nCDE"   # <- exact, rectangular (4 chars per row)

combined <- (figure_twoA$plot + table_2c + figure_twoB$plot + figure_twoC$plot + figure_twoD$plot) +
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'A')


print(combined)