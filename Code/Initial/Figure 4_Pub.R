library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)
library(patchwork)



df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df <- df %>%
  mutate(Diabetes = ifelse(Coded.MRN == 3, 1, Diabetes))

df <- df %>%
  mutate(Group = ifelse(Coded.MRN == 3, 4, Group))

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

#Figure 3A (diabetic treatment groups combined)
threeA_fit <- survfit(Surv(Survival, Vital.Status) ~ Group , data = diabetic)
threeA <- kruskal.test(Survival ~ Group, data = diabetic)
threeA_pval <- sprintf('p = %.4f', threeA$p.value)

figure_threeA <- ggsurvplot(threeA_fit,
                          data = diabetic,
                          pval = threeA_pval,
                          pval.size = 2.5,   # roughly equivalent to 7-pt
                          pval.coord = c(30, 0.4),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          xlim = c(0, 40),
                          break.time.by = 10,
                          font.main = c(5, "plain", "black"),
                          palette = c("cyan3", "green3", "purple", "red"),
                          legend = c(.8, .8),
                          legend.title = "",
                          legend.labs = c("Insulin", "NIDM(s)", "Both", "Neither"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black") )

figure_threeA$plot <- figure_threeA$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
#Figure 3B (Insulin vs NIDM)
insulin <- diabetic[diabetic$Group == 1, ]
NIDM <- diabetic[diabetic$Group == 2, ]
both <- diabetic[diabetic$Group == 3, ]
neither <- diabetic[diabetic$Group == 4, ]

threeB_df <- merge.data.frame(insulin, NIDM, all = TRUE)


threeB_fit <- survfit(Surv(Survival, Vital.Status) ~ Insulin , data = threeB_df)
threeB <- kruskal.test(Survival ~ Insulin, data = threeB_df)
threeB_pval <- sprintf('p = %.4f', threeB$p.value)

figure_threeB <- ggsurvplot(threeB_fit,
                          data = threeB_df,
                          pval = threeB_pval,
                          pval.size = 2,   # roughly equivalent to 7-pt
                          pval.coord = c(20, 0.50),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          xlim = c(0, 40),
                          break.time.by = 10,
                          font.main = c(5, "plain", "black"),
                          palette = c("green3", "cyan3"),
                          legend = c(.8, .8),
                          legend.title = "",
                          legend.labs = c("NIDM(s)", "Insulin"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black") )

figure_threeB$plot <- figure_threeB$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
#Figure 3C (Insulin vs Neither)
threeC_df <- merge.data.frame(insulin, neither, all = TRUE)


threeC_fit <- survfit(Surv(Survival, Vital.Status) ~ Insulin , data = threeC_df)
threeC <- kruskal.test(Survival ~ Insulin, data = threeC_df)
threeC_pval <- sprintf('p = %.4f', threeC$p.value)

figure_threeC <- ggsurvplot(threeC_fit,
                            data = threeC_df,
                            pval = threeC_pval,
                            pval.size = 2,   # roughly equivalent to 7-pt
                            pval.coord = c(25, 0.50),
                            xlab = "Months",     
                            ylab = "Survival Probability",
                            xlim = c(0, 40),
                            break.time.by = 10,
                            font.main = c(5, "plain", "black"),
                            palette = c("red", "cyan3"),
                            legend = c(.8, .8),
                            legend.title = "",
                            legend.labs = c("Neither", "Insulin"),
                            ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                            font.x = c(7, "plain", "black"),         # x-axis title
                            font.y = c(7, "plain", "black"),         # y-axis title
                            font.tickslab = c(7, "plain", "black"),  # axis tick labels
                            font.legend = c(7, "plain", "black"),    # legend text
                            font.pval = c(7, "plain", "black") )

figure_threeC$plot <- figure_threeC$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
#Figure 3D (NIDM vs both)
threeD_df <- merge.data.frame(NIDM, both, all = TRUE)


threeD_fit <- survfit(Surv(Survival, Vital.Status) ~ Insulin , data = threeD_df)
threeD <- kruskal.test(Survival ~ Insulin, data = threeD_df)
threeD_pval <- sprintf('p = %.4f', threeD$p.value)

figure_threeD <- ggsurvplot(threeD_fit,
                            data = threeD_df,
                            pval = threeD_pval,
                            pval.size = 2,   # roughly equivalent to 7-pt
                            pval.coord = c(20, 0.50),
                            xlab = "Months",     
                            ylab = "Survival Probability",
                            xlim = c(0, 40),
                            break.time.by = 10,
                            font.main = c(5, "plain", "black"),
                            palette = c("green3", "purple"),
                            legend = c(.8, .8),
                            legend.title = "",
                            legend.labs = c("NIDM(s)", "Both"),
                            ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                            font.x = c(7, "plain", "black"),         # x-axis title
                            font.y = c(7, "plain", "black"),         # y-axis title
                            font.tickslab = c(7, "plain", "black"),  # axis tick labels
                            font.legend = c(7, "plain", "black"),    # legend text
                            font.pval = c(7, "plain", "black") )

figure_threeD$plot <- figure_threeD$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
#Figure 3E (neither vs both)
threeE_df <- merge.data.frame(neither, both, all = TRUE)


threeE_fit <- survfit(Surv(Survival, Vital.Status) ~ Insulin , data = threeE_df)
threeE <- kruskal.test(Survival ~ Insulin, data = threeE_df)
threeE_pval <- sprintf('p = %.4f', threeE$p.value)

figure_threeE <- ggsurvplot(threeE_fit,
                            data = threeE_df,
                            pval = threeE_pval,
                            pval.size = 2,   # roughly equivalent to 7-pt
                            pval.coord = c(25, 0.50),
                            xlab = "Months",     
                            ylab = "Survival Probability",
                            xlim = c(0, 40),
                            break.time.by = 10,
                            font.main = c(5, "plain", "black"),
                            palette = c("red", "purple"),
                            legend = c(.8, .8),
                            legend.title = "",
                            legend.labs = c("Neither", "Both"),
                            ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                            font.x = c(7, "plain", "black"),         # x-axis title
                            font.y = c(7, "plain", "black"),         # y-axis title
                            font.tickslab = c(7, "plain", "black"),  # axis tick labels
                            font.legend = c(7, "plain", "black"),    # legend text
                            font.pval = c(7, "plain", "black") )

figure_threeE$plot <- figure_threeE$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )
#Figure 3F (neither vs NIDM)
threeF_df <- merge.data.frame(neither, NIDM, all = TRUE)

threeF_fit <- survfit(Surv(Survival, Vital.Status) ~ Non.insulin.Medication , data = threeF_df)
threeF <- kruskal.test(Survival ~ Non.insulin.Medication, data = threeF_df)
threeF_pval <- sprintf('p = %.4f', threeF$p.value)

figure_threeF <- ggsurvplot(threeF_fit,
                            data = threeF_df,
                            pval = threeF_pval,
                            pval.size = 2,   # roughly equivalent to 7-pt
                            pval.coord = c(25, 0.50),
                            xlab = "Months",     
                            ylab = "Survival Probability",
                            xlim = c(0, 40),
                            break.time.by = 10,
                            font.main = c(5, "plain", "black"),
                            palette = c("red", "green3"),
                            legend = c(.8, .8),
                            legend.title = "",
                            legend.labs = c("Neither", "NIDM(s)"),
                            ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                            font.x = c(7, "plain", "black"),         # x-axis title
                            font.y = c(7, "plain", "black"),         # y-axis title
                            font.tickslab = c(7, "plain", "black"),  # axis tick labels
                            font.legend = c(7, "plain", "black"),    # legend text
                            font.pval = c(7, "plain", "black") )

figure_threeF$plot <- figure_threeF$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

#Figure 3G (Insulin vs Both)
threeG_df <- merge.data.frame(insulin, both, all = TRUE)

threeG_fit <- survfit(Surv(Survival, Vital.Status) ~ Non.insulin.Medication , data = threeG_df)
threeG <- kruskal.test(Survival ~ Non.insulin.Medication, data = threeG_df)
threeG_pval <- sprintf('p = %.4f', threeG$p.value)

figure_threeG <- ggsurvplot(threeG_fit,
                            data = threeG_df,
                            pval = threeG_pval,
                            pval.size = 2,   # roughly equivalent to 7-pt
                            pval.coord = c(20, 0.50),
                            xlab = "Months",     
                            ylab = "Survival Probability",
                            xlim = c(0, 40),
                            break.time.by = 10,
                            palette = c("cyan3", "purple"),
                            legend = c(.8, .8),
                            legend.title = "",
                            legend.labs = c("Insulin", "Both"),
                            ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                            font.x = c(7, "plain", "black"),         # x-axis title
                            font.y = c(7, "plain", "black"),         # y-axis title
                            font.tickslab = c(7, "plain", "black"),  # axis tick labels
                            font.legend = c(7, "plain", "black"),    # legend text
                            font.pval = c(7, "plain", "black") )

figure_threeG$plot <- figure_threeG$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )















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


##### Figure 2D: Multivariate Analysis (Group Factor)

# 1. Model Calculation: Use the FULL dataset 'df' and the comprehensive 'Group' factor
cox_model_d <- coxph(
  Surv(Survival, Vital.Status) ~ Age.Class + Baseline.KPS + Resection.Status + MGMT.Status + IDH.Type + Group,
  data = df # <-- USE FULL DATASET 'df'
)

res_d <- tidy(cox_model_d, exponentiate = TRUE, conf.int = TRUE)

# 2. Define display_df: Map, Extract Levels, and Filter for display
display_df_d <- res_d %>%
  mutate(
    # Map factor names to clean display names
    Variable = case_when(
      # Target the Group factor
      str_detect(term, "^Group") ~ "Diabetes Treatment Group",
      TRUE ~ "Other Factor"
    ),
    
    # Robustly extract the level name that follows the factor name
    Level = str_replace(term, "^(Age\\.Class|Baseline\\.KPS|Resection\\.Status|MGMT\\.Status|IDH\\.Type|Group)(.*)$", "\\2"),
    
    Level = case_when(
      Level == "Insulin" ~ "Insulin", 
      Level == "NIDM(s)" ~ "NIDM(s)",
      Level == "Both"    ~ "Both",
      Level == "Neither" ~ "Neither",
      TRUE ~ Level
    ),
    
    HR_label = sprintf("HR = %.2f (%.1f–%.1f)",
                       estimate, conf.low, conf.high),
    p_label = ifelse(p.value < 0.0001, "p < 0.0001",
                     sprintf("p = %.4f", p.value))
  ) %>%
  filter(Variable == "Diabetes Treatment Group") %>%
  filter(!is.na(estimate)) %>% 
  select(Variable, Level, estimate, conf.low, conf.high, HR_label, p_label)

# 3. Define ref_df: Create the exact reference row 
# The reference level is the first level of 'Group', which is "Non-diabetic".
ref_df_d <- tibble(
  Variable = c("Diabetes Treatment Group"),
  Level = c("Non-Diabetics"),
  HR_label = "Reference",
  estimate = 1, conf.low = 1, conf.high = 1,
  p_label = ""
)

# 4. Combine and plot
plot_df_d <- bind_rows(ref_df_d, display_df_d) %>%
  mutate(
    # Set the precise order for ALL levels in the plot (reverse order for ggplot)
    Level = factor(Level, levels = rev(c(
      "Non-Diabetics",
      "Insulin",
      "NIDM(s)",
      "Both",
      "Neither"
    ))),
    Variable = factor(Variable, levels = c("Diabetes Treatment Group"))
  ) %>% 
  filter(!is.na(Level))

# Final plotting code
table_2d <- ggplot(plot_df_d, aes(y = Level, x = estimate), title = "Multivariate Analysis of Prognostic Factors (Combined Groups)") +
  # CI bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "lightgray") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2) +
  # Points
  geom_point(aes(color = Level), size = 2) +
  scale_color_manual(
    values = c(
      "Non-Diabetics" = "black",
      "Insulin" = "cyan3",
      "NIDM(s)" = "green3",
      "Both" = "purple",
      "Neither" = "red"
    )
  ) +
  # Add HR + CI text
  geom_text(aes(label = HR_label), vjust = 4, size = 1.8) +
  # Add p-value
  geom_text(aes(label = p_label), size = 1.8,
            vjust = 6, color = "black", fontface = "italic") +
  
  scale_x_log10(limits = c(0.09, 15), n.breaks = 4) +
  labs(x = "Hazard Ratio (log-scale)", y = NULL) +
  facet_grid(Variable ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme_minimal(base_size = 7) +
  theme(
    legend.position = "none",
    strip.text.y.left = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.spacing.y = unit(0, "cm"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) # <--- ADD THIS
    
  )

print(table_2d) # Print the final plot






layout <- "AABB\nCDBB"   # <- exact, rectangular (4 chars per row)

combined <- (figure_threeA$plot + table_2d + figure_threeB$plot + figure_threeC$plot) +
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'A')


print(combined)



