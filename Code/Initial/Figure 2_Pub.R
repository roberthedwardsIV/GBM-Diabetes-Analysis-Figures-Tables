library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)
library(patchwork)



df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))



df$Baseline.KPS[df$Baseline.KPS <= 60] = "0"
df$Baseline.KPS[df$Baseline.KPS >= 90 | df$Baseline.KPS == 100] = "2"
df$Baseline.KPS[df$Baseline.KPS >= 61 & df$Baseline.KPS <= 80] = "1"

df <- df %>% filter(Coded.MRN != 28)
df <- df %>%
  mutate(Diabetes = ifelse(Coded.MRN == 3, 1, Diabetes))


df <- within(df, {
  Diabetes <- factor(Diabetes, labels = c("Non-Diabetic", "Diabetic"))
})

diabetic <- df[df$Diabetes == "Diabetic", ]

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



diabetic <- within(diabetic, {
  Age.Class <- factor(Age.Class, labels = c("<40 years", "41-60 years", "61-80 years", "<80 years"))
  Baseline.KPS <- factor(Baseline.KPS, labels = c("0-60", "70-80", "90-100"))
  Resection.Status <- factor(Resection.Status, labels = c("None", "STR", "GTR"))
  Sex <- factor(Sex, labels = c("Male", "Female"))
  Treatment.Status <- factor (Treatment.Status, labels = c("None", "RT Only", "Chemo Only", "TMZ/RT Only", "TMZ/RT & 1+ Line" ))
  MGMT.Status <- factor(MGMT.Status, labels = c("Unmethylated", "Methylated"))
})
#Figure 1A (diabetics vs non-diabetics)
oneA_fit <- survfit(Surv(Survival, Vital.Status) ~ Diabetes, data = df)
oneA <- kruskal.test(Survival ~ Diabetes, data = df)
oneA_pval <- sprintf('p = %.4f', oneA$p.value)

figure_oneA <- ggsurvplot(oneA_fit,
                       data = df,
                       pval = oneA_pval,
                       pval.size = 2.5,   # roughly equivalent to 7-pt
                       pval.coord = c(60, 0.25),
                       xlab = "Months",     
                       ylab = "Survival Probability",
                       xlim = c(0, 80),
                       break.time.by = 20,
                       legend = c(.8, .8),
                       title = "Population",
                       legend.title = "",
                       legend.labs = c("Non-Diabetic", "Diabetic"),
                       ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                       font.x = c(7, "plain", "black"),         # x-axis title
                       font.y = c(7, "plain", "black"),         # y-axis title
                       font.tickslab = c(7, "plain", "black"),  # axis tick labels
                       font.legend = c(7, "plain", "black"),    # legend text
                       font.pval = c(4, "plain", "black"),
                       font.title = c(7, "bold", "black", "center"))

figure_oneA$plot <- figure_oneA$plot +
  theme(
    plot.title = element_text(hjust = 0.5),  # center the title
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
    )
figure_oneA


#Figure 1B (Diabetics Age Class)
oneB_fit <- survfit(Surv(Survival, Vital.Status) ~ Age.Class, data = diabetic)
oneB <- kruskal.test(Survival ~ Age.Class, data = diabetic)
oneB_pval <- sprintf('p = %.4f', oneB$p.value)

figure_oneB <- ggsurvplot(oneB_fit,
                          data = diabetic,
                          pval = oneB_pval,
                          pval.size = 2,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          title = "Age",
                          legend = c(.7, .8),
                          legend.title = "",
                          legend.labs = c("<40 years", "41-60 years", "61-80 years", ">80 years"),
                          palette = c("red", "green4", "cyan3", "purple2"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))


figure_oneB$plot <- figure_oneB$plot +
  theme(
    plot.title = element_text(hjust = 0.5),  # center the title
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

figure_oneB


#Figure 1C (Diabetics Sex)
oneC_fit <- survfit(Surv(Survival, Vital.Status) ~ Sex, data = diabetic)
oneC <- kruskal.test(Survival ~ Sex, data = diabetic)
oneC_pval <- sprintf('p = %.4f', oneC$p.value)

figure_oneC <- ggsurvplot(oneC_fit,
                          data = diabetic,
                          pval = oneC_pval,
                          pval.size = 2,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          title = "Sex",
                          legend = c(.7, .8),
                          legend.title = "",
                          legend.labs = c("Male", "Female"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))



figure_oneC$plot <- figure_oneC$plot +
  theme(
    plot.title = element_text(hjust = 0.5),  # center the title
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

figure_oneC


#Figure 1D (Diabetics KPS)
oneD_fit <- survfit(Surv(Survival, Vital.Status) ~ Baseline.KPS, data = diabetic)
oneD <- kruskal.test(Survival ~ Baseline.KPS, data = diabetic)
oneD_pval <- sprintf('p = %.4f', oneD$p.value)

figure_oneD <- ggsurvplot(oneD_fit,
                          data = diabetic,
                          pval = oneD_pval,
                          pval.size = 2,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          title = "Baseline KPS",
                          legend = c(.7, .8),
                          legend.title = "",
                          legend.labs = c("0-60", "70-80", "90-100"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))                          



figure_oneD$plot <- figure_oneD$plot +
  theme(
    plot.title = element_text(hjust = 0.5),  # center the title
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

figure_oneD

#Figure 1E (Diabetics Resection)
oneE_fit <- survfit(Surv(Survival, Vital.Status) ~ Resection.Status, data = diabetic)
oneE <- kruskal.test(Survival ~ Resection.Status, data = diabetic)
oneE_pval <- sprintf('p < 0.0001')

figure_oneE <- ggsurvplot(oneE_fit,
                          data = diabetic,
                          pval = oneE_pval,
                          pval.size = 2,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          title = "Resection Status",
                          legend = c(.7, .8),
                          legend.title = "",
                          legend.labs = c("None", "STR", "GTR"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(4, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))



figure_oneE$plot <- figure_oneE$plot +
  theme(
    plot.title = element_text(hjust = 0.5),  # center the title
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

figure_oneE


#Figure 1F (Diabetics MGMT)
oneF_fit <- survfit(Surv(Survival, Vital.Status) ~ MGMT.Status, data = diabetic)
oneF <- kruskal.test(Survival ~ MGMT.Status, data = diabetic)
oneF_pval <- sprintf('p = %.4f', oneF$p.value)

figure_oneF <- ggsurvplot(oneF_fit,
                          data = diabetic,
                          pval = oneF_pval,
                          pval.size = 2,   # roughly equivalent to 7-pt
                          pval.coord = c(40, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          title = "MGMT Status",
                          legend = c(.7, .8),
                          legend.title = "",
                          legend.labs = c("Unmethylated", "Methylated"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(4, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))

figure_oneF$plot <- figure_oneF$plot +
  theme(
    plot.title = element_text(hjust = 0.5),  # center the title
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

figure_oneF


#Figure 1G (Diabetics Ki67)
KI_df <- diabetic[diabetic$KI_Class < 5, ]

oneG_fit <- survfit(Surv(Survival, Vital.Status) ~ KI_Class, data = KI_df)
oneG <- kruskal.test(Survival ~ KI_Class, data = KI_df)
oneG_pval <- sprintf('p = %.4f', oneG$p.value)

figure_oneG <- ggsurvplot(oneG_fit,
                          data = KI_df,
                          pval = oneG_pval,
                          pval.size = 2,   # roughly equivalent to 7-pt
                          pval.coord = c(55, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          title = "Ki-67 Index",
                          legend = c(.7, .7),
                          legend.title = "",
                          legend.labs = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.pval = c(7, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))

figure_oneG$plot <- figure_oneG$plot +
  theme(
    plot.title = element_text(hjust = 0.5),  # center the title
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank()            # removes box around legend keys
  )

figure_oneG

layout <- "AABC\nDEFG"   # <- exact, rectangular (4 chars per row)

combined <- (figure_oneA$plot + figure_oneB$plot + figure_oneC$plot +
               figure_oneD$plot + figure_oneE$plot + figure_oneF$plot + figure_oneG$plot) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')

print(combined)
