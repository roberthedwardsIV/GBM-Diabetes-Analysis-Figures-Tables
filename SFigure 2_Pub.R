library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)

###WT vs. Mutant + Unknown
df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df$Diabetes <- as.factor(df$Diabetes)
diabetic <- df[df['Diabetes'] == 1, ]




###MGMT Unmethylated + Unknown vs. Methylated
df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df$Diabetes <- as.factor(df$Diabetes)
diabetic <- df[df['Diabetes'] == 1, ]
diabetic <- diabetic %>%
  mutate(`MGMT.Status` = replace(`MGMT.Status`, `MGMT.Status` == "" | is.na(`MGMT.Status`), 0))
df$IDH.Type <- as.factor(df$MGMT.Status)
df <- within(df, {
  MGMT.Status <- factor(MGMT.Status, labels = c("Unmethylated", "Methylated"))
})

MGMT_unk_and_unmeth <- kruskal.test(Survival ~ MGMT.Status, data = diabetic)
MGMT_unk_and_unmeth$p.value

threeA <- surv_fit(Surv(Survival, Vital.Status) ~ MGMT.Status, data = diabetic)

threeA_pval <- sprintf('p = %.4f', MGMT_unk_and_unmeth$p.value)

figure_sthreeA <- ggsurvplot(threeA,
                           data = diabetic,
                           pval = threeA_pval,
                           pval.size = 2.5,   # roughly equivalent to 7-pt
                           pval.coord = c(60, 0.25),
                           xlab = "Months",     
                           ylab = "Survival Probability",
                           legend = c(.6, .8),
                           legend.title = "MGMT Status",
                           legend.labs = c("Unknown + Unmethylated (Unmethylated*)", "Methylated"),
                           ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                           font.x = c(7, "plain", "black"),         # x-axis title
                           font.y = c(7, "plain", "black"),         # y-axis title
                           font.tickslab = c(7, "plain", "black"),  # axis tick labels
                           font.legend = c(7, "plain", "black"),    # legend text
                           font.legend.title = c(7, "bold", "black"),
                           font.pval = c(7, "plain", "black"),
                           font.title = c(7, "bold", "black", "center"))


figure_sthreeA$plot <- figure_sthreeA$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank(),            # removes box around legend keys
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "plain")
  )

figure_sthreeA












###MGMT Unmethylated vs. Methylated + Unknown
df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df <- df %>% filter(Coded.MRN != 28)
df <- df %>%
  mutate(Diabetes = ifelse(Coded.MRN == 3, 1, Diabetes))

df$Diabetes <- as.factor(df$Diabetes)
diabetic <- df[df['Diabetes'] == 1, ]
diabetic <- diabetic %>%
  mutate(`MGMT.Status` = replace(`MGMT.Status`, `MGMT.Status` == "" | is.na(`MGMT.Status`), 1))
df$IDH.Type <- as.factor(df$MGMT.Status)
df <- within(df, {
  MGMT.Status <- factor(MGMT.Status, labels = c("Unmethylated", "Methylated"))
})

MGMT_unk_and_meth <- kruskal.test(Survival ~ MGMT.Status, data = diabetic)
MGMT_unk_and_meth$p.value
threeB <- surv_fit(Surv(Survival, Vital.Status) ~ MGMT.Status, data = diabetic)



threeB_pval <- sprintf('p = %.4f', MGMT_unk_and_meth$p.value)

figure_sthreeB <- ggsurvplot(threeB,
                             data = diabetic,
                             pval = threeB_pval,
                             pval.size = 2.5,   # roughly equivalent to 7-pt
                             pval.coord = c(60, 0.25),
                             xlab = "Months",     
                             ylab = "Survival Probability",
                             legend = c(.6, .8),
                             legend.title = "MGMT Status",
                             legend.labs = c("Unmethylated", "Unknown + Methylated (Methylated*)"),
                             ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                             font.x = c(7, "plain", "black"),         # x-axis title
                             font.y = c(7, "plain", "black"),         # y-axis title
                             font.tickslab = c(7, "plain", "black"),  # axis tick labels
                             font.legend = c(7, "plain", "black"),    # legend text
                             font.legend.title = c(7, "bold", "black"),
                             font.pval = c(7, "plain", "black"),
                             font.title = c(7, "bold", "black", "center"))


figure_sthreeB$plot <- figure_sthreeB$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank(),            # removes box around legend keys
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "plain")
  )

figure_sthreeB




layout <- "AB"

combined <- (figure_sthreeA$plot + figure_sthreeB$plot) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')

print(combined)
