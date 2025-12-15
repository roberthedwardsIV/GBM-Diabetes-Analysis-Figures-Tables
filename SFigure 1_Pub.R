library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(survival)
library(survminer)

##SFigure 2A
###WT vs. Mutant + Unknown
df <- data.frame(read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3))
df <- df %>% filter(Coded.MRN != 28)
df <- df %>%
  mutate(Diabetes = ifelse(Coded.MRN == 3, 1, Diabetes))

df$Diabetes <- as.factor(df$Diabetes)
diabetic <- df[df['Diabetes'] == 1, ]
diabetic <- diabetic %>%
  mutate(`IDH.Type` = replace(`IDH.Type`, `IDH.Type` == "" | is.na(`IDH.Type`), 0))
df$IDH.Type <- as.factor(df$IDH.Type)
df <- within(df, {
  IDH.Type <- factor(IDH.Type, labels = c("Wild-Type", "Mutant"))
})

IDH_unk_and_WT <- kruskal.test(Survival ~ IDH.Type, data = diabetic)
IDH_unk_and_WT$p.value

sone_A <- surv_fit(Surv(Survival, Vital.Status) ~ IDH.Type, data = diabetic)

oneA_pval <- sprintf('p = %.4f', IDH_unk_and_WT$p.value)

figure_stwoA <- ggsurvplot(sone_A,
                          data = diabetic,
                          pval = oneA_pval,
                          pval.size = 2.5,   # roughly equivalent to 7-pt
                          pval.coord = c(60, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          legend = c(.8, .8),
                          legend.title = "IDH Status",
                          legend.labs = c("Wild-Type", "Unknown (Mutant*)"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.legend.title = c(7, "bold", "black"),
                          font.pval = c(7, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))


figure_stwoA$plot <- figure_stwoA$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank(),            # removes box around legend keys
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "plain")
  )



##SFigure 2B (Treatment Status vs Survival)
diabetic_tx <- kruskal.test(Survival ~ Treatment.Status, data = diabetic)

stwo_A <- surv_fit(Surv(Survival, Vital.Status) ~ Treatment.Status, data = diabetic)


twoB_pval <- sprintf('p = %.4f', diabetic_tx$p.value)

figure_twoB <- ggsurvplot(stwo_A,
                          data = diabetic,
                          pval = twoB_pval,
                          pval.size = 2.5,   # roughly equivalent to 7-pt
                          pval.coord = c(60, 0.25),
                          xlab = "Months",     
                          ylab = "Survival Probability",
                          legend = c(.8, .7),
                          legend.title = "Treatment Status",
                          legend.labs = c("None", "RO Only", "Chemo Only", "ChemoRT Only", "ChemoRT + \u2265 1L"),
                          ggtheme = theme_bw(base_size = 7),       # set base text size to 7 pt
                          font.x = c(7, "plain", "black"),         # x-axis title
                          font.y = c(7, "plain", "black"),         # y-axis title
                          font.tickslab = c(7, "plain", "black"),  # axis tick labels
                          font.legend = c(7, "plain", "black"),    # legend text
                          font.legend.title = c(7, "bold", "black"),
                          font.pval = c(7, "plain", "black"),
                          font.title = c(7, "bold", "black", "center"))


figure_twoB$plot <- figure_twoB$plot +
  theme(
    panel.grid = element_blank(),           # removes all grid lines
    panel.border = element_blank(),         # removes panel border
    axis.line = element_line(),             # keeps axis lines visible
    axis.ticks = element_line(),            # keeps axis ticks
    legend.key = element_blank(),            # removes box around legend keys
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "plain")
  )

figure_twoB$plot

layout <- "AB"

combined <- (figure_stwoA$plot + figure_twoB$plot) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')

print(combined)