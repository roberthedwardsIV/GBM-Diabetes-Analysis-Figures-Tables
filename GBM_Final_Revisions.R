# Paper Title: Insulin Use Negatively Impacts Glioblastoma Patient Survival (BJC Reports)
# Code Author: Robert Edwards
# Last Edited: 4/27/2026


# - 868 total patients
# - 142 diabetic patients 
# - 725 non-diabetic patients (715 w/ the 10 IDH patients removed)


# package imports ---------------------------------------------------------------------
# built-in
library(dplyr)
library(readxl)
library(stringr)
library(gtsummary)
library(gt)
library(ggtext)
library(parameters)
library(ggplot2)
library(scales)
library(mice)
library(patchwork)
library(tidyverse)
library(forcats)
library(flextable)
library(officer)
library(gdtools)


# external
library(survival)
library(survminer)
library(broom.helpers)
library(tidyr)
library(purrr)
library(broom)


# data import + formatting ------------------------------------------------------------
df <- read_excel("Diabetics.xlsx")
df$diabetes <- 1

tmp_df <- read_excel("Non-diabetics.xlsx")
tmp_df <- tmp_df %>% rename(diabetes = Diabetes, 
                            Baseline_KPS = `Baseline KPS`, 
                            IDH_Type = `IDH-Type`, 
                            MGMT_Status = `MGMT Status`)

all_patients <- bind_rows(
  df %>% mutate(Baseline_KPS = as.numeric(Baseline_KPS)),
  tmp_df %>% mutate(Ki67 = as.numeric(Ki67))
)

all_patients <- all_patients %>%
  mutate(
    `Sex` = factor(`Sex`, levels=c(1,2), labels=c("Male","Female")),
    `Resection Status` = factor(`Resection_Status`, levels=c(0,1,2), labels=c("None", "STR","GTR")),
    `Treatment Status` = factor(`Treatment_Status`, levels=c(0,1,2,3,4), labels=c("None","RT Only", "Chemotherapy Only", "TMZ/RT Only", "TMZ/RT & 1+ Line")),
    `MGMT Status` = factor(`MGMT_Status`, levels=c(0,1,2), labels=c("Unknown","Methylated", "Unmethylated")),
    `IDH Type` = factor(`IDH_Type`, levels=c(0,1,2), labels=c("Unknown", "Wild-Type", "Mutant"))
  )

all_patients <- all_patients %>%
  mutate(
    `Age (yrs.)` = case_when(
      `Age_At_Diagnosis` >= 0  & `Age_At_Diagnosis` < 40  ~ 0,            
      `Age_At_Diagnosis` >= 40 & `Age_At_Diagnosis` <= 60  ~ 1,          
      `Age_At_Diagnosis` > 60 & `Age_At_Diagnosis` <= 80  ~ 2,
      `Age_At_Diagnosis` > 80 ~ 3
    ),
    `Age (yrs.)` = factor(`Age (yrs.)`, levels = 0:3, labels = c("<40", "41-60", "61-80", ">80"))
  )

all_patients <- all_patients %>%
  mutate(
    `Baseline KPS` = case_when(
      is.na(`Baseline_KPS`) ~ 0,                              
      `Baseline_KPS` >= 0  & `Baseline_KPS` <= 60  ~ 1,            
      `Baseline_KPS` >= 70 & `Baseline_KPS` <= 80  ~ 2,          
      `Baseline_KPS` >= 90 & `Baseline_KPS` <= 100  ~ 3,          
      TRUE ~ 0                                      
    ),
    `Baseline KPS` = factor(`Baseline KPS`, levels = 0:3, labels = c("Unknown", "0-60", "70-80", "90-100"))
  )

all_patients <- all_patients %>%
  mutate(
    `KI67` = case_when(
      is.na(`Ki67`) ~ 0,                              
      `Ki67` >= 0  & `Ki67` <= 20  ~ 1,            
      `Ki67` >= 21 & `Ki67` <= 40  ~ 2,          
      `Ki67` >= 41 & `Ki67` <= 60  ~ 3,          
      `Ki67` >= 61 & `Ki67` <= 80  ~ 4,              
      `Ki67` >= 81 & `Ki67` <= 100 ~ 5,              
      TRUE ~ 0                                      
    ),
    `KI67` = factor(`KI67`, levels = 0:5, labels = c("Unknown", "0-20%", "21-40%", "41-60%", "61-80%", "81-100%"))
  )

all_patients <- all_patients %>%
  mutate(diabetes = if_else(diabetes == 1, "Diabetic", "Non-Diabetic"))

#Update: removal of 10 IDH-Mutant patients from Non-Diabetic Population
all_patients <- all_patients %>% filter(`IDH Type`!="Mutant")


# patient population slicing -----------------------------------------------------------
non_diabetic <- all_patients %>% filter(diabetes=="Non-Diabetic")
diabetic <- all_patients %>% filter(diabetes=="Diabetic")

ins_diabetic <- diabetic %>% filter(Post_Diagnosis_Insulin==1)
no_ins_diabetic <- diabetic %>% filter(Post_Diagnosis_Insulin==0)

insulin <- ins_diabetic %>% filter(NIDM==0)
NIDM <- no_ins_diabetic %>% filter(NIDM==1)
neither <- no_ins_diabetic %>% filter(NIDM==0)
both <- ins_diabetic %>% filter(NIDM==1)

ins_dosing_available <- ins_diabetic %>% filter(Ins_Dosing_Info_Avail==1)
tum_meas_available <- ins_dosing_available %>% filter(Eligible_for_scan_analysis==1) 




# Table 1: Demographics Summary --------------------------------------------------------------------------------
table1 <-
  tbl_summary(
    all_patients %>% dplyr::mutate(diabetes = factor(diabetes, levels = c("Non-Diabetic", "Diabetic"), labels = c("Non-Diabetics", "Diabetics"))),
    include = c(`Age (yrs.)`, `Age_At_Diagnosis`, `Sex`, `Baseline KPS`, `Baseline_KPS`, `Resection Status`, 
                `MGMT Status`, `KI67`, `Ki67`, `Treatment Status`),
    by = diabetes,
    missing = "no",
    type = all_continuous() ~ "continuous", 
    statistic = all_continuous() ~ "{median} ({min}, {max})",
    digits = list(
      `Age_At_Diagnosis` ~ function(x) ifelse(x < 1, "<1", as.character(round(x, 0)))
    ),
    label = list(
      `Age (yrs.)` ~ "Age (years)",
      KI67 ~ "Ki-67 Index",
      `Baseline KPS` ~ "Baseline KPS",
      `MGMT Status` ~ "*MGMT* Status"
    )
  ) |> 
  add_p(
    test = list(all_categorical() ~ "chisq.test", all_continuous() ~ "wilcox.test")
  ) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**{level}**<br>n = {n}<sup>1</sup>",
    p.value = "**p value**"
  ) |>
  bold_labels() |>
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = ifelse(grepl("80", label) & !grepl("61", label), "&gt;80", label),
        label = ifelse(var_type == "continuous", "Median", label),
        row_type = ifelse(var_type == "continuous", "level", row_type),
        p_fmt = ifelse(is.na(p.value), NA_character_, 
                       ifelse(p.value < 0.0001, "<0.0001", format(round(p.value, 4), nsmall = 4))),
        p.value = dplyr::case_when(
          !is.na(p.value) & var_type %in% c("categorical", "dichotomous") ~ paste0(p_fmt, "<sup>2</sup>"),
          !is.na(p.value) & var_type == "continuous" ~ paste0(p_fmt, "<sup>3</sup>"),
          TRUE ~ p_fmt
        )
      ) %>%
      dplyr::select(-p_fmt)
  ) |>
  modify_fmt_fun(p.value ~ function(x) x) |> 
  modify_column_indent(
    columns = label,
    rows = var_type == "continuous",
    indent = 4 
  )

table1 %>%
  as_gt() %>%
  fmt_markdown(columns = c(label, p.value)) %>%
  tab_header(
    title = md("**Table 1:** Demographic data and univariate analysis on non-diabetic and diabetic patients.")
  ) %>%
  tab_style(
    style = cell_text(v_align = "middle"),
    locations = cells_column_labels(columns = p.value)
  ) %>%
  tab_source_note(
    source_note = md("<sup>1</sup> n (%); Median (Min, Max)<br><sup>2</sup> Pearson's Chi-squared test<br><sup>3</sup> Wilcoxon rank sum test<br><br>KPS = Karnofsky Performance Scale; STR = Subtotal Resection; GTR = Gross Total Resection<br>MGMT = O<sup>6</sup>-methylguanine-DNA methyltransferase; RT = Radiation Therapy; TMZ = Temozolomide")
  ) %>%
  tab_options(
    table.font.size = px(11),        
    data_row.padding = px(2),         
    row_group.padding = px(3),       
    summary_row.padding = px(2)
  ) %>%
  gtsave(
    filename = "Table 1.png",
    vheight = 1500,                  
    vwidth = 1000,                    
    expand = 10
  )
# Table 2: Multivariate survival analysis of prognostic factors (continuous age)  -----------------------------
format_p <- function(x) { ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x)) }

shared_labels_s1 <- list(
  Age_At_Diagnosis     ~ "Age at Diagnosis (Continuous)", 
  Baseline_KPS         ~ "Baseline KPS (Continuous)",
  `Resection Status`   ~ "Resection Status",
  `MGMT Status`        ~ "MGMT Status",
  `Treatment Status`   ~ "Treatment Status"
)

all_patients$`MGMT Status` <- relevel(factor(all_patients$`MGMT Status`), ref = "Unmethylated")

all_patients$`Treatment Status` <- factor(
  all_patients$`Treatment Status`, 
  levels = c("None", "RT Only", "Chemotherapy Only", "TMZ/RT Only", "TMZ/RT & 1+ Line")
)

supp_t1_cox_nondiab <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = all_patients,
  subset = (diabetes == "Non-Diabetic")
)

supp_t1_tbl_nondiab <- tbl_regression(
  supp_t1_cox_nondiab, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

supp_t1_cox_diab <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = all_patients,
  subset = (diabetes == "Diabetic")
)

supp_t1_tbl_diab <- tbl_regression(
  supp_t1_cox_diab, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

tbl_merge(
  tbls = list(supp_t1_tbl_nondiab, supp_t1_tbl_diab),
  tab_spanner = c("**Non-Diabetics**", "**Diabetics**")
) %>%
  modify_header(
    label = "**Prognostic Factor**",
    p.value_1 = "**p value**", 
    p.value_2 = "**p value**"
    ) %>%
  bold_labels() %>%
  remove_abbreviation() %>%
  as_gt() %>%
  tab_header(
    title = md("**Table 2:** Multivariate survival analysis of prognostic factors on non-diabetic and diabetic patients.")
  ) %>%
  tab_options(
    table.font.size = px(12),        
    data_row.padding = px(3),        
    row_group.padding = px(4),
    heading.align = "left"
  ) %>%
  tab_source_note(
    source_note = md("Abbreviations: HR = Hazard Ratio; CI = Confidence Interval; KPS = Karnofsky Performance Status; MGMT = O<sup>6</sup>-methylguanine-DNA methyltransferase; STR = Subtotal Resection; GTR = Gross Total Resection; RT = Radiation Therapy; TMZ = Temozolomide.")
  ) %>%
  gtsave(filename = "Table 2.png", vwidth = 800, expand = 10)












# Figure 1: Population age analysis/correlations --------------------------------
fig1_patients <- all_patients %>%
  dplyr::mutate(diabetes = factor(diabetes, levels = c("Non-Diabetic", "Diabetic"), labels = c("Non-Diabetics", "Diabetics")))

figure_oneA <- ggplot(fig1_patients, aes(x=diabetes, y=Age_At_Diagnosis, fill=diabetes)) +
  geom_jitter(width=0.15, alpha=0.2, size=0.8) +          
  geom_violin(trim=FALSE, alpha=0.3, color="black") + 
  labs(title="", x=" ", y="Age (years)", fill="Group") +
  theme_minimal(base_size = 7) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 7, face = "bold", color = "black"),
    legend.key.height = unit(0.2, "cm"),
    legend.text = element_text(size = 5, color = "black"),
    axis.text = element_text(size = 7, face = "plain", color = "black"),
    axis.label = element_text(size = 7, face = "plain", color = "black"),
    text = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

summary_within <- fig1_patients %>%
  count(diabetes, `Age (yrs.)`) %>%
  group_by(diabetes) %>%
  mutate(percent_within = n / sum(n) * 100) %>%
  ungroup()

label_data <- summary_within %>%
  mutate(`Age (yrs.)` = fct_rev(`Age (yrs.)`)) %>%
  arrange(diabetes, `Age (yrs.)`) %>%
  group_by(diabetes) %>%
  mutate(csum = cumsum(percent_within)) %>%
  mutate(label_y_position = csum - (percent_within / 2)) %>%
  ungroup() %>%
  mutate(`Age (yrs.)` = fct_rev(`Age (yrs.)`))

figure_oneB <- ggplot(summary_within, aes(x = diabetes, y = percent_within, fill = `Age (yrs.)`)) +
  geom_col(position = "stack", width = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  geom_text(data = label_data, aes(y = label_y_position, label = paste0(round(percent_within, 0), "%")),
            hjust = 0,    
            nudge_x = 0.4, 
            size = 2,
            colour = "black") +
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
    legend.position = "top",
    legend.title = element_text(size = 7, face = "bold", color = "black"),
    legend.key.height = unit(0.2, "cm"),
    legend.text = element_text(size = 5, color = "black"),
    axis.text = element_text(size = 7, face = "plain", color = "black"),
    axis.label = element_text(size = 7, face = "plain", color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

nd_age <- non_diabetic$Age_At_Diagnosis
nd_survival <- non_diabetic$Survival_months

test <- cor.test(nd_age, nd_survival, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value) 
if (test$p.value < 0.0001) {correlation_p <- "p < 0.0001"}
correlation_label <- paste("PCC =", correlation_r, "     ", correlation_p)
linear_model <- lm(nd_survival ~ nd_age, data = non_diabetic)
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

figure_oneC <- ggplot(non_diabetic, aes(x = nd_age, y = nd_survival)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = nd_age, y = nd_survival, label = label),
    hjust = 1,
    vjust = 1.2,
    size = 2,
    color = "black",                  
    fill  = "white",                 
    label.size = 1,
    label.padding = unit(0.6, "lines")) +
  labs(title = "",
       x = "Age at Diagnosis (years)",
       y = "Survival (months)") +
  theme_minimal(base_size = 7) +
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black"),  # y-axis title
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )+
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  )

d_age <- diabetic$Age_At_Diagnosis
d_survival <- diabetic$Survival_months

test <- cor.test(d_age, d_survival, method = "pearson", use = "pairwise.complete.obs")
correlation_r <- sprintf(" %.4f", test$estimate)
correlation_p <- sprintf("p = %.4f", test$p.value)
if (test$p.value < 0.0001) {correlation_p <- "p < 0.0001"}
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

figure_oneD <- ggplot(diabetic, aes(x = d_age, y = d_survival)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_label(
    data = label_df,
    aes(x = d_age, y = d_survival, label = label),
    hjust = 1,
    vjust = 1.2,
    size = 2,
    color = "black",                 
    fill  = "white",                
    label.size = 1.0,
    label.padding = unit(0.6, "lines")) +
  labs(title = "",
       x = "Age at Diagnosis (years)",
       y = "Survival (months)") +
  theme_minimal(base_size = 7) + 
  theme(
    axis.text.x  = element_text(size = 7, color = "black"),  # x-axis tick labels
    axis.text.y  = element_text(size = 7, color = "black"),  # y-axis tick labels
    axis.title.x = element_text(size = 7, color = "black"),  # x-axis title
    axis.title.y = element_text(size = 7, color = "black"),  # y-axis title
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )+
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  )

layout <- "AB\nCD"

figure_legend <- paste0(
  "**Figure 1**: Age and survival in non-diabetic and diabetic patients with glioblastoma. ",
  "Age distribution and median age between non-diabetics and \n",
  "diabetics **(A)**. Categorical age distribution in non-diabetics according to age <40, 41-60, 61-80 and >80 years **(B)**. Pearson correlation between \n",
  "age and survival in non-diabetics **(C)** and diabetics **(D)**."
)

combined <- (figure_oneA + figure_oneB + figure_oneC + figure_oneD) +
  plot_layout(design = layout, guides = 'keep') +
  plot_annotation(
    tag_levels = 'A',
    title = 'Figure 1',
    caption = figure_legend) &
  theme(
    plot.tag = element_text(size = 10, face = 'bold', color = "black"),
    plot.title = element_text(size = 11, face = "bold", color = "black"),
    plot.caption = element_textbox_simple(
      size = 10,
      lineheight = 1.2,
      padding = margin(t = 5, r = 5, b = 5, l = 5),
      margin = margin(t = 10, r = 0, b = 0, l = 0)
    ),      
    plot.margin = margin(0, 5, 0, 5)
  )

print(combined)

ggsave(
  filename = "Figure 1.png",
  plot = combined,
  device = "png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)

# Figure 2: Univariate survival analysis of prognostic factors -----------------------------------------------------------
fig2_km_helper <- function(fit, data_used, title_label, labs, tag_label, p_coord = c(48, 0.5), palette_name = "npg") {
  n_table <- summary(fit)$table
  if (is.matrix(n_table)) { ns <- n_table[, "records"] } else { ns <- n_table["records"] }
  clean_labels <- paste0(labs, " (n=", ns, ")")
  pval_df <- surv_pvalue(fit, data = data_used)
  p_val <- pval_df$pval
  if (p_val < 0.0001) {
    p_val_str <- "p < 0.0001"
  } else {
    p_val_str <- paste0("p = ", format(round(p_val, 4), nsmall = 4, scientific = FALSE))
  }
  
  p <- ggsurvplot(
    fit,
    data = data_used,
    conf.int = TRUE,
    pval = p_val_str,
    pval.coord = p_coord,
    palette = palette_name,
    legend.title = title_label,
    legend.labs = clean_labels,    
    legend = c(0.99, 0.95), 
    xlim = c(0, 80),
    xlab = "Time (months)",
    break.time.by = 6,
    ggtheme = theme_bw() + theme(
      plot.title = element_text(face = "bold", hjust = 0.5), 
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold"),
      legend.justification = c("right", "top"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  )
  p$plot <- p$plot + 
    labs(tag = tag_label) +
    theme(plot.tag = element_text(face = "bold", size = 16))
  
  return(p)
}


fit_A <- survfit(Surv(Survival_months, Status) ~ diabetes, data = all_patients)
pA <- fig2_km_helper(fit_A, all_patients, "Population", c("Diabetic", "Non-Diabetic"), tag_label = "A", p_coord = c(48, 0.5))
mos_stats <- surv_median(fit_A)
mos_text <- paste0(
  "**Diabetic median OS**: *", sprintf("%.1f", mos_stats$median[1]), " months (95%CI:", sprintf("%.1f", mos_stats$lower[1]), "-", sprintf("%.1f", mos_stats$upper[1]), ")*<br>",
  "**Non-Diabetic median OS**: *", sprintf("%.1f", mos_stats$median[2]), " months (95%CI:", sprintf("%.1f", mos_stats$lower[2]), "-", sprintf("%.1f", mos_stats$upper[2]), ")*"
)

pA$plot <- pA$plot + 
  annotate("richtext", x = 18, y = 0.8, label = mos_text, size = 6, hjust = 0, fill = NA, label.color = NA)

names(diabetic)[names(diabetic) == "Age (yrs.)"] <- "Age_Class"
fit_B <- survfit(Surv(Survival_months, Status) ~ Age_Class, data = diabetic)
pB <- fig2_km_helper(fit_B, diabetic, "Age (yrs.)", c("<40", "41-60", "61-80", ">80"), tag_label = "B")

fit_C <- survfit(Surv(Survival_months, Status) ~ Sex, data = diabetic)
pC <- fig2_km_helper(fit_C, diabetic, "Sex", c("Male", "Female"), tag_label = "C")

diabetic_kps <- diabetic %>% 
  mutate(safe_kps = `Baseline KPS`) %>% 
  filter(safe_kps != "Unknown" & !is.na(safe_kps)) %>%
  mutate(safe_kps = factor(safe_kps))
fit_D <- survfit(Surv(Survival_months, Status) ~ safe_kps, data = diabetic_kps)
pD <- fig2_km_helper(fit_D, diabetic_kps, "Baseline KPS", c("0-60", "70-80", "90-100"), tag_label = "D")

diabetic_resection <- diabetic %>% 
  mutate(safe_resection = `Resection Status`) %>% 
  filter(!is.na(safe_resection))
fit_E <- survfit(Surv(Survival_months, Status) ~ safe_resection, data = diabetic_resection)
pE <- fig2_km_helper(fit_E, diabetic_resection, "Resection Status", c("None", "STR", "GTR"), tag_label = "E")

diabetic_mgmt <- diabetic %>% 
  mutate(safe_mgmt = `MGMT Status`) %>% 
  filter(safe_mgmt != "Unknown" & !is.na(safe_mgmt))
fit_F <- survfit(Surv(Survival_months, Status) ~ safe_mgmt, data = diabetic_mgmt)
pF <- fig2_km_helper(fit_F, diabetic_mgmt, expression(bolditalic("MGMT") ~ bold("Status")), c("Methylated", "Unmethylated"), tag_label = "F")

diabetic_67 <- diabetic %>% filter(KI67 != "Unknown" & !is.na(KI67))
fit_G <- survfit(Surv(Survival_months, Status) ~ KI67, data = diabetic_67)
pG <- fig2_km_helper(fit_G, diabetic_67, "Ki-67 Index", c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%"), tag_label = "G", p_coord = c(54, 0.5))

layout_fig2 <- "
AAA
BCD
EFG
"
figure2_legend <- paste0(
  "**Figure 2**: Kaplan-Mier survival and prognostic factors in non-diabetic and diabetic patients with glioblastoma.",
  " Non-diabetics had a longer median OS than diabetics **(A)**.",
  " Among the diabetics, age **(B)**, baseline KPS **(D)** and resection status **(E)** were ",
  "significant prognostic factors, but not sex **(C)**, *MGMT* promoter methylation status **(F)**, or Ki-67 index **(G)**."
)

combined_fig2 <- (pA$plot + pB$plot + pC$plot + pD$plot + pE$plot + pF$plot + pG$plot) +
  plot_layout(design = layout_fig2) +
  plot_annotation(
    title = 'Figure 2',
    caption = figure2_legend
  ) &
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    plot.caption = element_textbox_simple(
      size = 20,
      lineheight = 1.2,
      padding = margin(t = 5, r = 5, b = 5, l = 5),
      margin = margin(t = 15, r = 0, b = 0, l = 0)
    )
  )

print(combined_fig2)

ggsave(
  filename = "Figure 2.png",
  plot = combined_fig2,
  device = "png",
  width = 14,
  height = 16,
  units = "in",
  dpi = 300
)




# Figure 3: General insulin use survival analysis ---------------------------------
all_patients <- all_patients %>%
  mutate(
    Diabetes_Insulin_Status = case_when(
      diabetes == "Non-Diabetic" ~ "Non-Diabetic",
      diabetes == "Diabetic" & (Post_Diagnosis_Insulin == 1 | Pre_Diagnosis_Insulin == 1) ~ "Diabetic (Insulin-User)",
      diabetes == "Diabetic" ~ "Diabetic (Non-Insulin)"
    ),
    Diabetes_Insulin_Status = factor(
      Diabetes_Insulin_Status, 
      levels = c("Non-Diabetic", "Diabetic (Non-Insulin)", "Diabetic (Insulin-User)")
    )
  )

fig3_km_helper <- function(fit, data_used, title_label, labs, tag_label, p_coord = c(48, 0.5), palette_colors) {
  n_table <- summary(fit)$table
  if (is.matrix(n_table)) { ns <- n_table[, "records"] } else { ns <- n_table["records"] }
  clean_labels <- paste0(labs, " (n=", ns, ")")
  pval_df <- surv_pvalue(fit, data = data_used)
  p_val <- pval_df$pval
  if (p_val < 0.0001) {
    p_val_str <- "p < 0.0001"
  } else {
    p_val_str <- paste0("p = ", format(round(p_val, 4), nsmall = 4, scientific = FALSE))
  }
  
  p <- ggsurvplot(
    fit,
    data = data_used,
    conf.int = TRUE,
    pval = p_val_str,
    pval.coord = p_coord,
    palette = palette_colors,
    legend.title = title_label,
    legend.labs = clean_labels,    
    legend = c(0.99, 0.95), 
    xlim = c(0, 80),
    xlab = "Time (months)",
    break.time.by = 6,
    ggtheme = theme_bw() + theme(
      plot.title = element_text(face = "bold", hjust = 0.5), 
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold"),
      legend.justification = c("right", "top"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  )
  p$plot <- p$plot + 
    labs(tag = tag_label) +
    theme(plot.tag = element_text(face = "bold", size = 16))
  
  return(p)
}

fit_3a <- survfit(Surv(Survival_months, Status) ~ Diabetes_Insulin_Status, data = all_patients)
fit_3a_sum <- summary(fit_3a)$table
labels_3a <- paste0(
  c("Non-Diabetic", "Diabetic (Non-Insulin)", "Diabetic (Insulin-User)"), 
  ", mOS=", ifelse(is.na(fit_3a_sum[, "median"]), "NR", sprintf("%.1f", fit_3a_sum[, "median"])), 
  ", 95% CI: ", ifelse(is.na(fit_3a_sum[, "0.95LCL"]), "NR", sprintf("%.1f", fit_3a_sum[, "0.95LCL"])), 
  "-", ifelse(is.na(fit_3a_sum[, "0.95UCL"]), "NR", sprintf("%.1f", fit_3a_sum[, "0.95UCL"]))
)
p3a <- fig3_km_helper(fit_3a, all_patients, "Diabetes/Insulin Status", labels_3a, tag_label = "A", 
                      palette_colors = c("#E64B35FF", "#4DBBD5FF", "#00A087FF"))

cox_m1 <- coxph(Surv(Survival_months, Status) ~ Diabetes_Insulin_Status + Age_At_Diagnosis + Baseline_KPS + `Resection Status`, data = all_patients)
cox_m2 <- coxph(Surv(Survival_months, Status) ~ Diabetes_Insulin_Status + Age_At_Diagnosis + Baseline_KPS + `Resection Status` + `Treatment Status` + `MGMT Status`, data = all_patients)

t1 <- tidy(cox_m1, exponentiate = TRUE, conf.int = TRUE) %>% filter(str_detect(term, "Diabetes_Insulin_Status"))
t2 <- tidy(cox_m2, exponentiate = TRUE, conf.int = TRUE) %>% filter(str_detect(term, "Diabetes_Insulin_Status"))

n_nd <- sum(all_patients$Diabetes_Insulin_Status == "Non-Diabetic", na.rm = TRUE)
n_d_ni <- sum(all_patients$Diabetes_Insulin_Status == "Diabetic (Non-Insulin)", na.rm = TRUE)
n_d_i <- sum(all_patients$Diabetes_Insulin_Status == "Diabetic (Insulin-User)", na.rm = TRUE)

plot_df <- data.frame(
  y_order = c(9, 8, 7, 6, 5, 4, 3, 2, 1), 
  row_type = c("header", "ref", "var", "var", "spacer", "header", "ref", "var", "var"),
  model_id = c(1, 1, 1, 1, NA, 2, 2, 2, 2),
  Variable = c(
    "**Covariates: Age, KPS, Resection**", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Non-Diabetic (Reference)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Diabetic (Non-Insulin)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Diabetic (Insulin-User)",
    "", 
    "**Covariates: Age, KPS, Resection, *MGMT*, Treatment**", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Non-Diabetic (Reference)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Diabetic (Non-Insulin)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Diabetic (Insulin-User)"
  ),
  N = c(NA, n_nd, n_d_ni, n_d_i, NA, NA, n_nd, n_d_ni, n_d_i),
  estimate = c(NA, 1, t1$estimate[1], t1$estimate[2], NA, NA, 1, t2$estimate[1], t2$estimate[2]),
  conf.low = c(NA, NA, t1$conf.low[1], t1$conf.low[2], NA, NA, NA, t2$conf.low[1], t2$conf.low[2]),
  conf.high = c(NA, NA, t1$conf.high[1], t1$conf.high[2], NA, NA, NA, t2$conf.high[1], t2$conf.high[2]),
  p.value = c(NA, NA, t1$p.value[1], t1$p.value[2], NA, NA, NA, t2$p.value[1], t2$p.value[2])
)

plot_df <- plot_df %>%
  mutate(
    hr_text = case_when(
      row_type == "ref" ~ "1.00 (Reference)",
      row_type == "var" ~ sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      TRUE ~ ""
    ),
    p_text = case_when(
      row_type == "var" & p.value < 0.0001 ~ "<0.0001",
      row_type == "var" ~ sprintf("%.4f", p.value),
      TRUE ~ ""
    ),
    N_text = ifelse(is.na(N), "", as.character(N)),
    font_face = ifelse(row_type == "header", "bold", "plain"),
    plot_est = 6.5 + log(estimate) * 1.5,
    plot_low = 6.5 + log(conf.low) * 1.5,
    plot_high = 6.5 + log(conf.high) * 1.5
  )

shade_df <- plot_df %>%
  filter(row_type %in% c("ref", "var")) %>%
  group_by(model_id) %>%
  mutate(is_shaded = row_number() %% 2 == 0) %>%
  filter(is_shaded)

ticks_val <- c(0.5, 1, 2, 3)
ticks_pos <- 6.5 + log(ticks_val) * 1.5

p3b_plot <- ggplot(plot_df, aes(y = y_order)) +
  geom_rect(data = shade_df, aes(ymin = y_order - 0.5, ymax = y_order + 0.5, xmin = -0.5, xmax = 13.5),
            fill = "#E0E0E0", inherit.aes = FALSE) +
  annotate("segment", x = 6.5, xend = 6.5, y = 0.5, yend = 9.5, linetype = "dashed", color = "grey40") +
  geom_errorbarh(data = plot_df %>% filter(row_type == "var"),
                 aes(xmin = plot_low, xmax = plot_high, x = plot_est), height = 0, color = "black", linewidth = 0.7) +
  geom_point(data = plot_df %>% filter(row_type %in% c("var", "ref")),
             aes(x = plot_est), shape = 15, size = 2.5, color = "black") +
  geom_richtext(aes(x = 0, label = Variable, fontface = font_face), hjust = 0, size = 4, fill = NA, label.color = NA, label.padding = unit(0, "pt")) +
  
  geom_text(aes(x = 4.5, label = N_text, fontface = font_face), hjust = 0.5, size = 4) +
  geom_text(aes(x = 9.0, label = hr_text, fontface = font_face), hjust = 0, size = 4) +
  geom_text(aes(x = 12.0, label = p_text, fontface = font_face), hjust = 0, size = 4) +
  
  annotate("text", x = 0, y = 10.2, label = "Prognostic Factor", fontface = "bold", hjust = 0, size = 4) +
  annotate("text", x = 4.5, y = 10.2, label = "n", fontface = "bold", fontstyle = "italic", hjust = 0.5, size = 4) +
  annotate("text", x = 6.5, y = 10.2, label = "Hazard Ratio", fontface = "bold", hjust = 0.5, size = 4) + 
  annotate("text", x = 9.0, y = 10.2, label = "HR (95% CI)", fontface = "bold", hjust = 0, size = 4) +    
  annotate("text", x = 12.0, y = 10.2, label = "P value", fontface = "bold", hjust = 0, size = 4) +
  
  annotate("segment", x = min(ticks_pos) - 0.2, xend = max(ticks_pos) + 0.2, y = 0, yend = 0, color = "black") +
  annotate("segment", x = ticks_pos, xend = ticks_pos, y = 0, yend = -0.2, color = "black") +
  annotate("text", x = ticks_pos, y = -0.6, label = ticks_val, size = 3.5) +
  
  annotate("text", x = 6.3, y = -1.5, label = "\u2190 Beneficial", hjust = 1, size = 4) +
  annotate("text", x = 6.7, y = -1.5, label = "Detrimental \u2192", hjust = 0, size = 4) +
  
  # --- TIGHTENED LIMITS ---
  scale_x_continuous(limits = c(-0.5, 13.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-2.0, 10.5), expand = c(0, 0)) +
  theme_void() + 
  labs(
    tag = "B",
    caption = "KPS: Karnofsky Performance Scale; MGMT: O<sup>6</sup>-methylguanine-DNA methyltransferase"
  ) +
  theme(
    plot.tag = element_text(face = "bold", size = 18),
    plot.caption = ggtext::element_markdown(size = 12, hjust = 0, color = "black", margin = margin(t = 0)),
    plot.caption.position = "plot", 
    plot.margin = margin(5.5, 10, 10, 10) 
  )

f3c_df <- all_patients %>% filter(Diabetes_Insulin_Status %in% c("Non-Diabetic", "Diabetic (Insulin-User)"))
fit_3c <- survfit(Surv(Survival_months, Status) ~ Diabetes_Insulin_Status, data = f3c_df)
p3c <- fig3_km_helper(fit_3c, f3c_df, "Cohort", c("Non-Diabetic", "Diabetic (Insulin-User)"), tag_label = "C", 
                      palette_colors = c("#E64B35FF", "#00A087FF")) 

f3d_df <- all_patients %>% filter(Diabetes_Insulin_Status %in% c("Diabetic (Non-Insulin)", "Diabetic (Insulin-User)"))
fit_3d <- survfit(Surv(Survival_months, Status) ~ Diabetes_Insulin_Status, data = f3d_df)
p3d <- fig3_km_helper(fit_3d, f3d_df, "Cohort", c("Diabetic (Non-Insulin)", "Diabetic (Insulin-User)"), tag_label = "D", 
                      palette_colors = c("#4DBBD5FF", "#00A087FF")) 

f3e_df <- all_patients %>% filter(Diabetes_Insulin_Status %in% c("Non-Diabetic", "Diabetic (Non-Insulin)"))
fit_3e <- survfit(Surv(Survival_months, Status) ~ Diabetes_Insulin_Status, data = f3e_df)
p3e <- fig3_km_helper(fit_3e, f3e_df, "Cohort", c("Non-Diabetic", "Diabetic (Non-Insulin)"), tag_label = "E", 
                      palette_colors = c("#E64B35FF", "#4DBBD5FF"))

figure3_legend <- paste0(
  "**Figure 3**: Insulin usage in diabetic patients with glioblastoma. ",
  "Kaplan-Meier survival among non-diabetics, non-insulin-using diabetics, and insulin-using diabetics **(A)**. ",
  "Cox proportional hazard ratio of all diabetics, non-insulin-using diabetics and insulin-using diabetics, ",
  "adjusted using significant covariates for diabetics (upper panel: age, KPS and resection status) and ",
  "those significant for non-diabetics (lower panel: age, KPS, resection status, *MGMT* status and treatment) **(B)**. ",
  "Insulin-using diabetics had a shortened survival compared to non-diabetics **(C)** and ",
  "non-insulin-using diabetics **(D)**. There was no survival difference between non-diabetics and ",
  "non-insulin-using diabetics **(E)**.")


layout_fig3 <- "
AAABBB
CCDDEE
"

combined_fig3 <- (p3a$plot + p3b_plot + p3c$plot + p3d$plot + p3e$plot) +
  plot_layout(design = layout_fig3) +
  plot_annotation(
    title = 'Figure 3',
    caption = figure3_legend,
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", color = "black"),
      plot.caption = element_textbox_simple(
        size = 22,
        lineheight = 1.2,
        padding = margin(t = 10, r = 10, b = 10, l = 10),
        margin = margin(t = 20, r = 0, b = 0, l = 0),
        fill = "white" 
      )
    )
  )

ggsave(
  filename = "Figure 3.png",
  plot = combined_fig3,
  device = "png",
  width = 16,
  height = 14,
  units = "in",
  dpi = 300
)


# Figure 4: Insulin + NIDM use survival analysis ---------------------------------
all_patients <- all_patients %>%
  mutate(
    Ins_NIDM_Status = case_when(
      diabetes == "Non-Diabetic" ~ "Non-Diabetic",
      diabetes == "Diabetic" & (Post_Diagnosis_Insulin == 1 | Pre_Diagnosis_Insulin == 1) & NIDM == 1 ~ "Both",
      diabetes == "Diabetic" & (Post_Diagnosis_Insulin == 1 | Pre_Diagnosis_Insulin == 1) & NIDM == 0 ~ "Insulin Only",
      diabetes == "Diabetic" & (Post_Diagnosis_Insulin == 0 & Pre_Diagnosis_Insulin == 0) & NIDM == 1 ~ "NIDM Only",
      diabetes == "Diabetic" & (Post_Diagnosis_Insulin == 0 & Pre_Diagnosis_Insulin == 0) & NIDM == 0 ~ "Neither"
    ),
    Ins_NIDM_Status = factor(
      Ins_NIDM_Status, 
      levels = c("Non-Diabetic", "Neither", "NIDM Only", "Insulin Only", "Both")
    )
  )

fig4_km_helper <- function(fit, data_used, title_label, labs, tag_label, p_coord = c(48, 0.5), palette_colors) {
  n_table <- summary(fit)$table
  if (is.matrix(n_table)) { ns <- n_table[, "records"] } else { ns <- n_table["records"] }
  clean_labels <- paste0(labs, " (n=", ns, ")")
  pval_df <- surv_pvalue(fit, data = data_used)
  p_val <- pval_df$pval
  if (p_val < 0.0001) {
    p_val_str <- "p < 0.0001"
  } else {
    p_val_str <- paste0("p = ", format(round(p_val, 4), nsmall = 4, scientific = FALSE))
  }
  
  p <- ggsurvplot(
    fit,
    data = data_used,
    conf.int = TRUE,
    pval = p_val_str,
    pval.coord = p_coord,
    palette = palette_colors,
    legend.title = title_label,
    legend.labs = clean_labels,    
    legend = c(0.99, 0.95), 
    xlim = c(0, 80),
    xlab = "Time (months)",
    break.time.by = 6,
    ggtheme = theme_bw() + theme(
      plot.title = element_text(face = "bold", hjust = 0.5), 
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold"),
      legend.justification = c("right", "top"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  )
  p$plot <- p$plot + 
    labs(tag = tag_label) +
    theme(plot.tag = element_text(face = "bold", size = 16))
  
  return(p)
}

fit_4a <- survfit(Surv(Survival_months, Status) ~ Ins_NIDM_Status, data = all_patients)
fit_4a_sum <- summary(fit_4a)$table
labels_4a <- paste0(
  c("Non-Diabetic", "Neither", "NIDM Only", "Insulin Only", "Both"), 
  ", mOS=", ifelse(is.na(fit_4a_sum[, "median"]), "NR", round(fit_4a_sum[, "median"], 1)), 
  ", 95% CI: ", ifelse(is.na(fit_4a_sum[, "0.95LCL"]), "NR", round(fit_4a_sum[, "0.95LCL"], 1)), 
  "-", ifelse(is.na(fit_4a_sum[, "0.95UCL"]), "NR", round(fit_4a_sum[, "0.95UCL"], 1))
)
p4a <- fig4_km_helper(fit_4a, all_patients, "Insulin / NIDM Cohort", labels_4a, tag_label = "A", 
                      p_coord = c(48, 0.45), palette_colors = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"))

p4a$plot <- p4a$plot + 
  annotate(
    "text", 
    x = 61,             
    y = 0.56,             
    label = "NR = Not reached", 
    hjust = 1,           
    vjust = 1, 
    size = 9 / .pt,      
    color = "black"
  )

cox_4_m1 <- coxph(Surv(Survival_months, Status) ~ Ins_NIDM_Status + Age_At_Diagnosis + Baseline_KPS + `Resection Status`, data = all_patients)
cox_4_m2 <- coxph(Surv(Survival_months, Status) ~ Ins_NIDM_Status + Age_At_Diagnosis + Baseline_KPS + `Resection Status` + `Treatment Status` + `MGMT Status`, data = all_patients)

t4_1 <- tidy(cox_4_m1, exponentiate = TRUE, conf.int = TRUE) %>% filter(str_detect(term, "Ins_NIDM_Status"))
t4_2 <- tidy(cox_4_m2, exponentiate = TRUE, conf.int = TRUE) %>% filter(str_detect(term, "Ins_NIDM_Status"))

n_nd_4  <- sum(all_patients$Ins_NIDM_Status == "Non-Diabetic", na.rm = TRUE)
n_neith  <- sum(all_patients$Ins_NIDM_Status == "Neither", na.rm = TRUE)
n_nidm   <- sum(all_patients$Ins_NIDM_Status == "NIDM Only", na.rm = TRUE)
n_ins    <- sum(all_patients$Ins_NIDM_Status == "Insulin Only", na.rm = TRUE)
n_both   <- sum(all_patients$Ins_NIDM_Status == "Both", na.rm = TRUE)

plot_df_4 <- data.frame(
  y_order = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), 
  row_type = c("header", "ref", "var", "var", "var", "var", "spacer", "header", "ref", "var", "var", "var", "var"),
  model_id = c(1, 1, 1, 1, 1, 1, NA, 2, 2, 2, 2, 2, 2),
  Variable = c(
    "**Covariates: Age, KPS, Resection**", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Non-Diabetic (Reference)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Neither", 
    "&nbsp;&nbsp;&nbsp;&nbsp;NIDM Only",
    "&nbsp;&nbsp;&nbsp;&nbsp;Insulin Only",
    "&nbsp;&nbsp;&nbsp;&nbsp;Both",
    "", 
    "**Covariates: Age, KPS, Resection, *MGMT*, Treatment**", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Non-Diabetic (Reference)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Neither", 
    "&nbsp;&nbsp;&nbsp;&nbsp;NIDM Only",
    "&nbsp;&nbsp;&nbsp;&nbsp;Insulin Only",
    "&nbsp;&nbsp;&nbsp;&nbsp;Both"
  ),
  N = c(NA, n_nd_4, n_neith, n_nidm, n_ins, n_both, NA, NA, n_nd_4, n_neith, n_nidm, n_ins, n_both),
  estimate = c(NA, 1, t4_1$estimate[1], t4_1$estimate[2], t4_1$estimate[3], t4_1$estimate[4], NA, NA, 1, t4_2$estimate[1], t4_2$estimate[2], t4_2$estimate[3], t4_2$estimate[4]),
  conf.low = c(NA, NA, t4_1$conf.low[1], t4_1$conf.low[2], t4_1$conf.low[3], t4_1$conf.low[4], NA, NA, NA, t4_2$conf.low[1], t4_2$conf.low[2], t4_2$conf.low[3], t4_2$conf.low[4]),
  conf.high = c(NA, NA, t4_1$conf.high[1], t4_1$conf.high[2], t4_1$conf.high[3], t4_1$conf.high[4], NA, NA, NA, t4_2$conf.high[1], t4_2$conf.high[2], t4_2$conf.high[3], t4_2$conf.high[4]),
  p.value = c(NA, NA, t4_1$p.value[1], t4_1$p.value[2], t4_1$p.value[3], t4_1$p.value[4], NA, NA, NA, t4_2$p.value[1], t4_2$p.value[2], t4_2$p.value[3], t4_2$p.value[4])
)

plot_df_4 <- plot_df_4 %>%
  mutate(
    hr_text = case_when(
      row_type == "ref" ~ "1.00 (Reference)",
      row_type == "var" ~ sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      TRUE ~ ""
    ),
    p_text = case_when(
      row_type == "var" & p.value < 0.0001 ~ "<0.0001",
      row_type == "var" ~ sprintf("%.4f", p.value),
      TRUE ~ ""
    ),
    N_text = ifelse(is.na(N), "", as.character(N)),
    font_face = ifelse(row_type == "header", "bold", "plain"),
    plot_est = 6.5 + log(estimate) * 1.5,
    plot_low = 6.5 + log(conf.low) * 1.5,
    plot_high = 6.5 + log(conf.high) * 1.5
  )

shade_df_4 <- plot_df_4 %>%
  filter(row_type %in% c("ref", "var")) %>%
  group_by(model_id) %>%
  mutate(is_shaded = row_number() %% 2 == 0) %>%
  filter(is_shaded)

ticks_val <- c(0.5, 1, 2, 3)
ticks_pos <- 6.5 + log(ticks_val) * 1.5

p4b_plot <- ggplot(plot_df_4, aes(y = y_order)) +
  geom_rect(data = shade_df_4, aes(ymin = y_order - 0.5, ymax = y_order + 0.5, xmin = -0.5, xmax = 13.5),
            fill = "#E0E0E0", inherit.aes = FALSE) +
  annotate("segment", x = 6.5, xend = 6.5, y = 0.5, yend = 13.5, linetype = "dashed", color = "grey40") +
  geom_errorbarh(data = plot_df_4 %>% filter(row_type == "var"),
                 aes(xmin = plot_low, xmax = plot_high, x = plot_est), height = 0, color = "black", linewidth = 0.7) +
  geom_point(data = plot_df_4 %>% filter(row_type %in% c("var", "ref")),
             aes(x = plot_est), shape = 15, size = 2.5, color = "black") +
  geom_richtext(aes(x = 0, label = Variable, fontface = font_face), hjust = 0, size = 3.5, fill = NA, label.color = NA, label.padding = unit(0, "pt")) +
  
  geom_text(aes(x = 4.5, label = N_text, fontface = font_face), hjust = 0.5, size = 3.5) +
  geom_text(aes(x = 9.0, label = hr_text, fontface = font_face), hjust = 0, size = 3.5) +
  geom_text(aes(x = 12.0, label = p_text, fontface = font_face), hjust = 0, size = 3.5) +
  
  annotate("text", x = 0, y = 14.2, label = "Prognostic Factor", fontface = "bold", hjust = 0, size = 3.5) +
  annotate("text", x = 4.5, y = 14.2, label = "n", fontface = "bold", fontstyle = "italic", hjust = 0.5, size = 3.5) +
  annotate("text", x = 6.5, y = 14.2, label = "Hazard Ratio", fontface = "bold", hjust = 0.5, size = 3.5) +
  annotate("text", x = 9.0, y = 14.2, label = "HR (95% CI)", fontface = "bold", hjust = 0, size = 3.5) +
  annotate("text", x = 12.0, y = 14.2, label = "P value", fontface = "bold", hjust = 0, size = 3.5) +
  
  annotate("segment", x = min(ticks_pos) - 0.2, xend = max(ticks_pos) + 0.2, y = 0, yend = 0, color = "black") +
  annotate("segment", x = ticks_pos, xend = ticks_pos, y = 0, yend = -0.2, color = "black") +
  annotate("text", x = ticks_pos, y = -0.6, label = ticks_val, size = 3) +
  
  annotate("text", x = 6.3, y = -1.5, label = "\u2190 Beneficial", hjust = 1, size = 3.5) +
  annotate("text", x = 6.7, y = -1.5, label = "Detrimental \u2192", hjust = 0, size = 3.5) +
  
  scale_x_continuous(limits = c(-0.5, 13.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-2.0, 14.5), expand = c(0, 0)) +
  theme_void() + 
  labs(
    tag = "B",
    caption = "KPS: Karnofsky Performance Scale; MGMT: O<sup>6</sup>-methylguanine-DNA methyltransferase"
  ) +
  theme(
    plot.tag = element_text(face = "bold", size = 16),
    plot.caption = ggtext::element_markdown(size = 10, hjust = 0, color = "black", margin = margin(t = 0)),
    plot.caption.position = "plot", 
    plot.margin = margin(5.5, 10, 10, 10) 
  )

f4c_df <- all_patients %>% filter(Ins_NIDM_Status %in% c("NIDM Only", "Insulin Only"))
fit_4c <- survfit(Surv(Survival_months, Status) ~ Ins_NIDM_Status, data = f4c_df)
p4c <- fig4_km_helper(fit_4c, f4c_df, "Cohort", c("NIDM Only", "Insulin Only"), tag_label = "C", 
                      palette_colors = c("#00A087FF", "#3C5488FF")) 

f4d_df <- all_patients %>% filter(Ins_NIDM_Status %in% c("Neither", "Insulin Only"))
fit_4d <- survfit(Surv(Survival_months, Status) ~ Ins_NIDM_Status, data = f4d_df)
p4d <- fig4_km_helper(fit_4d, f4d_df, "Cohort", c("Neither", "Insulin Only"), tag_label = "D", 
                      palette_colors = c("#4DBBD5FF", "#3C5488FF")) 

f4e_df <- all_patients %>% filter(Ins_NIDM_Status %in% c("Neither", "Both"))
fit_4e <- survfit(Surv(Survival_months, Status) ~ Ins_NIDM_Status, data = f4e_df)
p4e <- fig4_km_helper(fit_4e, f4e_df, "Cohort", c("Neither", "Both"), tag_label = "E", 
                      palette_colors = c("#4DBBD5FF", "#F39B7FFF"))

figure4_legend <- paste0(
  "**Figure 4**: The influence of insulin versus non-insulin diabetes medication (NIDM) on ",
  "patient survival. Kaplan-Meier survival showed a difference among patients using ",
  "insulin, NIDM, both and neither **(A)**. Cox proportional hazard ratio among patients using ",
  "insulin, NIDM, both or neither, adjusted using significant covariates for diabetics (upper ",
  "panel: age, KPS and resection status) and those significant for non-diabetics (lower ",
  "panel: age, KPS, resection status, *MGMT* status and treatment) **(B)**. Shortened survival ",
  "was observed in patients using insulin compared to those using NIDM **(C)** or neither **(D)**. ",
  "There is a trend favoring patients using neither over those using both insulin and NIDM **(E)**.")


layout_fig4 <- "
AAABBB
CCDDEE
"

combined_fig4 <- (p4a$plot + p4b_plot + p4c$plot + p4d$plot + p4e$plot) +
  plot_layout(design = layout_fig4) +
  plot_annotation(
    title = 'Figure 4',
    caption = figure4_legend,
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", color = "black"),
      plot.caption = element_textbox_simple(
        size = 22,
        lineheight = 1.2,
        padding = margin(t = 10, r = 10, b = 10, l = 10),
        margin = margin(t = 20, r = 0, b = 0, l = 0),
        fill = "white"
      )
    )
  )

ggsave(
  filename = "Figure 4.png",
  plot = combined_fig4,
  device = "png",
  width = 16,
  height = 14,
  units = "in",
  dpi = 300
)
# Figure 5: Pre/post diagnosis insulin use survival analysis --------------------------------------------------------
all_patients <- all_patients %>%
  mutate(
    Insulin_Use = case_when(
      `Pre_Diagnosis_Insulin` == 0 & `Post_Diagnosis_Insulin` == 0 ~ 0,
      `Pre_Diagnosis_Insulin` == 1 & `Post_Diagnosis_Insulin` == 1 ~ 1,
      `Pre_Diagnosis_Insulin` == 0 & `Post_Diagnosis_Insulin` == 1 ~ 2,
      `Pre_Diagnosis_Insulin` == 1 & `Post_Diagnosis_Insulin` == 0 ~ 3
    ),
    Insulin_Use = factor(Insulin_Use, levels = 0:3, labels = c("-/-", "+/+", "-/+", "+/-")),
    Insulin_Use = as.character(Insulin_Use),
    Insulin_Use = case_when(
      diabetes == "Non-Diabetic" ~ "Non-Diabetic",
      TRUE ~ Insulin_Use
    ),
    Insulin_Use = factor(
      Insulin_Use, 
      levels = c("Non-Diabetic", "-/-", "+/+", "-/+", "+/-")
    )
  )

fig5_km_helper <- function(fit, data_used, title_label, labs, tag_label, p_coord = c(48, 0.5), palette_colors) {
  n_table <- summary(fit)$table
  if (is.matrix(n_table)) { ns <- n_table[, "records"] } else { ns <- n_table["records"] }
  clean_labels <- paste0(labs, " (n=", ns, ")")
  pval_df <- surv_pvalue(fit, data = data_used)
  p_val <- pval_df$pval
  if (p_val < 0.0001) {
    p_val_str <- "p < 0.0001"
  } else {
    p_val_str <- paste0("p = ", format(round(p_val, 4), nsmall = 4, scientific = FALSE))
  }
  
  p <- ggsurvplot(
    fit,
    data = data_used,
    conf.int = TRUE,
    pval = p_val_str,
    pval.coord = p_coord,
    palette = palette_colors,
    legend.title = title_label,
    legend.labs = clean_labels,    
    legend = c(0.99, 0.95), 
    xlim = c(0, 80),
    xlab = "Time (months)",
    break.time.by = 6,
    ggtheme = theme_bw() + theme(
      plot.title = element_text(face = "bold", hjust = 0.5), 
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold"),
      legend.justification = c("right", "top"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  )
  p$plot <- p$plot + 
    labs(tag = tag_label) +
    theme(plot.tag = element_text(face = "bold", size = 16))
  
  return(p)
}

fit_5a <- survfit(Surv(Survival_months, Status) ~ Insulin_Use, data = all_patients)
fit_5a_sum <- summary(fit_5a)$table
labels_5a <- paste0(
  c("Non-Diabetic", "-/-", "+/+", "-/+", "+/-"), 
  ", mOS=", ifelse(is.na(fit_5a_sum[, "median"]), "NR", round(fit_5a_sum[, "median"], 1)), 
  ", 95% CI: ", ifelse(is.na(fit_5a_sum[, "0.95LCL"]), "NR", round(fit_5a_sum[, "0.95LCL"], 1)), 
  "-", ifelse(is.na(fit_5a_sum[, "0.95UCL"]), "NR", round(fit_5a_sum[, "0.95UCL"], 1))
)
p5a <- fig5_km_helper(fit_5a, all_patients, "Pre-/Post-Diagnosis Insulin", labels_5a, tag_label = "A", 
                      p_coord = c(48, 0.4), palette_colors = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"))

p5a$plot <- p5a$plot + 
  annotate(
    "text", 
    x = 61,             
    y = 0.54,             
    label = "NR = Not reached", 
    hjust = 1,           
    vjust = 1, 
    size = 9 / .pt,      
    color = "black"
  )

cox_5_m1 <- coxph(Surv(Survival_months, Status) ~ Insulin_Use + Age_At_Diagnosis + Baseline_KPS + `Resection Status`, data = all_patients)
cox_5_m2 <- coxph(Surv(Survival_months, Status) ~ Insulin_Use + Age_At_Diagnosis + Baseline_KPS + `Resection Status` + `Treatment Status` + `MGMT Status`, data = all_patients)

t5_1 <- tidy(cox_5_m1, exponentiate = TRUE, conf.int = TRUE) %>% filter(str_detect(term, "Insulin_Use"))
t5_2 <- tidy(cox_5_m2, exponentiate = TRUE, conf.int = TRUE) %>% filter(str_detect(term, "Insulin_Use"))

n_nd_5 <- sum(all_patients$Insulin_Use == "Non-Diabetic", na.rm = TRUE)
n_00 <- sum(all_patients$Insulin_Use == "-/-", na.rm = TRUE)
n_11 <- sum(all_patients$Insulin_Use == "+/+", na.rm = TRUE)
n_01 <- sum(all_patients$Insulin_Use == "-/+", na.rm = TRUE)
n_10 <- sum(all_patients$Insulin_Use == "+/-", na.rm = TRUE)

plot_df_5 <- data.frame(
  y_order = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), 
  row_type = c("header", "ref", "var", "var", "var", "var", "spacer", "header", "ref", "var", "var", "var", "var"),
  model_id = c(1, 1, 1, 1, 1, 1, NA, 2, 2, 2, 2, 2, 2),
  Variable = c(
    "**Covariates: Age, KPS, Resection**", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Non-Diabetic (Reference)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;-/-", 
    "&nbsp;&nbsp;&nbsp;&nbsp;+/+",
    "&nbsp;&nbsp;&nbsp;&nbsp;-/+",
    "&nbsp;&nbsp;&nbsp;&nbsp;+/-",
    "", 
    "**Covariates: Age, KPS, Resection, *MGMT*, Treatment**", 
    "&nbsp;&nbsp;&nbsp;&nbsp;Non-Diabetic (Reference)", 
    "&nbsp;&nbsp;&nbsp;&nbsp;-/-", 
    "&nbsp;&nbsp;&nbsp;&nbsp;+/+",
    "&nbsp;&nbsp;&nbsp;&nbsp;-/+",
    "&nbsp;&nbsp;&nbsp;&nbsp;+/-"
  ),
  N = c(NA, n_nd_5, n_00, n_11, n_01, n_10, NA, NA, n_nd_5, n_00, n_11, n_01, n_10),
  estimate = c(NA, 1, t5_1$estimate[1], t5_1$estimate[2], t5_1$estimate[3], t5_1$estimate[4], NA, NA, 1, t5_2$estimate[1], t5_2$estimate[2], t5_2$estimate[3], t5_2$estimate[4]),
  conf.low = c(NA, NA, t5_1$conf.low[1], t5_1$conf.low[2], t5_1$conf.low[3], t5_1$conf.low[4], NA, NA, NA, t5_2$conf.low[1], t5_2$conf.low[2], t5_2$conf.low[3], t5_2$conf.low[4]),
  conf.high = c(NA, NA, t5_1$conf.high[1], t5_1$conf.high[2], t5_1$conf.high[3], t5_1$conf.high[4], NA, NA, NA, t5_2$conf.high[1], t5_2$conf.high[2], t5_2$conf.high[3], t5_2$conf.high[4]),
  p.value = c(NA, NA, t5_1$p.value[1], t5_1$p.value[2], t5_1$p.value[3], t5_1$p.value[4], NA, NA, NA, t5_2$p.value[1], t5_2$p.value[2], t5_2$p.value[3], t5_2$p.value[4])
)

plot_df_5 <- plot_df_5 %>%
  mutate(
    hr_text = case_when(
      row_type == "ref" ~ "1.00 (Reference)",
      row_type == "var" ~ sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      TRUE ~ ""
    ),
    p_text = case_when(
      row_type == "var" & p.value < 0.0001 ~ "<0.0001",
      row_type == "var" ~ sprintf("%.4f", p.value),
      TRUE ~ ""
    ),
    N_text = ifelse(is.na(N), "", as.character(N)),
    font_face = ifelse(row_type == "header", "bold", "plain"),
    plot_est = 6.5 + log(estimate) * 1.5,
    plot_low = 6.5 + log(conf.low) * 1.5,
    plot_high = 6.5 + log(conf.high) * 1.5
  )

shade_df_5 <- plot_df_5 %>%
  filter(row_type %in% c("ref", "var")) %>%
  group_by(model_id) %>%
  mutate(is_shaded = row_number() %% 2 == 0) %>%
  filter(is_shaded)

ticks_val <- c(0.5, 1, 2, 3)
ticks_pos <- 6.5 + log(ticks_val) * 1.5

p5b_plot <- ggplot(plot_df_5, aes(y = y_order)) +
  geom_rect(data = shade_df_5, aes(ymin = y_order - 0.5, ymax = y_order + 0.5, xmin = -0.5, xmax = 13.5),
            fill = "#E0E0E0", inherit.aes = FALSE) +
  annotate("segment", x = 6.5, xend = 6.5, y = 0.5, yend = 13.5, linetype = "dashed", color = "grey40") +
  geom_errorbarh(data = plot_df_5 %>% filter(row_type == "var"),
                 aes(xmin = plot_low, xmax = plot_high, x = plot_est), height = 0, color = "black", linewidth = 0.7) +
  geom_point(data = plot_df_5 %>% filter(row_type %in% c("var", "ref")),
             aes(x = plot_est), shape = 15, size = 2.5, color = "black") +
  geom_richtext(aes(x = 0, label = Variable, fontface = font_face), hjust = 0, size = 3.5, fill = NA, label.color = NA, label.padding = unit(0, "pt")) +
  
  geom_text(aes(x = 4.5, label = N_text, fontface = font_face), hjust = 0.5, size = 3.5) +
  geom_text(aes(x = 9.0, label = hr_text, fontface = font_face), hjust = 0, size = 3.5) +
  geom_text(aes(x = 12.0, label = p_text, fontface = font_face), hjust = 0, size = 3.5) +
  
  annotate("text", x = 0, y = 14.2, label = "Prognostic Factor", fontface = "bold", hjust = 0, size = 3.5) +
  annotate("text", x = 4.5, y = 14.2, label = "n", fontface = "bold", fontstyle = "italic", hjust = 0.5, size = 3.5) +
  annotate("text", x = 6.5, y = 14.2, label = "Hazard Ratio", fontface = "bold", hjust = 0.5, size = 3.5) +
  annotate("text", x = 9.0, y = 14.2, label = "HR (95% CI)", fontface = "bold", hjust = 0, size = 3.5) +
  annotate("text", x = 12.0, y = 14.2, label = "P value", fontface = "bold", hjust = 0, size = 3.5) +
  
  annotate("segment", x = min(ticks_pos) - 0.2, xend = max(ticks_pos) + 0.2, y = 0, yend = 0, color = "black") +
  annotate("segment", x = ticks_pos, xend = ticks_pos, y = 0, yend = -0.2, color = "black") +
  annotate("text", x = ticks_pos, y = -0.6, label = ticks_val, size = 3) +
  
  annotate("text", x = 6.3, y = -1.5, label = "\u2190 Beneficial", hjust = 1, size = 3.5) +
  annotate("text", x = 6.7, y = -1.5, label = "Detrimental \u2192", hjust = 0, size = 3.5) +
  
  scale_x_continuous(limits = c(-0.5, 13.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-2.0, 14.5), expand = c(0, 0)) +
  theme_void() + 
  labs(
    tag = "B",
    caption = "KPS: Karnofsky Performance Scale; MGMT: O<sup>6</sup>-methylguanine-DNA methyltransferase"
  ) +
  theme(
    plot.tag = element_text(face = "bold", size = 16),
    plot.caption = ggtext::element_markdown(size = 10, hjust = 0, color = "black", margin = margin(t = 10)), # Added the centered formatting fix here too
    plot.caption.position = "plot", 
    plot.margin = margin(5.5, 10, 10, 10) 
  )

f5c_df <- all_patients %>% filter(Insulin_Use == "+/+" | Insulin_Use == "-/-")
fit_5c <- survfit(Surv(Survival_months, Status) ~ Insulin_Use, data = f5c_df)
p5c <- fig5_km_helper(fit_5c, f5c_df, "Cohort", c("-/-", "+/+"), tag_label = "C", 
                      palette_colors = c("#4DBBD5FF", "#00A087FF"))

f5d_df <- all_patients %>% filter(Insulin_Use == "-/-" | Insulin_Use == "-/+")
fit_5d <- survfit(Surv(Survival_months, Status) ~ Insulin_Use, data = f5d_df)
p5d <- fig5_km_helper(fit_5d, f5d_df, "Cohort", c("-/-", "-/+"), tag_label = "D", 
                      palette_colors = c("#4DBBD5FF", "#3C5488FF"))

f5e_df <- all_patients %>% filter(Insulin_Use == "+/+" | Insulin_Use == "-/+")
fit_5e <- survfit(Surv(Survival_months, Status) ~ Insulin_Use, data = f5e_df)
p5e <- fig5_km_helper(fit_5e, f5e_df, "Cohort", c("+/+", "-/+"), tag_label = "E", 
                      palette_colors = c("#00A087FF", "#3C5488FF"))


figure5_legend <- paste0(
  "**Figure 5**: Pre- and post-diagnosis use of insulin among diabetics. Kaplan-Meier ",
  "survival among diabetic patients who did not use insulin (-/-) or used before and after ",
  "(+/+), only after (-/+) or only before diagnosis (+/-) **(A)**. Cox proportional hazard ratio of ",
  "-/-, +/+, -/+ or +/- groups, adjusted using significant covariates for diabetics (upper ",
  "panel: age, KPS and resection status) and those significant for non-diabetics (lower ",
  "panel: age, KPS, resection status, *MGMT* status and treatment) **(B)**. Diabetics who did ",
  "not use insulin (-/-) had significantly prolonged survival compared to those who used ",
  "before and after (+/+) **(C)** and those who used only after (-/+) diagnosis **(D)**. But there ",
  "was no survival difference between those who used insulin after (-/+) and before and ",
  "after (+/+) diagnosis **(E)**."
)

layout_fig5 <- "
AAABBB
CCDDEE
"

combined_fig5 <- (p5a$plot + p5b_plot + p5c$plot + p5d$plot + p5e$plot) +
  plot_layout(design = layout_fig5) +
  plot_annotation(
    title = 'Figure 5',
    caption = figure5_legend,
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.caption = element_textbox_simple(
        size = 20,
        lineheight = 1.2,
        padding = margin(t = 10, r = 10, b = 10, l = 10),
        margin = margin(t = 20, r = 0, b = 0, l = 0),
        fill = "white"
      )
    )
  )

ggsave(
  filename = "Figure 5.png",
  plot = combined_fig5,
  device = "png",
  width = 16,
  height = 14,
  units = "in",
  dpi = 300
)

# Figure 6: insulin-dose testing (including permutation test) ------------------------------------------------------------
# Permutation Tests for Optimal Insulin Dose Cutoff (Average + Cumulative Doses)
perm_data <- diabetic %>%
  filter(Post_Diagnosis_Insulin == 1 & !is.na(Average_Daily_Ins_Dose)) %>%
  dplyr::select(Survival_months, Status, Average_Daily_Ins_Dose)

get_max_logrank <- function(df) {
  cutoffs <- quantile(df$Average_Daily_Ins_Dose, probs = seq(0.2, 0.8, by = 0.05), na.rm = TRUE)
  max_stat <- 0
  
  for (c in cutoffs) {
    df$group <- ifelse(df$Average_Daily_Ins_Dose > c, "High", "Low")
    
    fit <- survdiff(Surv(Survival_months, Status) ~ group, data = df)
    stat <- fit$chisq
    
    if (!is.na(stat) && stat > max_stat) {
      max_stat <- stat
    }
  }
  return(max_stat)
}

true_max_stat <- get_max_logrank(perm_data)

set.seed(2026) 
n_permutations <- 1000
perm_stats <- numeric(n_permutations)

for (i in 1:n_permutations) {
  shuffled_df <- perm_data
  shuffled_idx <- sample(nrow(shuffled_df))
  shuffled_df$Survival_months <- shuffled_df$Survival_months[shuffled_idx]
  shuffled_df$Status <- shuffled_df$Status[shuffled_idx]
  
  perm_stats[i] <- get_max_logrank(shuffled_df)
}

adjusted_p_value <- sum(perm_stats >= true_max_stat) / n_permutations

cat("--------------------------------------------------\n")
cat("Results for Average Insulin Dose (units/day)\n")
cat("True Max Chi-Square Stat:   ", round(true_max_stat, 2), "\n")
cat("Permutation-Adjusted P-value:", format(adjusted_p_value, nsmall=4), "\n")
cat("--------------------------------------------------\n")

perm_data_cum <- diabetic %>%
  filter(Post_Diagnosis_Insulin == 1 & !is.na(Cumulative_Ins_Dose)) %>%
  dplyr::select(Survival_months, Status, Cumulative_Ins_Dose)

get_max_logrank_cum <- function(df) {
  cutoffs <- quantile(df$Cumulative_Ins_Dose, probs = seq(0.2, 0.8, by = 0.05), na.rm = TRUE)
  max_stat <- 0
  
  for (c in cutoffs) {
    df$group <- ifelse(df$Cumulative_Ins_Dose > c, "High", "Low")
    fit <- survdiff(Surv(Survival_months, Status) ~ group, data = df)
    stat <- fit$chisq
    if (!is.na(stat) && stat > max_stat) { max_stat <- stat }
  }
  return(max_stat)
}

true_max_stat_cum <- get_max_logrank_cum(perm_data_cum)

set.seed(2026) 
n_permutations <- 1000
perm_stats_cum <- numeric(n_permutations)

for (i in 1:n_permutations) {
  shuffled_df <- perm_data_cum
  shuffled_idx <- sample(nrow(shuffled_df))
  shuffled_df$Survival_months <- shuffled_df$Survival_months[shuffled_idx]
  shuffled_df$Status <- shuffled_df$Status[shuffled_idx]
  perm_stats_cum[i] <- get_max_logrank_cum(shuffled_df)
}

adjusted_p_value_cum <- sum(perm_stats_cum >= true_max_stat_cum) / n_permutations

cat("--------------------------------------------------\n")
cat("Results for Cumulative Insulin Dose (units)\n")
cat("True Max Chi-Square Stat:               ", round(true_max_stat_cum, 2), "\n")
cat("Cumulative Permutation-Adjusted P-value:", format(adjusted_p_value_cum, nsmall=4), "\n")
cat("--------------------------------------------------\n")

f6_data <- diabetic %>%
  filter(Post_Diagnosis_Insulin == 1) %>%
  mutate(
    Daily_Dose_Group = factor(ifelse(Average_Daily_Ins_Dose > 10, "High Dose (>10 units/day)", "Low Dose (<=10 units/day)"), 
                              levels = c("Low Dose (<=10 units/day)", "High Dose (>10 units/day)")),
    Cum_Dose_Group = factor(ifelse(Cumulative_Ins_Dose > 50, "High Dose (>50 units)", "Low Dose (<=50 units)"), 
                            levels = c("Low Dose (<=50 units)", "High Dose (>50 units)"))
  )

hist_theme <- theme_bw() + theme(
  plot.title = element_text(face = "bold", hjust = 0.5),
  panel.grid.minor = element_blank()
)

p6a <- ggplot(f6_data %>% filter(!is.na(Average_Daily_Ins_Dose)), aes(x = Average_Daily_Ins_Dose)) +
  geom_histogram(binwidth = 5, fill = "#87CEEB", color = "black") +
  labs(tag = "A", x = "Average Daily Insulin (units/day)", y = "Count") +
  hist_theme +
  theme(plot.tag = element_text(face = "bold", size = 16))

p6c <- ggplot(f6_data %>% filter(!is.na(Cumulative_Ins_Dose)), aes(x = Cumulative_Ins_Dose)) +
  geom_histogram(bins = 40, fill = "#87CEEB", color = "black") +
  scale_x_continuous(
    trans = "log1p", 
    labels = scales::comma,
    breaks = c(0, 50, 500, 2500, 10000, 25000)
  ) +
  labs(tag = "C", x = "Cumulative Insulin (units, log-scaled)", y = "Count") +
  hist_theme +
  theme(plot.tag = element_text(face = "bold", size = 16))

daily_pval_str <- paste0(
  "Max Chi-Square: ", round(true_max_stat, 2), "\n",
  "Iterations: ", n_permutations, "\n",
  ifelse(adjusted_p_value < 0.0001, 
         "Permutation p < 0.0001", 
         paste0("Permutation p = ", format(round(adjusted_p_value, 4), nsmall=4)))
)

cum_pval_str <- paste0(
  "Max Chi-Square: ", round(true_max_stat_cum, 2), "\n",
  "Iterations: ", n_permutations, "\n",
  ifelse(adjusted_p_value_cum < 0.0001, 
         "Permutation p < 0.0001", 
         paste0("Permutation p = ", format(round(adjusted_p_value_cum, 4), nsmall=4)))
)

fit_6b <- survfit(Surv(Survival_months, Status) ~ Daily_Dose_Group, data = f6_data %>% filter(!is.na(Daily_Dose_Group)))
p6b <- ggsurvplot(
  fit_6b,
  data = f6_data,
  conf.int = TRUE,
  pval = daily_pval_str, 
  pval.coord = c(23, 0.60),
  pval.size =3.5,
  palette = c("#87CEEB", "#FF0000"), 
  legend.title = "",
  legend.labs = c("Low Dose (<=10 units/day)", "High Dose (>10 units/day)"),
  legend = c(0.6, 0.85),
  xlim = c(0, 50),
  xlab = "Time (months)",
  ggtheme = hist_theme
)
p6b$plot <- p6b$plot + labs(tag = "B") + theme(plot.tag = element_text(face = "bold", size = 16))

fit_6d <- survfit(Surv(Survival_months, Status) ~ Cum_Dose_Group, data = f6_data %>% filter(!is.na(Cum_Dose_Group)))
p6d <- ggsurvplot(
  fit_6d,
  data = f6_data,
  conf.int = TRUE,
  pval = cum_pval_str,   
  pval.coord = c(23, 0.60),
  pval.size = 3.5,
  palette = c("#87CEEB", "#FF0000"),
  legend.title = "",
  legend.labs = c("Low Dose (<=50 units)", "High Dose (>50 units)"),
  legend = c(0.6, 0.85),
  xlim = c(0, 50),
  xlab = "Time (months)",
  ggtheme = hist_theme
)
p6d$plot <- p6d$plot + labs(tag = "D") + theme(plot.tag = element_text(face = "bold", size = 16))


figure6_legend <- paste0(
  "**Figure 6**: Low versus high insulin usage. Distribution of average daily insulin among ",
  "diabetic patients **(A)**. Kaplan-Meier survival difference between low versus high daily ",
  "insulin usage at the optimal cutoff of 10 units/day, and survival difference remained ",
  "significant after permutation testing with 1,000 iterations **(B)**. Distribution of cumulative ",
  "insulin among diabetic patients **(C)**. Kaplan-Meier survival difference between low ",
  "versus high cumulative insulin usage at the optimal cutoff of 50 units, but survival ",
  "difference was not significant after permutation testing with 1,000 iterations **(D)**."
)

layout_fig6 <- "
AB
CD
"

combined_fig6 <- (p6a + p6b$plot + p6c + p6d$plot) +
  plot_layout(design = layout_fig6) +
  plot_annotation(
    title = 'Figure 6',
    caption = figure6_legend,
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", color = "black"),
      plot.caption = element_textbox_simple(
        size = 16,
        lineheight = 1.2,
        padding = margin(t = 10, r = 10, b = 10, l = 10),
        margin = margin(t = 20, r = 0, b = 0, l = 0),
        fill = "white"
      )
    )
  )

ggsave(
  filename = "Figure 6.png",
  plot = combined_fig6,
  device = "png",
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)





# Supplementary Figure 1: Neither vs both, neither vs NIDM(s), and insulin vs both KM survival analysis --------------
# Graph A: Neither vs. Both
supp1_a_df <- all_patients %>% filter(Ins_NIDM_Status %in% c("NIDM Only", "Both"))
fit_s1a <- survfit(Surv(Survival_months, Status) ~ Ins_NIDM_Status, data = supp1_a_df)
p_s1a <- fig4_km_helper(
  fit_s1a, 
  supp1_a_df, 
  "Cohort", 
  c("NIDM Only", "Both"), 
  tag_label = "A", 
  palette_colors = c("#00A087FF", "#F39B7FFF")
)

# Graph B: Neither vs. NIDM Only
supp1_b_df <- all_patients %>% filter(Ins_NIDM_Status %in% c("Neither", "NIDM Only"))
fit_s1b <- survfit(Surv(Survival_months, Status) ~ Ins_NIDM_Status, data = supp1_b_df)
p_s1b <- fig4_km_helper(
  fit_s1b, 
  supp1_b_df, 
  "Cohort", 
  c("Neither", "NIDM Only"), 
  tag_label = "B", 
  palette_colors = c("#4DBBD5FF", "#00A087FF")
)

# Graph C: Insulin Only vs. Both
supp1_c_df <- all_patients %>% filter(Ins_NIDM_Status %in% c("Insulin Only", "Both"))
fit_s1c <- survfit(Surv(Survival_months, Status) ~ Ins_NIDM_Status, data = supp1_c_df)
p_s1c <- fig4_km_helper(
  fit_s1c, 
  supp1_c_df, 
  "Cohort", 
  c("Insulin Only", "Both"), 
  tag_label = "C", 
  palette_colors = c("#3C5488FF", "#F39B7FFF")
)

supp_figure1_legend <- paste0(
  "**Supplementary Figure 1**: Additional analysis on the influence of insulin versus non-insulin",
  " diabetes medications (NIDM) on patient survival. Compared to patients using ",
  "both insulin and NIDM, there is a trend favoring those using NIDM only **(A)**. No ",
  "difference was seen in survival between patients using NIDM and neither **(B)**, or ",
  "between those using insulin only and both insulin and NIDM **(D)**." 
)

layout_sfig1 <- "
ABC
"

combined_sfig1 <- (p_s1a$plot + p_s1b$plot + p_s1c$plot) +
  plot_layout(design = layout_sfig1) +
  plot_annotation(
    title = 'Supplementary Figure 1',
    caption = supp_figure1_legend,
    theme = theme(
      plot.title = element_text(size = 26, face = "bold", color = "black"),
      plot.caption = element_textbox_simple(
        size = 26,
        lineheight = 1.2,
        padding = margin(t = 10, r = 10, b = 10, l = 10),
        margin = margin(t = 20, r = 0, b = 0, l = 0),
        fill = "white"
      )
    )
  )

ggsave(
  filename = "Supplementary Figure 1.png",
  plot = combined_sfig1,
  device = "png",
  width = 18,
  height = 6.5,
  units = "in",
  dpi = 300
)

# Supplementary Figure 2: HbA1c, Steroid Dosing + Complication Count histograms --------------------------------------------
#make sure wilcoxon is spelled out
#chi-squared test spelled out
#make sure HD (>4mg) and LD (<= 4 mg)
supp_fig2_df <- diabetic %>%
  filter(!is.na(Post_Diagnosis_Insulin)) %>%
  mutate(
    Insulin_Group = factor(Post_Diagnosis_Insulin, levels = c(0, 1), labels = c("No Insulin", "Insulin")),
    Complication_Count = as.numeric(ifelse(is.na(Diabetes_Complications), 0, nchar(str_remove_all(Diabetes_Complications, ",."))))
  )

wilcox_comp <- wilcox.test(Complication_Count ~ Insulin_Group, data = supp_fig2_df)
chisq_comp <- chisq.test(table(supp_fig3_df$Complication_Count, supp_fig2_df$Insulin_Group))

wilcox_dex <- wilcox.test(Average_Dex_Dose ~ Insulin_Group, data = supp_fig3_df)

supp_fig2_df <- supp_fig2_df %>%
  mutate(Dex_HD_LD = case_when(
    is.na(Average_Dex_Dose) ~ NA_character_,
    Average_Dex_Dose <= 4 ~ "Low Dose (<= 4mg)",
    Average_Dex_Dose > 4 ~ "High Dose (> 4mg)"
  ))
chisq_dex_hdld <- chisq.test(table(supp_fig2_df$Dex_HD_LD, supp_fig2_df$Insulin_Group))

supp_fig2_df_hba1c <- supp_fig2_df %>% filter(!is.na(Baseline_HA1c) & Baseline_HA1c > 0)
wilcox_HbA1c <- wilcox.test(Baseline_HA1c ~ Insulin_Group, data = supp_fig2_df_hba1c)

supp2_theme <- theme_bw() + theme(
  plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
  legend.position = "bottom",
  legend.title = element_blank(),
  panel.grid.minor = element_blank()
)

# Graph A: Complications
df_A <- supp_fig2_df %>%
  filter(!is.na(Complication_Count)) %>%
  count(Insulin_Group, Complication_Count) %>%
  tidyr::complete(Insulin_Group, Complication_Count, fill = list(n = 0)) %>%
  group_by(Insulin_Group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

supp2_A <- ggplot(df_A, aes(x = factor(Complication_Count), y = pct, fill = Insulin_Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8, color = "black") +
  scale_fill_manual(values = c("No Insulin" = "#1f77b4", "Insulin" = "#d62728")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(name = "Number of Complications") +
  labs(tag = "A", title = "Diabetic Complication Count", y = "Percentage of Group") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 3.5,
           label = sprintf("\nWilcoxon p = %.4f  \nChi-squared p = %.4f  ", wilcox_comp$p.value, chisq_comp$p.value)) +
  supp2_theme


# Graph B: HbA1c (Previously C)
df_B <- supp_fig2_df_hba1c %>%
  filter(!is.na(Baseline_HA1c)) %>%
  mutate(Bin_Start = floor(Baseline_HA1c),
         Bin_Mid = Bin_Start + 0.5) %>%
  count(Insulin_Group, Bin_Mid) %>%
  tidyr::complete(Insulin_Group, Bin_Mid, fill = list(n = 0)) %>%
  group_by(Insulin_Group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

supp2_B <- ggplot(df_B, aes(x = Bin_Mid, y = pct, fill = Insulin_Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8, color = "black") +
  scale_fill_manual(values = c("No Insulin" = "#1f77b4", "Insulin" = "#d62728")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(name = "HbA1c (%)", breaks = seq(4, 16, by = 2)) +
  labs(tag = "B", title = "Baseline HbA1c", y = "Percentage of Group") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 3.5,
           label = sprintf("\nWilcoxon p = %.4f  ", wilcox_HbA1c$p.value)) +
  supp2_theme


# Graph C: Steroid Dose (Previously B)
df_C <- supp_fig2_df %>%
  filter(!is.na(Average_Dex_Dose)) %>%
  mutate(Bin_Start = floor(Average_Dex_Dose / 2.5) * 2.5,
         Bin_Mid = Bin_Start + 1.25) %>%
  count(Insulin_Group, Bin_Mid) %>%
  tidyr::complete(Insulin_Group, Bin_Mid, fill = list(n = 0)) %>%
  group_by(Insulin_Group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

supp2_C <- ggplot(df_C, aes(x = Bin_Mid, y = pct, fill = Insulin_Group)) +
  geom_col(position = position_dodge(width = 2.0), width = 2.0, color = "black") +
  scale_fill_manual(values = c("No Insulin" = "#1f77b4", "Insulin" = "#d62728")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(name = "Average Dexamethasone Dose (mg/day)", breaks = seq(0, 30, by = 5)) +
  labs(tag = "C", title = "Average Dexamethasone Dose (Cutoff = 4mg/day)", y = "Percentage of Group") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 3.5,
           label = sprintf("\nWilcoxon p = %.4f  \nHigh vs Low Dose Chi-squared p = %.4f  ", wilcox_dex$p.value, chisq_dex_hdld$p.value)) +
  supp2_theme

supp_figure2_legend <- paste0(
  "**Supplementary Figure 2**: Severity of diabetic complications, serum marker and ",
  "dexamethasone usage. There was no difference in the distribution of the number of ",
  "diabetic complications between the non-diabetic and diabetic cohorts **(A)**. A higher ",
  "hemoglobin A1c was seen in diabetic patients than non-diabetics **(B)**. Distribution of ",
  "dexamethasone dosage in insulin-using patients was only marginally higher compared ",
  "to non-insulin-using patients, and no difference was noted between high versus low ",
  "dose dexamethasone use in the two groups **(C)**."
)

layout_sfig2 <- "
ABC
"

combined_sfig2 <- (supp2_A + supp2_B + supp2_C) +
  plot_layout(design = layout_sfig2) +
  plot_annotation(
    title = 'Supplementary Figure 2',
    caption = supp_figure2_legend,
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", color = "black"),
      plot.caption = element_textbox_simple(
        size = 20,
        lineheight = 1.2,
        padding = margin(t = 10, r = 10, b = 10, l = 10),
        margin = margin(t = 20, r = 0, b = 0, l = 0),
        fill = "white"
      )
    )
  )

ggsave(
  filename = "Supplementary Figure 2.png",
  plot = combined_sfig2,
  device = "png",
  width = 14,
  height = 7,
  units = "in",
  dpi = 300
)






# Supplementary Figure 3: Tumor/FLAIR measurement analysis -----------------------------------------------------------------
sfig3_data <- read_excel("~/Desktop/GBM/Updated Tumor Velocites.xlsx")
sfig3_data <- subset(sfig3_data, sfig3_data$`Cumulative Insulin Dose Since Last Scan (units)` > 0)

scan_long <- sfig3_data %>%
  mutate(
    Avg_Ins = as.numeric(`Avg Daily Ins Dose Since Last Scan (units/day)`),
    Cumulative_Ins_Dose = as.numeric(`Cumulative Insulin Dose Since Last Scan (units)`),
    Tum_Meas = as.numeric(`Tumor Measurement (mm^2)`),
    Fla_Meas = as.numeric(`FLAIR Measurement (mm^2)`),
    Tum_Vel = as.numeric(`Tumor Velocity Since Last Scan (mm^2/day)`),
    Fla_Vel = as.numeric(`FLAIR Velocity Since Last Scan (mm^2/day)`)
  )

plot_scatter <- function(df, x_col, y_col, xlab, ylab, tag) {
  df_plot <- df %>% filter(!is.na(.data[[x_col]]) & !is.na(.data[[y_col]]))
  fit <- lm(df_plot[[y_col]] ~ df_plot[[x_col]])
  r2 <- summary(fit)$r.squared
  cor_test <- cor.test(df_plot[[x_col]], df_plot[[y_col]], method = "pearson", use = "pairwise.complete.obs")
  pcc <- cor_test$estimate
  pval <- cor_test$p.value
  m <- coef(fit)[2]
  b <- coef(fit)[1]
  eq_str <- sprintf("y = %.2f x + %.2f", m, b)
  r2_str <- ifelse(r2 < 0.0001, "R^2 < 0.0001", sprintf("R^2 = %.4f", r2))
  pval_str <- ifelse(pval < 0.0001, "p < 0.0001", sprintf("p = %.4f", pval))
  pcc_str <- sprintf("PCC = %.4f    %s", pcc, pval_str)
  stat_text <- paste(eq_str, r2_str, pcc_str, sep = "\n")
  plot_title <- paste(ylab, "vs.", gsub(" \\(.*", "", xlab)) 
  ggplot(df_plot, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(size = 2, color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
    labs(tag = tag, x = xlab, y = ylab, title = plot_title) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.tag = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank()
    ) +
    annotate("label", x = Inf, y = Inf, label = stat_text, 
             hjust = 1.0, vjust = 1.2, size = 3.5, 
             fill = "white", color = "black", label.size = 1)
}

ps3a <- plot_scatter(scan_long, "Avg_Ins", "Tum_Meas", "Average Insulin (units/day)", "Tumor Measurement (mm^2)", "A")
ps3b <- plot_scatter(scan_long, "Avg_Ins", "Fla_Meas", "Average Insulin (units/day)", "FLAIR Measurement (mm^2)", "B")
ps3c <- plot_scatter(scan_long, "Cumulative_Ins_Dose", "Tum_Meas", "Cumulative Insulin (units)", "Tumor Measurement (mm^2)", "C")
ps3d <- plot_scatter(scan_long, "Cumulative_Ins_Dose", "Fla_Meas", "Cumulative Insulin (units)", "FLAIR Measurement (mm^2)", "D")

ps3e <- plot_scatter(scan_long, "Avg_Ins", "Tum_Vel", "Average Insulin (units/day)", "Tumor Velocity (mm^2/day)", "E")
ps3f <- plot_scatter(scan_long, "Avg_Ins", "Fla_Vel", "Average Insulin (units/day)", "FLAIR Velocity (mm^2/day)", "F")
ps3g <- plot_scatter(scan_long, "Cumulative_Ins_Dose", "Tum_Vel", "Cumulative Insulin (units)", "Tumor Velocity (mm^2/day)", "G")
ps3h <- plot_scatter(scan_long, "Cumulative_Ins_Dose", "Fla_Vel", "Cumulative Insulin (units)", "FLAIR Velocity (mm^2/day)", "H")

supp_figure3_legend <- paste0(
  "**Supplementary Figure 3**: Glioblastoma tumor size and growth velocity versus daily ",
  "and cumulative insulin dose. No difference was seen between average daily insulin ",
  "dose and bi-dimensional tumor measurements from T1-weighted post-gadolinium MP ",
  "RAGE **(A)** and FLAIR **(B)** images. No difference was also observed between ",
  "cumulative insulin dose and bi-dimensional tumor measurements from T1-weighted ",
  "post-gadolinium MP RAGE **(C)** and FLAIR **(D)** images. No difference was noted ",
  "between average daily insulin dose and tumor growth velocity as measured using T1-",
  "weighted post-gadolinium MP RAGE images **(E)**. There is a slight but downward ",
  "sloping relationship between average daily insulin dose and tumor growth velocity as ",
  "measured using FLAIR images **(F)**. No difference was seen between the cumulative ",
  "insulin dose and tumor growth velocity as estimated using T1-weighted post-gadolinium ",
  "MP RAGE **(G)** or FLAIR images **(H)**."
)

layout_sfig3 <- "
AB
CD
EF
GH
"

combined_sfig3 <- (ps3a + ps3b + ps3c + ps3d + ps3e + ps3f + ps3g + ps3h) +
  plot_layout(design = layout_sfig3) +
  plot_annotation(
    title = 'Supplementary Figure 3',
    caption = supp_figure3_legend,
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", color = "black"),
      plot.caption = element_textbox_simple(
        size = 18,
        lineheight = 1.2,
        padding = margin(t = 10, r = 10, b = 10, l = 10),
        margin = margin(t = 20, r = 0, b = 0, l = 0),
        fill = "white"
      )
    )
  )

print(combined_sfig3)

ggsave(
  filename = "Supplementary Figure 3.png",
  plot = combined_sfig3,
  device = "png",
  width = 12,
  height = 16,
  units = "in",
  dpi = 300
)












# UNUSED FIGURES -------------------------------------------------------
# Supplementary Figure 1: Univariate survival analysis for treatment status + MGMT status sensitivity --------------------------------
supp1_tx_df <- diabetic %>% 
  mutate(safe_treatment = `Treatment Status`) %>% 
  filter(!is.na(safe_treatment))

fit_s1a <- survfit(Surv(Survival_months, Status) ~ safe_treatment, data = supp1_tx_df)
p_s1a <- fig2_km_helper(
  fit_s1a, 
  supp1_tx_df, 
  "Treatment Status", 
  c("None", "RT Only", "Chemotherapy Only", "TMZ/RT Only", "TMZ/RT & 1+ Line"), 
  tag_label = "A",
  p_coord = c(48, 0.5) 
)

supp1_mgmt_meth_df <- diabetic %>%
  mutate(
    MGMT_Sens_Meth = case_when(
      `MGMT Status` == "Unknown" ~ "Methylated",
      TRUE ~ as.character(`MGMT Status`)
    ),
    MGMT_Sens_Meth = factor(MGMT_Sens_Meth, levels = c("Methylated", "Unmethylated"))
  )

fit_s1b <- survfit(Surv(Survival_months, Status) ~ MGMT_Sens_Meth, data = supp1_mgmt_meth_df)
p_s1b <- fig2_km_helper(
  fit_s1b, 
  supp1_mgmt_meth_df, 
  expression(bolditalic("MGMT") ~ bold("Status (Unknowns=Meth)")), 
  c("Methylated", "Unmethylated"), 
  tag_label = "B"
)

supp1_mgmt_unmeth_df <- diabetic %>%
  mutate(
    MGMT_Sens_Unmeth = case_when(
      `MGMT Status` == "Unknown" ~ "Unmethylated",
      TRUE ~ as.character(`MGMT Status`)
    ),
    MGMT_Sens_Unmeth = factor(MGMT_Sens_Unmeth, levels = c("Methylated", "Unmethylated"))
  )

fit_s1c <- survfit(Surv(Survival_months, Status) ~ MGMT_Sens_Unmeth, data = supp1_mgmt_unmeth_df)
p_s1c <- fig2_km_helper(
  fit_s1c, 
  supp1_mgmt_unmeth_df, 
  expression(bolditalic("MGMT") ~ bold("Status (Unknowns=Unmeth)")), 
  c("Methylated", "Unmethylated"), 
  tag_label = "C"
)

pdf("Supplementary Figure 1.pdf", width = 18, height = 6)
gridExtra::grid.arrange(
  p_s1a$plot, p_s1b$plot, p_s1c$plot, 
  ncol = 3
)
dev.off()

# Supplementary Table 1: Multivariate survival analysis of prognostic factors (stratified age) ------------------
format_p <- function(x) { ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x)) }

shared_labels_t2 <- list(
  `Age (yrs.)`         ~ "Age at Diagnosis", 
  Baseline_KPS         ~ "Baseline KPS (Continuous)",
  `Resection Status`   ~ "Resection Status",
  `MGMT Status`        ~ "MGMT Status",
  `Treatment Status`   ~ "Treatment Status"
)

all_patients$`MGMT Status` <- relevel(factor(all_patients$`MGMT Status`), ref = "Unmethylated")

all_patients$`Treatment Status` <- relevel(factor(all_patients$`Treatment Status`), ref = "TMZ/RT Only")

t2_cox_nondiab <- coxph(
  Surv(Survival_months, Status) ~ `Age (yrs.)` + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = all_patients,
  subset = (diabetes == "Non-Diabetic")
)

t2_tbl_nondiab <- tbl_regression(
  t2_cox_nondiab, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_t2
)

t2_cox_diab <- coxph(
  Surv(Survival_months, Status) ~ `Age (yrs.)` + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = all_patients,
  subset = (diabetes == "Diabetic")
)

t2_tbl_diab <- tbl_regression(
  t2_cox_diab, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_t2
) 

tbl_merge(
  tbls = list(t2_tbl_nondiab, t2_tbl_diab),
  tab_spanner = c("**Non-Diabetics**", "**Diabetics Only**")
) %>%
  modify_header(label = "**Prognostic Factor**") %>%
  modify_caption("**Supplementary Table 1: Prognostic Factor Multivariable Cox Models**") %>%
  bold_labels() %>%
  remove_abbreviation() %>%
  as_gt() %>%
  tab_options(
    table.font.size = px(12),        
    data_row.padding = px(3),        
    row_group.padding = px(4)
  ) %>%
  tab_source_note(
    source_note = "Abbreviations: HR = Hazard Ratio; CI = Confidence Interval; KPS = Karnofsky Performance Status; MGMT = O6-methylguanine-DNA methyltransferase; STR = Subtotal Resection; GTR = Gross Total Resection; RT = Radiation Therapy; TMZ = Temozolomide."
  ) %>%
  gtsave(filename = "Supplementary Table 1.pdf", vwidth = 1000, expand = 10)




#Table 2a: Removing Unknown MGMT only ----------------
df_v1 <- all_patients %>% filter(`MGMT Status` != "Unknown")

df_v1$`MGMT Status` <- relevel(factor(df_v1$`MGMT Status`), ref = "Unmethylated")
df_v1$`Treatment Status` <- relevel(factor(df_v1$`Treatment Status`), ref = "TMZ/RT Only")

format_p <- function(x) { ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x)) }

shared_labels_s1 <- list(
  Age_At_Diagnosis     ~ "Age at Diagnosis (Continuous)", 
  Baseline_KPS         ~ "Baseline KPS (Continuous)",
  `Resection Status`   ~ "Resection Status",
  `MGMT Status`        ~ "MGMT Status",
  `Treatment Status`   ~ "Treatment Status"
)

# 2. Models
supp_t1_cox_nondiab_v1 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = df_v1,
  subset = (diabetes == "Non-Diabetic")
)
supp_t1_tbl_nondiab_v1 <- tbl_regression(
  supp_t1_cox_nondiab_v1, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

supp_t1_cox_diab_v1 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = df_v1,
  subset = (diabetes == "Diabetic")
)
supp_t1_tbl_diab_v1 <- tbl_regression(
  supp_t1_cox_diab_v1, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

# 3. Table Merge & Export
tbl_merge(
  tbls = list(supp_t1_tbl_nondiab_v1, supp_t1_tbl_diab_v1),
  tab_spanner = c("**Non-Diabetics**", "**Diabetics Only**")
) %>%
  modify_header(label = "**Prognostic Factor**") %>%
  modify_caption("**Table 2 (V1)**: Prognostic factor multivariable cox models**") %>%
  bold_labels() %>%
  remove_abbreviation() %>%
  as_gt() %>%
  tab_options(
    table.font.size = px(12),        
    data_row.padding = px(3),        
    row_group.padding = px(4)
  ) %>%
  tab_source_note(
    source_note = "Abbreviations: HR = Hazard Ratio; CI = Confidence Interval; KPS = Karnofsky Performance Status; MGMT = O6-methylguanine-DNA methyltransferase; STR = Subtotal Resection; GTR = Gross Total Resection; RT = Radiation Therapy; TMZ = Temozolamide."
  ) %>%
  gtsave(filename = "Table 2a (Unknown MGMT Removed Only).pdf", vwidth = 1000, expand = 10)




#Table 2b: Removing Unknown MGMT + Unmethylated* vs. Methylated ---------
# 1. Data Prep: Group Unknowns into Unmethylated*
df_v2 <- all_patients %>%
  mutate(`MGMT Status` = case_when(
    `MGMT Status` %in% c("Unmethylated", "Unknown") ~ "Unmethylated*",
    TRUE ~ as.character(`MGMT Status`)
  ))

df_v2$`MGMT Status` <- relevel(factor(df_v2$`MGMT Status`), ref = "Unmethylated*")
df_v2$`Treatment Status` <- relevel(factor(df_v2$`Treatment Status`), ref = "TMZ/RT Only")

# 2. Models
supp_t1_cox_nondiab_v2 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = df_v2,
  subset = (diabetes == "Non-Diabetic")
)
supp_t1_tbl_nondiab_v2 <- tbl_regression(
  supp_t1_cox_nondiab_v2, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

supp_t1_cox_diab_v2 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = df_v2,
  subset = (diabetes == "Diabetic")
)
supp_t1_tbl_diab_v2 <- tbl_regression(
  supp_t1_cox_diab_v2, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

# 3. Table Merge & Export
tbl_merge(
  tbls = list(supp_t1_tbl_nondiab_v2, supp_t1_tbl_diab_v2),
  tab_spanner = c("**Non-Diabetics**", "**Diabetics Only**")
) %>%
  modify_header(label = "**Prognostic Factor**") %>%
  modify_caption("**Table 2 (V2)**: Prognostic factor multivariable cox models**") %>%
  bold_labels() %>%
  remove_abbreviation() %>%
  as_gt() %>%
  tab_options(
    table.font.size = px(12),        
    data_row.padding = px(3),        
    row_group.padding = px(4)
  ) %>%
  tab_source_note(
    source_note = "Abbreviations: HR = Hazard Ratio; CI = Confidence Interval; KPS = Karnofsky Performance Status; MGMT = O6-methylguanine-DNA methyltransferase; STR = Subtotal Resection; GTR = Gross Total Resection; RT = Radiation Therapy; TMZ = Temozolamide. *Unmethylated MGMT status includes patients with unknown status."
  ) %>%
  gtsave(filename = "Table 2b (Unknowns Grouped with Unmethylated.pdf", vwidth = 1000, expand = 10)

#Table 2c: Removing Unknown MGMT + Methylated* vs. Unmethylated -------
# 1. Data Prep: Group Unknowns into Methylated*
df_v3 <- all_patients %>%
  mutate(`MGMT Status` = case_when(
    `MGMT Status` %in% c("Methylated", "Unknown") ~ "Methylated*",
    TRUE ~ as.character(`MGMT Status`)
  ))

df_v3$`MGMT Status` <- relevel(factor(df_v3$`MGMT Status`), ref = "Unmethylated")
df_v3$`Treatment Status` <- relevel(factor(df_v3$`Treatment Status`), ref = "TMZ/RT Only")

# 2. Models
supp_t1_cox_nondiab_v3 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = df_v3,
  subset = (diabetes == "Non-Diabetic")
)
supp_t1_tbl_nondiab_v3 <- tbl_regression(
  supp_t1_cox_nondiab_v3, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

supp_t1_cox_diab_v3 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = df_v3,
  subset = (diabetes == "Diabetic")
)
supp_t1_tbl_diab_v3 <- tbl_regression(
  supp_t1_cox_diab_v3, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

# 3. Table Merge & Export
tbl_merge(
  tbls = list(supp_t1_tbl_nondiab_v3, supp_t1_tbl_diab_v3),
  tab_spanner = c("**Non-Diabetics**", "**Diabetics Only**")
) %>%
  modify_header(label = "**Prognostic Factor**") %>%
  modify_caption("**Table 2 (V3)**: Prognostic factor multivariable cox models**") %>%
  bold_labels() %>%
  remove_abbreviation() %>%
  as_gt() %>%
  tab_options(
    table.font.size = px(12),        
    data_row.padding = px(3),        
    row_group.padding = px(4)
  ) %>%
  tab_source_note(
    source_note = "Abbreviations: HR = Hazard Ratio; CI = Confidence Interval; KPS = Karnofsky Performance Status; MGMT = O6-methylguanine-DNA methyltransferase; STR = Subtotal Resection; GTR = Gross Total Resection; RT = Radiation Therapy; TMZ = Temozolamide. *Methylated MGMT status includes patients with unknown status."
  ) %>%
  gtsave(filename = "Table 2c (Unknowns Grouped with Methylated.pdf", vwidth = 1000, expand = 10)


#Table 2d: Removing Unknown MGMT + Treatment Status -------
# 1. Data Prep: Remove unknowns
df_v4 <- all_patients %>% filter(`MGMT Status` != "Unknown")

df_v4$`MGMT Status` <- relevel(factor(df_v4$`MGMT Status`), ref = "Unmethylated")
# No need to relevel Treatment Status since we are dropping it from the model

# Adjusted labels map without Treatment Status
shared_labels_v4 <- list(
  Age_At_Diagnosis     ~ "Age at Diagnosis (Continuous)", 
  Baseline_KPS         ~ "Baseline KPS (Continuous)",
  `Resection Status`   ~ "Resection Status",
  `MGMT Status`        ~ "MGMT Status"
)

# 2. Models (Treatment Status removed from formulas)
supp_t1_cox_nondiab_v4 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status`,
  data = df_v4,
  subset = (diabetes == "Non-Diabetic")
)
supp_t1_tbl_nondiab_v4 <- tbl_regression(
  supp_t1_cox_nondiab_v4, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_v4
)

supp_t1_cox_diab_v4 <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status`,
  data = df_v4,
  subset = (diabetes == "Diabetic")
)
supp_t1_tbl_diab_v4 <- tbl_regression(
  supp_t1_cox_diab_v4, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_v4
)

# 3. Table Merge & Export
tbl_merge(
  tbls = list(supp_t1_tbl_nondiab_v4, supp_t1_tbl_diab_v4),
  tab_spanner = c("**Non-Diabetics**", "**Diabetics Only**")
) %>%
  modify_header(label = "**Prognostic Factor**") %>%
  modify_caption("**Table 2 (V4)**: Prognostic factor multivariable cox models**") %>%
  bold_labels() %>%
  remove_abbreviation() %>%
  as_gt() %>%
  tab_options(
    table.font.size = px(12),        
    data_row.padding = px(3),        
    row_group.padding = px(4)
  ) %>%
  tab_source_note(
    source_note = "Abbreviations: HR = Hazard Ratio; CI = Confidence Interval; KPS = Karnofsky Performance Status; MGMT = O6-methylguanine-DNA methyltransferase; STR = Subtotal Resection; GTR = Gross Total Resection."
  ) %>%
  gtsave(filename = "Table 2d (MGMT Unknowns + Tx Status Removed).pdf", vwidth = 1000, expand = 10)

# figure 5: inverse probability weighting for insulin-use exposure ---------------------------------------------------------------
f4_data <- diabetic %>%
  filter(!is.na(Post_Diagnosis_Insulin)) %>%
  dplyr::select(Survival_months, Status, Post_Diagnosis_Insulin, 
                Age_At_Diagnosis, `Baseline KPS`, `Resection Status`, 
                `Treatment Status`, `MGMT Status`, Sex, `Average_Dex_Dose`, NIDM) %>%
  na.omit()

ps_model <- glm(
  Post_Diagnosis_Insulin ~ Age_At_Diagnosis + `Baseline KPS` + `Resection Status` + 
    `Treatment Status` + `MGMT Status` + Sex + `Average_Dex_Dose` + NIDM,
  family = binomial(link = "logit"),
  data = f4_data
)

f4_data$PS <- predict(ps_model, type = "response")
p_insulin <- mean(f4_data$Post_Diagnosis_Insulin == 1)

f4_data <- f4_data %>%
  mutate(
    IPW_stabilized = ifelse(
      Post_Diagnosis_Insulin == 1,
      p_insulin / PS,
      (1 - p_insulin) / (1 - PS)
    ),
    Insulin_Factor = factor(Post_Diagnosis_Insulin, 
                            levels = c(1, 0), 
                            labels = c("Insulin-User", "No Insulin"))
  )

# Max weight was >17 prior to outlier filtering -> 2.68 after
lower_bound <- quantile(f3_data$IPW_stabilized, 0.01)
upper_bound <- quantile(f3_data$IPW_stabilized, 0.99)


f4_data <- f4_data %>%
  mutate(
    IPW_truncated = case_when(
      IPW_stabilized > upper_bound ~ upper_bound,
      IPW_stabilized < lower_bound ~ lower_bound,
      TRUE ~ IPW_stabilized
    )
  )

ipw_cox <- coxph(
  Surv(Survival_months, Status) ~ Insulin_Factor,
  data = f4_data,
  weights = IPW_truncated,
  robust = TRUE 
)

robust_p <- summary(ipw_cox)$robscore["pvalue"]
if (robust_p < 0.0001) {
  ipw_pval_str <- "p < 0.0001 (Weighted)"
} else {
  ipw_pval_str <- paste0("p = ", format(round(robust_p, 4), nsmall = 4), " (Weighted)")
}

ipw_fit <- survfit(
  Surv(Survival_months, Status) ~ Insulin_Factor,
  data = f4_data,
  weights = IPW_truncated
)

fig4_plot <- ggsurvplot(
  ipw_fit,
  data = f4_data,
  conf.int = TRUE,
  pval = ipw_pval_str, 
  pval.coord = c(48, 0.5),
  palette = "npg",
  legend.title = "IPW Adjusted Cohort",
  legend.labs = c("Insulin-User", "No Insulin"),
  legend = c(0.99, 0.95),
  xlim = c(0, 80),
  xlab = "Time (months)",
  break.time.by = 6,
  ggtheme = theme_bw() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold"),
    legend.justification = c("right", "top"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
)

fig4_plot$plot <- fig4_plot$plot + 
  labs(title = "IPW-Adjusted Survival by Insulin-Use Exposure") +
  theme(plot.margin = margin(5, 5, 5, 5, unit="pt"))

pdf("IPW-KM Surve.pdf", width = 8, height = 6)
print(fig4_plot)
dev.off()







# Table 1 .docx -----
library(flextable)
library(officer)
library(gtsummary)
library(dplyr)

heavy_border <- fp_border(color = "black", width = 1.5)
light_border <- fp_border(color = "#D3D3D3", width = 0.5)

table1 <-
  tbl_summary(
    all_patients %>% dplyr::mutate(diabetes = factor(diabetes, levels = c("Non-Diabetic", "Diabetic"), labels = c("Non-Diabetics", "Diabetics"))),
    include = c(`Age (yrs.)`, `Age_At_Diagnosis`, `Sex`, `Baseline KPS`, `Baseline_KPS`, `Resection Status`, 
                `MGMT Status`, `KI67`, `Ki67`, `Treatment Status`),
    by = diabetes,
    missing = "no",
    type = all_continuous() ~ "continuous", 
    statistic = all_continuous() ~ "{median} ({min}, {max})",
    digits = list(
      `Age_At_Diagnosis` ~ function(x) ifelse(x < 1, "<1", as.character(round(x, 0)))
    ),
    label = list(
      `Age (yrs.)` ~ "Age (years)",
      KI67 ~ "Ki-67 Index",
      `Baseline KPS` ~ "Baseline KPS",
      `MGMT Status` ~ "MGMT Status"
    )
  ) |> 
  add_p(
    test = list(all_categorical() ~ "chisq.test", all_continuous() ~ "wilcox.test")
  ) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(
    label = "Variable",
    all_stat_cols() ~ "{level}\nn = {n}¹", 
    p.value = "p value"
  ) |>
  bold_labels() |>
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = ifelse(grepl("80", label) & !grepl("61", label), ">80", label), 
        label = ifelse(var_type == "continuous", "Median", label),
        row_type = ifelse(var_type == "continuous", "level", row_type),
        p_fmt = ifelse(is.na(p.value), NA_character_, 
                       ifelse(p.value < 0.0001, "<0.0001", format(round(p.value, 4), nsmall = 4))),
        p.value = dplyr::case_when(
          !is.na(p.value) & var_type %in% c("categorical", "dichotomous") ~ paste0(p_fmt, "²"),
          !is.na(p.value) & var_type == "continuous" ~ paste0(p_fmt, "³"),
          TRUE ~ p_fmt
        ),
        label = ifelse(row_type == "level", paste0("\U00A0\U00A0\U00A0\U00A0", label), label)
      ) %>%
      dplyr::select(-p_fmt)
  ) |>
  modify_fmt_fun(p.value ~ function(x) x)

table1 %>%
  as_flex_table() %>%
  border_remove() %>% 
  
  hline(part = "body", border = light_border) %>%
  hline_top(part = "header", border = heavy_border) %>%
  hline_bottom(part = "header", border = heavy_border) %>%
  hline_bottom(part = "body", border = heavy_border) %>%
  
  flextable::valign(valign = "center", part = "all") %>%           
  flextable::padding(padding = 0, part = "all") %>%                
  flextable::line_spacing(space = 1, part = "all") %>%             
  flextable::height_all(height = 0.1, part = "all") %>%            
  flextable::hrule(rule = "atleast", part = "all") %>%             
  
  set_table_properties(layout = "fixed") %>%
  width(j = 1, width = 2.5) %>%  
  width(j = 2:4, width = 1.2) %>% 
  
  flextable::font(fontname = "Arial", part = "all") %>%
  flextable::fontsize(size = 10, part = "all") %>%
  flextable::fontsize(size = 9, part = "footer") %>%
  flextable::align(j = 2:4, align = "center", part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::bold(part = "header") %>%
  
  set_caption(caption = as_paragraph(as_b("Table 1: "), "Demographic data and univariate analysis on non-diabetic and diabetic patients."),
              align_with_table = FALSE, 
              fp_p = fp_par(text.align = "left") 
  ) %>%
  add_footer_lines(values = paste0(
    "¹ n (%); Median (Min, Max)\n",
    "² Pearson's Chi-squared test\n",
    "³ Wilcoxon rank sum test\n\n",
    "KPS = Karnofsky Performance Scale; STR = Subtotal Resection; GTR = Gross Total Resection; ",
    "MGMT = O⁶-methylguanine-DNA methyltransferase; RT = Radiation Therapy; TMZ = Temozolomide"
  )) %>%
  
  save_as_docx(path = "Table 1.docx")



# Table 2 .docx ------
format_p <- function(x) { ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x)) }

shared_labels_s1 <- list(
  Age_At_Diagnosis     ~ "Age at Diagnosis (Continuous)", 
  Baseline_KPS         ~ "Baseline KPS (Continuous)",
  `Resection Status`   ~ "Resection Status",
  `MGMT Status`        ~ "MGMT Status",
  `Treatment Status`   ~ "Treatment Status"
)

all_patients$`MGMT Status` <- relevel(factor(all_patients$`MGMT Status`), ref = "Unmethylated")

all_patients$`Treatment Status` <- factor(
  all_patients$`Treatment Status`, 
  levels = c("None", "RT Only", "Chemotherapy Only", "TMZ/RT Only", "TMZ/RT & 1+ Line")
)

supp_t1_cox_nondiab <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = all_patients,
  subset = (diabetes == "Non-Diabetic")
)

supp_t1_tbl_nondiab <- tbl_regression(
  supp_t1_cox_nondiab, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

supp_t1_cox_diab <- coxph(
  Surv(Survival_months, Status) ~ Age_At_Diagnosis + Baseline_KPS + 
    `Resection Status` + `MGMT Status` + `Treatment Status`,
  data = all_patients,
  subset = (diabetes == "Diabetic")
)

supp_t1_tbl_diab <- tbl_regression(
  supp_t1_cox_diab, exponentiate = TRUE, pvalue_fun = format_p, label = shared_labels_s1
)

std_border <- fp_border(color = "black", width = 1)

heavy_border <- fp_border(color = "black", width = 1.5)
light_border <- fp_border(color = "#D3D3D3", width = 0.5)

tbl_merge(
  tbls = list(supp_t1_tbl_nondiab, supp_t1_tbl_diab),
  tab_spanner = c("**Non-Diabetics**", "**Diabetics**") 
) %>%
  modify_header(
    label = "**Prognostic Factor**", 
    p.value_1 = "**p value**", 
    p.value_2 = "**p value**"
  ) %>%
  bold_labels() %>%
  remove_abbreviation() %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = ifelse(row_type == "level", paste0("\U00A0\U00A0\U00A0\U00A0", label), label)
      )
  ) %>%
  as_flex_table() %>%
  
  border_remove() %>%
  
  add_header_lines(values = as_paragraph(as_b("Table 2: "), "Multivariate survival analysis of prognostic factors on non-diabetic and diabetic patients.")) %>%
  
  hline(i = 1, part = "header", border = heavy_border) %>%          
  hline(i = 2, j = 2:4, part = "header", border = light_border) %>% 
  hline(i = 2, j = 5:7, part = "header", border = light_border) %>% 
  hline(i = 3, part = "header", border = heavy_border) %>%          
  
  
  border_inner_h(part = "body", border = light_border) %>%          
  hline_bottom(part = "body", border = heavy_border) %>%            
  
  flextable::valign(valign = "center", part = "all") %>%
  flextable::padding(padding = 0, part = "all") %>%
  
  flextable::padding(j = 4, padding.right = 25, part = "all") %>%
  flextable::padding(j = 5, padding.left = 25, part = "all") %>%
  
  flextable::line_spacing(space = 1, part = "all") %>%
  flextable::height_all(height = 0.1, part = "all") %>%
  flextable::hrule(rule = "atleast", part = "all") %>%
  
  set_table_properties(layout = "fixed") %>%
  width(j = 1, width = 2.5) %>%
  width(j = c(2, 3, 6, 7), width = 0.8) %>%
  width(j = c(4, 5), width = 1.15) %>%
  
  flextable::font(fontname = "Arial", part = "all") %>%
  flextable::fontsize(size = 10, part = "all") %>%
  flextable::fontsize(size = 9, part = "footer") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:7, align = "center", part = "all") %>%
  
  flextable::align(i = 1, align = "left", part = "header") %>%
  
  add_footer_lines(values = "Abbreviations: HR = Hazard Ratio; CI = Confidence Interval; KPS = Karnofsky Performance Status; MGMT = O⁶-methylguanine-DNA methyltransferase; STR = Subtotal Resection; GTR = Gross Total Resection; RT = Radiation Therapy; TMZ = Temozolomide.") %>%
  flextable::align(align = "left", part = "footer") %>%
  
  save_as_docx(path = "Table 2.docx")