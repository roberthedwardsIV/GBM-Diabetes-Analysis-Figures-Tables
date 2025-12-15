library(tidyverse)
library(readxl)
library(survival)
library(broom)
library(stringr)
library(patchwork)

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
                      labels = c("<40 years", "40-60 years", "61-80 years", "81+ years"))
  Baseline.KPS <- factor(Baseline.KPS, labels = c("0-60", "70-80", "90-100"))
  Resection.Status <- factor(Resection.Status, labels = c("None", "STR", "GTR"))
  Group <- factor(Group, labels = c("Non-diabetic", "Insulin", "NIDM(s)", "Both", "Neither"))
  Sex <- factor(Sex, labels = c("Male", "Female"))
  Treatment.Status <- factor (Treatment.Status, labels = c("None", "RT Only", "Chemo Only", "TMZ/RT Only", "TMZ/RT & 1+ Line" ))
  MGMT.Status <- factor(MGMT.Status, labels = c("Unmethylated", "Methylated"))
  IDH.Type <- factor(IDH.Type, labels = c("Wild-Type", "Mutant"))
  Ki.67 <- factor(Ki.67, labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%"))
})

diabetic <- df[df$Diabetes == "Diabetic", ]
nondiabetic <- df[df$Diabetes == "Non-Diabetic", ]

# --- 2. Figure 2a (Non-Diabetics) ---
cox_model_2a <- coxph(
  Surv(Survival, Vital.Status) ~ Age.Class + Baseline.KPS + Resection.Status + MGMT.Status + IDH.Type, 
  data = nondiabetic
)
res_2a <- tidy(cox_model_2a, exponentiate = TRUE, conf.int = TRUE)

display_df_2a <- res_2a %>%
  mutate(
    Variable = case_when(
      str_detect(term, "Age.Class")       ~ "Age at Diagnosis",
      str_detect(term, "Baseline.KPS")     ~ "Baseline KPS",
      str_detect(term, "Resection.Status")~ "Resection Status",
      str_detect(term, "MGMT.Status")     ~ "MGMT Status",
      str_detect(term, "IDH.Type")        ~ "IDH Type",
      TRUE ~ "Other"
    ),
    Level = str_remove(term, "^(Age\\.Class|Baseline\\.KPS|Resection\\.Status|MGMT\\.Status|IDH\\.Type)"),
    HR_label = sprintf("HR = %.2f (%.1f–%.1f)", estimate, conf.low, conf.high),
    p_label = ifelse(p.value < 0.0001, "p < 0.0001", sprintf("p = %.4f", p.value))
  ) %>%
  select(Variable, Level, estimate, conf.low, conf.high, HR_label, p_label)

ref_df_2a <- tibble(
  Variable = c("Age at Diagnosis", "Baseline KPS", "Resection Status", "MGMT Status", "IDH Type"),
  Level = c("<40 years", "0-60", "None", "Unmethylated", "Wild-Type"), 
  HR_label = "Reference", estimate = 1, conf.low = 1, conf.high = 1, p_label = ""
)

plot_df_2a <- bind_rows(ref_df_2a, display_df_2a) %>%
  mutate(
    conf.low = pmax(conf.low, 0.1), 
    conf.high = pmin(conf.high, 9)
  ) %>%
  mutate(Level = factor(Level, levels = rev(unique(Level)))) 

table_2a <- ggplot(plot_df_2a, aes(y = Level, x = estimate)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "lightgray") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(color = Variable), size = 2) +
  geom_text(aes(label = HR_label), vjust = 2, size = 1.6) +
  geom_text(aes(label = p_label), size = 1.6, vjust = 4, color = "black", fontface = "italic") +
  scale_x_log10(limits = c(0.1, 12)) +
  labs(x = "Hazard Ratio (log-scale)", y = NULL, title = "Non-Diabetics") +
  facet_grid(Variable ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme_minimal(base_size = 7) +
  theme(
    legend.title = element_text(face = "bold", hjust = 0.5),
    strip.text.y.left = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.spacing.y = unit(0.4, "cm"),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  ) +
  guides(color = guide_legend(title = "Prognostic Factor"))






b_diabetic <- diabetic[diabetic$Age.Class != "<40 years", ]


# --- 3. Figure 2b (Diabetics) ---

b_diabetic <- diabetic %>%
  filter(Age.Class != "<40 years") %>%
  mutate(Age.Class = droplevels(Age.Class)) %>%   # drop the unused "<40yrs"
  mutate(Age.Class = relevel(Age.Class, ref = "40-60 years"))  # set proper reference


cox_model_2b <- coxph(
  Surv(Survival, Vital.Status) ~ Age.Class + Baseline.KPS + Resection.Status + MGMT.Status + IDH.Type, 
  data = b_diabetic
)
res_2b <- tidy(cox_model_2b, exponentiate = TRUE, conf.int = TRUE)

display_df_2b <- res_2b %>%
  mutate(
    Variable = case_when(
      str_detect(term, "Age.Class")       ~ "Age at Diagnosis",
      str_detect(term, "Baseline.KPS")     ~ "Baseline KPS",
      str_detect(term, "Resection.Status")~ "Resection Status",
      str_detect(term, "MGMT.Status")     ~ "MGMT Status",
      str_detect(term, "IDH.Type")        ~ "IDH Type",
      TRUE ~ "Other"
    ),
    # Use str_remove
    Level = str_remove(term, "^(Age\\.Class|Baseline\\.KPS|Resection\\.Status|MGMT\\.Status|IDH\\.Type)"),
    
    Level = case_when(
      str_detect(term, "Age\\.Class61-80 years") ~ "61-80 years", 
      str_detect(term, "Age\\.Class81\\+ years") ~ "81+ years", # Handling the '+'
      TRUE ~ Level 
    ),
    
    HR_label = sprintf("HR = %.2f (%.1f–%.1f)", estimate, conf.low, conf.high),
    p_label = ifelse(p.value < 0.0001, "p < 0.0001", sprintf("p = %.4f", p.value))
  ) %>%
  select(Variable, Level, estimate, conf.low, conf.high, HR_label, p_label)

ref_df_2b <- tibble(
  Variable = c("Age at Diagnosis", "Baseline KPS", "Resection Status", "MGMT Status", "IDH Type"),
  # Use simplified reference label
  Level = c("40-60 years", "0-60", "None", "Unmethylated", "Wild-Type"), 
  HR_label = "Reference", estimate = 1, conf.low = 1, conf.high = 1, p_label = ""
)

master_level_order <- c(
  # IDH Type (Top of plot)
  "Wild-Type", "Mutant", 
  # MGMT Status
  "Unmethylated", "Methylated", 
  # Resection Status
  "None", "STR", "GTR", 
  # Baseline KPS
  "0-60", "70-80", "90-100", 
  # Age at Diagnosis (Bottom of Age group)
  "40-60 years", "61-80 years", "81+ years"
)

plot_df_2b <- bind_rows(ref_df_2b, display_df_2b) %>%
  mutate(
    conf.low = pmax(conf.low, 0.1), 
    conf.high = pmin(conf.high, 15),
    
    Level = factor(Level, levels = rev(master_level_order)) 
  )

table_2b <- ggplot(plot_df_2b, aes(y = Level, x = estimate)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "lightgray") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(color = Variable), size = 2) +
  geom_text(aes(label = HR_label), vjust = 2, size = 1.8) +
  geom_text(aes(label = p_label), size = 1.8, vjust = 4, color = "black", fontface = "italic") +
  scale_x_log10(limits = c(0.1, 15)) +
  labs(x = "Hazard Ratio (log-scale)", y = NULL, title = "Diabetics") +
  facet_grid(Variable ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme_minimal(base_size = 7) +
  theme(
    legend.position = "none",
    strip.text.y.left = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.spacing.y = unit(0.02, "cm"),
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )









layout <- "AB\nAB"   # <- exact, rectangular (4 chars per row)

combined <- (table_2a + table_2b) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')

print(combined)


