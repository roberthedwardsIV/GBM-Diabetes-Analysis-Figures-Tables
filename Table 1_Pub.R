library(dplyr)
library(readxl)
library(tibble)
library(survival)
library(tidyr)
library(knitr)
library(kableExtra)
library(webshot2)

# Load data
df <- read_excel("~/Desktop/GBM/GBM Diabetes Data Analysis.xlsx", sheet = 3)
df <- df %>% filter(df$`Coded MRN` != 28)
df <- df %>%
  mutate(`Diabetes` = ifelse(`Coded MRN` == 3, 1, Diabetes))

df <- df %>%
  mutate(`MGMT Status` = ifelse(is.na(`MGMT Status`) | `MGMT Status` == "", 3, `MGMT Status`))

df <- df %>%
  mutate(`IDH-Type` = ifelse(is.na(`IDH-Type`) | `IDH-Type` == "", 3, `IDH-Type`))

df <- df %>%
  mutate(
    `KI-Class` = case_when(
      is.na(`Ki-67`) ~ 5,                              # Unknowns → 5
      `Ki-67` >= 0  & `Ki-67` <= 20  ~ 0,               # 0–20%
      `Ki-67` >= 21 & `Ki-67` <= 40  ~ 1,               # 21–40%
      `Ki-67` >= 41 & `Ki-67` <= 60  ~ 2,               # 41–60%
      `Ki-67` >= 61 & `Ki-67` <= 80  ~ 3,               # 61–80%
      `Ki-67` >= 81 & `Ki-67` <= 100 ~ 4,               # 81–100%
      TRUE ~ 5                                      # Any other unexpected values → 5
    ),
    `KI-Class` = factor(`KI-Class`, levels = 0:5, labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%", "Unknown"))
  )

df <- df %>%
  mutate(
    `KPS Class` = case_when(
      `Baseline KPS` > 0  & `Baseline KPS` <= 60  ~ 0, 
      `Baseline KPS` >= 61  & `Baseline KPS` <= 80  ~ 1,               
      `Baseline KPS` >= 81  & `Baseline KPS` <= 100  ~ 2,                
    ),
    `KPS Class` = factor(`KPS Class`, levels = 0:2, labels = c("0-60", "70-80", "90-100"))
  )
# Factor labeling
df <- df %>%
  mutate(
    Diabetes = factor(Diabetes, levels=c(0,1), labels=c("Non-Diabetic","Diabetic")),
    Sex = factor(Sex, levels=c(1,2), labels=c("Male","Female")),
    `Resection Status` = factor(`Resection Status`, levels=c(0,1,2), labels=c("None", "STR","GTR")),
    `Treatment Status` = factor(`Treatment Status`, levels=c(0,1,2,3,4), labels=c("None","RT Only", "Chemo Only", "TMZ/RT Only", "TMZ/RT & 1+ Line")),
    `MGMT Status` = factor(`MGMT Status`, levels=c(0,1,3), labels=c("Unmethylated","Methylated", "Unknown")),
    `IDH-Type` = factor(`IDH-Type`, levels=c(1,2,3), labels=c("Wild-Type", "Mutant", "Unknown")),
    `Age Class` = factor(`Age Class`, levels=c(0,1,2,3), labels=c("<40 years", "40-60 years", "61-80 years", ">80 years"))
  )

# Split groups
df <- df %>% filter(df$`Coded MRN` != 28)
non_diabetic <- df %>% filter(Diabetes=="Non-Diabetic")
diabetic <- df %>% filter(Diabetes=="Diabetic")

# Helper function to format p-values
format_pval <- function(p){
  if(is.na(p)) return(NA_character_)
  if(p < 0.0001) return("<0.0001")
  sprintf("%.4f", p)
}

# n row
n_row <- tibble(
  Variable = "",
  `Non-Diabetic   N=725 (84%)` = paste0("n = ", nrow(non_diabetic)),
  `Diabetic      N=142 (16%)` = paste0("n = ", nrow(diabetic)),
  `P-value` = ""
)

# Age header and rows
age_wilcox_analysis <- wilcox.test(`Age at Diagnosis` ~ `Diabetes`, data = df)

age_header <- tibble(Variable="Age (years)", `Non-Diabetic   N=725 (84%)`="", `Diabetic      N=142 (16%)`="", `P-value`="")

age_median <- tibble(
  Variable="Median",
  `Non-Diabetic   N=725 (84%)`=as.character(median(non_diabetic$`Age at Diagnosis`, na.rm=TRUE)),
  `Diabetic      N=142 (16%)`=as.character(median(diabetic$`Age at Diagnosis`, na.rm=TRUE)),
  `P-value`= format_pval(age_wilcox_analysis$p.value)
)
age_code <- "&lt;1-93"
age_range <- tibble(
  Variable="Range",
  `Non-Diabetic   N=725 (84%)`=as.character(age_code),
  `Diabetic      N=142 (16%)`=paste0(min(diabetic$`Age at Diagnosis`, na.rm=TRUE), "-", max(diabetic$`Age at Diagnosis`, na.rm=TRUE)),
  `P-value`=""
)


#less than 0.0001 so actual p-value replaced


contingency_table <- table(df$Diabetes, df$`Age Class`)
age_chi_analysis <- chisq.test(contingency_table)


age_class_zero <- tibble(
  Variable="&lt;40 years",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Age Class` == "<40 years", na.rm = TRUE), " (", round(((sum(non_diabetic$`Age Class` == "<40 years", na.rm = TRUE)/nrow(non_diabetic))*100), digits = 0), "%)"),
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Age Class` == "<40 years", na.rm = TRUE), " (", round(((sum(diabetic$`Age Class` == "<40 years", na.rm = TRUE)/nrow(diabetic))*100), digits = 0), "%)"),
  `P-value`= sprintf("%.4f", age_chi_analysis$p.value)
)


age_class_one <- tibble(
  Variable="40-60 years",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Age Class` == "40-60 years", na.rm = TRUE), " (", round(((sum(non_diabetic$`Age Class` == "40-60 years", na.rm = TRUE)/nrow(non_diabetic))*100), digits = 0), "%)"),
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Age Class` == "40-60 years", na.rm = TRUE), " (", round(((sum(diabetic$`Age Class` == "40-60 years", na.rm = TRUE)/nrow(diabetic))*100), digits = 0), "%)"),
  `P-value`= ""
)

age_class_two <- tibble(
  Variable="61-80 years",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Age Class` == "61-80 years", na.rm = TRUE), " (", round(((sum(non_diabetic$`Age Class` == "61-80 years", na.rm = TRUE)/nrow(non_diabetic))*100), digits = 0), "%)"),
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Age Class` == "61-80 years", na.rm = TRUE), " (", round(((sum(diabetic$`Age Class` == "61-80 years", na.rm = TRUE)/nrow(diabetic))*100), digits = 0), "%)"),
  `P-value`= ""
)

age_class_three <- tibble(
  Variable="&gt;80 years",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Age Class` == ">80 years", na.rm = TRUE), " (", round(((sum(non_diabetic$`Age Class` == ">80 years", na.rm = TRUE)/nrow(non_diabetic))*100), digits = 0), "%)"),
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Age Class` == ">80 years", na.rm = TRUE), " (", round(((sum(diabetic$`Age Class` == ">80 years", na.rm = TRUE)/nrow(diabetic))*100), digits = 0), "%)"),
  `P-value`= ""
)






# Sex header and ratio row
sex_header <- tibble(Variable="Sex", `Non-Diabetic   N=725 (84%)`="", `Diabetic      N=142 (16%)`="", `P-value`="")

sex_counts <- df %>%
  group_by(Diabetes, Sex) %>%
  summarise(n=n(), .groups="drop") %>%
  pivot_wider(names_from=Sex, values_from=n, values_fill=0)

sex_contingency_table <- table(df$Diabetes, df$Sex)
sex_chi_analysis <- chisq.test(sex_contingency_table)

sex_ratio_row <- tibble(
  Variable="M:F",
  `Non-Diabetic   N=725 (84%)`=sprintf("%.1f", sex_counts$Male[sex_counts$Diabetes=="Non-Diabetic"]/sex_counts$Female[sex_counts$Diabetes=="Non-Diabetic"]),
  `Diabetic      N=142 (16%)`=sprintf("%.1f", sex_counts$Male[sex_counts$Diabetes=="Diabetic"]/sex_counts$Female[sex_counts$Diabetes=="Diabetic"]),
  `P-value`= sprintf("%.4f", sex_chi_analysis$p.value)
)

males <- tibble(
  Variable="Males",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Sex` == "Male", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Sex` == "Male", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"),
  `Diabetic      N=142 (16%)`= paste0(sum(diabetic$`Sex` == "Male", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Sex` == "Male", na.rm = TRUE)/nrow(diabetic))*100)), "%)"),
  `P-value`= ""
)

females <- tibble(
  Variable="Females",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Sex` == "Female", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Sex` == "Female", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"),
  `Diabetic      N=142 (16%)`= paste0(sum(diabetic$`Sex` == "Female", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Sex` == "Female", na.rm = TRUE)/nrow(diabetic))*100)), "%)"),
  `P-value`= ""
)





# KPS header and rows
KPS_header <- tibble(Variable="Baseline KPS", `Non-Diabetic   N=725 (84%)`="", `Diabetic      N=142 (16%)`="", `P-value`="")

KPS_contingency_table <- table(df$Diabetes, df$`KPS Class`)
KPS_chi_analysis <- chisq.test(KPS_contingency_table)

kps_median_row <- tibble(
  Variable="Median",
  `Non-Diabetic   N=725 (84%)`=as.character(median(non_diabetic$`Baseline KPS`, na.rm=TRUE)),
  `Diabetic      N=142 (16%)`=as.character(median(diabetic$`Baseline KPS`, na.rm=TRUE)),
  `P-value`=sprintf("%.4f", KPS_chi_analysis$p.value)
)

kps_range_row <- tibble(
  Variable="Range",
  `Non-Diabetic   N=725 (84%)`=as.character("0-100"),
  `Diabetic      N=142 (16%)`=as.character("0-100"),
  `P-value`=""
)

kps_class_zero <- tibble(
  Variable="0-60",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KPS Class` == "0-60", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KPS Class` == "0-60", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"),
  `Diabetic      N=142 (16%)`= paste0(sum(diabetic$`KPS Class` == "0-60", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KPS Class` == "0-60", na.rm = TRUE)/nrow(diabetic))*100)), "%)"),
  `P-value`= ""
)

kps_class_one <- tibble(
  Variable="70-80",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KPS Class` == "70-80", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KPS Class` == "70-80", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"),
  `Diabetic      N=142 (16%)`= paste0(sum(diabetic$`KPS Class` == "70-80", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KPS Class` == "70-80", na.rm = TRUE)/nrow(diabetic))*100)), "%)"),
  `P-value`= ""
)

kps_class_two <- tibble(
  Variable="90-100",
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KPS Class` == "90-100", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KPS Class` == "90-100", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"),
  `Diabetic      N=142 (16%)`= paste0(sum(diabetic$`KPS Class` == "90-100", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KPS Class` == "90-100", na.rm = TRUE)/nrow(diabetic))*100)), "%)"),
  `P-value`= ""
)








#Resection status
resection_surv <- kruskal.test(`Resection Status` ~ Survival, data = diabetic)


resection_header <- tibble(Variable="Resection Status", 
                           `Non-Diabetic   N=725 (84%)`="", 
                           `Diabetic      N=142 (16%)`="", 
                           `P-value`= "")


resection_contingency_table <- table(df$Diabetes, df$`Resection Status`)
resection_chi_analysis <- chisq.test(resection_contingency_table)

noresection <- tibble(
  Variable="None", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Resection Status` == "None", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Resection Status` == "None", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Resection Status` == "None", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Resection Status` == "None", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`=sprintf("%.4f", resection_chi_analysis$p.value))

STR <- tibble(
  Variable="STR", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Resection Status` == "STR", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Resection Status` == "STR", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Resection Status` == "STR", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Resection Status` == "STR", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

GTR <- tibble(
  Variable="GTR", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Resection Status` == "GTR", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Resection Status` == "GTR", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Resection Status` == "GTR", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Resection Status` == "GTR", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")


#Treatment status
treatment_header <- tibble(Variable="Treatment Status", 
                           `Non-Diabetic   N=725 (84%)`="", 
                           `Diabetic      N=142 (16%)`="", 
                           `P-value`= "")

tx_contingency_table <- table(df$Diabetes, df$`Treatment Status`)
tx_chi_analysis <- chisq.test(tx_contingency_table)

notx <- tibble(
  Variable="None", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Treatment Status` == "None", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Treatment Status` == "None", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Treatment Status` == "None", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Treatment Status` == "None", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`=sprintf("%.4f", tx_chi_analysis$p.value))

RTOnly <- tibble(
  Variable="RT Only", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Treatment Status` == "RT Only", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Treatment Status` == "RT Only", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Treatment Status` == "RT Only", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Treatment Status` == "RT Only", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

ChemoOnly <- tibble(
  Variable="Chemo Only", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Treatment Status` == "Chemo Only", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Treatment Status` == "Chemo Only", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Treatment Status` == "Chemo Only", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Treatment Status` == "Chemo Only", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

TMZRTOnly <- tibble(
  Variable="TMZ/RT Only", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Treatment Status` == "TMZ/RT Only", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Treatment Status` == "TMZ/RT Only", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`Treatment Status` == "TMZ/RT Only", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Treatment Status` == "TMZ/RT Only", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

TMZRTPLUS <- tibble(
  Variable="TMZ/RT & 1+ Line", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`Treatment Status` == "TMZ/RT & 1+ Line", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`Treatment Status` == "TMZ/RT & 1+ Line", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`= paste0(sum(diabetic$`Treatment Status` == "TMZ/RT & 1+ Line", na.rm = TRUE), " (", ceiling(((sum(diabetic$`Treatment Status` == "TMZ/RT & 1+ Line", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")








#MGMT Status
MGMT_header <- tibble(Variable="MGMT Status", 
                           `Non-Diabetic   N=725 (84%)`="", 
                           `Diabetic      N=142 (16%)`="", 
                           `P-value`= "")

MGMT_contingency_table <- table(df$Diabetes, df$`MGMT Status`)
MGMT_chi_analysis <- chisq.test(MGMT_contingency_table)

Unmeth <- tibble(
  Variable="Unmethylated", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`MGMT Status` == "Unmethylated", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`MGMT Status` == "Unmethylated", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`MGMT Status` == "Unmethylated", na.rm = TRUE), " (", ceiling(((sum(diabetic$`MGMT Status` == "Unmethylated", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`=sprintf("%.4f", MGMT_chi_analysis$p.value))

meth <- tibble(
  Variable="Methylated", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`MGMT Status` == "Methylated", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`MGMT Status` == "Methylated", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`MGMT Status` == "Methylated", na.rm = TRUE), " (", ceiling(((sum(diabetic$`MGMT Status` == "Methylated", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

MGMT_unknown <- tibble(
  Variable="Unknown", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`MGMT Status` == "Unknown", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`MGMT Status` == "Unknown", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`MGMT Status` == "Unknown", na.rm = TRUE), " (", ceiling(((sum(diabetic$`MGMT Status` == "Unknown", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

#IDH-Type
IDH_header <- tibble(Variable="IDH Type", 
                      `Non-Diabetic   N=725 (84%)`="", 
                      `Diabetic      N=142 (16%)`="", 
                      `P-value`= "")

IDH_contingency_table <- table(df$Diabetes, df$`IDH-Type`)
IDH_chi_analysis <- chisq.test(IDH_contingency_table)

IDH_wt <- tibble(
  Variable="Wild-Type", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`IDH-Type` == "Wild-Type", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`IDH-Type` == "Wild-Type", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`IDH-Type` == "Wild-Type", na.rm = TRUE), " (", ceiling(((sum(diabetic$`IDH-Type` == "Wild-Type", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`= sprintf("%.4f", IDH_chi_analysis$p.value))

IDH_mut <- tibble(
  Variable="Mutant", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`IDH-Type` == "Mutant", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`IDH-Type` == "Mutant", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`IDH-Type` == "Mutant", na.rm = TRUE), " (", ceiling(((sum(diabetic$`IDH-Type` == "Mutant", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

IDH_unk <- tibble(
  Variable="Unknown", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`IDH-Type` == "Unknown", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`IDH-Type` == "Unknown", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`IDH-Type` == "Unknown", na.rm = TRUE), " (", ceiling(((sum(diabetic$`IDH-Type` == "Unknown", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")







#KI-67
KI_header <- tibble(Variable="Ki-67 Index", 
                     `Non-Diabetic   N=725 (84%)`="", 
                     `Diabetic      N=142 (16%)`="", 
                     `P-value`= "")

KI_contingency_table <- table(df$Diabetes, df$`KI-Class`)
KI_chi_analysis <- chisq.test(KI_contingency_table)

KI_zero <- tibble(
  Variable="0-20%", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KI-Class` == "0-20%", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KI-Class` == "0-20%", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`KI-Class` == "0-20%", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KI-Class` == "0-20%", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`=sprintf("%.4f", KI_chi_analysis$p.value))

KI_one <- tibble(
  Variable="21-40%", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KI-Class` == "21-40%", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KI-Class` == "21-40%", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`KI-Class` == "21-40%", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KI-Class` == "21-40%", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

KI_two <- tibble(
  Variable="41-60%", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KI-Class` == "41-60%", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KI-Class` == "41-60%", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`KI-Class` == "41-60%", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KI-Class` == "41-60%", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

KI_three <- tibble(
  Variable="61-80%", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KI-Class` == "61-80%", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KI-Class` == "61-80%", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`KI-Class` == "61-80%", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KI-Class` == "61-80%", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

KI_four <- tibble(
  Variable="81-100%", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KI-Class` == "81-100%", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KI-Class` == "81-100%", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`KI-Class` == "81-100%", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KI-Class` == "81-100%", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")

KI_five <- tibble(
  Variable="Unknown", 
  `Non-Diabetic   N=725 (84%)`= paste0(sum(non_diabetic$`KI-Class` == "Unknown", na.rm = TRUE), " (", ceiling(((sum(non_diabetic$`KI-Class` == "Unknown", na.rm = TRUE)/nrow(non_diabetic))*100)), "%)"), 
  `Diabetic      N=142 (16%)`=paste0(sum(diabetic$`KI-Class` == "Unknown", na.rm = TRUE), " (", ceiling(((sum(diabetic$`KI-Class` == "Unknown", na.rm = TRUE)/nrow(diabetic))*100)), "%)"), 
  `P-value`="")


# Survival statistics
nd_surv_stats <- survfit(Surv(Survival, `Vital Status`) ~ 1, data=non_diabetic)
d_surv_stats <- survfit(Surv(Survival, `Vital Status`) ~ 1, data=diabetic)
surv_kt <- kruskal.test(`Survival` ~ `Diabetes`, data = df)

surv_header <- tibble(Variable="Survival Analysis", 
                      `Non-Diabetic   N=725 (84%)`="", 
                      `Diabetic      N=142 (16%)`="", 
                      `P-value`= "") 

surv_mOS <- tibble(
  Variable="Median OS (months)",
  `Non-Diabetic   N=725 (84%)`=sprintf("%.1f", summary(nd_surv_stats)$table["median"]),
  `Diabetic      N=142 (16%)`=sprintf("%.1f", summary(d_surv_stats)$table["median"]),
  `P-value`= sprintf("%.4f", surv_kt$p.value)
)

surv_CI <- tibble(
  Variable="95% CI",
  `Non-Diabetic   N=725 (84%)`=sprintf("%.1f - %.1f", summary(nd_surv_stats)$table["0.95LCL"], summary(nd_surv_stats)$table["0.95UCL"]),
  `Diabetic      N=142 (16%)`=sprintf("%.1f - %.1f", summary(d_surv_stats)$table["0.95LCL"], summary(d_surv_stats)$table["0.95UCL"]),
  `P-value`="" 
)

surv_events <- tibble(
  Variable="Events/N",
  `Non-Diabetic   N=725 (84%)`="602 (83%)",
  `Diabetic      N=142 (16%)`="138 (98%)",
  `P-value`="" 
)

# Combine all rows
final_table_1 <- bind_rows(
  age_header, age_median, age_range, age_class_zero, age_class_one, age_class_two, age_class_three,
  sex_header, sex_ratio_row, males, females,
  KPS_header, kps_median_row, kps_range_row, kps_class_zero, kps_class_one, kps_class_two,
  resection_header, noresection, STR, GTR
)

# Force all columns to character
final_table_1 <- final_table_1 %>%
  mutate(
    `Non-Diabetic   N=725 (84%)` = as.character(`Non-Diabetic   N=725 (84%)`),
    `Diabetic      N=142 (16%)` = as.character(`Diabetic      N=142 (16%)`),
    `P-value` = as.character(`P-value`)
  )

# Wrap P-value in cell_spec for guaranteed display
final_table_1 <- final_table_1 %>%
  mutate(`P-value` = cell_spec(`P-value`, format="html"))

# Render HTML table
final_table_1 %>%
  kable(format = "html", escape = FALSE, align = "lccc") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "bordered")) %>%
  row_spec(which(final_table_1$Variable %in% c("Age (years)", "Sex", "Baseline KPS", "Resection Status")), bold = TRUE) %>%
  column_spec(1, width = "6cm") %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "3cm") %>%
  column_spec(4, width = "2cm")



# Combine all rows
final_table_2 <- bind_rows(
  treatment_header, notx, RTOnly, ChemoOnly, TMZRTOnly, TMZRTPLUS,
  MGMT_header, Unmeth, meth, MGMT_unknown,
  IDH_header, IDH_wt, IDH_mut, IDH_unk,
  KI_header, KI_zero, KI_one, KI_two, KI_three, KI_four, KI_five,
  surv_header, surv_mOS, surv_CI, surv_events
)

# Force all columns to character
final_table_2 <- final_table_2 %>%
  mutate(
    `Non-Diabetic   N=725 (84%)` = as.character(`Non-Diabetic   N=725 (84%)`),
    `Diabetic      N=142 (16%)` = as.character(`Diabetic      N=142 (16%)`),
    `P-value` = as.character(`P-value`)
  )

# Wrap P-value in cell_spec for guaranteed display
final_table_2 <- final_table_2 %>%
  mutate(`P-value` = cell_spec(`P-value`, format="html"))

# Render HTML table
final_table_2 %>%
  kable(format = "html", escape = FALSE, align = "lccc") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "bordered")) %>%
  row_spec(which(final_table_2$Variable %in% c("Treatment Status", "MGMT Status", "IDH Type", "Ki-67 Index", "Survival Analysis")), bold = TRUE) %>%
  column_spec(1, width = "6cm") %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "3cm") %>%
  column_spec(4, width = "2cm")

max_rows <- max(nrow(final_table_1), nrow(final_table_2))

# pad shorter tables with empty rows
pad_rows <- function(tbl, n) {
  if (nrow(tbl) < n) {
    extra <- tibble::tibble(
      Variable = rep("", n - nrow(tbl))
    )
    # add empty columns for the rest
    for (col in setdiff(names(tbl), "Variable")) {
      extra[[col]] <- ""
    }
    tbl <- bind_rows(tbl, extra)
  }
  tbl
}

final_table_1 <- pad_rows(final_table_1, max_rows)
final_table_2 <- pad_rows(final_table_2, max_rows)

combined_table <- bind_cols(final_table_1, final_table_2)

# Give unique column names
colnames(combined_table) <- c(
  "Variable_1", "Non-Diabetic_1", "Diabetic_1", "p_1",
  "Variable_2", "Non-Diabetic_2", "Diabetic_2", "p_2"
)

# Bold only the first Variable column selectively
combined_table <- combined_table %>%
  mutate(
    Variable_1 = ifelse(Variable_1 %in% c(
      "Age (years)", "Sex", "Baseline KPS", "Resection Status",
      "Treatment Status", "MGMT Status", "IDH Type", "Ki-67 Index", "Survival Analysis"),
      cell_spec(Variable_1, bold = TRUE),
      Variable_1)
  )

combined_table <- combined_table %>%
  mutate(
    Variable_2 = ifelse(Variable_2 %in% c(
      "Treatment Status", "MGMT Status", "IDH Type", "Ki-67 Index", "Survival Analysis"),
      cell_spec(Variable_2, bold = TRUE),
      Variable_2)
  )

# kable
figure_final <- kable(combined_table,
      format = "html",
      escape = FALSE,
      align = "c"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "bordered"),
    full_width = FALSE,
    font_size = 7  ) %>%
  add_header_above(c(
    " " = 1,                 # Variable_1
    "Non-Diabetic     N=725 (84%)" = 1,      # Non-Diabetic_1
    "Diabetic     N=142 (16%)" = 1,          # Diabetic_1
    "p" = 1,                 # p_1
    " " = 1,                 # Variable_2
    "Non-Diabetic     N=725 (84%)" = 1,      # Non-Diabetic_2
    "Diabetic     N=142 (16%)" = 1,          # Diabetic_2
    "p" = 1                  # p_2
  ), line = FALSE      # remove the extra line that adds vertical space
) %>%
  row_spec(0, bold = FALSE, extra_css = "display: none;") %>%
  column_spec(1, width = "2.5cm") %>%
  column_spec(2, width = "1.5cm") %>%
  column_spec(3, width = "1.5cm") %>%
  column_spec(4, width = "1cm") %>%
  column_spec(5, width = "2.5cm") %>%
  column_spec(6, width = "1.5cm") %>%
  column_spec(7, width = "1.5cm") %>%
  column_spec(8, width = "1cm")


kableExtra::save_kable(x = figure_final, file = "temp_table_for_pdf.html", self_contained = TRUE)

webshot2::webshot(
  url = "temp_table_for_pdf.html", 
  file = "Table 1.pdf",
  delay = 1,      
  zoom = 1.5,     # Helps with high-quality output
  vwidth = 1200   # Ensures the table has enough room
)