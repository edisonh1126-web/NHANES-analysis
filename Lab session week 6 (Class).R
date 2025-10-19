# ================= Q2A: Education Level (EDU) =================

# 1) Check the original coding distribution
## Ensure required data objects exist (demo, bmx, bpx, dat_clean)
data_dir <- "C:/Users/Edison/Downloads/健康大數據分析作業資料"
if(!exists("demo")) {
  if(file.exists(file.path(data_dir, "DEMO_L.XPT"))) demo <- read_xpt(file.path(data_dir, "DEMO_L.XPT")) %>% clean_names()
}
if(!exists("bmx")) {
  if(file.exists(file.path(data_dir, "BMX_L.XPT"))) bmx <- read_xpt(file.path(data_dir, "BMX_L.XPT")) %>% clean_names()
}
if(!exists("bpx")) {
  if(file.exists(file.path(data_dir, "BPXO_L.XPT"))) bpx <- read_xpt(file.path(data_dir, "BPXO_L.XPT")) %>% clean_names()
}

if(!exists("demo")) stop("`demo` dataset not found. Please ensure DEMO_L.XPT is available in data_dir or run previous script.")

## Quick check of original coding distribution for dmdeduc2
demo %>% count(dmdeduc2) %>% arrange(desc(n)) %>% print()

# 2) Recode & relabel
dat_edu <- demo %>%
  transmute(
    seqn,
    age = ridageyr,
    EDU = case_when(
      dmdeduc2 == 1 ~ "<9th grade",
      dmdeduc2 == 2 ~ "9–11th grade",
      dmdeduc2 == 3 ~ "High school/GED",
      dmdeduc2 == 4 ~ "Some college/AA",
      dmdeduc2 == 5 ~ "College or above",
      TRUE ~ NA_character_
    )
  ) %>%
  # attach cleaned BMI if available; otherwise attach raw BMI from BMX
  left_join(
    (if(exists("dat_clean")) dat_clean %>% select(seqn, bmxbmi_clean) else bmx %>% transmute(seqn, bmxbmi_clean = bmxbmi)),
    by = "seqn") %>%
  mutate(EDU = factor(EDU, levels = c("<9th grade","9–11th grade","High school/GED","Some college/AA","College or above"))) %>%
  drop_na(EDU, bmxbmi_clean)

# 3) distribution table
edu_dist <- dat_edu %>%
  count(EDU) %>%                 # count occurrences of each education level
  mutate(prop = n / sum(n),      # calculate proportions
         variable = "EDU") %>%   # add a variable column for clarity
  rename(category = EDU)         # rename EDU to category for consistency

# 4) output table & csv
if(!dir.exists("outputs")) dir.create("outputs")
write.csv(edu_dist, file = "outputs/EDU_distribution.csv", row.names = FALSE) #row.names=FALSE to avoid writing row numbers

library(knitr)
kable(edu_dist, digits = 3, caption = "Distribution of Educational Attainment (EDU)")

# 5) Boxplot for visualization
p_bmi <- dat_edu %>%
  ggplot(aes(x = EDU, y = bmxbmi_clean)) +                               # aes() defines the aesthetic mapping: x-axis is EDU, y-axis is cleaned BMI
  geom_boxplot(position = position_dodge(0.8), outlier.alpha = 0.2) +    # position_dodge(0.8) separates boxplots for clarity; outlier.alpha adjusts outlier visibility
  labs(title = "BMI across Education Groups",
       x = "Education Level", y = "BMI") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("outputs/BMI_by_EDU_1.png", p_bmi, width = 10, height = 6, bg = "white")

# Also save a horizontal version (BMI on x-axis)
p_bmi_h <- p_bmi + coord_flip() + labs(title = "BMI across Education Groups (horizontal)")
ggsave("outputs/BMI_by_EDU_1_horizontal.png", p_bmi_h, width = 10, height = 6, bg = "white")

# Q1: Race distribution
race_dist <- demo %>%
  transmute(seqn, ridreth3) %>%
  mutate(ridreth3 = as.integer(ridreth3)) %>%
  mutate(race = case_when(
    ridreth3 == 1 ~ "Mexican American",
    ridreth3 == 2 ~ "Other Hispanic",
    ridreth3 == 3 ~ "Non-Hispanic White",
    ridreth3 == 4 ~ "Non-Hispanic Black",
    ridreth3 == 6 ~ "Non-Hispanic Asian",
    ridreth3 == 7 ~ "Other/Multi-racial",
    TRUE ~ NA_character_
  )) %>%
  drop_na(race) %>%
  count(race) %>% mutate(prop = n / sum(n)) %>% rename(category = race)

write.csv(race_dist, file = "outputs/RACE_distribution.csv", row.names = FALSE)
knitr::kable(race_dist, digits = 3, caption = "Distribution of Race (ridreth3)")

# Boxplot: BMI by Race
dat_bmi_race <- dat_edu %>%
  select(seqn, bmxbmi_clean) %>%
  left_join(demo %>% transmute(seqn, ridreth3), by = "seqn") %>%
  mutate(race = case_when(
    ridreth3 == 1 ~ "Mexican American",
    ridreth3 == 2 ~ "Other Hispanic",
    ridreth3 == 3 ~ "Non-Hispanic White",
    ridreth3 == 4 ~ "Non-Hispanic Black",
    ridreth3 == 6 ~ "Non-Hispanic Asian",
    ridreth3 == 7 ~ "Other/Multi-racial",
    TRUE ~ NA_character_
  )) %>% drop_na(race, bmxbmi_clean)

p_bmi_race <- ggplot(dat_bmi_race, aes(x = race, y = bmxbmi_clean, fill = race)) +
  geom_boxplot(outlier.alpha = 0.2) + labs(title = "BMI by Race", x = "Race", y = "BMI") +
  theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "none")
ggsave("outputs/BMI_by_Race.png", p_bmi_race, width = 10, height = 6, bg = "white")

# Also a plot showing BMI distribution filled by EDU (for the subjects with EDU info)
p_bmi_filled_edu <- dat_edu %>% ggplot(aes(x = EDU, y = bmxbmi_clean, fill = EDU)) + geom_boxplot(outlier.alpha = 0.2) +
  labs(title = "BMI by Education (filled)", x = "Education", y = "BMI") + theme_minimal() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave("outputs/BMI_by_EDU_filled.png", p_bmi_filled_edu, width = 10, height = 6, bg = "white")

# Q3
library(tidyverse)

# 1) Capture only the necessary columns for SBP & DBP and transform to long format ------------------------- # nolint
#    Support both naming patterns (bpxosy1 or bpxsy1); the 'o' is optional.
## Q2 I: reshape BPX from wide to long (support both bpxsy1 / bpxosy1 naming)
sbp_cols <- names(bpx)[stringr::str_detect(names(bpx), "^bpxo?sy[1-3]$")]
dbp_cols <- names(bpx)[stringr::str_detect(names(bpx), "^bpxo?di[1-3]$")]

bpx_long_clean <- bpx %>%
  select(seqn, any_of(c(sbp_cols, dbp_cols))) %>%
  pivot_longer(cols = -seqn,
               names_to = c("measure", "trial"),
               names_pattern = "^bpxo?(sy|di)([1-3])$",
               values_to = "value") %>%
  mutate(measure = recode(measure, "sy" = "SBP", "di" = "DBP"),
         trial = as.integer(trial)) %>%
  filter(!is.na(value))

# II: boxplot comparing distribution of SBP and DBP across three trials, faceted by measure
p_bp_trials <- ggplot(bpx_long_clean, aes(x = factor(trial), y = value, fill = factor(trial))) +
  geom_boxplot(outlier.alpha = 0.15) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(title = "Distribution of SBP & DBP across 3 Trials", x = "Trial", y = "Blood pressure (mmHg)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")
ggsave("outputs/BP_trials_boxplot.png", p_bp_trials, width = 10, height = 6, bg = "white")

# III: For each subject and measure, select the two trials (readings) with largest difference.
# Approach: for each seqn & measure, find min and max reading and keep those trials (if only one reading, it'll be duplicated; we drop when only one non-NA)
bpx_two_extreme <- bpx_long_clean %>%
  group_by(seqn, measure) %>%
  filter(n() >= 2) %>%      # require at least two readings to compare
  mutate(val_min = min(value, na.rm = TRUE), val_max = max(value, na.rm = TRUE)) %>%
  filter(value %in% c(val_min, val_max)) %>%
  ungroup() %>%
  distinct(seqn, measure, trial, value)

# Plot distributions for the two-trial subset (min vs max)
p_bp_two <- ggplot(bpx_two_extreme, aes(x = factor(trial), y = value, fill = factor(trial))) +
  geom_boxplot(outlier.alpha = 0.15) + facet_wrap(~ measure, scales = "free_y") +
  labs(title = "Distribution of BP for the two trials with largest within-subject difference", x = "Trial (selected min/max)", y = "Blood pressure (mmHg)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")
ggsave("outputs/BP_two_extreme_boxplot.png", p_bp_two, width = 10, height = 6, bg = "white")

# IV: Simple inference about whether measurements were taken at long intervals or same day
cat("\nInference about BP measurement timing (based on distributions):\n")
cat("- If most within-subject differences between trials are small (e.g., mean absolute difference < ~5 mmHg), likely measured the same day during a single visit.\n")
cat("- If large systematic shifts or distinct clusters across trials are observed, they might be measured across visits.\n")
cat("Please inspect 'outputs/BP_trials_boxplot.png' and 'outputs/BP_two_extreme_boxplot.png' to visually assess within-subject variability.\n\n")

ggplot(bpx_long_clean, aes(x = factor(trial), y = value, fill = measure)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~ measure, scales = "free_y") +
  labs(title = "Distribution of SBP & DBP across 3 Trials (Cleaned Data)",
       x = "Trial", y = "Blood Pressure (mmHg)") +
  theme_minimal(base_size = 13)
