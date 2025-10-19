# ================= Class Lab: BMI Cleaning & Visualization =================

# 1) Packages and folders ---------------------------------------------------
pkgs <- c("tidyverse","haven","janitor","stringr","scales","skimr","naniar","broom","ggpubr") # tidyverse: metapackage (including dplyr, tidyr, ggplot2), haven: read SAS/XPT files
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))
library(broom)
library(ggpubr)

dir.create("outputs", showWarnings = FALSE) # where plots will be saved
data_dir <- "C:/Users/Edison/Downloads/健康大數據分析作業資料"                      # folder containing .XPT files


# 2) Load raw data ----------------------------------------------------------
demo <- read_xpt(file.path(data_dir,"DEMO_L.XPT")) %>% clean_names()  # %>% is one of the most important operators in the tidyverse, it pronounce as“and then”
bpx  <- read_xpt(file.path(data_dir,"BPXO_L.XPT")) %>% clean_names()  # clean_names() from janitor package: make column names consistent (lowercase, no spaces or special characters)
bmx  <- read_xpt(file.path(data_dir,"BMX_L.XPT"))  %>% clean_names()

# quick overviews (on-screen)
skimr::skim(demo); skimr::skim(bpx); skimr::skim(bmx)

gg_miss_var(bpx, show_pct = TRUE) +
  theme_minimal(base_size = 14) +
  labs(title = "Proportion of Missing Values per Variable")   # (not saved)

# 3) Detect Systolic blood pressure/Diastolic blood pressure reading columns ----------------------------------------------------
#    Support both naming patterns (bpxosy1 or bpxsy1); the 'o' is optional.
sbp_cols <- names(bpx)[stringr::str_detect(names(bpx), "^bpxo?sy[1-3]$")]  # names() returns the column names of a data frame.(character vector)
dbp_cols <- names(bpx)[stringr::str_detect(names(bpx), "^bpxo?di[1-3]$")]  # str_detect(x, pattern) returns TRUE or FALSE for each element of x, depending on whether it matches the regex pattern.
# This code finds the column names in the dataset bpx that correspond to the 3 repeated measurements of systolic (sy) or diastolic (di) blood pressure.

# Compute mean SBP/DBP per participant (row-wise mean across available readings)
bpx_summary <- bpx %>%
  transmute(seqn,
            mean_sbp = rowMeans(select(., any_of(sbp_cols)), na.rm = TRUE),
            mean_dbp = rowMeans(select(., any_of(dbp_cols)), na.rm = TRUE)) %>%
  mutate(mean_sbp = ifelse(is.nan(mean_sbp), NA_real_, mean_sbp),
         mean_dbp = ifelse(is.nan(mean_dbp), NA_real_, mean_dbp))


# 4) Build BEFORE (raw) variables and dataset ------------------------------------------
#    bmi_raw = original BMI from BMX.
bmi_raw <- bmx %>%
  transmute(seqn, bmi_raw = bmxbmi)  # transmute() keeps only the variables you create, unlike mutate() which keeps all existing variables.

table(demo$riagendr) # $ means "grab" the column from the data frame

demo <- demo %>%
  mutate(riagendr = as.numeric(riagendr)) %>%       # convert to numeric (some values are character)
  filter(is.na(riagendr) | riagendr %in% c(1, 2))   # drop rows with riagendr==3 (keep NA and 1/2)

demo_sex <- demo %>%
  transmute(seqn, age = ridageyr,
            sex = factor(riagendr, levels=c(1,2), labels=c("Male","Female")))

dat_raw <- demo_sex %>%
  left_join(bmi_raw, by="seqn") %>%  # join demo (left) with bmi_raw (right) by seqn
  left_join(bpx_summary, by = "seqn") %>%
  filter(age >= 20) %>%
  mutate(
    # normalize NaN from rowMeans when all readings missing
    bmi_raw = ifelse(is.nan(bmi_raw), NA_real_, bmi_raw) # normalize NaN to NA
  )

# 5) Draw BEFORE plots ---------------------------------------------------------------------
# ---- BMI boxplot (BEFORE) ----
bmi_before_df <- dat_raw %>% transmute(stage = "Before (raw BMI)", value = bmi_raw)
x <- bmi_before_df$value
qs <- quantile(x, c(.25,.75), na.rm = TRUE)  # na.rm=TRUE to ignore missing values
iqr <- qs[2]-qs[1]
upper_whisker <- min(max(x, na.rm = TRUE), qs[2] + 1.5*iqr)  # upper whisker position, Q3 + 1.5×IQR, capped by max value.
bmi_before_label_y <- upper_whisker + 0.05*iqr
bmi_before_N <- sum(!is.na(x))   # count of non-missing values, !is.na() means "not NA"

p_bmi_before <- ggplot(bmi_before_df, aes(stage, value, fill = stage)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.15, fatten = 1.2) +
  geom_text(data = tibble(stage="Before (raw BMI)", y=bmi_before_label_y, N=bmi_before_N),
            aes(stage, y, label=paste0("n = ", N)), hjust = -1, size = 3.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Before (raw BMI)" = "#D6E9F8")) +
  labs(title = "BMI (BEFORE): Raw Distribution", x = NULL, y = "BMI") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  theme_minimal(base_size = 12) + theme(legend.position = "none", panel.grid.minor = element_blank())
ggsave("outputs/q1_box_bmi_before.png", p_bmi_before, bg = "white")

# ---- SBP boxplot (BEFORE) ----
sbp_before_df <- dat_raw %>% transmute(stage = "Before (raw SBP)", value = mean_sbp)
xb <- sbp_before_df$value
qs <- quantile(xb, c(.25,.75), na.rm = TRUE)
iqr_b <- qs[2]-qs[1]
upper_whisker_b <- min(max(xb, na.rm = TRUE), qs[2] + 1.5*iqr_b)
sbp_before_label_y <- upper_whisker_b + 0.05*iqr_b
sbp_before_N <- sum(!is.na(xb))

p_sbp_before <- ggplot(sbp_before_df, aes(stage, value, fill = stage)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.15, fatten = 1.2) +
  geom_text(data = tibble(stage="Before (raw SBP)", y=sbp_before_label_y, N=sbp_before_N),
            aes(stage, y, label=paste0("n = ", N)), hjust = -1, size = 3.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Before (raw SBP)" = "#D6F8E6")) +
  labs(title = "Mean SBP (BEFORE): Raw Distribution", x = NULL, y = "Mean SBP (mmHg)") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  theme_minimal(base_size = 12) + theme(legend.position = "none", panel.grid.minor = element_blank())
ggsave("outputs/q1_box_sbp_before.png", p_sbp_before, bg = "white")


# 6) OUTLIER CLEANING (then compute cleaned means) -------------------------------------
#    Rule = physiologic bounds + IQR fences + MAD z-score; after removal we create "clean" vars.
BMI_LO <- 10; BMI_HI <- 80
# Fix MAD scaling: mad() returns scaled MAD (default constant = 1.4826), so use it directly
bmi_clean <- bmx %>%
  transmute(seqn, bmxbmi) %>%
  mutate(
    q1 = quantile(bmxbmi, 0.25, na.rm=TRUE),
    q3 = quantile(bmxbmi, 0.75, na.rm=TRUE),
    iqr = q3 - q1,
    lo_iqr = q1 - 1.5*iqr,
    hi_iqr = q3 + 1.5*iqr,
    med = median(bmxbmi, na.rm=TRUE),
    madv = mad(bmxbmi, na.rm=TRUE),
    z = ifelse(madv > 0, (bmxbmi - med)/madv, 0),
    flag = (bmxbmi < BMI_LO | bmxbmi > BMI_HI) | (bmxbmi < lo_iqr | bmxbmi > hi_iqr) | (abs(z) > 3.5),
    bmxbmi_clean = ifelse(flag, NA_real_, bmxbmi)
  ) %>% select(seqn, bmxbmi_clean)

# SBP cleaning: apply same rules but with physiologic bounds for SBP
SBP_LO <- 70; SBP_HI <- 260
sbp_clean <- bpx_summary %>%
  transmute(seqn, mean_sbp) %>%
  mutate(
    q1 = quantile(mean_sbp, 0.25, na.rm=TRUE),
    q3 = quantile(mean_sbp, 0.75, na.rm=TRUE),
    iqr = q3 - q1,
    lo_iqr = q1 - 1.5*iqr,
    hi_iqr = q3 + 1.5*iqr,
    med = median(mean_sbp, na.rm=TRUE),
    madv = mad(mean_sbp, na.rm=TRUE),
    z = ifelse(madv > 0, (mean_sbp - med)/madv, 0),
    flag = (mean_sbp < SBP_LO | mean_sbp > SBP_HI) | (mean_sbp < lo_iqr | mean_sbp > hi_iqr) | (abs(z) > 3.5),
    mean_sbp_clean = ifelse(flag, NA_real_, mean_sbp)
  ) %>% select(seqn, mean_sbp_clean)

# 7) Build AFTER (clean) dataset --------------------------------------------------------
dat_clean <- demo_sex %>%
  left_join(bmi_clean, by="seqn") %>%
  left_join(sbp_clean, by = "seqn") %>%
  filter(age >= 20) %>%
  mutate(
    bmxbmi_clean = ifelse(is.nan(bmxbmi_clean), NA_real_, bmxbmi_clean)  # normalize NaN to NA
  )

# Normalize SBP NaN
dat_clean <- dat_clean %>% mutate(mean_sbp_clean = ifelse(exists("mean_sbp_clean") & is.nan(mean_sbp_clean), NA_real_, mean_sbp_clean))

# 8) AFTER plots ----------------------------------------------------------------------
# ---- BMI boxplot (AFTER) ----
bmi_after_df <- dat_clean %>% transmute(stage = "After (clean BMI)", value = bmxbmi_clean)
x <- bmi_after_df$value
qs <- quantile(x, c(.25,.75), na.rm = TRUE); 
iqr <- qs[2]-qs[1]
upper_whisker <- min(max(x, na.rm = TRUE), qs[2] + 1.5*iqr)
bmi_after_label_y <- upper_whisker + 0.05*iqr
bmi_after_N <- sum(!is.na(x))

p_bmi_after <- ggplot(bmi_after_df, aes(stage, value, fill = stage)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.15, fatten = 1.2) +
  geom_text(data = tibble(stage="After (clean BMI)", y=bmi_after_label_y, N=bmi_after_N),
            aes(stage, y, label=paste0("n = ", N)), hjust = -1, size = 3.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c("After (clean BMI)" = "#FCE5CD")) +
  labs(title = "BMI (AFTER): Cleaned Distribution", x = NULL, y = "BMI") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  theme_minimal(base_size = 12) + theme(legend.position = "none", panel.grid.minor = element_blank())
ggsave("outputs/q1_box_bmi_after.png", p_bmi_after, bg = "white")

# ---- SBP boxplot (AFTER) ----
sbp_after_df <- dat_clean %>% transmute(stage = "After (clean SBP)", value = mean_sbp_clean)
xb2 <- sbp_after_df$value
qs <- quantile(xb2, c(.25,.75), na.rm = TRUE); iqr_b2 <- qs[2]-qs[1]
upper_whisker_b2 <- min(max(xb2, na.rm = TRUE), qs[2] + 1.5*iqr_b2)
sbp_after_label_y <- upper_whisker_b2 + 0.05*iqr_b2
sbp_after_N <- sum(!is.na(xb2))

p_sbp_after <- ggplot(sbp_after_df, aes(stage, value, fill = stage)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.15, fatten = 1.2) +
  geom_text(data = tibble(stage="After (clean SBP)", y=sbp_after_label_y, N=sbp_after_N),
            aes(stage, y, label=paste0("n = ", N)), hjust = -1, size = 3.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c("After (clean SBP)" = "#FFE6F2")) +
  labs(title = "Mean SBP (AFTER): Cleaned Distribution", x = NULL, y = "Mean SBP (mmHg)") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  theme_minimal(base_size = 12) + theme(legend.position = "none", panel.grid.minor = element_blank())
ggsave("outputs/q1_box_sbp_after.png", p_sbp_after, bg = "white")

# 9) Missing value comparison ----------------------------------------------------------------------
miss_before <- tibble(
  stage     = "Before",
  variable  = "BMI",
  n_missing = sum(is.na(dat_raw$bmi_raw)),
  n_total   = nrow(dat_raw)
) %>% mutate(p_missing = n_missing / n_total)

miss_after <- tibble(
  stage     = "After",
  variable  = "BMI",
  n_missing = sum(is.na(dat_clean$bmxbmi_clean)),
  n_total   = nrow(dat_clean)
) %>% mutate(p_missing = n_missing / n_total)

# Add SBP missingness
miss_before_sbp <- tibble(
  stage = "Before",
  variable = "SBP",
  n_missing = sum(is.na(dat_raw$mean_sbp)),
  n_total = nrow(dat_raw)
) %>% mutate(p_missing = n_missing / n_total)

miss_after_sbp <- tibble(
  stage = "After",
  variable = "SBP",
  n_missing = sum(is.na(dat_clean$mean_sbp_clean)),
  n_total = nrow(dat_clean)
) %>% mutate(p_missing = n_missing / n_total)

miss_long <- bind_rows(miss_before, miss_after, miss_before_sbp, miss_after_sbp) %>%
  mutate(stage = factor(stage, levels = c("Before","After")))

miss_long <- bind_rows(miss_before, miss_after) %>%
  mutate(stage = factor(stage, levels = c("Before","After")),  # ensure order in plot legend
         variable = factor(variable, levels = "BMI"))          # ensure order in x-axis

p_na_bar_1 <- ggplot(miss_long, aes(variable, p_missing, fill = stage)) +
  geom_col(width=0.6, position="dodge") +                                                  # dodge to separate bars
  geom_text(aes(label = paste0(scales::percent(p_missing, 0.1),
                               "\n(", n_missing, "/", n_total, ")")),                      # label on top of bars
            vjust=-0.2, size=3.5) +
  scale_y_continuous(labels=scales::percent) +
  labs(title = "SBP Missingness Before vs After Cleaning", x=NULL, y="Missing rate") +
  theme_minimal(base_size=12) + theme(legend.position="top")

pos <- position_dodge(width = 0.65) # to align text labels with bars when using dodge

p_na_bar_2 <- ggplot(miss_long, aes(variable, p_missing, fill = stage)) +
  geom_col(width = 0.6, position = pos) +
  geom_text(aes(label = paste0(scales::percent(p_missing, 0.1),
                               "\n(", n_missing, "/", n_total, ")")),
            position = pos, vjust = -0.2, size = 3.5, lineheight = 0.95) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c("Before" = "#9EC5FE", "After" = "#FFCF99")) +
  labs(title = "Missingness (NA) Before vs After Outlier Removal (BMI)",
       x = NULL, y = "Missing rate", fill = "Stage") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave("outputs/q1_na_bmi_before_after.png", p_na_bar_2, bg = "white")

# Also save SBP NA plot
ggsave("outputs/q1_na_bmi_sbp_before_after.png", p_na_bar_2, bg = "white")

# 10) Scatter plot BMI vs SBP by sex and linear models -----------------------
dat_clean <- dat_clean %>% mutate(sex = factor(sex, levels = c("Male","Female")))

p_scatter_sex <- ggplot(dat_clean, aes(x = bmxbmi_clean, y = mean_sbp_clean, color = sex)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
  labs(title = "BMI vs Mean SBP (cleaned) by Sex", x = "BMI (kg/m^2)", y = "Mean SBP (mmHg)") +
  theme_minimal(base_size = 12)
ggsave("outputs/q1_scatter_bmi_sbp_by_sex.png", p_scatter_sex, width = 8, height = 5, bg = "white")

# Linear models: stratified by sex
fit_by_sex <- dat_clean %>% filter(!is.na(bmxbmi_clean) & !is.na(mean_sbp_clean) & !is.na(sex)) %>%
  group_by(sex) %>%
  do(tidy(lm(mean_sbp_clean ~ bmxbmi_clean + age, data = .))) %>%
  ungroup()

print("--- Stratified linear model coefficients (adjusted for age) ---")
print(fit_by_sex)

# Interaction model
dat_for_model <- dat_clean %>% filter(!is.na(bmxbmi_clean) & !is.na(mean_sbp_clean) & !is.na(sex))
mod_inter <- lm(mean_sbp_clean ~ bmxbmi_clean * sex + age, data = dat_for_model)
print("--- Interaction model (BMI * sex) ---")
print(tidy(mod_inter))
print(glance(mod_inter))