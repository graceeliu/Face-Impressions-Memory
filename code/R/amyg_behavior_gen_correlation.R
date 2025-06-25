library(lme4) # fit linear mixed-effects models
library(tidyverse)
library(dplyr)
library(purrr)
install.packages("ggeffects")
library(ggplot2)
library(ggeffects)

file_path <- "/Users/graceliu/Desktop/ColumbiaThesis/code/R/trial_by_trial_WSW_with_ratings.csv"
df <- read.csv(file_path)

neural_diff_path <- "/Users/graceliu/Desktop/ColumbiaThesis/code/amyg_trust_diff_overall_ALL.csv"
neural_diff_df <-read.csv(neural_diff_path)

df <- df %>%
  filter(subj_id != 11) %>%
  mutate(subj_id = as.factor(subj_id))

# Fit glm() per subject to get their trustworthiness slope
# not glmer() because that's for random effects and we don't need to account for subject-specific variance because we're doing subject specific here
# created dataframe that has each subject's trustworthiness slope
subject_slopes <- df %>%
  group_by(subj_id) %>%
  summarise(trust_slope = coef(glm(correct ~ trustworthiness + cat_correct,
                                   data = cur_data(), #cur_data() uses only current subject's data
                                   family = "binomial"))["trustworthiness"]) %>%
  ungroup()

subject_slopes$subj_id <- sprintf("sub-%03d", as.numeric(subject_slopes$subj_id))

#### AMYGDALA ####

###ALL FACES###
# Merge behavioral slopes with neural differences
merged_df <- inner_join(subject_slopes, neural_diff_df, by = "subj_id")

ggplot(merged_df, aes(x = trust_slope, y = neural_diff)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title = "Brain-Behavior Correlation",
    subtitle = "Trustworthiness Slope vs. Amygdala Activation Difference",
    x = "Behavioral Trustworthiness Slope (Recall Accuracy)",
    y = "Neural Activation Difference\n(Trustworthy - Untrustworthy)"
  ) +
  theme_minimal(base_size = 12)

# correlation test
cor.test(merged_df$trust_slope, merged_df$neural_diff, method = "pearson")

### BLACK MALES ONLY ###

neural_diff_BM_path <- "/Users/graceliu/Desktop/ColumbiaThesis/code/amyg_trust_diff_overall_BM.csv"
neural_diff_BM_df <-read.csv(neural_diff_BM_path)
# Merge behavioral slopes with neural differences
merged_BM_df <- inner_join(subject_slopes, neural_diff_BM_df, by = "subj_id")

ggplot(merged_BM_df, aes(x = trust_slope, y = neural_diff)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title = "Brain-Behavior Correlation",
    subtitle = "Trustworthiness Slope vs. Amygdala Activation Difference (Black males only)",
    x = "Behavioral Trustworthiness Slope (Recall Accuracy)",
    y = "Neural Activation Difference\n(Trustworthy - Untrustworthy)"
  ) +
  theme_minimal(base_size = 12)

# correlation test
cor.test(merged_BM_df$trust_slope, merged_BM_df$neural_diff, method = "pearson")

### WHITE MALES ONLY ###

neural_diff_WM_path <- "/Users/graceliu/Desktop/ColumbiaThesis/code/amyg_trust_diff_overall_WM.csv"
neural_diff_WM_df <-read.csv(neural_diff_WM_path)

# Merge behavioral slopes with neural differences
merged_WM_df <- inner_join(subject_slopes, neural_diff_WM_df, by = "subj_id")

ggplot(merged_WM_df, aes(x = trust_slope, y = neural_diff)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title = "Brain-Behavior Correlation",
    subtitle = "Trustworthiness Slope vs. Amygdala Activation Difference (White males only)",
    x = "Behavioral Trustworthiness Slope (Recall Accuracy)",
    y = "Neural Activation Difference\n(Trustworthy - Untrustworthy)"
  ) +
  theme_minimal(base_size = 12)

# correlation test
cor.test(merged_WM_df$trust_slope, merged_WM_df$neural_diff, method = "pearson")



#### HIPPOCAMPUS ####

###ALL FACES###
neural_diff_path_hipp <- "/Users/graceliu/Desktop/ColumbiaThesis/code/hipp_trust_diff_overall_ALL.csv"
hipp_neural_diff_df <-read.csv(neural_diff_path_hipp)

# Merge behavioral slopes with neural differences
hipp_merged_df <- inner_join(subject_slopes, hipp_neural_diff_df, by = "subj_id")

ggplot(hipp_merged_df, aes(x = trust_slope, y = neural_diff)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title = "Brain-Behavior Correlation",
    subtitle = "Trustworthiness Slope vs. Hippocampus Activation Difference",
    x = "Behavioral Trustworthiness Slope (Recall Accuracy)",
    y = "Neural Activation Difference\n(Trustworthy - Untrustworthy)"
  ) +
  theme_minimal(base_size = 12)

# correlation test
cor.test(hipp_merged_df$trust_slope, hipp_merged_df$neural_diff, method = "pearson")

### BLACK MALES ONLY ###

hipp_neural_diff_BM_path <- "/Users/graceliu/Desktop/ColumbiaThesis/code/hipp_trust_diff_overall_BM.csv"
hipp_neural_diff_BM_df <-read.csv(hipp_neural_diff_BM_path)
# Merge behavioral slopes with neural differences
hipp_merged_BM_df <- inner_join(subject_slopes, hipp_neural_diff_BM_df, by = "subj_id")

ggplot(hipp_merged_BM_df, aes(x = trust_slope, y = neural_diff)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title = "Brain-Behavior Correlation",
    subtitle = "Trustworthiness Slope vs. Hippocampus Activation Difference (Black males only)",
    x = "Behavioral Trustworthiness Slope (Recall Accuracy)",
    y = "Neural Activation Difference\n(Trustworthy - Untrustworthy)"
  ) +
  theme_minimal(base_size = 12)

# correlation test
cor.test(hipp_merged_BM_df$trust_slope, hipp_merged_BM_df$neural_diff, method = "pearson")

### WHITE MALES ONLY ###

hipp_neural_diff_WM_path <- "/Users/graceliu/Desktop/ColumbiaThesis/code/hipp_trust_diff_overall_WM.csv"
hipp_neural_diff_WM_df <-read.csv(hipp_neural_diff_WM_path)

# Merge behavioral slopes with neural differences
hipp_merged_WM_df <- inner_join(subject_slopes, hipp_neural_diff_WM_df, by = "subj_id")

ggplot(hipp_merged_WM_df, aes(x = trust_slope, y = neural_diff)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  labs(
    title = "Brain-Behavior Correlation",
    subtitle = "Trustworthiness Slope vs. Hippocampus Activation Difference (White males only)",
    x = "Behavioral Trustworthiness Slope (Recall Accuracy)",
    y = "Neural Activation Difference\n(Trustworthy - Untrustworthy)"
  ) +
  theme_minimal(base_size = 12)

# correlation test
cor.test(hipp_merged_WM_df$trust_slope, hipp_merged_WM_df$neural_diff, method = "pearson")
