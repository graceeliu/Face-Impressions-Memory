install.packages("lme4")
library(lme4) # fit linear mixed-effects models
library(dplyr)
install.packages("ggeffects")
library(ggplot2)
library(ggeffects)

file_path <- "/Users/graceliu/Desktop/ColumbiaThesis/code/R/trial_by_trial_WSW_with_ratings.csv"
df <- read.csv(file_path)

df <- df %>%
  filter(subj_id != 11) %>%
  mutate(subj_id = as.factor(subj_id))

### TRUSTWORTHINESS ###

# 1. no variance accounted for
model1_trust_glm <- glm(correct ~ trustworthiness,
                        data = df,
                        family = binomial)
summary(model1_trust_glm)

# 2. accounts for subject by subject variance
model_subject_trust <- glmer(correct ~ trustworthiness + (1|subj_id),
               data = df,
               family = binomial)
summary(model_subject_trust)

# 3. accounts for race variance 
model_race_trust <- glmer(correct ~ trustworthiness + (1|cat_correct),
                   data = df,
                   family = binomial)

summary(model_race_trust)

# 4. accounts for both subject-specific and race variances (this is best)
model_trust_both <- glmer(correct ~ trustworthiness + (1|subj_id) + (1|cat_correct),
                          data = df,
                          family = binomial)

summary_trust_both <- summary(model_trust_both)

p_val <- summary_trust_both$coefficients["trustworthiness", "Pr(>|z|)"]
p_val_text <- paste0("p = ", signif(p_val, 3)) 

preds <- ggpredict(model_trust_both, terms = "trustworthiness")

# plot 1
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#3498DB", alpha = 0.2) +
  labs(
    title = "Effect of Trustworthiness on Accuracy",
    subtitle = p_val_text,
    x = "Trustworthiness Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )


##### BM ONLY #####
df_black <- subset(df, cat_correct == "B")

model_trust_both_BM <- glmer(correct ~ trustworthiness + (1|subj_id),
                          data = df_black,
                          family = binomial)

summary_trust_BM <- summary(model_trust_both_BM)

p_val <- summary_trust_BM$coefficients["trustworthiness", "Pr(>|z|)"]
p_val_text <- paste0("p = ", signif(p_val, 3)) 

preds <- ggpredict(model_trust_both_BM, terms = "trustworthiness")

# plot 1
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#3498DB", alpha = 0.2) +
  labs(
    title = "Effect of Trustworthiness on Accuracy (BM only)",
    subtitle = p_val_text,
    x = "Trustworthiness Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )


##### WM ONLY #####
df_white <- subset(df, cat_correct == "W")

model_trust_both_WM <- glmer(correct ~ trustworthiness + (1|subj_id),
                             data = df_white,
                             family = binomial)

summary_trust_WM <- summary(model_trust_both_WM)

p_val <- summary_trust_WM$coefficients["trustworthiness", "Pr(>|z|)"]
p_val_text <- paste0("p = ", signif(p_val, 3)) 

preds <- ggpredict(model_trust_both_WM, terms = "trustworthiness")

# plot 1
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#3498DB", alpha = 0.2) +
  labs(
    title = "Effect of Trustworthiness on Accuracy (WM only)",
    subtitle = p_val_text,
    x = "Trustworthiness Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )



# spaghetti plot

# Create predictions for each subject
# We'll generate fitted values for each subject using the original data
df$predicted <- predict(model_trust_both, type = "response")

ggplot(df, aes(x = trustworthiness, y = predicted, group = subj_id)) +
  geom_line(alpha = 0.3, color = "#95A5A6") +  # Individual (spaghetti) lines
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "#2C3E50", size = 1.3) +  # Overall trend
  labs(
    title = "Effect of Trustworthiness on Accuracy",
    subtitle = p_val_text,
    x = "Trustworthiness Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )



### DOMINANCE ###

# 1. no variance accounted for 
model1_dom_glm <- glm(correct ~ dominance,
                      data = df,
                      family = binomial)
summary(model1_dom_glm)

# 2. accounts for subject-specific variance
model1_dom <- glmer(correct ~ dominance + (1|subj_id),
                data = df,
                family = binomial)
summary(model1_dom)

# 3. accounts for race variance
model_race_dom <- glmer(correct ~ dominance + (1|cat_correct),
                    data = df,
                    family = binomial)

summary(model_race_dom)

# 4. accounts for both subject-specific and race variance
model_dom_both <- glmer(correct ~ dominance + (1|subj_id) + (1|cat_correct),
                        data = df,
                        family = binomial)

summary_dom_both <- summary(model_dom_both)

p_val <- summary_dom_both$coefficients["dominance", "Pr(>|z|)"]
p_val_text <- paste0("p = ", signif(p_val, 3)) 

preds <- ggpredict(model_dom_both, terms = "dominance")

# plot
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#3498DB", alpha = 0.2) +
  labs(
    title = "Effect of Dominance on Accuracy",
    subtitle = p_val_text,
    x = "Dominance Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

##### BM ONLY ######
model_dom_BM <- glmer(correct ~ dominance + (1|subj_id),
                        data = df_black,
                        family = binomial)

summary_dom_BM <- summary(model_dom_BM)

p_val <- summary_dom_BM$coefficients["dominance", "Pr(>|z|)"]
p_val_text <- paste0("p = ", signif(p_val, 3)) 

preds <- ggpredict(model_dom_both, terms = "dominance")

# plot
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#3498DB", alpha = 0.2) +
  labs(
    title = "Effect of Dominance on Accuracy (BM only)",
    subtitle = p_val_text,
    x = "Dominance Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

##### WM ONLY ######
model_dom_WM <- glmer(correct ~ dominance + (1|subj_id),
                      data = df_white,
                      family = binomial)

summary_dom_WM <- summary(model_dom_WM)

p_val <- summary_dom_WM$coefficients["dominance", "Pr(>|z|)"]
p_val_text <- paste0("p = ", signif(p_val, 3)) 

preds <- ggpredict(model_dom_both, terms = "dominance")

# plot
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "#2C3E50", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#3498DB", alpha = 0.2) +
  labs(
    title = "Effect of Dominance on Accuracy (WM only)",
    subtitle = p_val_text,
    x = "Dominance Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )


# spaghetti plot

# Generate predicted probabilities for each observation
df$predicted_dom <- predict(model_dom_both, type = "response")

ggplot(df, aes(x = dominance, y = predicted_dom, group = subj_id)) +
  geom_line(alpha = 0.3, color = "#95A5A6") +  # Individual lines per subject
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "#2C3E50", size = 1.3) +  # Overall trend
  labs(
    title = "Effect of Dominance on Accuracy",
    subtitle = p_val_text,
    x = "Dominance Rating",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )



### INTERACTION ###
df$trust_c <- scale(df$trustworthiness, center = TRUE, scale = FALSE)
df$dom_c <- scale(df$dominance, center = TRUE, scale = FALSE)
interaction_td <- glmer(correct ~ trust_c * dom_c + (1 | subj_id) + (1|cat_correct), data = df, family = binomial)

summary(interaction_td)

# Use ggpredict to get predicted probabilities from the model
preds <- ggpredict(interaction_td, terms = c("trust_c", "dom_c [-2, 0, 2]"))

# Plot using ggplot2
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "Centered Trustworthiness",
    y = "Predicted Probability of Correct Response",
    color = "Dominance Level",
    fill = "Dominance Level",
    title = "Interaction Between Trust and Dominance on Accuracy"
  ) +
  theme_minimal()

### INTERACTION (BM ONLY) ###
df_black <- subset(df, cat_correct == "B")

df_black$trust_c <- scale(df_black$trustworthiness, center = TRUE, scale = FALSE)
df_black$dom_c <- scale(df_black$dominance, center = TRUE, scale = FALSE)
interaction_BM_td <- glmer(correct ~ trust_c * dom_c + (1 | subj_id), data = df_black, family = binomial)

summary(interaction_BM_td)

# Use ggpredict to get predicted probabilities from the model
preds <- ggpredict(interaction_BM_td, terms = c("trust_c", "dom_c [-2, 0, 2]"))

# Plot using ggplot2
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "Centered Trustworthiness",
    y = "Predicted Probability of Correct Response",
    color = "Dominance Level",
    fill = "Dominance Level",
    title = "Interaction Between Trust and Dominance on Accuracy (BM only)"
  ) +
  theme_minimal()


### INTERACTION (WM ONLY) ###
df_white <- subset(df, cat_correct == "W")

df_white$trust_c <- scale(df_white$trustworthiness, center = TRUE, scale = FALSE)
df_white$dom_c <- scale(df_white$dominance, center = TRUE, scale = FALSE)
interaction_WM_td <- glmer(correct ~ trust_c * dom_c + (1 | subj_id), data = df_white, family = binomial)

summary(interaction_WM_td)

# Use ggpredict to get predicted probabilities from the model
preds <- ggpredict(interaction_WM_td, terms = c("trust_c", "dom_c [-2, 0, 2]"))

# Plot using ggplot2
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "Centered Trustworthiness",
    y = "Predicted Probability of Correct Response",
    color = "Dominance Level",
    fill = "Dominance Level",
    title = "Interaction Between Trust and Dominance on Accuracy (WM only)"
  ) +
  theme_minimal()




