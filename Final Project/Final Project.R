# ============================================================
# MIE237 Project – Audio Condition & Cognitive Performance
# R Analysis Script
# ============================================================

# ── 0. Install / load packages ────────────────────────────────────────────────
packages <- c("tidyverse", "rstatix", "ggpubr", "ez", "emmeans", "effectsize", "ggplot2")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# ── 1. Load data ──────────────────────────────────────────────────────────────

df_raw <- read.csv("data_collection_template.csv")

# ── 2. Clean & factor ────────────────────────────────────────────────────────
df <- df_raw %>%
  select(participant_id, audio_condition, trial_number,
         completion_time_sec, arith_accuracy_pct, colour_accuracy_pct) %>%
  mutate(
    participant_id  = factor(participant_id),
    audio_condition = factor(audio_condition,
                             levels = c("Silence", "Instrumental", "Lyrics"))
  )

cat("=== Data preview ===\n")
print(head(df, 9))
cat("\nN per condition:\n")
print(table(df$audio_condition))

# ── 3. Visual Exploration ─────────────────────────────────────────────────────

# Accuracy by condition
ggplot(df, aes(x = audio_condition, y = arith_accuracy_pct)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Arithmetic Accuracy by Audio Condition",
       x = "Audio Condition", y = "Arithmetic Accuracy (%)")

ggplot(df, aes(x = audio_condition, y = colour_accuracy_pct)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Colour Matching Accuracy by Audio Condition",
       x = "Audio Condition", y = "Colour Matching Accuracy (%)")

ggplot(df, aes(x = audio_condition, y = completion_time_sec)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Completion Time by Audio Condition",
       x = "Audio Condition", y = "Completion Time (seconds)")

# check whether participants differ from each other
# This is important in a within-subjects (blocked) design
ggplot(df, aes(x = participant_id, y = arith_accuracy_pct)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Arithmetic Accuracy by Participant",
       x = "Participant", y = "Arithmetic Accuracy (%)")

ggplot(df, aes(x = participant_id, y = completion_time_sec)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Completion Time by Participant",
       x = "Participant", y = "Completion Time (seconds)")
# ── 4. Descriptive Statistics ─────────────────────────────────────────────────
# summarise by condition and by participant

cat("\n=== Summary Statistics by Condition ===\n")

# Completion time
df %>%
  group_by(audio_condition) %>%
  summarise(
    mean_time = mean(completion_time_sec),
    sd_time   = sd(completion_time_sec),
    n         = n(),
    .groups   = "drop"
  )

# Arithmetic accuracy
df %>%
  group_by(audio_condition) %>%
  summarise(
    mean_arith_acc = mean(arith_accuracy_pct),
    sd_arith_acc   = sd(arith_accuracy_pct),
    n              = n(),
    .groups        = "drop"
  )

# Colour matching accuracy
df %>%
  group_by(audio_condition) %>%
  summarise(
    mean_colour_acc = mean(colour_accuracy_pct),
    sd_colour_acc   = sd(colour_accuracy_pct),
    n               = n(),
    .groups         = "drop"
  )

#check participant-level means to see if blocking is needed
cat("\n=== Summary Statistics by Participant ===\n")
df %>%
  group_by(participant_id) %>%
  summarise(
    mean_time       = mean(completion_time_sec),
    sd_time         = sd(completion_time_sec),
    mean_arith_acc  = mean(arith_accuracy_pct),
    mean_colour_acc = mean(colour_accuracy_pct),
    .groups         = "drop"
  )

# ── 5. Normality Check (Shapiro-Wilk) ────────────────────────────────────────
cat("\n=== Shapiro-Wilk Normality Tests ===\n")

# Completion time
cat("Completion Time:\n")
df %>%
  group_by(audio_condition) %>%
  summarise(sw = list(shapiro.test(completion_time_sec)), .groups = "drop") %>%
  mutate(W = sapply(sw, `[[`, "statistic"),
         p = sapply(sw, `[[`, "p.value")) %>%
  select(audio_condition, W, p)

# Arithmetic accuracy
cat("\nArithmetic Accuracy:\n")
df %>%
  group_by(audio_condition) %>%
  summarise(sw = list(shapiro.test(arith_accuracy_pct)), .groups = "drop") %>%
  mutate(W = sapply(sw, `[[`, "statistic"),
         p = sapply(sw, `[[`, "p.value")) %>%
  select(audio_condition, W, p)

# Colour accuracy
cat("\nColour Matching Accuracy:\n")
df %>%
  group_by(audio_condition) %>%
  summarise(sw = list(shapiro.test(colour_accuracy_pct)), .groups = "drop") %>%
  mutate(W = sapply(sw, `[[`, "statistic"),
         p = sapply(sw, `[[`, "p.value")) %>%
  select(audio_condition, W, p)

# ── 6. Blocked (Within-Subject) ANOVA ────────────────────────────────────────
# blocked ANOVA using aov(dv ~ condition + participant)
# participant is added to account for baseline differences across participants
# and remove that variability from the error term 

cat("\n=== Blocked ANOVA: Completion Time ===\n")
anova_time <- aov(completion_time_sec ~ audio_condition + participant_id,
                  data = df)
summary(anova_time)

cat("\n=== Blocked ANOVA: Arithmetic Accuracy ===\n")
anova_arith <- aov(arith_accuracy_pct ~ audio_condition + participant_id,
                   data = df)
summary(anova_arith)

cat("\n=== Blocked ANOVA: Colour Matching Accuracy ===\n")
anova_colour <- aov(colour_accuracy_pct ~ audio_condition + participant_id,
                    data = df)
summary(anova_colour)

# ── 7. Tukey Post-Hoc Comparisons ─────────────────────────────────────────────
# TukeyHSD() after aov() to identify which specific pairs differ
# Controls for multiple comparisons and provides confidence intervals

cat("\n=== Tukey Post-Hoc: Completion Time ===\n")
tukey_time <- TukeyHSD(anova_time, "audio_condition")
tukey_time
plot(tukey_time)

cat("\n=== Tukey Post-Hoc: Arithmetic Accuracy ===\n")
tukey_arith <- TukeyHSD(anova_arith, "audio_condition")
tukey_arith
plot(tukey_arith)

cat("\n=== Tukey Post-Hoc: Colour Matching Accuracy ===\n")
tukey_colour <- TukeyHSD(anova_colour, "audio_condition")
tukey_colour
plot(tukey_colour)

#Individual Participant Profiles. 
ggplot(df, aes(x = audio_condition, y = colour_accuracy_pct,
               group = participant_id, colour = participant_id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  theme_classic() +
  labs(title = "Individual Participant Profiles: Colour Accuracy",
       x = "Audio Condition", y = "Colour Accuracy (%)")


# ── 8. Estimated Marginal Means (emmeans) ─────────────────────────────────────
# emmeans() computes model-based averages that adjust for
# other factors in the model (here: participant blocking)
# Basic syntax: emmeans(model, ~ factor)

cat("\n=== Estimated Marginal Means: Completion Time ===\n")
emm_time <- emmeans(anova_time, ~ audio_condition)
emm_time
pairs(emm_time, adjust = "tukey")

cat("\n=== Estimated Marginal Means: Arithmetic Accuracy ===\n")
emm_arith <- emmeans(anova_arith, ~ audio_condition)
emm_arith
pairs(emm_arith, adjust = "tukey")

cat("\n=== Estimated Marginal Means: Colour Matching Accuracy ===\n")
emm_colour <- emmeans(anova_colour, ~ audio_condition)
emm_colour
pairs(emm_colour, adjust = "tukey")

#plot estimated means with confidence intervals
emm_colour_df <- as.data.frame(emm_colour)

ggplot(emm_colour_df,
       aes(x = audio_condition, y = emmean)) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  theme_classic(base_size = 14) +
  labs(title = "Estimated Marginal Means: Colour Matching Accuracy",
       x = "Audio Condition", y = "Estimated Mean Accuracy (%)")

# ── 9. Model Diagnostics ──────────────────────────────────────────────────────
#diagnostic plots to check normality and equal variance assumptions

cat("\n=== Model Diagnostics ===\n")

# Completion time
par(mfrow = c(2, 2))
plot(anova_time)
par(mfrow = c(1, 1))

# Arithmetic accuracy
par(mfrow = c(2, 2))
plot(anova_arith)
par(mfrow = c(1, 1))

# Colour accuracy
par(mfrow = c(2, 2))
plot(anova_colour)
par(mfrow = c(1, 1))

# ── 10. ANOVA as Regression ───────────────────────────────────────────────────
# ANOVA can equivalently be estimated using lm()
# The conclusions should match the ANOVA output

cat("\n=== ANOVA as Regression: Completion Time ===\n")
summary(lm(completion_time_sec ~ audio_condition + participant_id, data = df))

cat("\n=== ANOVA as Regression: Arithmetic Accuracy ===\n")
summary(lm(arith_accuracy_pct ~ audio_condition + participant_id, data = df))

cat("\n=== ANOVA as Regression: Colour Matching Accuracy ===\n")
summary(lm(colour_accuracy_pct ~ audio_condition + participant_id, data = df))

# ── 11. Correlation Between DVs ───────────────────────────────────────────────
cat("\n=== Correlation Matrix Between Dependent Variables ===\n")
cor(df[, c("completion_time_sec", "arith_accuracy_pct", "colour_accuracy_pct")])

cat("\nCompletion Time vs Arithmetic Accuracy:\n")
cor.test(df$completion_time_sec, df$arith_accuracy_pct, method = "pearson")

cat("\nCompletion Time vs Colour Accuracy:\n")
cor.test(df$completion_time_sec, df$colour_accuracy_pct, method = "pearson")

cat("\nArithmetic Accuracy vs Colour Accuracy:\n")
cor.test(df$arith_accuracy_pct, df$colour_accuracy_pct, method = "pearson")

cat("\nAnalysis complete.\n")


