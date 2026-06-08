# =============================================================================
# MODERATION ANALYSIS: Neuroticism, Mastery, and Depressive Symptoms
# =============================================================================
# Outcome:    PHQ-8 (Depressive Symptoms)
# Predictor:  Neuroticism
# Moderator:  Mastery (Personal Mastery Scale)
# =============================================================================


# =============================================================================
# STEP 1: Keep only finished responses and relevant variables
# =============================================================================

cols <- c(
  paste0("PQ-8_", 1:8),
  paste0("Neuroticism_", 1:3),
  paste0("Personal Mastery_", 1:7)
)

data <- Response_Data[Response_Data$Finished == "True", cols]
data <- na.omit(data)

cat("Rows after filtering:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")
sapply(data[, cols], class)


# =============================================================================
# STEP 2: Recode text answers into numbers
# =============================================================================

# PHQ-8: 0 = Not at all ... 3 = Nearly every day
PHQ8_labels <- c("Not at all", "Several days",
                 "More than half the days", "Nearly every day")
PHQ8_scores <- c(0, 1, 2, 3)
for (i in 1:8) {
  col <- paste0("PQ-8_", i)
  data[[col]] <- PHQ8_scores[match(data[[col]], PHQ8_labels)]
}

# Neuroticism: 1-5 Likert (Strongly disagree to Strongly agree)
N_labels <- c("Strongly disagree", "Somewhat disagree",
              "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
N_scores <- c(1, 2, 3, 4, 5)
for (i in 1:3) {
  col <- paste0("Neuroticism_", i)
  data[[col]] <- N_scores[match(data[[col]], N_labels)]
}

# Mastery (Personal Mastery Scale): 1-4 Likert
PM_labels <- c("Strongly disagree", "Disagree", "Agree", "Strongly Agree")
PM_scores <- c(1, 2, 3, 4)
for (i in 1:7) {
  col <- paste0("Personal Mastery_", i)
  data[[col]] <- PM_scores[match(data[[col]], PM_labels)]
}


# =============================================================================
# STEP 3: Reverse-score negatively worded items
# =============================================================================

# Mastery: items 1, 2, 3, 5, 7 are negatively worded
# Formula: new = (max + 1) - old, so 1 <-> 4
data$`Personal Mastery_1` <- 5 - data$`Personal Mastery_1`
data$`Personal Mastery_2` <- 5 - data$`Personal Mastery_2`
data$`Personal Mastery_3` <- 5 - data$`Personal Mastery_3`
data$`Personal Mastery_5` <- 5 - data$`Personal Mastery_5`
data$`Personal Mastery_7` <- 5 - data$`Personal Mastery_7`
# Items 4 and 6 are positively worded — no reversal needed

# Neuroticism item 3 is negatively worded
data$Neuroticism_3 <- 6 - data$Neuroticism_3

# Quick check: all Mastery items should correlate positively with each other
cat("\n--- Mastery inter-item correlations ---\n")
cor(data[, paste0("Personal Mastery_", 1:7)])


# =============================================================================
# STEP 4: Calculate mean composite scores
# =============================================================================

data$PHQ8                <- rowMeans(data[, paste0("PQ-8_", 1:8)],               na.rm = TRUE)
data$Neuroticism_full    <- rowMeans(data[, paste0("Neuroticism_", 1:3)],         na.rm = TRUE)
data$Neuroticism_clean   <- rowMeans(data[, c("Neuroticism_1", "Neuroticism_3")], na.rm = TRUE)  # without item 2
data$Mastery             <- rowMeans(data[, paste0("Personal Mastery_", 1:7)],    na.rm = TRUE)


# =============================================================================
# STEP 5: Mean-centre predictors 
# =============================================================================

data$Neuroticism_full_c    <- scale(data$Neuroticism_full,  center = TRUE, scale = FALSE)
data$Neuroticism_clean_c   <- scale(data$Neuroticism_clean, center = TRUE, scale = FALSE)
data$Mastery_c             <- scale(data$Mastery,           center = TRUE, scale = FALSE)


# =============================================================================
# STEP 6: Assumption checks and outlier 
# =============================================================================

library(car)

# Fit preliminary model for diagnostics
lm_check <- lm(PHQ8 ~ Neuroticism_full_c * Mastery_c, data = data)

# ── 6a. Linearity & Homoscedasticity ──────────────────────────────────────────
par(family = "serif", cex.main = 1.2, cex.lab = 1.1, cex.axis = 0.95,
    mgp = c(2.5, 0.8, 0), mar = c(5, 4.5, 4, 2))

plot(lm_check, which = 1,
     col = "#2166AC", pch = 19, cex = 0.85, lwd = 2,
     caption = "", sub = "", ann = FALSE,
     main = "Residuals vs. Fitted Values")
title(xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "#B2182B", lty = 2, lwd = 1.8)

# ── 6b. Normality of Residuals ─────────────────────────────────────────────────
par(family = "serif", cex.main = 1.2, cex.lab = 1.1, cex.axis = 0.95,
    mgp = c(2.5, 0.8, 0), mar = c(5, 4.5, 4, 2))

# Histogram of standardised residuals
hist(rstandard(lm_check),
     main   = "",
     xlab   = "Standardised Residuals",
     ylab   = "Frequency",
     col    = "#2166AC",
     border = "white",
     breaks = 15)
abline(v = 0, col = "#B2182B", lty = 2, lwd = 1.8)

# Normal Q-Q plot
plot(lm_check, which = 2,
     col = "#2166AC", pch = 19, cex = 0.85,
     caption = "", sub = "",
     main = "Normal Q-Q Plot")

# ── 6c. Outliers: |z| > 3 ──────────────────────────────────────────────────────
zresid <- rstandard(lm_check)
cat("\n--- Outliers (|z| > 3) ---\n")
cat("Number of outliers:", sum(abs(zresid) > 3), "\n")
cat("Row indices:", which(abs(zresid) > 3), "\n")

# ── 6d. Leverage: 3*(p+1)/N ────────────────────────────────────────────────────
hats      <- hatvalues(lm_check)
p         <- 3
N         <- nrow(data)
threshold <- 3 * (p + 1) / N
cat("\n--- Leverage ---\n")
cat("Threshold:", round(threshold, 4), "\n")
cat("High-leverage points:", sum(hats > threshold), "\n")

# ── 6e. Cook's Distance > 1 ────────────────────────────────────────────────────
cooks <- cooks.distance(lm_check)
cat("\n--- Cook's Distance > 1 ---\n")
cat("Influential cases:", sum(cooks > 1), "\n")

# ── 6f. Multicollinearity: VIF < 10 ────────────────────────────────────────────
cat("\n--- VIF (should be < 10) ---\n")
print(vif(lm_check))


# =============================================================================
# STEP 7: Remove outliers — create clean dataset
# =============================================================================

outlier_rows <- which(abs(zresid) > 3)
data_clean   <- data[-outlier_rows, ]

# Force centred columns to plain numeric (not matrix)
data_clean$Neuroticism_full_c  <- as.numeric(data_clean$Neuroticism_full_c)
data_clean$Neuroticism_clean_c <- as.numeric(data_clean$Neuroticism_clean_c)
data_clean$Mastery_c           <- as.numeric(data_clean$Mastery_c)

# Recheck after removal
lm_recheck <- lm(PHQ8 ~ Neuroticism_full_c * Mastery_c, data = data_clean)
cat("\n--- Post-removal checks ---\n")
cat("Remaining outliers (|z| > 3):", sum(abs(rstandard(lm_recheck)) > 3), "\n")
cat("Cook's D > 1:", sum(cooks.distance(lm_recheck) > 1), "\n")
cat("VIF:\n"); print(vif(lm_recheck))


# =============================================================================
# STEP 8: Cronbach's Alpha
# =============================================================================

library(psych)

alpha_PHQ8       <- psych::alpha(data_clean[, paste0("PQ-8_", 1:8)])
alpha_Neuro      <- psych::alpha(data_clean[, paste0("Neuroticism_", 1:3)])
alpha_PC         <- psych::alpha(data_clean[, paste0("Personal Mastery_", 1:7)])
alpha_Neuro_2    <- psych::alpha(data_clean[, c("Neuroticism_1", "Neuroticism_3")])

cat("\n--- Cronbach's Alpha ---\n")
cat(sprintf("PHQ-8             (8 items):  alpha = %.3f\n", alpha_PHQ8$total$raw_alpha))
cat(sprintf("Neuroticism       (3 items):  alpha = %.3f\n", alpha_Neuro$total$raw_alpha))
cat(sprintf("Neuroticism       (2 items):  alpha = %.3f\n", alpha_Neuro_2$total$raw_alpha))
cat(sprintf("Mastery           (7 items):  alpha = %.3f\n", alpha_PC$total$raw_alpha))


# =============================================================================
# STEP 9: Descriptive statistics
# =============================================================================

cat("\n--- Descriptive Statistics ---\n")
describe(data_clean[, c("PHQ8", "Neuroticism_full", "Neuroticism_clean", "Mastery")])


# =============================================================================
# STEP 10: Pearson Correlations
# =============================================================================

cat("\n--- Pearson Correlations ---\n")
cor(data_clean[, c("PHQ8", "Neuroticism_full", "Neuroticism_clean", "Mastery")])


# =============================================================================
# STEP 11: Correlation Scatter Plots (APA style, with confidence intervals)
# =============================================================================

# Detach psych BEFORE loading patchwork to avoid + operator conflict
if ("package:psych" %in% search()) detach("package:psych", unload = TRUE)
library(ggplot2)
library(patchwork)

apa_theme <- theme_classic() +
  theme(
    text         = element_text(family = "serif", size = 11),
    plot.title   = element_text(size = 11, hjust = 0.5),
    axis.title   = element_text(size = 10),
    axis.text.x  = element_text(size = 8, angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0.5, size = 9)
  )

# Plot 1: PHQ-8 ~ Neuroticism
p1 <- ggplot(data_clean, aes(x = Neuroticism_full_c, y = PHQ8)) +
  geom_point(colour = "#2166AC", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", colour = "#B2182B", se = TRUE, linewidth = 0.8) +
  labs(
    title   = "PHQ-8 and\nNeuroticism",
    x       = "Neuroticism\n(Mean-Centred)",
    y       = "PHQ-8\n(Depressive Symptoms)",
    caption = "r = .52"
  ) +
  apa_theme

# Plot 2: PHQ-8 ~ Mastery
p2 <- ggplot(data_clean, aes(x = Mastery_c, y = PHQ8)) +
  geom_point(colour = "#2166AC", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", colour = "#B2182B", se = TRUE, linewidth = 0.8) +
  labs(
    title   = "PHQ-8 and\nMastery",
    x       = "Mastery\n(Mean-Centred)",
    y       = "PHQ-8\n(Depressive Symptoms)",
    caption = "r = \u2212.43"
  ) +
  apa_theme

# Plot 3: Neuroticism ~ Mastery
p3 <- ggplot(data_clean, aes(x = Mastery_c, y = Neuroticism_full_c)) +
  geom_point(colour = "#2166AC", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", colour = "#B2182B", se = TRUE, linewidth = 0.8) +
  labs(
    title   = "Neuroticism and\nMastery",
    x       = "Mastery\n(Mean-Centred)",
    y       = "Neuroticism\n(Mean-Centred)",
    caption = "r = \u2212.41"
  ) +
  apa_theme

# Combine
wrap_plots(p1, p2, p3, ncol = 3)
ggsave("correlation_plots.png", width = 10, height = 4, dpi = 300, bg = "white")


# =============================================================================
# STEP 12: Hierarchical Moderated Regression
# Step 1 = main effects only      (tests H1: Neuroticism; H2: Mastery)
# Step 2 = + interaction term     (tests H3: moderation)
# Run twice: once with full Neuroticism (3 items), once with 2 items
# =============================================================================

# ── Analysis A: Full Neuroticism (3 items) ────────────────────────────────────
mA_step1 <- lm(PHQ8 ~ Neuroticism_full_c + Mastery_c, data = data_clean)
summary(mA_step1)
confint(mA_step1)

mA_step2 <- lm(PHQ8 ~ Neuroticism_full_c * Mastery_c, data = data_clean)
summary(mA_step2)
confint(mA_step2)

anova(mA_step1, mA_step2)

# ── Analysis B: Neuroticism without item 2 (2 items) ─────────────────────────
mB_step1 <- lm(PHQ8 ~ Neuroticism_clean_c + Mastery_c, data = data_clean)
summary(mB_step1)
confint(mB_step1)

mB_step2 <- lm(PHQ8 ~ Neuroticism_clean_c * Mastery_c, data = data_clean)
summary(mB_step2)
confint(mB_step2)

anova(mB_step1, mB_step2)

# ── Coefficient comparison ────────────────────────────────────────────────────
cat("\n--- Coefficients: full (3-item) vs. clean (2-item) Neuroticism ---\n")
cat("\nAnalysis A (3 items):\n");  print(round(summary(mA_step2)$coefficients, 3))
cat("\nAnalysis B (2 items):\n");  print(round(summary(mB_step2)$coefficients, 3))


# =============================================================================
# STEP 13: Bayes Factors
# =============================================================================

library(BayesFactor)

# ── Analysis A BFs ────────────────────────────────────────────────────────────
bf_A_main        <- lmBF(PHQ8 ~ Neuroticism_full_c + Mastery_c,
                         data = data_clean)
bf_A_full        <- lmBF(PHQ8 ~ Neuroticism_full_c * Mastery_c,
                         data = data_clean)
bf_A_interaction <- bf_A_full / bf_A_main   # BF for the interaction term alone

cat("\n--- Bayes Factors: Analysis A (3-item Neuroticism) ---\n")
cat("Main effects model BF10:\n");         print(bf_A_main)
cat("Full model (+ interaction) BF10:\n"); print(bf_A_full)
cat("BF for interaction specifically:\n"); print(bf_A_interaction)

# ── Analysis B BFs ────────────────────────────────────────────────────────────
bf_B_main        <- lmBF(PHQ8 ~ Neuroticism_clean_c + Mastery_c,
                         data = data_clean)
bf_B_full        <- lmBF(PHQ8 ~ Neuroticism_clean_c * Mastery_c,
                         data = data_clean)
bf_B_interaction <- bf_B_full / bf_B_main

cat("\n--- Bayes Factors: Analysis B (2-item Neuroticism) ---\n")
cat("Main effects model BF10:\n");         print(bf_B_main)
cat("Full model (+ interaction) BF10:\n"); print(bf_B_full)
cat("BF for interaction specifically:\n"); print(bf_B_interaction)


# =============================================================================
# STEP 14: Simple Slopes
# =============================================================================

library(interactions)

cat("\n--- Simple Slopes: Analysis A (3-item Neuroticism) ---\n")
sim_slopes(mA_step2,
           pred   = Neuroticism_full_c,
           modx   = Mastery_c,
           jnplot = FALSE)

cat("\n--- Simple Slopes: Analysis B (2-item Neuroticism) ---\n")
sim_slopes(mB_step2,
           pred   = Neuroticism_clean_c,
           modx   = Mastery_c,
           jnplot = FALSE)


# =============================================================================
# STEP 15: Interaction Plot
# =============================================================================

if ("package:psych" %in% search()) detach("package:psych", unload = TRUE)
library(ggplot2)

# Define Mastery levels: -1 SD, Mean, +1 SD
Mastery_levels <- c(
  mean(data_clean$Mastery_c) - sd(data_clean$Mastery_c),
  mean(data_clean$Mastery_c),
  mean(data_clean$Mastery_c) + sd(data_clean$Mastery_c)
)

# Prediction grid
pred_grid <- expand.grid(
  Neuroticism_full_c = seq(min(data_clean$Neuroticism_full_c),
                           max(data_clean$Neuroticism_full_c),
                           length.out = 100),
  Mastery_c = Mastery_levels
)

preds              <- predict(mA_step2, newdata = pred_grid, interval = "confidence")
pred_grid$fit      <- preds[, "fit"]
pred_grid$lwr      <- preds[, "lwr"]
pred_grid$upr      <- preds[, "upr"]

pred_grid$Mastery <- factor(
  pred_grid$Mastery_c,
  levels = Mastery_levels,
  labels = c("Low (-1 SD)", "Mean", "High (+1 SD)")
)

# 
data_clean$Mastery_group <- cut(
  data_clean$Mastery_c,
  breaks = c(-Inf,
             mean(data_clean$Mastery_c) - sd(data_clean$Mastery_c) / 2,
             mean(data_clean$Mastery_c) + sd(data_clean$Mastery_c) / 2,
             Inf),
  labels = c("Low (-1 SD)", "Mean", "High (+1 SD)")
)

apa_theme <- theme_classic() +
  theme(
    text         = element_text(family = "serif", size = 11),
    axis.title   = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

ggplot() +
  # CI ribbons
  geom_ribbon(
    data  = pred_grid,
    aes(x = Neuroticism_full_c, ymin = lwr, ymax = upr,
        fill = Mastery),
    alpha = 0.20
  ) +
  # Raw data points
  geom_point(
    data  = data_clean,
    aes(x = Neuroticism_full_c, y = PHQ8, colour = Mastery_group),
    size  = 0.7, alpha = 0.25
  ) +
  # Regression lines
  geom_line(
    data  = pred_grid,
    aes(x = Neuroticism_full_c, y = fit, colour = Mastery),
    linewidth = 1.0
  ) +
  scale_colour_manual(
    name   = "Mastery",
    values = c("Low (-1 SD)"  = "#B2182B",
               "Mean"         = "#4D9221",
               "High (+1 SD)" = "#2166AC"),
    breaks = c("High (+1 SD)", "Mean", "Low (-1 SD)")
  ) +
  scale_fill_manual(
    name   = "Mastery",
    values = c("Low (-1 SD)"  = "#B2182B",
               "Mean"         = "#4D9221",
               "High (+1 SD)" = "#2166AC"),
    breaks = c("High (+1 SD)", "Mean", "Low (-1 SD)")
  ) +
  labs(
    x = "Neuroticism (Mean-Centred)",
    y = "Depressive Symptoms (PHQ-8)"
  ) +
  apa_theme +
  theme(
    legend.key.width = unit(1.5, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

ggsave("interaction_plot.png", width = 6, height = 5, dpi = 300, bg = "white")


# =============================================================================
# STEP 16: Demographics
# =============================================================================

demo_data <- Response_Data[
  as.numeric(rownames(data_clean)),
  c("Age", "Gender Identity", "Education", "Country of Residence")
]

# ── Age ───────────────────────────────────────────────────────────────────────
demo_data$Age <- as.numeric(unlist(demo_data$Age))
demo_data$Age[demo_data$Age < 18 | demo_data$Age > 87] <- NA

cat("\n--- Age ---\n")
cat(sprintf("Valid N:  %d\n",   sum(!is.na(demo_data$Age))))
cat(sprintf("Mean:     %.2f\n", mean(demo_data$Age, na.rm = TRUE)))
cat(sprintf("SD:       %.2f\n", sd(demo_data$Age,   na.rm = TRUE)))
cat(sprintf("Range:    %d - %d\n",
            min(demo_data$Age, na.rm = TRUE),
            max(demo_data$Age, na.rm = TRUE)))

# Age groups
demo_data$Age_Group <- cut(
  demo_data$Age,
  breaks = c(18, 25, 35, 45, 55, 65, 87),
  labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-87"),
  include.lowest = TRUE
)
cat("\n--- Age Groups ---\n")
age_group_table <- table(demo_data$Age_Group, useNA = "ifany")
print(data.frame(
  Age_Group  = names(age_group_table),
  n          = as.integer(age_group_table),
  percentage = round(prop.table(age_group_table) * 100, 1)
))

# ── Gender ────────────────────────────────────────────────────────────────────
cat("\n--- Gender Identity ---\n")
gender_table <- table(demo_data$`Gender Identity`, useNA = "ifany")
print(data.frame(
  n          = as.integer(gender_table),
  percentage = round(prop.table(gender_table) * 100, 1)
))

# ── Education ─────────────────────────────────────────────────────────────────
cat("\n--- Education ---\n")
edu_table <- table(demo_data$`Education`, useNA = "ifany")
print(data.frame(
  n          = as.integer(edu_table),
  percentage = round(prop.table(edu_table) * 100, 1)
))

# ── Country of Residence ──────────────────────────────────────────────────────
cat("\n--- Country of Residence ---\n")
country_table <- table(demo_data$`Country of Residence`, useNA = "ifany")
print(data.frame(
  n          = as.integer(country_table),
  percentage = round(prop.table(country_table) * 100, 1)
))

# ── Final summary ─────────────────────────────────────────────────────────────
cat("\n--- Final Sample Summary ---\n")
cat(sprintf("Total N after outlier removal: %d\n", nrow(demo_data)))
cat(sprintf("Valid Age:       %d\n", sum(!is.na(demo_data$Age))))
cat(sprintf("Valid Gender:    %d\n", sum(!is.na(demo_data$`Gender Identity`))))
cat(sprintf("Valid Education: %d\n", sum(!is.na(demo_data$`Education`))))
cat(sprintf("Valid Country:   %d\n", sum(!is.na(demo_data$`Country of Residence`))))