# Thesis R Code – Neuroticism, Mastery, and Depressive Symptoms

This repository contains the R script used for the data analysis in my Bachelor's thesis in Psychology at Leiden University.

## What This Study Is About

This study examined whether mastery (a sense of personal control over one's life) moderates the relationship between neuroticism and depressive symptoms.
In other words: does having a stronger sense of control moderate the effect of neuroticism on depression?

- Predictor: Neuroticism (BFI)
- Moderator: Mastery (Personal Mastery Scale, Pearlin & Schooler, 1978)
- Outcome: Depressive Symptoms (PHQ-8)

## What the Script Does

The script runs the full analysis in 16 steps:

1. Filter completed responses and select relevant variables
2. Recode text answers into numbers
3. Reverse-score negatively worded items
4. Calculate mean composite scores
5. Mean-centre predictors
6. Check assumptions (linearity, homoscedasticity, normality, multicollinearity)
7. Remove outliers
8. Calculate Cronbach's Alpha (reliability)
9. Descriptive statistics
10. Pearson correlations
11. Correlation scatter plots
12. Hierarchical moderated regression
13. Bayes Factors
14. Simple slopes analysis
15. Interaction plot
16. Demographics

## Requirements

Run this in R (version 4.4.1), The following packages are needed:

```r
install.packages(c("car", "psych", "ggplot2", "patchwork", "BayesFactor", "interactions"))
