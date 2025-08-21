# Title: Beyond Average Scores: Identification of Consistent and
# Inconsistent Academic Achievement in Grouping Units
# Author: Marwin Carmo
# Purpose:   This script reproduces the empirical example in the paper.

# ==========================================================
# 0. Setup ----
# ==========================================================

# Install ivd from github
# install.packages("devtools")
devtools::install_github("consistentlybetter/ivd")

# Load required packages
# (install first if not already installed)
library(ivd)
library(data.table)
library(rstantools)

# ==========================================================
# 1. Data Preparation ----
# ==========================================================

## Calculate school-level SES
school_ses <- saeb[, .(school_ses = mean(student_ses, na.rm = TRUE)), by = school_id]

## Join the school_ses back to the original dataset
saeb <- saeb[school_ses, on = "school_id"]

## Define student level SES as deviation from the school SES
saeb$student_ses <- saeb$student_ses - saeb$school_ses

## Grand mean center school ses
saeb$school_ses <- c(scale(saeb$school_ses, scale = FALSE))

# ==========================================================
# 2. Run ivd ----
# ==========================================================

# A. Model 1: Random intercept only ------------------------

m1 <- ivd(location_formula = math_proficiency ~ 1 + (1|school_id),
           scale_formula =  ~ 1 + (1|school_id),
           data = saeb,
           niter = 3000, nburnin = 12000, WAIC = TRUE, workers = 6)

# B. Model 2: Covariate model -----------------------------

m2 <- ivd(location_formula = math_proficiency ~ student_ses * school_ses + (1|school_id),
          scale_formula =  ~ student_ses * school_ses + (1|school_id),
          data = saeb,
          niter = 3000, nburnin = 12000, WAIC = TRUE, workers = 6)

# C. Model 3: Random slope model --------------------------

m3 <- ivd(location_formula = math_proficiency ~ student_ses * school_ses + (1|school_id),
          scale_formula =  ~ student_ses * school_ses + (student_ses|school_id),
          data = saeb,
          niter = 3000, nburnin = 12000, WAIC = TRUE, workers = 6)

# ==========================================================
# 3. Model comparison ----
# ==========================================================

## LOO-CV

r_eff1 <- loo::relative_eff( exp( m1$logLik_array ) )
r_eff2 <- loo::relative_eff( exp( m2$logLik_array ) )
r_eff3 <- loo::relative_eff( exp( m3$logLik_array ) )

loo1 <- loo::loo(m1$logLik_array, r_eff = r_eff1)
loo2 <- loo::loo(m2$logLik_array, r_eff = r_eff2)
loo3 <- loo::loo(m3$logLik_array, r_eff = r_eff3)

loo_dif <- loo::loo_compare(loo1, loo2, loo3)

## Plotting Pareto k diagnostics

plot(loo1)
plot(loo2)
plot(loo3)
