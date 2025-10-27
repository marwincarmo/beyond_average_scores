## This script updates the first simulation study by adding a correlation between the location
## and scale random effects. Additionally, we also compare the performance of ivd and hlm
## in identifying unusually high or low variability as qualitatively different.
## The JAGS model has been updated to account for this correlation.

## 0 Load in required packages -----

library(lme4)
library(car)
library(metafor)
library(ggplot2)
library(dplyr)
library(R2jags)

### Load in the required helper functions
source("R/helpers.R")

## 1 Simulation loop -----

# We can determine the sd_slab from the increase in sigma_j due to the random effects'
# variance: exp((tau^2)/2:
sigma_increase <- function(s) sqrt(log(s^2))

sd_scl_intc <- sigma_increase(c(1.05, 1.1, 1.25, 1.50))

# Grid for when there are slab schools
grid_with_slab <- expand.grid(
  n_students = c(50, 75, 150),
  n_schools  = c(50, 150, 300),
  eta0       = -0.25,
  p_slab     = c(0.05, 0.1, 0.25),   # only non-zero slab probs
  sd_slab    = sd_scl_intc,          # all non-zero sd values
  cor_li_si  = c(0., 0.5)
)

# Grid for when there are NO slab schools
grid_no_slab <- expand.grid(
  n_students = c(50, 75, 150),
  n_schools  = c(50, 150, 300),
  eta0       = -0.25,
  p_slab     = 0,   # no slab
  sd_slab    = 0,   # forced zero here
  cor_li_si  = 0    # with all schools in the 01_report_ivd-v-hlm_spike.Rmd
)

# Combine them into the final grid
sim_conditions_grid <- rbind(grid_no_slab, grid_with_slab)

reps  <- 1
results_list <- list()

for (i in 1:nrow(sim_conditions_grid)) {
  params <- sim_conditions_grid[i, ]

  cat("Running condition:", i, "/", nrow(sim_conditions_grid), "\n")
  print(params)

  for (rep in 1:reps) {
     cat(".") # Progress marker

     sim_output <- generate_data(
       n_students = params$n_students,
       n_schools = params$n_schools,
       eta0 = params$eta0,
       prob_slab = params$p_slab,
       sd_slab = params$sd_slab,
       cor_li_si = params$cor_li_si,
       loc_intc = 0,
       loc_intc_sd = 0.33 # estimated from the empirical example
     )


     ## generate data from simulation parameters
     d <- sim_output$data
     true_slab_ids <- sim_output$idx_slab

     P_slab <- length(true_slab_ids)
     N_total <- params$n_schools
     N_baseline <- N_total - P_slab

     ## Fit ivd model in JAGS ----
     ## we use JAGS instead of ivd because it runs faster than nimble
     ## which is used in the backend of ivd
     ## The results of both are virtually identical

     d$intercept <- 1
     ## Create the bval vector for the spike-and-slab priors on scale effects
     bval_scale <- matrix(rep(0.5, 1), ncol = 1) ## prior probability of delta

     jags_data <- list(Y = d$y,
                       groupid = d$school, # school ids
                       N = length(d$y), # number of obs
                       X = matrix(d$intercept, ncol = 1), # location predictor matrix (intercept only)
                       X_scale = matrix(d$intercept, ncol = 1), # location random predictor matrix
                       Z = matrix(d$intercept, ncol = 1),
                       Z_scale =  matrix(d$intercept, ncol = 1),
                       P = 2, # number of random effects
                       J = length(unique(d$school)),
                       K = 1, # location fixed effects
                       S = 1, # scale fixed effects
                       Kr = 1, # location random effects
                       Sr = 1, # scale random effects
                       bval = bval_scale
                       )

     parameters <- c("v_final", "sigma_rand", "delta", "tau", "R")

     fit_jags <- jags.parallel(jags_data,
                               parameters.to.save = parameters,
                               model.file = "models/SpikeSlab_cor_ranef.bug",
                               n.iter = 5000,
                               n.chains = 4
                               )

     ## JAGS samples
     delta_samples <- fit_jags$BUGSoutput$sims.list$delta
     pip <- colMeans(delta_samples)
     schoolid <- unique(d$school)

     ## extract to level 2
     df_pip <- data.frame(school = schoolid, pip = pip )
     df_pip <- df_pip[order(df_pip$pip), ]
     df_pip$ordered <- 1:nrow(df_pip)

     flagged_ids <- schoolid[pip >= .75]

     ## Fit two-stage HLM ---
     fit_lmer <- lmer(y ~ 1 + (1 | school), data = d)
     stage2_data <- data.frame(school_id = factor(1:params$n_schools))
     stage2_data$s_j <- tapply(residuals(fit_lmer), d$school, sd)
     stage2_data$n_j <- tapply(d$y, d$school, length)
     stage2_data$p <- 1 # number of predictors
     stage2_data$df_j <- stage2_data$n_j - stage2_data$p

     ## apply the transformation from R&B (1987)
     stage2_data$d_j <- log(stage2_data$s_j) + 1 / (2 * stage2_data$df_j)
     ## define the known sampling variance and SE
     stage2_data$v_j <- 1 / (2 * stage2_data$df_j)

     fit_metafor <- rma.uni(
       yi = as.vector(stage2_data$d_j),
       vi = as.vector(stage2_data$v_j),
       method = "REML"
     )

     ## the estimated grand mean
     delta_hat_metafor <- fit_metafor$b
     ## the estimated between-school variance (Var(U_i) or tau^2)
     var_u_i_metafor <- fit_metafor$tau2

     ## the blup() function returns the shrunken estimates for each school's effect.
     blup_estimates <- blup(fit_metafor)
     ## divide the shrunken deviation by its standard error to get the Z_i statistic.
     Zi_metafor <- blup_estimates$pred / blup_estimates$se

     metafor_flagged_empirical <- qq_outside(Zi_metafor)

     ## post-hoc computation of the random effects' correlation
     u_j <- ranef(fit_lmer)$school$`(Intercept)`
     v_j <- ranef(fit_metafor)$pred
     cor_uv <- cor(u_j, v_j)

     ## collect metrics of true and false positives/negatives
     tp_hlm <- sum(metafor_flagged_empirical %in% true_slab_ids)
     fp_hlm <- sum(metafor_flagged_empirical %in% schoolid[!schoolid %in% true_slab_ids])
     fn_hlm <- P_slab - tp_hlm
     tn_hlm <- N_baseline - fp_hlm

     tp_ivd <- sum(flagged_ids %in% true_slab_ids)
     fp_ivd <- sum(flagged_ids %in% schoolid[!schoolid %in% true_slab_ids])
     fn_ivd <- P_slab - tp_ivd
     tn_ivd <- N_baseline - fp_ivd


     results_list[[length(results_list) + 1]] <- data.frame(
       condition_id = i, replication = rep,
       params,
       ivd_sd_scl_Intc = fit_jags$BUGSoutput$mean$sigma_rand[2],
       hlm_sd_scl_Intc = sqrt(fit_metafor$tau2),
       r_ivd = colMeans(fit_jags$BUGSoutput$sims.list$R)[1,2],
       r_hlm = cor_uv,
       tp_hlm, fp_hlm, fn_hlm, tn_hlm,
       tp_ivd, fp_ivd, fn_ivd, tn_ivd
     )

  }

  cat("\n")

  results_df <- do.call(rbind, results_list)
}

saveRDS(results_df, "output/01_simulation_main_results.rds")
