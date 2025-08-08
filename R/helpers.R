# Helper functions for the simulation study

## Generate data from a hierarchical structure, following the MELSM
## framework, where location and scale random effects are sampled
## simultaneously from a multivariate normal distribution.
## This function generates data from an intercept-only model.

generate_data <- function(
    n_students,
    n_schools,
    eta0,            # Fixed scale intercept (log-scale)
    prob_slab,       # Probability a school's scale effect is from the slab
    sd_slab,         # SD of the scale random effects (log-scale)
    loc_intc,        # Location intercept
    loc_intc_sd,     # SD of random intercepts for location
    cor_li_si        # Correlation among location and scale random intercept
) {

  N <- n_students * n_schools
  school_id <- rep(1:n_schools, each = n_students)

  sd_mat <- matrix(c((loc_intc_sd), 0, 0, (sd_slab)
  ), 2, 2)
  r_mat <- matrix(c(
    1, cor_li_si,
    cor_li_si, 1), 2, 2)

  Sigma <- sd_mat %*% r_mat %*% sd_mat

  v <- MASS::mvrnorm(
    n = n_schools,
    mu = c(0, 0), Sigma
  )

  ## Location model components
  rand_intercepts <- v[, 1][school_id]
  linpred <- loc_intc + rand_intercepts

  # Scale model components
  # Determine which schools are in the slab vs. spike
  is_slab <- sample(n_schools, prob_slab * n_schools)

  random_effect <- numeric(n_schools)
  random_effect[is_slab] = v[, 2][is_slab]

  sigma_j <- exp(eta0 + random_effect)

  y_residuals <- rnorm(N, 0, sigma_j[school_id])
  y <- linpred + y_residuals
  data <- data.frame(
    y = y,
    school = factor(school_id)
  )
  idx_slab <- c(1:n_schools)[is_slab]

  return(list(
    data = data,
    idx_slab = idx_slab,
    slab = is_slab,
    random_effect = random_effect
  ))
}

## This function is used to compare the quantiles of a given vector
## to a normal distribution. It also creates 95% confidence intervals
## around these points, and identifies the points that fall beyond these limts

qq_outside <- function(y) {

  ord <- order(y)
  n <- length(y)
  y_sorted <- y[ord]
  x_theor <- qnorm(ppoints(n, a = 0.5))


  zq <- qnorm(c(0.25, 0.75))
  yq <- as.numeric(quantile(y_sorted, c(0.25, 0.75), names = FALSE))
  slope <- diff(yq) / diff(zq)
  intercept <- yq[1] - slope * zq[1]


  # Simulate order stats under the null (z-scale), then transform to y-scale
  nrepl = 1000
  alpha <- (1 - 0.95) / 2
  sim_z <- replicate(nrepl, sort(rnorm(n)))
  lower_z <- apply(sim_z, 1, quantile, probs = alpha)
  upper_z <- apply(sim_z, 1, quantile, probs = 1 - alpha)

  # Transform envelope to the fitted line scale
  lower <- intercept + slope * lower_z
  upper <- intercept + slope * upper_z

  # Identify points outside the envelope
  outside_sorted <- (y_sorted < lower) | (y_sorted > upper)
  flagged_ids <- ord[outside_sorted]
  return(flagged_ids)
}
