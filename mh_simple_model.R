library(dplyr)
library(lubridate)
library(ggplot2)

# ---- Load and prepare data ----
wolves <- read.csv("Hebblewhite_Alberta-BC_Wolves.csv")

j112_subset1 <- wolves %>%
  filter(individual.local.identifier == "J112") %>%
  mutate(timestamp = ymd_hms(timestamp),
         lat = location.lat,
         lon = location.long,
         date = as.Date(timestamp)) %>%
  filter(date >= ymd("2009-07-06"), date <= ymd("2009-07-25")) %>%
  arrange(timestamp)

# Thin to every 15th observation (~200 points)
j112_thin <- j112_subset1[seq(1, nrow(j112_subset1), by = 5), ]
cat("Thinned observations:", nrow(j112_thin), "\n")

Y_lon <- j112_thin$lon
Y_lat <- j112_thin$lat
n <- length(Y_lon)

# Fixed sigma_measurement
sigma_measurement <- 0.001

# log posterior function
log_gamma <- function(X_lon, X_lat, sigma_movement) {
  if (sigma_movement <= 0) return(-Inf)
  
  lp <- 0
  
  lp <- lp + dexp(sigma_movement, rate = 1/0.01, log = TRUE)
  
  lp <- lp + dnorm(X_lon[1], mean = Y_lon[1], sd = 0.01, log = TRUE)
  lp <- lp + dnorm(X_lat[1], mean = Y_lat[1], sd = 0.01, log = TRUE)
  
  for (i in 2:n) {
    lp <- lp + dnorm(X_lon[i], mean = X_lon[i-1], sd = sigma_movement, log = TRUE)
    lp <- lp + dnorm(X_lat[i], mean = X_lat[i-1], sd = sigma_movement, log = TRUE)
  }
  
  for (i in 1:n) {
    lp <- lp + dnorm(Y_lon[i], mean = X_lon[i], sd = sigma_measurement, log = TRUE)
    lp <- lp + dnorm(Y_lat[i], mean = X_lat[i], sd = sigma_measurement, log = TRUE)
  }
  
  return(lp)
}

# MH
simple_mh <- function(log_gamma, X_lon_init, X_lat_init, sigma_init, n_iters, proposal_sd = 0.0005) {
  n <- length(X_lon_init)
  
  X_lon_samples <- matrix(NA, nrow = n_iters, ncol = n)
  X_lat_samples <- matrix(NA, nrow = n_iters, ncol = n)
  sigma_samples <- numeric(n_iters)
  
  # current state
  X_lon <- X_lon_init
  X_lat <- X_lat_init
  sigma_movement <- sigma_init
  current_lp <- log_gamma(X_lon, X_lat, sigma_movement)
  
  accept_count <- 0
  total_proposals <- 0
  start_time <- Sys.time()
  
  for (iter in 1:n_iters) {
    
    # update each X_lon[i]
    for (i in 1:n) {
      X_lon_prop <- X_lon
      X_lon_prop[i] <- rnorm(1, mean = X_lon[i], sd = proposal_sd)
      
      prop_lp <- log_gamma(X_lon_prop, X_lat, sigma_movement)
      if (log(runif(1)) < (prop_lp - current_lp)) {
        X_lon <- X_lon_prop
        current_lp <- prop_lp
        accept_count <- accept_count + 1
      }
      total_proposals <- total_proposals + 1
    }
    
    # update each X_lat[i]
    for (i in 1:n) {
      X_lat_prop <- X_lat
      X_lat_prop[i] <- rnorm(1, mean = X_lat[i], sd = proposal_sd)
      
      prop_lp <- log_gamma(X_lon, X_lat_prop, sigma_movement)
      if (log(runif(1)) < (prop_lp - current_lp)) {
        X_lat <- X_lat_prop
        current_lp <- prop_lp
        accept_count <- accept_count + 1
      }
      total_proposals <- total_proposals + 1
    }
    
    # update sigma_movement
    sigma_prop <- rnorm(1, mean = sigma_movement, sd = proposal_sd)
    if (sigma_prop > 0) {
      prop_lp <- log_gamma(X_lon, X_lat, sigma_prop)
      if (log(runif(1)) < (prop_lp - current_lp)) {
        sigma_movement <- sigma_prop
        current_lp <- prop_lp
        accept_count <- accept_count + 1
      }
    }
    total_proposals <- total_proposals + 1
    
    # store samples
    X_lon_samples[iter, ] <- X_lon
    X_lat_samples[iter, ] <- X_lat
    sigma_samples[iter] <- sigma_movement
    
    # Progress
    if (iter %% 500 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      cat(sprintf("Iteration %d/%d (%.1f min) | Acceptance: %.1f%%\n",
                  iter, n_iters, elapsed, 100 * accept_count / total_proposals))
    }
  }
  
  cat("Total time:", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1), "min\n")
  cat("Acceptance rate:", round(100 * accept_count / total_proposals, 1), "%\n")
  
  return(list(X_lon = X_lon_samples, X_lat = X_lat_samples, sigma = sigma_samples))
}


n_iters <- 2500
result <- simple_mh(log_gamma,
                    X_lon_init = Y_lon,
                    X_lat_init = Y_lat,
                    sigma_init = 0.005,
                    n_iters = n_iters,
                    proposal_sd = 0.003)


burn_in <- 400
X_lon_post <- result$X_lon[(burn_in+1):n_iters, ]
X_lat_post <- result$X_lat[(burn_in+1):n_iters, ]
sigma_post <- result$sigma[(burn_in+1):n_iters]



# ---- Diagnostics ----
plot(sigma_post, type = "l",
     main = "Trace: sigma_movement (MH)",
     xlab = "Sample", ylab = "sigma_movement")

hist(sigma_post, breaks = 50,
     main = "Posterior: sigma_movement (MH)",
     xlab = "sigma_movement", col = "steelblue")

cat("sigma_movement mean:", mean(sigma_post), "\n")
cat("sigma_movement 95% CI:", quantile(sigma_post, c(0.025, 0.975)), "\n")













# forward simulator
forward <- function() {
  sigma_movement <- rexp(1, rate = 1/0.01)
  
  X_lon <- numeric(n)
  X_lat <- numeric(n)
  X_lon[1] <- rnorm(1, mean = Y_lon[1], sd = 0.01)
  X_lat[1] <- rnorm(1, mean = Y_lat[1], sd = 0.01)
  for (i in 2:n) {
    X_lon[i] <- rnorm(1, mean = X_lon[i-1], sd = sigma_movement)
    X_lat[i] <- rnorm(1, mean = X_lat[i-1], sd = sigma_movement)
  }
  
  Y_lon_sim <- rnorm(n, mean = X_lon, sd = sigma_measurement)
  Y_lat_sim <- rnorm(n, mean = X_lat, sd = sigma_measurement)
  
  return(list(
    X_lon = X_lon,
    X_lat = X_lat,
    sigma_movement = sigma_movement,
    Y_lon = Y_lon_sim,
    Y_lat = Y_lat_sim
  ))
}


# runs forward sim, then optionally does MH iterations
stationary_mcmc <- function(n_iterations) {
  init <- forward()
  
  if (n_iterations == 0) {
    return(init$sigma_movement)
  }
  
  
  X_lon <- init$X_lon
  X_lat <- init$X_lat
  sigma_mov <- init$sigma_movement
  Y_lon_sim <- init$Y_lon
  Y_lat_sim <- init$Y_lat
  
  log_gamma_sim <- function(X_lon, X_lat, sigma_movement) {
    if (sigma_movement <= 0) return(-Inf)
    lp <- dexp(sigma_movement, rate = 1/0.01, log = TRUE)
    lp <- lp + dnorm(X_lon[1], mean = Y_lon_sim[1], sd = 0.01, log = TRUE)
    lp <- lp + dnorm(X_lat[1], mean = Y_lat_sim[1], sd = 0.01, log = TRUE)
    for (i in 2:n) {
      lp <- lp + dnorm(X_lon[i], mean = X_lon[i-1], sd = sigma_movement, log = TRUE)
      lp <- lp + dnorm(X_lat[i], mean = X_lat[i-1], sd = sigma_movement, log = TRUE)
    }
    for (i in 1:n) {
      lp <- lp + dnorm(Y_lon_sim[i], mean = X_lon[i], sd = sigma_measurement, log = TRUE)
      lp <- lp + dnorm(Y_lat_sim[i], mean = X_lat[i], sd = sigma_measurement, log = TRUE)
    }
    return(lp)
  }
  
  current_lp <- log_gamma_sim(X_lon, X_lat, sigma_mov)
  proposal_sd <- 0.003
  
  for (iter in 1:n_iterations) {
    # Update each X_lon
    for (i in 1:n) {
      X_lon_prop <- X_lon
      X_lon_prop[i] <- rnorm(1, mean = X_lon[i], sd = proposal_sd)
      prop_lp <- log_gamma_sim(X_lon_prop, X_lat, sigma_mov)
      if (log(runif(1)) < (prop_lp - current_lp)) {
        X_lon <- X_lon_prop
        current_lp <- prop_lp
      }
    }
    # Update each X_lat
    for (i in 1:n) {
      X_lat_prop <- X_lat
      X_lat_prop[i] <- rnorm(1, mean = X_lat[i], sd = proposal_sd)
      prop_lp <- log_gamma_sim(X_lon, X_lat_prop, sigma_mov)
      if (log(runif(1)) < (prop_lp - current_lp)) {
        X_lat <- X_lat_prop
        current_lp <- prop_lp
      }
    }
    # Update sigma
    sigma_prop <- rnorm(1, mean = sigma_mov, sd = proposal_sd)
    if (sigma_prop > 0) {
      prop_lp <- log_gamma_sim(X_lon, X_lat, sigma_prop)
      if (log(runif(1)) < (prop_lp - current_lp)) {
        sigma_mov <- sigma_prop
        current_lp <- prop_lp
      }
    }
  }
  
  return(sigma_mov)
}

# exact invariance test

n_orig <- n
n <- 10  # temporarily use very few points for fast testing

forward_only <- replicate(500, stationary_mcmc(0))
with_mcmc    <- replicate(500, stationary_mcmc(5))

test_result <- ks.test(forward_only, with_mcmc)
print(test_result)


n <- n_orig  # restore original n