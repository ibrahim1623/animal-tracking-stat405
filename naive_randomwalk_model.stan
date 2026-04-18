data {
  int<lower=1> T;                 // number of observation times
  vector[T] Y_lon;                // observed GPS longitudes
  vector[T] Y_lat;                // observed GPS latitudes
  real<lower=0> sigma_meas;       // fixed GPS measurement error (in degrees)
}

parameters {
  vector[T] X_lon;                // latent true longitude at each time
  vector[T] X_lat;                // latent true latitude at each time
  real<lower=0> sigma_move;       // movement noise SD
}

model {
  // Prior on movement variability 
  // Controls how much the true location can change between consecutive times
  sigma_move ~ exponential(1.0 / 0.01);

  // Initial state priors 
  // Assume the first true position is close to the first observed GPS fix
  X_lon[1] ~ normal(Y_lon[1], 0.01);
  X_lat[1] ~ normal(Y_lat[1], 0.01);

  // State evolution: random walk 
  // The naive model assumes the animal's true position at time t
  // is centered at its previous true position, plus movement noise
  for (t in 2:T) {
    X_lon[t] ~ normal(X_lon[t - 1], sigma_move);
    X_lat[t] ~ normal(X_lat[t - 1], sigma_move);
  }

  // Observation model
  // Observed GPS coordinates are noisy measurements of the true position
  // sigma_meas is treated as fixed, not estimated
  for (t in 1:T) {
    Y_lon[t] ~ normal(X_lon[t], sigma_meas);
    Y_lat[t] ~ normal(X_lat[t], sigma_meas);
  }
}