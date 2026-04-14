data {
  int<lower=1> n;          // number of time steps
  vector[n] Y_lon;          // observed GPS longitudes
  vector[n] Y_lat;          // observed GPS latitudes
  vector[n-1] dt;           // time gaps between consecutive observations (in hours)
  real Y1_lon;              // first observed longitude (for initial state prior)
  real Y1_lat;              // first observed latitude (for initial state prior)
  real<lower=0> sigma_measurement;
}

parameters {
  // Hidden position states
  vector[n] P_lon;
  vector[n] P_lat;

  // Hidden velocity states
  vector[n] V_lon;
  vector[n] V_lat;

  // Standard deviations
  real<lower=0> sigma_position;
  real<lower=0> sigma_velocity;
}

model {
  // ---- Priors on standard deviations ----
  sigma_position    ~ exponential(1.0 / 0.01);
  sigma_velocity    ~ exponential(1.0 / 0.005);

  // ---- Initial state priors ----
  P_lon[1] ~ normal(Y1_lon, 0.01);
  P_lat[1] ~ normal(Y1_lat, 0.01);
  V_lon[1] ~ normal(0, 0.01);
  V_lat[1] ~ normal(0, 0.01);

  
  for (i in 2:n) {
    
    P_lon[i] ~ normal(P_lon[i-1] + V_lon[i-1] * dt[i-1], sigma_position);
    P_lat[i] ~ normal(P_lat[i-1] + V_lat[i-1] * dt[i-1], sigma_position);

    
    V_lon[i] ~ normal(V_lon[i-1], sigma_velocity);
    V_lat[i] ~ normal(V_lat[i-1], sigma_velocity);
  }


  for (i in 1:n) {
    Y_lon[i] ~ normal(P_lon[i], sigma_measurement);
    Y_lat[i] ~ normal(P_lat[i], sigma_measurement);
  }
}

generated quantities {
  // simulated observations
  vector[n] Y_lon_pred;
  vector[n] Y_lat_pred;

  // Estimated speed at each time step (degrees per hour)
  vector[n] speed;

  for (i in 1:n) {
    Y_lon_pred[i] = normal_rng(P_lon[i], sigma_measurement);
    Y_lat_pred[i] = normal_rng(P_lat[i], sigma_measurement);
    speed[i] = sqrt(V_lon[i]^2 + V_lat[i]^2);
  }
}
