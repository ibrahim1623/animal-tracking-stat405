data {
  int<lower=1> T;                 // number of observation times
  vector[T] Y_lon;                // observed GPS longitudes
  vector[T] Y_lat;                // observed GPS latitudes
  real<lower=0> sigma_meas;       // fixed GPS measurement error (in degrees)
}

parameters {
  vector[T] X_lon;                
  vector[T] X_lat;                
  real<lower=0> sigma_move;       
}

model {
  
  sigma_move ~ exponential(1.0 / 0.01);

  
  X_lon[1] ~ normal(Y_lon[1], 0.01);
  X_lat[1] ~ normal(Y_lat[1], 0.01);


  for (t in 2:T) {
    X_lon[t] ~ normal(X_lon[t - 1], sigma_move);
    X_lat[t] ~ normal(X_lat[t - 1], sigma_move);
  }


  for (t in 1:T) {
    Y_lon[t] ~ normal(X_lon[t], sigma_meas);
    Y_lat[t] ~ normal(X_lat[t], sigma_meas);
  }
}