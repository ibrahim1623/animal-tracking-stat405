library(readr)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(cmdstanr)
library(bayesplot)

wolves <- read_csv("Hebblewhite_Alberta-BC_Wolves.csv")

j112 <- wolves |>
  filter(`tag-local-identifier` == "J112") |>
  mutate(timestamp = ymd_hms(timestamp)) %>%
  arrange(timestamp)

ggplot(j112, aes(x = `location-long`, y = `location-lat`, color = timestamp)) +
  geom_path(alpha = 0.3) +
  geom_point(size = 0.5) +
  scale_color_viridis_c() +
  labs(title = "Wolf J112 (Sunwapta pack) — full trajectory",
       x = "Longitude", y = "Latitude", color = "Time") +
  theme_minimal()

j112_subset1 <- wolves %>%
  filter(`individual-local-identifier` == "J112") %>%
  mutate(timestamp = ymd_hms(timestamp), lat = `location-lat`, lon = `location-long`, date = as.Date(timestamp)) %>%
  filter(date >= ymd("2009-07-06"), date <= ymd("2009-07-25")) %>%
  arrange(timestamp)


timestamps <- j112_subset1$timestamp
dt_hours <- as.numeric(difftime(timestamps[-1], timestamps[-length(timestamps)], units = "hours"))

# ---- Prepare Stan data ----
stan_data <- list(
  T       = nrow(j112_subset1),
  Y_lon   = j112_subset1$lon,
  Y_lat   = j112_subset1$lat,
  sigma_meas = 0.001
)

# ---- Compile and run ----
mod <- cmdstan_model("simple.stan")

fit_simple <- mod$sample(
  data = stan_data,
  seed = 42,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500
)


# Extract draws in a format bayesplot can use
draws_array_simple <- fit_simple$draws()

# ---- Variance parameters ----
mcmc_trace(draws_array_simple, pars = c("sigma_move" ))
mcmc_rank_hist(draws_array_simple, pars = c("sigma_move"))

# ---- A few position states (early, middle, late) ----
mcmc_trace(draws_array_simple, pars = c("X_lon[1]", "X_lon[400]"))
mcmc_rank_hist(draws_array_simple, pars = c("X_lon[1]", "X_lon[400]"))