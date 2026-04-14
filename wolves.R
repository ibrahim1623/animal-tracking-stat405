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

j112v1 <- j112 %>%
  mutate(gap_hours = as.numeric(difftime(timestamp, lag(timestamp), units = "hours")))

ggplot(j112v1 %>% filter(!is.na(gap_hours)), aes(x = gap_hours)) +
  geom_histogram(bins = 50, fill = "steelblue") +
  xlim(0, 10) +
  labs(title = "Distribution of time gaps between GPS fixes",
       x = "Gap (hours)", y = "Count") +
  theme_minimal()

# 3. Longitude and latitude over time (see movement patterns)
ggplot(j112v1, aes(x = timestamp, y = `location-lat`)) +
  geom_line(alpha = 0.5) +
  labs(title = "Latitude over time", x = "Date", y = "Latitude") +
  theme_minimal()

ggplot(j112v1, aes(x = timestamp, y = `location-long`)) +
  geom_line(alpha = 0.5) +
  labs(title = "Longitude over time", x = "Date", y = "Longitude") +
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
  n       = nrow(j112_subset1),
  Y_lon   = j112_subset1$lon,
  Y_lat   = j112_subset1$lat,
  dt      = dt_hours,
  Y1_lon  = j112_subset1$lon[1],
  Y1_lat  = j112_subset1$lat[1],
  sigma_measurement = 0.001
)

# ---- Compile and run ----
mod <- cmdstan_model("velocity_state_space.stan")

fit <- mod$sample(
  data = stan_data,
  seed = 42,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500
)


# Extract draws in a format bayesplot can use
draws_array <- fit$draws()

# ---- Variance parameters ----
mcmc_trace(draws_array, pars = c("sigma_position", "sigma_velocity"))
mcmc_rank_hist(draws_array, pars = c("sigma_position", "sigma_velocity"))

# ---- A few position states (early, middle, late) ----
mcmc_trace(draws_array, pars = c("P_lon[1]", "P_lon[400]"))
mcmc_rank_hist(draws_array, pars = c("P_lon[1]", "P_lon[400]"))

# ---- Velocity states ----
mcmc_trace(draws_array, pars = c("V_lon[1]", "V_lon[400]", "V_lon[800]"))
mcmc_rank_hist(draws_array, pars = c("V_lon[1]", "V_lon[400]", "V_lon[800]"))

# ---- Speed (derived quantity) ----
mcmc_trace(draws_array, pars = c("speed[1]", "speed[800]"))