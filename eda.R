library(readr)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(cmdstanr)
library(bayesplot)

wolves <- read_csv("~/Downloads/wolves.csv")
wolves_ref <- read_csv("~/Downloads/wolves-ref.csv")
glimpse(wolves)
head(wolves_ref)

wolves %>%
  count(`individual-local-identifier`) %>%
  arrange(desc(n)) %>%
  print(n = 68)

j112 <- wolves %>%
  filter(`individual-local-identifier` == "J112") %>%
  mutate(
    timestamp = ymd_hms(timestamp),
    lat = `location-lat`,
    lon = `location-long`,
    date = as.Date(timestamp)
  ) %>%
  arrange(timestamp)

j112 %>%
  filter(date >= ymd("2009-05-01"), date <= ymd("2009-09-30")) %>%
  select(timestamp, lat, lon) %>%
  mutate(across(c(lat, lon), ~ (. - mean(.)) / sd(.))) %>%
  pivot_longer(cols = c(lat, lon), names_to = "coord", values_to = "value") %>%
  ggplot(aes(x = timestamp, y = value, color = coord)) +
  geom_point(size = 0.5, alpha = 0.4) +
  geom_line(alpha = 0.3) +
  scale_color_manual(
    values = c(lat = "#378ADD", lon = "#D85A30"),
    labels = c(lat = "Latitude", lon = "Longitude")
  ) +
  labs(title = "Latitude & longitude (standardized): May–Sep 2009",
       x = NULL, y = "Standard deviations from mean", color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

library(patchwork)

plot_window <- function(start, end, title) {
  df <- j112 %>%
    filter(date >= ymd(start), date <= ymd(end))
  
  lat_range <- range(df$lat)
  lon_range <- range(df$lon)
  
  df %>%
    ggplot(aes(x = timestamp)) +
    geom_point(aes(y = lat), color = "#378ADD", size = 0.6, alpha = 0.6) +
    geom_line(aes(y = lat), color = "#378ADD", alpha = 0.4) +
    geom_point(
      aes(y = scales::rescale(lon, to = lat_range, from = lon_range)),
      color = "#D85A30", size = 0.6, alpha = 0.6
    ) +
    geom_line(
      aes(y = scales::rescale(lon, to = lat_range, from = lon_range)),
      color = "#D85A30", alpha = 0.4
    ) +
    scale_y_continuous(
      name = "Latitude",
      sec.axis = sec_axis(
        ~ scales::rescale(., to = lon_range, from = lat_range),
        name = "Longitude"
      )
    ) +
    labs(title = title, x = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title.y.left  = element_text(color = "#378ADD"),
      axis.text.y.left   = element_text(color = "#378ADD"),
      axis.title.y.right = element_text(color = "#D85A30"),
      axis.text.y.right  = element_text(color = "#D85A30")
    )
}

j112 %>%
  filter(date >= ymd("2009-07-06"), date <= ymd("2009-07-22")) %>%
  nrow()

plot_window("2009-07-06", "2009-07-22", "Window 3: Jul 13 – Jul 22")


j112_subset <- j112 %>%
  filter(date >= ymd("2009-07-06"), date <= ymd("2009-07-25"))
# Trajectory colored by time
# install.packages(c("maptiles", "terra"))
library(maptiles)
library(terra)
library(sf)

# Create bounding box
bbox_sf <- st_as_sf(j112_subset, coords = c("lon", "lat"), crs = 4326) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(0.1)

# install.packages("slippymath")
tiles <- get_tiles(bbox_sf, provider = "OpenTopoMap", zoom = 10)
# install.packages("viridis")
# Plot
par(mar = c(0,0,0,0))
plot_tiles(tiles)
points(j112_subset$lon, j112_subset$lat, pch = 19, cex = 0.5,
       col = viridis::viridis(nrow(j112_subset)))
#install.packages("tidyterra")
library(tidyterra)
ggplot() +
  geom_spatraster_rgb(data = tiles) +
  geom_path(data = j112_subset, aes(x = lon, y = lat), 
            linewidth = 1.5, color = "white") +
  geom_path(data = j112_subset, aes(x = lon, y = lat), 
            linewidth = 0.8, color = "black") +
  geom_point(data = j112_subset, aes(x = lon, y = lat, color = as.numeric(timestamp)), 
             size = 3) +
  scale_color_gradient(low = "cyan", high = "magenta", name = "Time", labels = NULL) +
  labs(title = "Wolf J112 Trajectory — Jul 6–25, 2009",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# --- Data quality ---
cat("Total obs:", nrow(j112_subset), "\n")
cat("NA lat:", sum(is.na(j112_subset$lat)), "\n")
cat("NA lon:", sum(is.na(j112_subset$lon)), "\n")
cat("Duplicate timestamps:", sum(duplicated(j112_subset$timestamp)), "\n")
cat("Median fix interval:", median(j112_subset$dt_min, na.rm = TRUE), "min\n")

j112_subset1 <- wolves %>%
  filter(`individual-local-identifier` == "J112") %>%
  mutate(timestamp = ymd_hms(timestamp), lat = `location-lat`, lon = `location-long`, date = as.Date(timestamp)) %>%
  filter(date >= ymd("2009-07-06"), date <= ymd("2009-07-25")) %>%
  arrange(timestamp)

# Check structure and first rows
str(j112_subset1)
head(j112_subset1)

# Confirm date range
range(j112_subset1$date)

# Confirm only J112
unique(j112_subset1$`individual-local-identifier`)

# Row count
nrow(j112_subset1)

