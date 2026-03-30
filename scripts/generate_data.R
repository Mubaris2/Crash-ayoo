# ----------------------------------------
# SYNTHETIC DATA GENERATOR
# Generates realistic vehicle sensor data
# with balanced event class distribution:
#   ~85% normal, ~8% harsh_brake, ~7% crash
# ----------------------------------------

set.seed(123)
library(dplyr)
library(readr)

n_rows      <- 15000
vehicle_ids <- paste0("V", 100:149)
start_time  <- as.POSIXct("2026-03-01 08:00:00")

# Base lat/lon around Bangalore
base_lat <- 12.9
base_lon <- 77.6

# Assign event labels with target distribution
event_labels <- sample(
  c("normal", "harsh_brake", "crash"),
  size    = n_rows,
  replace = TRUE,
  prob    = c(0.85, 0.08, 0.07)
)

# Generate sensor readings per event type
generate_row <- function(label) {
  if (label == "normal") {
    accel     <- rnorm(1, mean = 0,    sd = 1.5)
    gyro      <- rnorm(1, mean = 0,    sd = 0.4)
    speed     <- runif(1, min = 10,    max = 90)
    vibration <- abs(rnorm(1, mean = 0.2, sd = 0.15))

  } else if (label == "harsh_brake") {
    accel     <- rnorm(1, mean = -7.5, sd = 1.0)   # strong deceleration
    gyro      <- rnorm(1, mean = 0,    sd = 0.5)
    speed     <- runif(1, min = 20,    max = 80)
    vibration <- abs(rnorm(1, mean = 1.2, sd = 0.4))

  } else {  # crash
    accel     <- rnorm(1, mean = -12,  sd = 2.0)   # severe deceleration spike
    gyro      <- rnorm(1, mean = 0,    sd = 2.5)   # high rotation (spin/roll)
    speed     <- runif(1, min = 30,    max = 100)
    vibration <- abs(rnorm(1, mean = 6.0, sd = 1.5)) # high vibration
  }

  list(
    accel     = accel,
    gyro      = gyro,
    speed     = speed,
    vibration = vibration
  )
}

sensor_data <- lapply(event_labels, generate_row)
sensor_df   <- bind_rows(sensor_data)

synthetic_data <- data.frame(
  vehicle_id  = sample(vehicle_ids, n_rows, replace = TRUE),
  timestamp   = format(start_time + seq(0, n_rows - 1), "%Y-%m-%d %H:%M:%S"),
  acceleration = sensor_df$accel,
  gyroscope    = sensor_df$gyro,
  speed        = sensor_df$speed,
  vibration    = sensor_df$vibration,
  latitude     = round(base_lat + runif(n_rows, -0.05, 0.05), 9),
  longitude    = round(base_lon + runif(n_rows, -0.05, 0.05), 9),
  event_label  = event_labels
)

write_csv(synthetic_data, "data/synthetic_data.csv")

# Print summary
cat("Data generation complete.\n")
cat("Total rows:", nrow(synthetic_data), "\n")
print(table(synthetic_data$event_label))
