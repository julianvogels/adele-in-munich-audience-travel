# Load necessary libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("geosphere", quietly = TRUE)) {
  install.packages("geosphere")
}

library(tidyverse)
library(geosphere)

# Define the coordinates of Munich, Germany
munich_coords <- c(11.5820, 48.1351) # Longitude, Latitude of Munich

# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Ensure latitude and longitude columns are numeric and filter for flights
all_data <- all_data %>%
  mutate(
    `Itinerary Origin latitude` = as.numeric(`Itinerary Origin latitude`),
    `Itinerary Origin longitude` = as.numeric(`Itinerary Origin longitude`)
  ) %>%
  filter(
    `Itinerary Means of transportation` == "flight",
    !(`Itinerary index` > 1 &
      `Participant Identifier` %in%
        all_data$`Participant Identifier`[all_data$`Itinerary index` == (`Itinerary index` - 1) &
          all_data$`Itinerary Means of transportation` == "flight"])
  )

# Calculate the distance from each origin point to Munich
all_data <- all_data %>%
  rowwise() %>%
  mutate(Distance_to_Munich = distHaversine(
    c(`Itinerary Origin longitude`, `Itinerary Origin latitude`),
    munich_coords
  ))

# Calculate the total distance to Munich for all points
total_distance_to_munich <- sum(all_data$Distance_to_Munich, na.rm = TRUE)
print(paste("Total distance to Munich:", round(total_distance_to_munich, 2), "meters"))

# Define the tour locations data frame
tour_locations <- data.frame(
  City = c("London", "Berlin", "Paris", "Amsterdam", "Rome", "Madrid", "Vienna", "Prague", "Copenhagen", "Munich"),
  Latitude = c(51.5074, 52.5200, 48.8566, 52.3676, 41.9028, 40.4168, 48.2082, 50.0755, 55.6761, 48.1351),
  Longitude = c(-0.1278, 13.4050, 2.3522, 4.9041, 12.4964, -3.7038, 16.3738, 14.4378, 12.5683, 11.5820)
)

# Find the closest city for each point and calculate the distance to it
all_data <- all_data %>%
  rowwise() %>%
  mutate(
    Closest_City = tour_locations$City[which.min(
      distHaversine(
        c(`Itinerary Origin longitude`, `Itinerary Origin latitude`),
        tour_locations[, c("Longitude", "Latitude")]
      )
    )],
    Distance_to_Closest_City = min(
      distHaversine(
        c(`Itinerary Origin longitude`, `Itinerary Origin latitude`),
        tour_locations[, c("Longitude", "Latitude")]
      )
    )
  )

# Calculate the sum of all distances to the closest cities
total_distance_to_closest_cities <- sum(all_data$Distance_to_Closest_City, na.rm = TRUE)
print(paste("Total distance to the closest touring cities:", round(total_distance_to_closest_cities, 2), "meters"))
print(paste("Decrease in travelled distance:", round((total_distance_to_munich - total_distance_to_closest_cities) / total_distance_to_munich * 100, 2), "%"))
