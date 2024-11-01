# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to process data for car occupancy analysis with distance buckets
process_car_occupancy_data <- function(data) {
    # Filter data for valid car occupancy values and exclude missing values
    car_data <- data %>%
        filter(!is.na(`Itinerary Automobile passengers`)) %>%
        filter(!is.na(`Itinerary Distance (single journey, km)`))

    # Create distance buckets: 0-5 km, 5-10 km, ..., 45-50 km, and >50 km
    car_data <- car_data %>%
        mutate(DistanceBucket = case_when(
            `Itinerary Distance (single journey, km)` <= 5 ~ "0-5 km",
            `Itinerary Distance (single journey, km)` > 5 & `Itinerary Distance (single journey, km)` <= 10 ~ "5-10 km",
            `Itinerary Distance (single journey, km)` > 10 & `Itinerary Distance (single journey, km)` <= 15 ~ "10-15 km",
            `Itinerary Distance (single journey, km)` > 15 & `Itinerary Distance (single journey, km)` <= 20 ~ "15-20 km",
            `Itinerary Distance (single journey, km)` > 20 & `Itinerary Distance (single journey, km)` <= 25 ~ "20-25 km",
            `Itinerary Distance (single journey, km)` > 25 & `Itinerary Distance (single journey, km)` <= 30 ~ "25-30 km",
            `Itinerary Distance (single journey, km)` > 30 & `Itinerary Distance (single journey, km)` <= 35 ~ "30-35 km",
            `Itinerary Distance (single journey, km)` > 35 & `Itinerary Distance (single journey, km)` <= 40 ~ "35-40 km",
            `Itinerary Distance (single journey, km)` > 40 & `Itinerary Distance (single journey, km)` <= 45 ~ "40-45 km",
            `Itinerary Distance (single journey, km)` > 45 & `Itinerary Distance (single journey, km)` <= 50 ~ "45-50 km",
            `Itinerary Distance (single journey, km)` > 50 & `Itinerary Distance (single journey, km)` <= 55 ~ "50-55 km",
            `Itinerary Distance (single journey, km)` > 55 & `Itinerary Distance (single journey, km)` <= 60 ~ "55-60 km",
            `Itinerary Distance (single journey, km)` > 60 & `Itinerary Distance (single journey, km)` <= 65 ~ "60-65 km",
            `Itinerary Distance (single journey, km)` > 65 & `Itinerary Distance (single journey, km)` <= 70 ~ "65-70 km",
            `Itinerary Distance (single journey, km)` > 70 & `Itinerary Distance (single journey, km)` <= 75 ~ "70-75 km",
            `Itinerary Distance (single journey, km)` > 75 & `Itinerary Distance (single journey, km)` <= 80 ~ "75-80 km",
            `Itinerary Distance (single journey, km)` > 80 & `Itinerary Distance (single journey, km)` <= 85 ~ "80-85 km",
            `Itinerary Distance (single journey, km)` > 85 & `Itinerary Distance (single journey, km)` <= 90 ~ "85-90 km",
            `Itinerary Distance (single journey, km)` > 90 & `Itinerary Distance (single journey, km)` <= 95 ~ "90-95 km",
            `Itinerary Distance (single journey, km)` > 95 & `Itinerary Distance (single journey, km)` <= 100 ~ "95-100 km",
            `Itinerary Distance (single journey, km)` > 100 ~ "Über 100 km"
        ))

    return(car_data)
}

# Process the data
car_occupancy_data <- process_car_occupancy_data(all_data)

# Calculate weighted mean car occupancy for each distance bucket
mean_occupancy_data <- car_occupancy_data %>%
    group_by(DistanceBucket) %>%
    summarize(WeightedMeanOccupancy = weighted.mean(`Itinerary Automobile passengers`, weight, na.rm = TRUE))

# Create the box plot for car occupancy by distance buckets, with weighted means
car_occupancy_distance_plot <- ggplot(car_occupancy_data, aes(
    x = DistanceBucket,
    y = `Itinerary Automobile passengers`
)) +
    geom_boxplot(fill = custom_colors["lightBlue"], alpha = 1) + # Box plot with semi-transparent fill
    geom_line(
        data = mean_occupancy_data, aes(x = DistanceBucket, y = WeightedMeanOccupancy, group = 1),
        color = custom_colors["tangelo"], size = 2
    ) + # Line plot for weighted mean values
    geom_point(
        data = mean_occupancy_data, aes(x = DistanceBucket, y = WeightedMeanOccupancy),
        color = custom_colors["tangelo"], size = 4
    ) + # Points for weighted mean values
    labs(
        title = "Car Occupancy by Distance Bucket",
        x = "Distance Bucket",
        y = "Number of Passengers"
    ) +
    custom_theme + # Use custom theme for consistency
    scale_fill_manual(values = rev(repeat_colors(length(unique(car_occupancy_data$DistanceBucket))))) + # Apply color scheme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

# Print the plot
print(car_occupancy_distance_plot)

# Save the box plot as an image
ggsave("output/car_occupancy_distance_buckets_with_mean_line_weighted.png", car_occupancy_distance_plot, width = 12, height = 8)
