# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to process modal split data using single journey distance and weights
process_modal_split_data <- function(data) {
    # Filter out rows with missing distance and valid transportation modes
    modal_split_data <- data %>%
        filter(!is.na(`Itinerary Distance (single journey, km)`), !is.na(`Itinerary Means of transportation`))

    # Create distance buckets: 0-5 km, 5-10 km, ..., 195-300 km, and >200 km
    modal_split_data <- modal_split_data %>%
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
            `Itinerary Distance (single journey, km)` > 100 & `Itinerary Distance (single journey, km)` <= 105 ~ "100-105 km",
            `Itinerary Distance (single journey, km)` > 105 & `Itinerary Distance (single journey, km)` <= 110 ~ "105-110 km",
            `Itinerary Distance (single journey, km)` > 110 & `Itinerary Distance (single journey, km)` <= 115 ~ "110-115 km",
            `Itinerary Distance (single journey, km)` > 115 & `Itinerary Distance (single journey, km)` <= 120 ~ "115-120 km",
            `Itinerary Distance (single journey, km)` > 120 & `Itinerary Distance (single journey, km)` <= 125 ~ "120-125 km",
            `Itinerary Distance (single journey, km)` > 125 & `Itinerary Distance (single journey, km)` <= 130 ~ "125-130 km",
            `Itinerary Distance (single journey, km)` > 130 & `Itinerary Distance (single journey, km)` <= 135 ~ "130-135 km",
            `Itinerary Distance (single journey, km)` > 135 & `Itinerary Distance (single journey, km)` <= 140 ~ "135-140 km",
            `Itinerary Distance (single journey, km)` > 140 & `Itinerary Distance (single journey, km)` <= 145 ~ "140-145 km",
            `Itinerary Distance (single journey, km)` > 145 & `Itinerary Distance (single journey, km)` <= 150 ~ "145-150 km",
            `Itinerary Distance (single journey, km)` > 150 & `Itinerary Distance (single journey, km)` <= 155 ~ "150-155 km",
            `Itinerary Distance (single journey, km)` > 155 & `Itinerary Distance (single journey, km)` <= 160 ~ "155-160 km",
            `Itinerary Distance (single journey, km)` > 160 & `Itinerary Distance (single journey, km)` <= 165 ~ "160-165 km",
            `Itinerary Distance (single journey, km)` > 165 & `Itinerary Distance (single journey, km)` <= 170 ~ "165-170 km",
            `Itinerary Distance (single journey, km)` > 170 & `Itinerary Distance (single journey, km)` <= 175 ~ "170-175 km",
            `Itinerary Distance (single journey, km)` > 175 & `Itinerary Distance (single journey, km)` <= 180 ~ "175-180 km",
            `Itinerary Distance (single journey, km)` > 180 & `Itinerary Distance (single journey, km)` <= 185 ~ "180-185 km",
            `Itinerary Distance (single journey, km)` > 185 & `Itinerary Distance (single journey, km)` <= 190 ~ "185-190 km",
            `Itinerary Distance (single journey, km)` > 190 & `Itinerary Distance (single journey, km)` <= 195 ~ "190-195 km",
            `Itinerary Distance (single journey, km)` > 195 & `Itinerary Distance (single journey, km)` <= 200 ~ "195-200 km",
            `Itinerary Distance (single journey, km)` > 300 ~ ">200 km"
        ))


    # Group by distance bucket and transportation mode, then calculate the weighted percentage
    modal_split_data <- modal_split_data %>%
        group_by(DistanceBucket, `Itinerary Means of transportation`) %>%
        summarize(Total_Weighted = sum(weight, na.rm = TRUE), .groups = "drop") %>%
        ungroup() %>%
        group_by(DistanceBucket) %>%
        mutate(Total_All = sum(Total_Weighted)) %>%
        mutate(Percentage = (Total_Weighted / Total_All) * 100) %>%
        ungroup() %>%
        select(-Total_All) %>%
        mutate(`Itinerary Means of transportation` = sapply(`Itinerary Means of transportation`, map_transport_mode)) # Apply custom German labels

    return(modal_split_data)
}

# Process the data for modal split by distance buckets
modal_split_data <- process_modal_split_data(all_data)

# Ensure the transportation modes are ordered by total percentage
modal_split_data <- modal_split_data %>%
    mutate(`Itinerary Means of transportation` = factor(`Itinerary Means of transportation`,
        levels = modal_split_data %>%
            group_by(`Itinerary Means of transportation`) %>%
            summarize(Total = sum(Percentage)) %>%
            arrange(desc(Total)) %>%
            pull(`Itinerary Means of transportation`)
    ))

# Correct the order of distance buckets to start from the smallest distance
modal_split_data <- modal_split_data %>%
    mutate(DistanceBucket = factor(DistanceBucket,
        levels = c(
            "0-5 km", "5-10 km", "10-15 km", "15-20 km", "20-25 km", "25-30 km",
            "30-35 km", "35-40 km", "40-45 km", "45-50 km", "50-55 km", "55-60 km",
            "60-65 km", "65-70 km", "70-75 km", "75-80 km", "80-85 km", "85-90 km",
            "90-95 km", "95-100 km", "100-105 km", "105-110 km", "110-115 km",
            "115-120 km", "120-125 km", "125-130 km", "130-135 km", "135-140 km",
            "140-145 km", "145-150 km", "150-155 km", "155-160 km", "160-165 km",
            "165-170 km", "170-175 km", "175-180 km", "180-185 km", "185-190 km",
            "190-195 km", "195-200 km", ">200 km"
        )
    ))

# Plot the transportation usage by distance buckets with multiple lines for each mode of transportation
modal_split_plot <- ggplot(modal_split_data, aes(
    x = DistanceBucket,
    y = pmax(Percentage, 0),
    color = `Itinerary Means of transportation`,
    group = `Itinerary Means of transportation`
)) +
    geom_line(size = 1.5, alpha = 0.2) + # Line for each transportation mode with 20% opacity
    geom_point(size = 3, alpha = 0.2) + # Points for each transportation mode with 20% opacity
    geom_smooth(method = "loess", span = 0.6, se = FALSE, size = 1.5) + # Smooth lines for each transportation mode
    scale_color_manual(values = transport_mode_colors) + # Custom color palette for transportation modes
    labs(
        title = "Nutzung von Verkehrsmitteln nach Entfernung",
        x = "Entfernungsbereich",
        y = "Prozentuale Nutzung",
        color = "Verkehrsmittel"
    ) +
    custom_theme + # Apply consistent theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

# Print the plot
print(modal_split_plot)

# Save the plot as an image
ggsave("output/modal_split_distance_buckets_de_smooth_200km.png", modal_split_plot, width = 12, height = 8)



# Plot the transportation usage by distance buckets with multiple lines for each mode of transportation
modal_split_plot <- ggplot(modal_split_data, aes(
    x = DistanceBucket,
    y = pmax(Percentage, 0),
    color = `Itinerary Means of transportation`,
    group = `Itinerary Means of transportation`
)) +
    geom_line(size = 1.5, alpha = 1) + # Line for each transportation mode with 20% opacity
    geom_point(size = 3, alpha = 1) + # Points for each transportation mode with 20% opacity
    scale_color_manual(values = transport_mode_colors) + # Custom color palette for transportation modes
    labs(
        title = "Nutzung von Verkehrsmitteln nach Entfernung",
        x = "Entfernungsbereich",
        y = "Prozentuale Nutzung",
        color = "Verkehrsmittel"
    ) +
    custom_theme + # Apply consistent theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

# Print the plot
print(modal_split_plot)

# Save the plot as an image
ggsave("output/modal_split_distance_buckets_de_200km.png", modal_split_plot, width = 12, height = 8)
