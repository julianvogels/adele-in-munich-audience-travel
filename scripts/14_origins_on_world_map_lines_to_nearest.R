library(dplyr)
library(ggplot2)
library(geosphere)

# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Ensure latitude and longitude columns are numeric and filter for flights
all_data <- all_data %>%
    mutate(
        `Itinerary Origin latitude` = as.numeric(`Itinerary Origin latitude`),
        `Itinerary Origin longitude` = as.numeric(`Itinerary Origin longitude`)
    ) %>%
    filter(`Itinerary Means of transportation` == "flight")

# Filter out changeovers (exclude itineraries with an `Itinerary index` > 1 if there is a previous leg)
all_data_filtered <- all_data %>%
    filter(
        !(`Itinerary index` > 1 &
            `Participant Identifier` %in%
                all_data$`Participant Identifier`[all_data$`Itinerary index` == (`Itinerary index` - 1) &
                    all_data$`Itinerary Means of transportation` == "flight"])
    )

# Calculate the MapLineSize based on the frequency of identical data points
all_data_filtered <- all_data_filtered %>%
    group_by(`Itinerary Origin latitude`, `Itinerary Origin longitude`) %>%
    mutate(MapLineSize = n()) %>%
    ungroup()

# Define the tour locations data frame
tour_locations <- data.frame(
    City = c("London", "Berlin", "Paris", "Amsterdam", "Rome", "Madrid", "Vienna", "Prague", "Copenhagen", "Munich"),
    Latitude = c(51.5074, 52.5200, 48.8566, 52.3676, 41.9028, 40.4168, 48.2082, 50.0755, 55.6761, 48.1351),
    Longitude = c(-0.1278, 13.4050, 2.3522, 4.9041, 12.4964, -3.7038, 16.3738, 14.4378, 12.5683, 11.5820)
)

# Find the closest city for each point and calculate the distance to it
all_data_filtered <- all_data_filtered %>%
    rowwise() %>%
    mutate(
        Closest_City = tour_locations$City[which.min(
            distHaversine(
                c(`Itinerary Origin longitude`, `Itinerary Origin latitude`),
                tour_locations[, c("Longitude", "Latitude")]
            )
        )],
        Closest_City_Longitude = tour_locations$Longitude[which.min(
            distHaversine(
                c(`Itinerary Origin longitude`, `Itinerary Origin latitude`),
                tour_locations[, c("Longitude", "Latitude")]
            )
        )],
        Closest_City_Latitude = tour_locations$Latitude[which.min(
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

# Load map data
world_map <- map_data("world")
plotted_map <- world_map

# Create a base plot with ggplot2
p <- ggplot() +
    coord_fixed() +
    xlab("") +
    ylab("")

# Create a base plot with ggplot2
p <- ggplot() +
    coord_fixed(xlim = c(-25, 45), ylim = c(35, 70)) + # Zoom in on Europe
    xlab("") +
    ylab("")

# Add the map to the base plot
base_map_messy <- p + geom_polygon(
    data = plotted_map, aes(x = long, y = lat, group = group),
    colour = custom_colors["lightBlue100"], fill = custom_colors["lightBlue200"]
)

# Strip the map down so it looks clean
cleanup <-
    theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.line = element_line(colour = "transparent"), legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank()
    )

base_map <- base_map_messy + cleanup

# Add lines for flights to the nearest cities with thickness based on MapLineSize
map_data_sized <- base_map +
    geom_segment(
        data = all_data_filtered,
        aes(
            x = `Itinerary Origin longitude`,
            y = `Itinerary Origin latitude`,
            xend = Closest_City_Longitude,
            yend = Closest_City_Latitude,
            size = MapLineSize # Thickness based on the number of flights from that origin
        ),
        colour = custom_colors["tangelo"],
        alpha = 0.15
    ) +
    geom_point(
        data = tour_locations,
        aes(x = Longitude, y = Latitude),
        colour = custom_colors["ripeMango"],
        fill = custom_colors["ripeMango400"],
        pch = 21,
        size = 5
    )

print(map_data_sized)

# Save the plot as an image
ggsave("output/flights_with_lines_to_nearest_cities.png", map_data_sized, width = 12, height = 8)
