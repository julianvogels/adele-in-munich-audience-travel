library(dplyr)
library(ggplot2)
library(geosphere)

# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Filter to include only the origin flights (remove changeovers)
all_data_filtered <- all_data %>%
    filter(
        `Itinerary Means of transportation` == "flight",
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

# Define the coordinates for Munich
munich_coords <- data.frame(
    longitude = 11.5820,
    latitude = 48.1351
)

# Load map data
world_map <- map_data("world")
plotted_map <- world_map

# Create a base plot with ggplot2: World
p <- ggplot() +
    coord_fixed() +
    xlab("") +
    ylab("")

# Create a base plot with ggplot2: Europs
p <- ggplot() +
    coord_fixed(xlim = c(-25, 45), ylim = c(35, 70)) + # Zoom in on Europe
    xlab("") +
    ylab("")

# Add the map to the base plot
base_map_messy <- p + geom_polygon(
    data = plotted_map, aes(x = long, y = lat, group = group),
    colour = custom_colors["gray400"], fill = custom_colors["gray500"]
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

# Add lines for flights to Munich with thickness based on MapLineSize
map_data_sized <- base_map +
    geom_segment(
        data = all_data_filtered,
        aes(
            x = `Itinerary Origin longitude`,
            y = `Itinerary Origin latitude`,
            xend = munich_coords$longitude,
            yend = munich_coords$latitude,
            size = MapLineSize * 0.5
        ),
        colour = custom_colors["tangelo"],
        alpha = 0.15
    ) +
    geom_point(
        data = munich_coords,
        aes(x = longitude, y = latitude),
        colour = custom_colors["ripeMango"],
        fill = custom_colors["ripeMango400"],
        pch = 21,
        size = 5
    )

print(map_data_sized)

# Save the plot as an image
ggsave("output/flights_with_lines_to_munich.png", map_data_sized, width = 12, height = 8)
