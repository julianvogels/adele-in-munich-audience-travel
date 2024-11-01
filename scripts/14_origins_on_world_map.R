# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

world_map <- map_data("world")

plotted_map <- world_map

# Creat a base plot with gpplot2

# Plot on world map
p <- ggplot() +
    coord_fixed() +
    xlab("") +
    ylab("")

# Plot on map of europe
p <- ggplot() +
    coord_fixed(xlim = c(-25, 45), ylim = c(35, 70)) + # Zoom in on Europe
    xlab("") +
    ylab("")

# Add map to base plot
base_map_messy <- p + geom_polygon(
    data = plotted_map, aes(x = long, y = lat, group = group),
    colour = custom_colors["lightBlue100"], fill = custom_colors["lightBlue200"]
)

# Strip the map down so it looks super clean (and beautiful!)
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

# Calculate the MapPointSize based on the frequency of identical data points
all_data <- all_data %>%
    group_by(`Itinerary Origin latitude`, `Itinerary Origin longitude`) %>%
    mutate(MapPointSize = n()) %>%
    ungroup()

# Add data points to map with value affecting size

# Plot flights only
map_data_sized <-
    base_map +
    geom_point(
        data = all_data %>% filter(
            `Itinerary Means of transportation` == "flight",
            !(`Itinerary index` > 1 &
                `Participant Identifier` %in%
                    all_data$`Participant Identifier`[all_data$`Itinerary index` == (`Itinerary index` - 1) &
                        all_data$`Itinerary Means of transportation` == "flight"])
        ),
        aes(x = `Itinerary Origin longitude`, y = `Itinerary Origin latitude`, size = MapPointSize), colour = custom_colors["tangelo"],
        fill = custom_colors["tangelo400"], pch = 21, alpha = I(0.7)
    )

# Plot everything except flights
# map_data_sized <-
#     base_map +
#     geom_point(
#         data = all_data %>% filter(
#             `Itinerary Means of transportation` != "flight",
#             `Itinerary index` == 1
#         ),
#         aes(x = `Itinerary Origin longitude`, y = `Itinerary Origin latitude`, size = MapPointSize), colour = custom_colors["tangelo"],
#         fill = custom_colors["tangelo400"], pch = 21, alpha = I(0.7)
#     )

print(map_data_sized)

# Save the box plot as an image
ggsave("output/flights_on_world_map.png", map_data_sized, width = 12, height = 8)
