# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

world_map <- map_data("world")

# Add a column for map point color based on transportation type
all_data <- all_data %>%
    mutate(
        color_stroke = ifelse(`Itinerary Means of transportation` == "flight", custom_colors["tangelo"], custom_colors["lightBlue"]),
        color_fill = ifelse(`Itinerary Means of transportation` == "flight", custom_colors["tangelo400"], custom_colors["lightBlue400"])
    )

# Function to create a plot based on selected map region and plot type
plot_map <- function(map_region = "world", plot_type = "all") {
    # Base plot with coordinates adjusted by map region
    p <- ggplot() +
        coord_fixed(
            xlim = if (map_region == "europe") c(-25, 45) else NULL,
            ylim = if (map_region == "europe") c(35, 70) else NULL
        ) +
        xlab("") +
        ylab("")

    # Add map to base plot
    base_map_messy <- p + geom_polygon(
        data = world_map, aes(x = long, y = lat, group = group),
        colour = custom_colors["gray400"], fill = custom_colors["gray500"]
    )

    # Clean up map appearance
    cleanup <- theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.line = element_line(colour = "transparent"), legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank()
    )

    base_map <- base_map_messy + cleanup

    # Calculate MapPointSize based on frequency of identical data points
    all_data <- all_data %>%
        group_by(`Itinerary Origin latitude`, `Itinerary Origin longitude`) %>%
        mutate(MapPointSize = n()) %>%
        ungroup()

    # Switch case to determine plot type
    map_data_sized <- switch(plot_type,
        "all" = base_map +
            geom_point(
                data = all_data,
                aes(
                    x = `Itinerary Origin longitude`,
                    y = `Itinerary Origin latitude`,
                    size = MapPointSize,
                    fill = color_fill, # Define fill color from color_fill column
                    colour = color_stroke # Define stroke color from color_stroke column
                ),
                pch = 21, alpha = I(0.7)
            ) +
            scale_fill_identity() +
            scale_color_identity(),
        "flights_only" = base_map +
            geom_point(
                data = all_data %>% filter(
                    `Itinerary Means of transportation` == "flight",
                    !(`Itinerary index` > 1 &
                        `Participant Identifier` %in%
                            all_data$`Participant Identifier`[all_data$`Itinerary index` == (`Itinerary index` - 1) &
                                all_data$`Itinerary Means of transportation` == "flight"])
                ),
                aes(
                    x = `Itinerary Origin longitude`,
                    y = `Itinerary Origin latitude`,
                    size = MapPointSize,
                    fill = color_fill, # Define fill color from color_fill column
                    colour = color_stroke # Define stroke color from color_stroke column
                ),
                pch = 21, alpha = I(0.7)
            ) +
            scale_fill_identity() +
            scale_color_identity(),
        "no_flights" = base_map +
            geom_point(
                data = all_data %>% filter(
                    `Itinerary Means of transportation` != "flight",
                    `Itinerary index` == 1
                ),
                aes(
                    x = `Itinerary Origin longitude`,
                    y = `Itinerary Origin latitude`,
                    size = MapPointSize,
                    fill = color_fill, # Define fill color from color_fill column
                    colour = color_stroke # Define stroke color from color_stroke column
                ),
                pch = 21, alpha = I(0.7)
            ) +
            scale_fill_identity() +
            scale_color_identity(),
        stop("Invalid plot type. Choose from 'all', 'flights_only', 'no_flights'.")
    )

    # Display the plot
    print(map_data_sized)

    # Save the plot as an image
    ggsave(paste0("output/map_", map_region, "_", plot_type, ".png"), map_data_sized, width = 12, height = 8)
}

# Call the function with the desired map region and plot type
plot_map("world", "all") # Options: "world" or "europe" for map_region; "all", "flights_only", "no_flights" for plot_type
plot_map("world", "no_flights")
plot_map("world", "flights_only")
plot_map("europe", "all")
plot_map("europe", "no_flights")
plot_map("europe", "flights_only")
