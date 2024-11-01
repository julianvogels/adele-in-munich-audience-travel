# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Define plot variables
column_name <- "Itinerary Means of transportation"
plot_title <- "Modal Split: Durchschnittliche Reiseentfernung"
plot_subtitle <- "Durchschnittlich gereiste Entfernung (einzelne Fahrt)"
plot_x <- "Durchschnittliche Entfernung (km)"
plot_y <- "Verkehrsmittel"

# Function to process modal split data using single journey distance and weights
process_modal_split_data <- function(data) {
    modal_split_data <- data %>%
        group_by(Location, `Itinerary Means of transportation`) %>%
        summarize(
            Total_Distance = sum(`Itinerary Distance (single journey, km)`, na.rm = TRUE),
            Count = n(),
            .groups = "drop"
        ) %>%
        mutate(Average_Distance = Total_Distance / Count) %>%
        ungroup() %>%
        mutate(`Itinerary Means of transportation` = sapply(`Itinerary Means of transportation`, map_transport_mode)) %>% # Apply custom labels
        filter(`Itinerary Means of transportation` != "Andere") %>% # Exclude "Andere"
        arrange(desc(Average_Distance)) # Sort by Average_Distance in descending order

    # Ensure factor levels are set to match the sorted data (largest bars on top)
    modal_split_data[[column_name]] <- factor(modal_split_data[[column_name]],
        levels = modal_split_data[[column_name]][order(modal_split_data$Average_Distance)]
    )

    return(modal_split_data)
}

# Process modal split data
modal_split_data <- process_modal_split_data(all_data)

# Calculate the number of unique participants who provided data
participant_count <- n_distinct(all_data$`Participant Identifier`)

# Plot modal split comparison with horizontal bars for average traveled distance
modal_split_plot <- ggplot(modal_split_data, aes(
    x = Average_Distance,
    y = .data[[column_name]],
    fill = .data[[column_name]]
)) +
    geom_bar(stat = "identity") +
    custom_theme +
    scale_fill_manual(values = rev(repeat_colors(length(unique(modal_split_data[[column_name]]))))) + # Ensure the colors are mapped correctly
    labs(
        title = plot_title,
        subtitle = plot_subtitle,
        x = plot_x,
        y = plot_y,
        caption = paste("n =", participant_count)
    ) +
    theme(
        legend.position = "none",
        plot.subtitle = element_text(hjust = 0.5) # Center the subtitle
    ) +
    geom_text(aes(label = sprintf("%.1f km", Average_Distance)),
        hjust = -0.1,
        size = 3
    )

# Print and save the plot
print(modal_split_plot)
ggsave("output/modal_split_average_traveled_distance.png", modal_split_plot, width = 20, height = 8)
