# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Define plot variables
column_name <- "Itinerary Means of transportation"
plot_title <- "Modal Split: Transport performance"
plot_subtitle <- "Proportion of all kilometres travelled by mode of transport"
plot_x <- "Proportion of all kilometres travelled"
plot_y <- "Mode of transport"

# Function to process modal split data using single journey distance and weights
process_modal_split_data <- function(data) {
    modal_split_data <- data %>%
        group_by(Location, `Itinerary Means of transportation`) %>%
        summarize(Total_Distance = sum(weighted_distance, na.rm = TRUE), .groups = "drop") %>%
        ungroup() %>%
        mutate(`Itinerary Means of transportation` = sapply(`Itinerary Means of transportation`, map_transport_mode)) %>% # Apply custom labels
        group_by(Location) %>%
        mutate(Total_Distance_All = sum(Total_Distance)) %>%
        mutate(Percentage = (Total_Distance / Total_Distance_All) * 100) %>%
        ungroup() %>%
        select(-Total_Distance_All) %>%
        filter(`Itinerary Means of transportation` != "Andere") %>% # Exclude "Andere"
        arrange(Percentage)

    # Ensure factor levels are unique and set them to match the sorted data
    unique_levels <- unique(modal_split_data[[column_name]])
    modal_split_data[[column_name]] <- factor(modal_split_data[[column_name]],
        levels = unique_levels
    )

    return(modal_split_data)
}

# Process modal split data
modal_split_data <- process_modal_split_data(all_data)

# Calculate the number of participants who answered this question
participant_count <- nrow(modal_split_data)

# Plot modal split comparison with horizontal bars
modal_split_plot <- ggplot(modal_split_data, aes(
    x = Percentage,
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
        plot.subtitle = element_text(hjust = 0.5)
    ) + # Center the subtitle
    geom_text(aes(label = sprintf("%.1f%%", Percentage)),
        hjust = -0.1,
        size = 3
    )

# Print and save the plot
print(modal_split_plot)
ggsave("output/modal_split_transport_mode.png", modal_split_plot, width = 20, height = 8)
