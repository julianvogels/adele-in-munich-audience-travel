# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Threshold under which transport modes should be combined into "Other"
other_threshold <- 0.5

# Function to determine the main means of transportation for each participant
determine_main_transport <- function(data) {
  main_transport <- data %>%
    group_by(`Participant Identifier`, `Itinerary Means of transportation`) %>%
    summarize(Total_Distance_Mode = sum(`Itinerary Distance (single journey, km)`, na.rm = TRUE), .groups = "drop") %>%
    group_by(`Participant Identifier`) %>%
    slice_max(Total_Distance_Mode, with_ties = FALSE) %>%
    ungroup()
  return(main_transport)
}

# Function to process modal split data using the main means of transport
process_modal_split_data <- function(data) {
  # Determine the main means of transportation
  main_transport <- determine_main_transport(data)

  # Group the main transport data by Location and Means of transportation
  modal_split_data <- main_transport %>%
    left_join(data %>% select(`Participant Identifier`, Location, weight), by = "Participant Identifier") %>%
    group_by(Location, `Itinerary Means of transportation`) %>%
    summarize(Count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    group_by(Location) %>%
    mutate(Total_Count = sum(Count)) %>%
    mutate(Percentage = (Count / Total_Count) * 100) %>%
    ungroup() %>%
    select(-Total_Count) %>%
    mutate(`Itinerary Means of transportation` = sapply(`Itinerary Means of transportation`, map_transport_mode)) # Apply custom labels

  # Combine categories with <1% into "Andere"
  modal_split_data <- modal_split_data %>%
    mutate(`Itinerary Means of transportation` = ifelse(Percentage < other_threshold, "Andere", `Itinerary Means of transportation`)) %>%
    group_by(Location, `Itinerary Means of transportation`) %>%
    summarize(Percentage = sum(Percentage), .groups = "drop") %>%
    ungroup()

  return(modal_split_data)
}

# Process modal split data
modal_split_data <- process_modal_split_data(all_data)

# Reorder the levels of Means of transportation based on the highest to lowest percentage, with "Andere" at the end
modal_split_data <- modal_split_data %>%
  mutate(`Itinerary Means of transportation` = factor(`Itinerary Means of transportation`,
    levels = c(
      modal_split_data %>%
        group_by(`Itinerary Means of transportation`) %>%
        summarize(Total = sum(Percentage)) %>%
        arrange(desc(Total)) %>%
        pull(`Itinerary Means of transportation`) %>%
        setdiff("Andere"),
      "Andere"
    )
  ))

# Filter out any labels with Percentage <= 1%
modal_split_data_filtered <- modal_split_data %>% filter(Percentage > other_threshold | `Itinerary Means of transportation` == "Andere")

# Plot modal split comparison using facets
plot <- ggplot(modal_split_data, aes(x = "", y = Percentage, fill = `Itinerary Means of transportation`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~Location) +
  theme_minimal() +
  custom_theme +
  scale_fill_manual(values = transport_mode_colors) +
  labs(
    title = "Modal Split",
    subtitle = "Share of main means of transportation",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x = element_blank(), # Remove x-axis text
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(), # Remove grid lines
    plot.title = element_text(hjust = 0.5), # Center title
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) + # Center subtitle and hide legend title
  geom_label_repel(
    data = modal_split_data_filtered, # Use filtered data for labels
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE, # Hide legend for labels
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey50"
  )

# Save and print the plot
ggsave("output/modal_split_main_transport_with_Andere_plot.png", plot, width = 10, height = 6)
print(plot)
