source("scripts/load/load_libraries.R")
source("scripts/load/load_plot_styles.R")

# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Threshold under which transport modes should be combined into "Other"
other_threshold <- 1

# Function to process modal split data without weights (using raw distances)
process_modal_split_data_no_weights <- function(data) {
  modal_split_data <- data %>%
    group_by(Location, `Itinerary Means of transportation`) %>%
    summarize(Total_Distance = sum(`Itinerary Distance (single journey, km)`, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    group_by(Location) %>%
    mutate(Total_Distance_All = sum(Total_Distance)) %>%
    mutate(Percentage = (Total_Distance / Total_Distance_All) * 100) %>%
    ungroup() %>%
    select(-Total_Distance_All) %>%
    mutate(`Itinerary Means of transportation` = sapply(`Itinerary Means of transportation`, map_transport_mode)) # Apply custom labels

  # Combine categories with <1% into "Other"
  modal_split_data <- modal_split_data %>%
    mutate(`Itinerary Means of transportation` = ifelse(Percentage < other_threshold, "Other", `Itinerary Means of transportation`)) %>%
    group_by(Location, `Itinerary Means of transportation`) %>%
    summarize(Percentage = sum(Percentage), .groups = "drop") %>%
    ungroup()

  return(modal_split_data)
}

# Function to process modal split data with weights
process_modal_split_data_with_weights <- function(data) {
  modal_split_data <- data %>%
    group_by(Location, `Itinerary Means of transportation`) %>%
    summarize(Total_Distance = sum(weighted_distance, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    group_by(Location) %>%
    mutate(Total_Distance_All = sum(Total_Distance)) %>%
    mutate(Percentage = (Total_Distance / Total_Distance_All) * 100) %>%
    ungroup() %>%
    select(-Total_Distance_All) %>%
    mutate(`Itinerary Means of transportation` = sapply(`Itinerary Means of transportation`, map_transport_mode)) # Apply custom labels

  # Combine categories with <1% into "Other"
  modal_split_data <- modal_split_data %>%
    mutate(`Itinerary Means of transportation` = ifelse(Percentage < other_threshold, "Other", `Itinerary Means of transportation`)) %>%
    group_by(Location, `Itinerary Means of transportation`) %>%
    summarize(Percentage = sum(Percentage), .groups = "drop") %>%
    ungroup()

  return(modal_split_data)
}

# Process modal split data without weights
modal_split_data_no_weights <- process_modal_split_data_no_weights(all_data)

# Process modal split data with weights
modal_split_data_with_weights <- process_modal_split_data_with_weights(all_data)

# Reorder the levels of Means of transportation based on the highest to lowest percentage for both datasets
modal_split_data_no_weights <- modal_split_data_no_weights %>%
  mutate(`Itinerary Means of transportation` = factor(`Itinerary Means of transportation`,
    levels = modal_split_data_no_weights %>%
      group_by(`Itinerary Means of transportation`) %>%
      summarize(Total = sum(Percentage)) %>%
      arrange(desc(Total)) %>%
      pull(`Itinerary Means of transportation`)
  ))

modal_split_data_with_weights <- modal_split_data_with_weights %>%
  mutate(`Itinerary Means of transportation` = factor(`Itinerary Means of transportation`,
    levels = modal_split_data_with_weights %>%
      group_by(`Itinerary Means of transportation`) %>%
      summarize(Total = sum(Percentage)) %>%
      arrange(desc(Total)) %>%
      pull(`Itinerary Means of transportation`)
  ))

# Filter out any labels with Percentage <= 1% for both datasets
modal_split_data_filtered_no_weights <- modal_split_data_no_weights %>%
  filter(Percentage > other_threshold | `Itinerary Means of transportation` == "Other")

modal_split_data_filtered_with_weights <- modal_split_data_with_weights %>%
  filter(Percentage > other_threshold | `Itinerary Means of transportation` == "Other")

# Plot modal split comparison without weights
plot_no_weights <- ggplot(modal_split_data_no_weights, aes(x = "", y = Percentage, fill = `Itinerary Means of transportation`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~Location) +
  theme_minimal() +
  custom_theme +
  scale_fill_manual(values = transport_mode_colors) +
  labs(
    title = "Modal Split Without Weights",
    subtitle = "Share of transport performance",
    x = NULL, y = NULL
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  geom_label_repel(
    data = modal_split_data_filtered_no_weights,
    aes(label = sprintf("%.2f%%", Percentage)),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey50"
  )

# Plot modal split comparison with weights
plot_with_weights <- ggplot(modal_split_data_with_weights, aes(x = "", y = Percentage, fill = `Itinerary Means of transportation`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~Location) +
  theme_minimal() +
  custom_theme +
  scale_fill_manual(values = transport_mode_colors) +
  labs(
    title = "Modal Split With Weights",
    subtitle = "Share of transport performance",
    x = NULL, y = NULL
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  geom_label_repel(
    data = modal_split_data_filtered_with_weights,
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey50"
  )

# Combine the two plots using patchwork syntax
combined_plot <- plot_no_weights + plot_with_weights + plot_layout(ncol = 2)

# Save the combined plot to a file
ggsave("output/combined_modal_split.png", combined_plot, width = 20, height = 6)

# Print the combined plot
print(combined_plot)
