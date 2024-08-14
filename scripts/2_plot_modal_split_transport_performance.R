# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Get the unique transport modes present in the dataset
unique_transport_modes <- unique(all_data$`Means of transportation`)

# Generate transport group colors dynamically based on present groups
transport_colors <- get_transport_colors(c(unique_transport_modes, "Other"))  # Include "Other" in the colors

# Function to process modal split data using single journey distance and weights
process_modal_split_data <- function(data) {
  modal_split_data <- data %>%
    group_by(Location, `Means of transportation`) %>%
    summarize(Total_Distance = sum(weighted_distance, na.rm = TRUE), .groups = 'drop') %>%
    ungroup() %>%
    group_by(Location) %>%
    mutate(Total_Distance_All = sum(Total_Distance)) %>%
    mutate(Percentage = (Total_Distance / Total_Distance_All) * 100) %>%
    ungroup() %>%
    select(-Total_Distance_All) %>%
    mutate(`Means of transportation` = sapply(`Means of transportation`, map_transport_mode)) # Apply custom labels
  
  # Combine categories with <1% into "Other"
  modal_split_data <- modal_split_data %>%
    mutate(`Means of transportation` = ifelse(Percentage < 1, "Other", `Means of transportation`)) %>%
    group_by(Location, `Means of transportation`) %>%
    summarize(Percentage = sum(Percentage), .groups = 'drop') %>%
    ungroup()

  return(modal_split_data)
}

# Process modal split data
modal_split_data <- process_modal_split_data(all_data)

# Reorder the levels of Means of transportation based on the highest to lowest percentage
modal_split_data <- modal_split_data %>%
  mutate(`Means of transportation` = factor(`Means of transportation`, 
                                            levels = modal_split_data %>%
                                              group_by(`Means of transportation`) %>%
                                              summarize(Total = sum(Percentage)) %>%
                                              arrange(desc(Total)) %>%
                                              pull(`Means of transportation`)))

# Filter out any labels with Percentage <= 1%
modal_split_data_filtered <- modal_split_data %>% filter(Percentage > 1 | `Means of transportation` == "Other")

# Plot modal split comparison using facets
plot <- ggplot(modal_split_data, aes(x = "", y = Percentage, fill = `Means of transportation`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~Location) +
  theme_minimal() +
  custom_theme +
  scale_fill_manual(values = transport_colors) +
  labs(title = "Modal Split",
       subtitle = "Share of transport performance",
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks = element_blank(),   # Remove axis ticks
        panel.grid = element_blank(),   # Remove grid lines
        plot.title = element_text(hjust = 0.5),  # Center title
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank()) +  # Center subtitle and hide legend title
  geom_label_repel(data = modal_split_data_filtered,  # Use filtered data for labels
                   aes(label = sprintf("%.1f%%", Percentage)),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,  # Hide legend for labels
                   box.padding = 0.3,
                   point.padding = 0.3,
                   segment.color = 'grey50')

# Save and print the plot
ggsave("output/modal_split_transport_performance_with_other.png", plot, width = 10, height = 6)
print(plot)
