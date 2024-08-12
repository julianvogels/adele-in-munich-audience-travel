# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Function to process modal split data using single journey distance
process_modal_split_data <- function(data) {
  modal_split_data <- data %>%
    group_by(Location, `Means of transportation`) %>%
    summarize(Total_Distance = sum(`Distance (single journey, km)`, na.rm = TRUE), .groups = 'drop') %>%
    ungroup() %>%
    group_by(Location) %>%
    mutate(Total_Distance_All = sum(Total_Distance)) %>%
    mutate(Percentage = (Total_Distance / Total_Distance_All) * 100) %>%
    ungroup() %>%
    select(-Total_Distance_All) %>%
    mutate(`Means of transportation` = sapply(`Means of transportation`, map_transport_mode)) # Apply custom labels
  
  return(modal_split_data)
}

# Process modal split data
modal_split_data <- process_modal_split_data(all_data)

# Filter out any labels with Percentage <= 1%
modal_split_data_filtered <- modal_split_data %>% filter(Percentage > 1)

# Plot modal split comparison using facets
plot <- ggplot(modal_split_data, aes(x = "", y = Percentage, fill = `Means of transportation`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  facet_wrap(~Location) +
  theme_minimal() +
  labs(title = "Modal Split",
       subtitle = "Share of transport performance",
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks = element_blank(),   # Remove axis ticks
        panel.grid = element_blank(),   # Remove grid lines
        plot.title = element_text(hjust = 0.5),  # Center title
        plot.subtitle = element_text(hjust = 0.5)) +  # Center subtitle
  geom_label_repel(data = modal_split_data_filtered,  # Use filtered data for labels
                   aes(label = sprintf("%.1f%%", Percentage)),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,  # Hide legend for labels
                   box.padding = 0.3,
                   point.padding = 0.3,
                   segment.color = 'grey50') 

print(plot)
