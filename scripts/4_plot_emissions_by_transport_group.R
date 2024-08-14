# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Get the unique transport modes present in the dataset
unique_transport_modes <- unique(all_data$`Means of transportation`)

# Generate transport group colors dynamically based on present groups
transport_colors <- get_transport_group_colors(unique_transport_modes)

# Process emissions data and group by transportation groups with weights
process_emissions_data <- function(data) {
  emissions_data <- data %>%
    mutate(Transport_Group = sapply(`Means of transportation`, group_transport_modes)) %>%
    filter(!is.na(Transport_Group)) %>%  # Remove NA values
    filter(Transport_Group != "Non-Motorized Private Transport") %>%  # Remove non-motorized transport
    group_by(Transport_Group) %>%
    summarize(Total_Emissions = sum(`Emissions (single journey, kg CO2e)` * weight, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Percentage = (Total_Emissions / sum(Total_Emissions)) * 100)  # Calculate percentage
  
  return(emissions_data)
}

# Process the data
emissions_data <- process_emissions_data(all_data)

# Plot the emissions by transport group with styled percentage labels
plot <- ggplot(emissions_data, aes(x = "", y = Total_Emissions, fill = Transport_Group)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") +
  scale_fill_manual(values = transport_colors) +
  theme_minimal() +
  custom_theme + 
  labs(title = "Transport Emissions by Group",
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +  # Hide the legend title
  geom_label_repel(aes(label = ifelse(Percentage > 1, paste0(round(Percentage, 1), "%"), "")),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,  # Hide legend for labels
                   box.padding = 0.3,
                   point.padding = 0.3,
                   segment.color = 'grey50',
                   size = 4)  # Label background color

# Save the plot
ggsave("output/transport_emissions_by_group.png", plot, width = 10, height = 6)

# Print the plot
print(plot)
