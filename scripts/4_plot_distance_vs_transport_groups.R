# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Function to bucketize total distance into 50 equal buckets with labels
bucketize_distance <- function(total_distance, n_buckets = 50) {
  breaks <- seq(min(total_distance, na.rm = TRUE), max(total_distance, na.rm = TRUE), length.out = n_buckets + 1)
  labels <- paste0(round(breaks[-length(breaks)]), "-", round(breaks[-1]), " km")
  cut(total_distance, breaks = breaks, labels = labels, include.lowest = TRUE)
}

# Function to determine the main means of transportation
determine_main_transport <- function(data) {
  main_transport <- data %>%
    group_by(`Participant Identifier`, `Means of transportation`) %>%
    summarize(Total_Distance_Mode = sum(`Distance (single journey, km)`, na.rm = TRUE), .groups = 'drop') %>%
    group_by(`Participant Identifier`) %>%
    slice_max(Total_Distance_Mode, with_ties = FALSE) %>%
    ungroup()
  return(main_transport)
}

# Function to group transportation modes into "Flights" and "Other"
group_transport_modes <- function(mode) {
  if (mode %in% c("flight")) {
    return("Flights")
  } else {
    return("Other Transport Modes")
  }
}

# Process the data
process_transport_data <- function(data, n_buckets = 50) {
  # Determine the main means of transportation
  main_transport <- determine_main_transport(data)
  
  # Bucketize the total distance
  main_transport <- main_transport %>%
    left_join(data %>% select(`Participant Identifier`, `Total distance (round trip, km)`), by = "Participant Identifier") %>%
    mutate(Distance_Bucket = bucketize_distance(`Total distance (round trip, km)`, n_buckets))
  
  # Group transportation modes into "Flights" and "Other"
  main_transport <- main_transport %>%
    mutate(Transport_Group = sapply(`Means of transportation`, group_transport_modes)) %>%
    filter(!is.na(Transport_Group))  # Exclude any NA groups
  
  # Count the number of participants per bucket and per transportation group
  transport_counts <- main_transport %>%
    group_by(Distance_Bucket, Transport_Group) %>%
    summarize(Count = n(), .groups = 'drop')
  
  return(transport_counts)
}

# Process the data
transport_counts <- process_transport_data(all_data)

# Plot the data comparing Flights vs. Other Transport Modes
plot <- ggplot(transport_counts, aes(x = Distance_Bucket, y = Count, color = Transport_Group, group = Transport_Group)) +
  geom_line(size = 1) +
  labs(title = "Participants by Total Distance and Main Means of Transportation",
       subtitle = "Comparison of Flights vs. Other Transport Modes",
       x = "Total Distance (km)",
       y = "Number of Participants",
       color = "Transport Group") +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle the x-axis labels for better readability

# Save the plot
ggsave("output/participants_flights_vs_other_transport.png", plot, width = 10, height = 6)

# Print the plot
print(plot)
