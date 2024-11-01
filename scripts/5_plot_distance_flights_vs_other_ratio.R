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
    group_by(`Participant Identifier`, `Itinerary Means of transportation`) %>%
    summarize(Total_Distance_Mode = sum(`Itinerary Distance (single journey, km)`, na.rm = TRUE), .groups = 'drop') %>%
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
  
  # Bucketize the single journey distance
  main_transport <- main_transport %>%
    left_join(data %>% select(`Participant Identifier`, `Itinerary Distance (single journey, km)`, weight), by = "Participant Identifier") %>%
    mutate(Distance_Bucket = bucketize_distance(`Itinerary Distance (single journey, km)`, n_buckets))
  
  # Filter out any NAs in Distance_Bucket and ensure only main transport modes are kept
  main_transport <- main_transport %>%
    filter(!is.na(Distance_Bucket)) %>%
    mutate(Transport_Group = sapply(`Itinerary Means of transportation`, group_transport_modes)) %>%
    filter(!is.na(Transport_Group))  # Exclude any NA groups
  
  # Create a full combination of Distance_Bucket and Transport_Group to ensure all combinations are present
  full_combinations <- expand.grid(Distance_Bucket = unique(main_transport$Distance_Bucket),
                                   Transport_Group = c("Flights", "Other Transport Modes"))
  
  # Count the number of participants per bucket and per transportation group, and fill missing combinations with zero
  transport_counts <- main_transport %>%
    group_by(Distance_Bucket, Transport_Group) %>%
    summarize(Count = n(), .groups = 'drop') %>%
    right_join(full_combinations, by = c("Distance_Bucket", "Transport_Group")) %>%
    replace_na(list(Count = 0))
  
  return(transport_counts)
}

# Process the data
transport_counts <- process_transport_data(all_data)

# Find the bucket where flights start to exceed other transport modes
flight_vs_others <- transport_counts %>%
  spread(key = Transport_Group, value = Count, fill = 0) %>%
  mutate(Difference = Flights - `Other Transport Modes`) %>%
  mutate(Crossover = ifelse(lag(Difference, default = -Inf) < 0 & Difference >= 0, TRUE, FALSE))

crossover_bucket <- flight_vs_others %>%
  filter(Crossover == TRUE) %>%
  slice(1)

if (nrow(crossover_bucket) > 0) {
  crossover_distance <- as.character(crossover_bucket$Distance_Bucket)
  print(paste("The distance at which flights start to exceed other modes is approximately:", crossover_distance))
} else {
  print("No clear crossover point found where flights exceed other modes.")
}

# Plot the data comparing Flights vs. Other Transport Modes
plot <- ggplot(transport_counts, aes(x = Distance_Bucket, y = Count, color = Transport_Group, group = Transport_Group)) +
  geom_line(size = 1) +
  labs(title = "Participants by Total Distance and Main Means of Transportation",
       subtitle = "Comparison of Flights vs. Other Transport Modes",
       x = "Total Distance (km)",
       y = "Number of Participants",
       color = "Transport Group") +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.7, 0.7),  # Position the legend inside the plot (top-right corner)
        legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Add background to the legend
        legend.key = element_blank())  # Remove legend key box

# Add vertical line and annotation only if a crossover is found and within the plot range
if (nrow(crossover_bucket) > 0 && !is.na(crossover_distance)) {
  plot <- plot + geom_vline(xintercept = as.numeric(as.character(crossover_bucket$Distance_Bucket)), linetype = "dashed", color = "red") +
    annotate("text", x = as.numeric(as.character(crossover_bucket$Distance_Bucket)) + 0.5, y = max(transport_counts$Count, na.rm = TRUE) * 0.8,
             label = paste("Crossover at", crossover_distance), color = "red", angle = 90, vjust = -0.5)
}

# Save the plot
ggsave("output/participants_flights_vs_other_transport_crossover.png", plot, width = 10, height = 6)

# Print the plot
print(plot)
