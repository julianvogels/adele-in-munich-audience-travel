# Function to process allocation data
process_allocation_data <- function(data) {
  allocation_data <- data %>%
    filter(`Itinerary index` == 1) %>%
    filter(!is.na(Allocation)) %>%
    group_by(Location, Allocation) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(Location) %>%
    mutate(
      Percentallocation = Count / sum(Count) * 100,
      TotalCount = sum(Count)
    ) %>%
    ungroup()

  # Correct factor level order to match the desired bar order and color mapping
  allocation_data$Allocation <- factor(allocation_data$Allocation,
    levels = c(
      "I travelled here especially for this event.",
      "This event is one of several reasons for my journey.",
      "I wanted to travel here anyway, before I decided to attend the event."
    )
  )

  return(allocation_data)
}

# Process allocation data
allocation_data <- process_allocation_data(all_data)

# Plot allocation distribution using facets
allocation_plot <- ggplot(allocation_data, aes(
  x = Percentallocation,
  y = Allocation,
  fill = Allocation
)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Location, scales = "free_y") +
  custom_theme + 
  scale_fill_manual(values = repeat_colors(3)) +  # Ensure the colors are mapped in the correct order
  labs(
    title = "Allocation of flights",
    x = "",
    y = "",
    caption = paste("n =", sum(allocation_data$TotalCount))
  ) + 
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%.1f%%", Percentallocation)),
    hjust = -0.1,
    size = 3
  )

print(allocation_plot)
