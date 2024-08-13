# European Climate Pact color scheme
repeat_colors <- function(n) {
  colors <- c("#4463E0", "#E25449", "#4FAF4C", "#B34922", "#377370", "#A6BDF5", 
              "#EAA292", "#99E492", "#F1A23A", "#CFA8F9", "#71C3AF", "#D7E4FA",
              "#EDD7D4", "#D5FDD5", "#F6C87F", "#E8D7FC", "#B6F4E0")
  return(unname(rep(colors, length.out = n)))
}

# Define a color mapping for all possible transportation modes
transport_mode_colors <- c(
  "automobile" = "#4463E0",
  "flight" = "#E25449",
  "trainLongDistance" = "#4FAF4C",
  "trainRegional" = "#B34922",
  "trainUrban" = "#377370",
  "busLocal" = "#A6BDF5",
  "busLongDistance" = "#EAA292",
  "cycling" = "#99E492",
  "taxi" = "#F1A23A",
  "tram" = "#CFA8F9",
  "ferry" = "#71C3AF",
  "pedelec" = "#D7E4FA",
  "walking" = "#EDD7D4",
  "motorScooterTwoStroke" = "#D5FDD5",
  "motorScooterElectric" = "#F6C87F",
  "motorbike" = "#E8D7FC",
  "kickScooterElectric" = "#B6F4E0" 
)

# Function to get colors for transportation modes present in the dataset
get_transport_colors <- function(transport_modes) {
  # Filter the colors to only those present in the dataset
  filtered_colors <- transport_mode_colors[transport_modes]

  # Return unnamed vector of colors
  return(unname(filtered_colors))
}

# Define a color mapping for the transportation groups
transport_group_colors <- c(
  "Motorized Private Transport" = "#4463E0",
  "Public Transport" = "#4FAF4C",
  "Non-Motorized Private Transport" = "#F1A23A",
  "Flights" = "#E25449"
)

# Function to group transportation modes into categories
group_transport_modes <- function(mode) {
  if (mode %in% c("automobile", "motorbike", "motorScooterTwoStroke", "motorScooterElectric", "taxi")) {
    return("Motorized Private Transport")
  } else if (mode %in% c("busLongDistance", "busLocal", "trainLongDistance", "trainRegional", "trainUrban", "tram", "ferry")) {
    return("Public Transport")
  } else if (mode %in% c("pedelec", "walking", "cycling")) {
    return("Non-Motorized Private Transport")
  } else if (mode %in% c("flight")) {
    return("Flights")
  } else {
    return(NA)
  }
}

# Function to get colors for transportation groups present in the dataset
get_transport_group_colors <- function(transport_modes) {
  # Group the transportation modes into categories
  transport_groups <- unique(sapply(transport_modes, group_transport_modes))
  
  # Filter the colors to only those groups present in the dataset
  filtered_colors <- transport_group_colors[transport_groups]
  
  # Return unnamed vector of colors
  return(unname(filtered_colors))
}

# Define a custom font family
custom_font_family <- "Open Sans"

# Set a custom theme for all plots
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = custom_font_family),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "right",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
)