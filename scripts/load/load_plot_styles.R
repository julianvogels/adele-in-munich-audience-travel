# Load custom colors for transport modes and groups
custom_colors <- c(
  tangelo = "#FF4400",
  lightBlue = "#6077FA",
  ripeMango = "#FDA333",
  capri = "#00B9FF",
  juneBud = "#B0D45D",
  tangelo100 = "#FD6833",
  lightBlue100 = "#29C4FF",
  ripeMango100 = "#FDC833",
  capri100 = "#29C4FF",
  juneBud100 = "#C0DD7D",
  tangelo200 = "#FB8B66",
  lightBlue200 = "#52CEFF",
  ripeMango200 = "#FDC855",
  capri200 = "#52CEFF",
  juneBud200 = "#D0E59E",
  tangelo300 = "#FAAF98",
  lightBlue300 = "#7CD9FF",
  ripeMango300 = "#FDD075",
  capri300 = "#7CD9FF",
  juneBud300 = "#DFEEBE",
  tangelo400 = "#F8D3CB",
  lightBlue400 = "#A5E3FF",
  ripeMango400 = "#FDD895",
  capri400 = "#A5E3FF",
  juneBud400 = "#EFF6DF",
  tangelo500 = "#F8E5E5",
  lightBlue500 = "#CEEDFF",
  ripeMango500 = "#F8E5E5",
  capri500 = "#CEEDFF",
  juneBud500 = "#F3F8EE",
  gray500 = "#E4E4EB"
)

# Crowd Impact color scheme
repeat_colors <- function(n) {
  return(unname(rep(c(
    "#FF4400", "#6077FA", "#FDA333", "#00B9FF", "#B0D45D", "#FD6833", "#29C4FF",
    "#FDC833", "#29C4FF", "#C0DD7D", "#FB8B66", "#52CEFF", "#FDC855", "#52CEFF",
    "#D0E59E", "#FAAF98", "#7CD9FF", "#FDD075", "#7CD9FF", "#DFEEBE", "#F8D3CB",
    "#A5E3FF", "#FDD895", "#A5E3FF", "#EFF6DF", "#F8E5E5", "#CEEDFF", "#F8E5E5",
    "#CEEDFF", "#F3F8EE", "#E4E4EB"
  ), length.out = n)))
}

# Transport mode and group color mappings
transport_mode_colors <- c(
  "automobile" = "#6077FA", # lightBlue
  "flight" = "#FF4400", # tangelo
  "trainLongDistance" = "#FDA333", # ripeMango
  "trainRegional" = "#00B9FF", # capri
  "trainUrban" = "#B0D45D", # juneBud
  "busLocal" = "#FD6833", # tangelo100
  "busLongDistance" = "#29C4FF", # lightBlue100
  "cycling" = "#C0DD7D", # juneBud100
  "taxi" = "#FDC833", # ripeMango100
  "tram" = "#29C4FF", # capri100
  "ferry" = "#FB8B66", # tangelo200
  "pedelec" = "#52CEFF", # lightBlue200
  "walking" = "#D0E59E", # juneBud200,
  "Car" = "#6077FA", # lightBlue for display name
  "Flight" = "#FF4400", # tangelo for display name
  "Long Distance Train" = "#FDA333", # ripeMango for display name
  "Regional Train" = "#00B9FF", # capri for display name
  "Urban Train" = "#B0D45D", # juneBud for display name
  "Public Transport Bus Local" = "#FD6833", # tangelo100 for display name
  "Public Transport Bus" = "#29C4FF", # lightBlue100 for display name
  "Cycling" = "#C0DD7D", # juneBud100 for display name
  "Taxi" = "#FDC833", # ripeMango100 for display name
  "Tram" = "#29C4FF", # capri100 for display name
  "Ferry" = "#FB8B66", # tangelo200 for display name
  "Pedelec" = "#52CEFF", # lightBlue200 for display name
  "Walking" = "#D0E59E", # juneBud200 for display name
  "Other" = "#E4E4EB", # Gray for 'Other'
  "Auto" = "#6077FA", # lightBlue for display name
  "Flug" = "#FF4400", # tangelo for display name
  "Fernzug" = "#FDA333", # ripeMango for display name
  "Regionalzug" = "#00B9FF", # capri for display name
  "S-/U-Bahn" = "#B0D45D", # juneBud for display name
  "ÖPNV-Linienbus" = "#FD6833", # tangelo100 for display name
  "Fernbus" = "#29C4FF", # lightBlue100 for display name
  "Fahrrad" = "#C0DD7D", # juneBud100 for display name
  # "Taxi" = "#FDC833", # ripeMango100 for display name #duplicate because same in German
  "Straßenbahn" = "#29C4FF", # capri100 for display name
  "Fähre" = "#FB8B66", # tangelo200 for display name
  "E-Bike" = "#52CEFF", # lightBlue200 for display name
  "Zu Fuß" = "#D0E59E", # juneBud200 for display name
  "Andere" = "#E4E4EB" # Gray for 'Other'
)

transport_group_colors <- c(
  "Motorized Private Transport" = "#6077FA", # lightBlue
  "Public Transport" = "#FDA333", # ripeMango
  "Non-Motorized Private Transport" = "#B0D45D", # juneBud
  "Flights" = "#FF4400", # tangelo
  "Other Transport Modes" = "#00B9FF", # capri
  "Motorisierter Individualverkehr" = "#6077FA", # lightBlue
  "Öffentlicher Verkehr" = "#FDA333", # ripeMango
  "Nicht-motorisierter Individualverkehr" = "#B0D45D", # juneBud
  "Flüge" = "#FF4400", # tangelo
  "Andere Transportmittel" = "#00B9FF" # capri
)

# Function to map raw transport modes to display names
map_transport_mode <- function(mode) {
  switch(mode,
    "automobile" = "Auto",
    "flight" = "Flug",
    "trainLongDistance" = "Fernzug",
    "trainRegional" = "Regionalzug",
    "trainUrban" = "S-/U-Bahn",
    "busLocal" = "ÖPNV-Linienbus",
    "busLongDistance" = "Fernbus",
    "cycling" = "Fahrrad",
    "taxi" = "Taxi",
    "tram" = "Straßenbahn",
    "ferry" = "Fähre",
    "pedelec" = "E-Bike",
    "walking" = "Zu Fuß",
    "Andere"
  )
}

# Function to group transportation modes into categories
group_transport_modes <- function(mode) {
  if (mode %in% c("automobile", "motorbike", "motorScooterTwoStroke", "motorScooterElectric", "taxi")) {
    return("Motorisierter Individualverkehr")
  } else if (mode %in% c("busLongDistance", "busLocal", "trainLongDistance", "trainRegional", "trainUrban", "tram", "ferry")) {
    return("Öffentlicher Verkehr")
  } else if (mode %in% c("pedelec", "walking", "cycling")) {
    return("Nicht-motorisierter Individualverkehr")
  } else if (mode %in% c("flight")) {
    return("Flüge")
  } else {
    return(NA)
  }
}

# Import fonts if needed, this will take time
# font_import()

# Load fonts for all devices
if (!"Poppins" %in% fonts()) {
  loadfonts(device = "all")
}

# List available fonts
# available_fonts <- fonts()
# print(available_fonts)

# Define a custom theme for all plots
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Poppins"),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "right",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )
