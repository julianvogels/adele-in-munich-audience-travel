# European Climate Pact color scheme
custom_colors <- c(
  color1 = "#4463E0",
  color2 = "#E25449",
  color3 = "#4FAF4C",
  color4 = "#B34922",
  color5 = "#377370",
  color6 = "#A6BDF5",
  color7 = "#EAA292",
  color8 = "#99E492",
  color9 = "#F1A23A",
  color10 = "#CFA8F9",
  color11 = "#71C3AF",
  color12 = "#D7E4FA",
  color13 = "#EDD7D4",
  color14 = "#D5FDD5",
  color15 = "#F6C87F",
  color16 = "#E8D7FC",
  color17 = "#B6F4E0"
)

# Function to create a repeated color palette
repeat_colors <- function(n) {
  rep(custom_colors, length.out = n)
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