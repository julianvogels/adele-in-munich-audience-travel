# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Function to calculate sample size
calculate_sample_size <- function(mean, sd, population, e, z = 1.96) {
  e_marge <- mean * e
  return(ceiling((sd^2) / (((e_marge^2) / (z^2)) + ((sd^2) / (population)))))
}

# Function to load and analyze sample size
load_and_analyze_sample_size <- function(file_path, location, population, e = 0.05) {
  data <- read_csv(file_path) %>%
    mutate(Location = location)

  # Calculate mean and standard deviation of "Emissions per km (kg CO2e)"
  mean_emissions_km <- mean(data$`Emissions per km (kg CO2e)`, na.rm = TRUE)
  sd_emissions_km <- sd(data$`Emissions per km (kg CO2e)`, na.rm = TRUE)

  mean_emissions_total <- mean(data$`Total emissions (round trip, kg CO2e)`, na.rm = TRUE)
  sd_emissions_total <- sd(data$`Total emissions (round trip, kg CO2e)`, na.rm = TRUE)

  # Calculate the required sample size
  required_sample_size_emissions_km <- calculate_sample_size(mean_emissions_km, sd_emissions_km, population, e)
  required_sample_size_emissions_total <- calculate_sample_size(mean_emissions_total, sd_emissions_total, population, e)

  # Get the actual number of participants (rows where itinerary index is 1)
  actual_sample_size <- nrow(data %>% filter(`Itinerary index` == 1))

  return(list(
    location = location,
    population = population,
    actual_sample_size = actual_sample_size,
    mean_emissions_km = mean_emissions_km,
    sd_emissions_km = sd_emissions_km,
    required_sample_size_emissions_km = required_sample_size_emissions_km,
    sufficient_sample_emissions_km = actual_sample_size >= required_sample_size_emissions_km,
    mean_emissions_total = mean_emissions_total,
    sd_emissions_total = sd_emissions_total,
    required_sample_size_emissions_total = required_sample_size_emissions_total,
    sufficient_sample_emissions_total = actual_sample_size >= required_sample_size_emissions_total
  ))
}

# Verify sample size for each dataset
sample_size_analysis <- lapply(files_and_metadata, function(fl) {
  load_and_analyze_sample_size(fl$file_path, fl$location, fl$population)
})
print(sample_size_analysis)

# Display sample size analysis results
sample_size_analysis_df <- bind_rows(sample_size_analysis)
print(sample_size_analysis_df)
