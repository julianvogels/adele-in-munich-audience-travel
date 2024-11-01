# Load the data
all_data <- readRDS("output/all_data.rds")

# Define constants
total_audience_size <- population # Total number of visitors
cost_per_ton_co2e <- 860 # Externalized cost in EUR per ton of CO2e as per Umweltbundesamt
allocation_factor <- 0.912 * 1 + 0.078 * 0.5 + 0.01 * 0 # Based on allocation survey data (91,% for full emissions, 7,8% for half emissions, 1% for no emissions)

# Function to calculate total emissions, total kilometers, sample size, and single trip distance
calculate_aggregates <- function(data, allocation_factor = 1) {
  # Filter out invalid or missing data for emissions and distances
  data <- data %>%
    filter(
      !is.na(`Itinerary Emissions (single journey, kg CO2e)`),
      !is.na(`Itinerary Distance (single journey, km)`)
    )

  # Apply allocation factor for flights only
  data <- data %>%
    mutate(Adjusted_Emissions = ifelse(`Itinerary Means of transportation` == "flight",
      `Itinerary Emissions (single journey, kg CO2e)` * allocation_factor,
      `Itinerary Emissions (single journey, kg CO2e)`
    ))

  # Count unique participants
  sample_size <- n_distinct(data$`Participant Identifier`)

  # Calculate total roundtrip emissions (weighted and multiplied by 2 for roundtrip)
  total_emissions_sample <- data %>%
    mutate(Roundtrip_Emissions = Adjusted_Emissions * 2 * weight) %>%
    summarize(Total_Emissions_Sample = sum(Roundtrip_Emissions, na.rm = TRUE)) %>%
    pull(Total_Emissions_Sample)

  # Calculate total roundtrip kilometers (weighted and multiplied by 2 for roundtrip)
  total_km_sample <- data %>%
    mutate(Roundtrip_Km = weighted_distance * 2) %>%
    summarize(Total_Km_Sample = sum(Roundtrip_Km, na.rm = TRUE)) %>%
    pull(Total_Km_Sample)

  return(list(
    total_emissions_sample = total_emissions_sample,
    total_km_sample = total_km_sample,
    sample_size = sample_size
  ))
}

# Function to calculate emissions per person and per kilometer
calculate_average_emissions <- function(total_emissions, sample_size, total_km) {
  # Average emissions per person (kg CO2e)
  avg_emissions_per_person <- total_emissions / sample_size

  # Emissions per kilometer (kg CO2e)
  emissions_per_km <- total_emissions / total_km

  return(list(
    avg_emissions_per_person = avg_emissions_per_person,
    emissions_per_km = emissions_per_km
  ))
}

# Function to extrapolate total emissions to the audience size
extrapolate_total_emissions <- function(avg_emissions_per_person, total_audience_size) {
  total_emissions_extrapolated <- avg_emissions_per_person * total_audience_size
  return(total_emissions_extrapolated)
}

# Function to calculate the externalized cost to society of emissions
calculate_external_cost <- function(total_emissions_kg, cost_per_ton_co2e) {
  total_emissions_ton <- total_emissions_kg / 1000 # Convert kg to tons
  external_cost <- total_emissions_ton * cost_per_ton_co2e
  return(external_cost)
}

# Step 1: Calculate aggregates (total emissions, total kilometers, sample size) with the allocation factor for flights
result <- calculate_aggregates(all_data, allocation_factor = allocation_factor)
total_emissions_sample <- result$total_emissions_sample
total_km_sample <- result$total_km_sample
sample_size <- result$sample_size

# Step 2: Calculate average emissions per person and per kilometer
average_emissions <- calculate_average_emissions(total_emissions_sample, sample_size, total_km_sample)
avg_emissions_per_person <- average_emissions$avg_emissions_per_person
emissions_per_km <- average_emissions$emissions_per_km

# Step 3: Extrapolate total emissions for the full audience
num_concerts_surveyed <- 2 # Number of concerts surveyed
num_concerts_total <- 10 # Total number of concerts in the season
total_emissions_extrapolated <- extrapolate_total_emissions(avg_emissions_per_person, total_audience_size) / num_concerts_surveyed
total_emissions_extrapolated_concert_series <- total_emissions_extrapolated * num_concerts_total # Extrapolate for the whole season (10 events)

# Step 4: Calculate externalized cost for the extrapolated emissions
external_cost <- calculate_external_cost(total_emissions_extrapolated_concert_series, cost_per_ton_co2e)

# Output the results
cat("Extrapolated Total Emissions for", total_audience_size / 2, "visitors:", round(total_emissions_extrapolated / 1000, 2), "t CO2e\n")
cat("Extrapolated Total Emissions for whole season (10 events):", round(total_emissions_extrapolated_concert_series / 1000, 2), "t CO2e\n")
cat("Average Emissions per Person (kg CO2e):", round(avg_emissions_per_person, 2), "\n")
cat("Emissions per Kilometer (kg CO2e):", round(emissions_per_km, 4), "\n")
cat("Externalized Cost to Society for the extrapolated emissions (EUR):", round(external_cost, 2), "EUR\n")

# Step 5: Save the results to files
write.csv(data.frame(Extrapolated_Total_Emissions_t_CO2e = total_emissions_extrapolated / 1000), "output/extrapolated_total_emissions.csv", row.names = FALSE)
write.csv(data.frame(External_Cost_EUR = external_cost), "output/external_cost.csv", row.names = FALSE)
