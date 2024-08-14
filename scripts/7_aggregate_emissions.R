# Load necessary libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(dplyr)
library(readr)

# Load the data from the RDS file
all_data <- readRDS("output/all_data.rds")

# Define the total audience size
total_audience_size <- 750000  # Total number of visitors to the Adele concert series

# Function to calculate aggregate emissions and sample size with weights
calculate_aggregate_emissions <- function(data) {
  # Count the unique Participant Identifiers
  sample_size <- n_distinct(data$`Participant Identifier`)
  
  # Aggregate total emissions for the sample, applying weights
  total_emissions_sample <- data %>%
    mutate(Weighted_Emissions = `Emissions (single journey, kg CO2e)` * 2 * weight) %>%
    summarize(Total_Emissions_Sample = sum(Weighted_Emissions, na.rm = TRUE))
  
  return(list(total_emissions_sample = total_emissions_sample, sample_size = sample_size))
}

# Function to extrapolate total emissions
extrapolate_total_emissions <- function(aggregate_emissions_sample, sample_size, total_audience_size) {
  # Calculate the scaling factor
  scaling_factor <- total_audience_size / sample_size
  
  # Extrapolate total emissions
  total_emissions <- aggregate_emissions_sample$Total_Emissions_Sample * scaling_factor
  
  return(total_emissions)
}

# Function to calculate the average emissions per person
calculate_average_emissions_per_person <- function(total_emissions, total_audience_size) {
  average_emissions_per_person <- total_emissions / total_audience_size
  return(average_emissions_per_person)
}

# Step 1: Calculate the aggregate emissions and sample size
result <- calculate_aggregate_emissions(all_data)
aggregate_emissions_sample <- result$total_emissions_sample
sample_size <- result$sample_size

# Print the results for the sample
print(paste("Sample Size:", sample_size))
print(aggregate_emissions_sample)

# Save the aggregate emissions and sample size to a file
write.csv(aggregate_emissions_sample, "output/aggregate_emissions_sample.csv", row.names = FALSE)
write.csv(data.frame(Sample_Size = sample_size), "output/sample_size.csv", row.names = FALSE)

# Step 2: Extrapolate total emissions
total_emissions <- extrapolate_total_emissions(aggregate_emissions_sample, sample_size, total_audience_size)

# Print the extrapolated total emissions
print(paste("Extrapolated Total Emissions for", total_audience_size, "visitors:", total_emissions))

# Save the total emissions to a file
write.csv(data.frame(Total_Emissions = total_emissions), "output/total_emissions_extrapolated.csv", row.names = FALSE)

# Step 3: Calculate the average emissions per person
average_emissions_per_person <- calculate_average_emissions_per_person(total_emissions, total_audience_size)

# Print the average emissions per person
print(paste("Average Emissions per Person (kg CO2e):", average_emissions_per_person))

# Save the average emissions per person to a file
write.csv(data.frame(Average_Emissions_Per_Person = average_emissions_per_person), "output/average_emissions_per_person.csv", row.names = FALSE)
