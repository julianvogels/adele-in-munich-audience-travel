# Function to load and process data with weights
load_and_process_data <- function(file_path, location, weights = NULL) {
  # Load data
  data <- read_csv(file_path) %>%
    mutate(Location = location)
  
  # Join with weights if provided
  if (!is.null(weights)) {
    data <- data %>%
      left_join(weights, by = c("Survey name" = "name")) %>%
      mutate(weighted_distance = ifelse(is.na(weight), 0, weight) * `Distance (single journey, km)`)
  } else {
    data <- data %>%
      mutate(weighted_distance = `Distance (single journey, km)`)
  }
  
  # Ensure there are no NA values in critical columns after weighting
  data <- data %>%
    filter(!is.na(`weighted_distance`)) %>%
    filter(!is.na(`Means of transportation`)) %>%
    filter(!is.na(`Distance (single journey, km)`))
  
  return(data)
}

# Calculate weights for different entrances, based on estimated audience flow
entrance_1_percent_audience <- 65
entrance_2_percent_audience <- 30
entrance_3_percent_audience <- 5 # No data was collected for this entrance
observed_audience <- (entrance_1_percent_audience + entrance_2_percent_audience)

entrance_1_weight <- entrance_1_percent_audience / observed_audience
entrance_2_weight <- entrance_2_percent_audience / observed_audience

# Calculate weights for different concerts, based on estimated audience numbers
concert_9_aug_pop <- 75000
concert_10_aug_pop <- 75000
concert_total_audience <- (concert_9_aug_pop + concert_10_aug_pop)

concert_9_aug_weight <- concert_9_aug_pop / concert_total_audience
concert_10_aug_weight <- concert_10_aug_pop / concert_total_audience

# List of file paths and corresponding locations with weights for the Adele concert
files_and_metadata <- list(
  list(
    file_path = "data/2024-08-11-adele-2024-Publikumsumfrage-v1-4-0.csv",
    location = "Adele in Munich",
    population = 750000,
    weights = tibble(
      name = c(
        "Adele Mobilitätsbefragung Publikum Eingang 1 Fr., 9. August",
        "Adele Mobiliätsbefragung Publikum Eingang 2 Fr. 9. August",
        "Adele Mobilitätsbefragung Publikum Eingang 1 Sa., 10. August",
        "Adele Mobilitätsbefragung Publikum Eingang 2 Sa., 10 August"
      ),
      weight = c(
        entrance_1_weight * concert_9_aug_weight,
        entrance_2_weight * concert_9_aug_weight,
        entrance_1_weight * concert_10_aug_weight,
        entrance_2_weight * concert_10_aug_weight
      )
    )
  )
)

# Load and combine data from all locations, applying weights where necessary
all_data <- bind_rows(lapply(files_and_metadata, function(fl) {
  load_and_process_data(fl$file_path, fl$location, fl$weights)
}))

print(dim(all_data))  # Print dimensions of the data
print(head(all_data))  # Inspect the first few rows to ensure data integrity

# Save the loaded data to an RDS file
saveRDS(all_data, file = "output/all_data.rds")
