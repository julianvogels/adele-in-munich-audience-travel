# Function to load and process data with weights from groups
load_and_process_data <- function(file_path, location, groups) {
  # Load data
  data <- read_csv(file_path) %>%
    mutate(Location = location)

  # Extract survey names and weights from groups
  survey_weights <- do.call(rbind, lapply(names(groups), function(group_name) {
    group <- groups[[group_name]]
    surveys <- group$surveys
    tibble(
      group_name = group$name,
      survey_name = unlist(lapply(surveys, function(survey) survey$name)),
      weight = unlist(lapply(surveys, function(survey) survey$normalised_weight_product))
    )
  }))

  # Filter the data to include only rows where "Survey group name" and "Survey name" match the names in the survey_weights
  data <- data %>%
    filter(`Survey group name` %in% survey_weights$group_name & `Survey name` %in% survey_weights$survey_name)

  # Join with survey_weights
  data <- data %>%
    left_join(survey_weights, by = c("Survey group name" = "group_name", "Survey name" = "survey_name")) %>%
    mutate(weighted_distance = ifelse(is.na(weight), `Itinerary Distance (single journey, km)`, weight) * `Itinerary Distance (single journey, km)`) %>%
    mutate(weighted_emissions = ifelse(is.na(weight), `Itinerary Emissions (single journey, kg CO2e)`, weight) * `Itinerary Emissions (single journey, kg CO2e)`)

  # Ensure there are no NA values in critical columns after weighting
  data <- data %>%
    filter(!is.na(`weighted_distance`)) %>%
    filter(!is.na(`Itinerary Means of transportation`)) %>%
    filter(!is.na(`Itinerary Distance (single journey, km)`)) %>%
    filter(!is.na(`weighted_emissions`)) %>%
    filter(!is.na(`Itinerary Emissions (single journey, kg CO2e)`))

  return(data)
}

# Calculate weights for different concerts, based on estimated audience numbers
population <- 148000
groups <- list(
  friday = list(
    name = "Friday Concert",
    user_weight_absolute = 74000,
    surveys = list(
      entrance_1 = list(
        name = "Adele Mobilitätsbefragung Publikum Eingang 1 Fr., 9. August",
        user_weight_absolute = 65 / 95,
        sample_size = 541
      ),
      entrance_2 = list(
        name = "Adele Mobiliätsbefragung Publikum Eingang 2 Fr. 9. August",
        user_weight_absolute = 30 / 95,
        sample_size = 203
      )
    )
  ),
  saturday = list(
    name = "Saturday Concert",
    user_weight_absolute = 74000,
    surveys = list(
      entrance_1 = list(
        name = "Adele Mobilitätsbefragung Publikum Eingang 1 Sa., 10. August",
        user_weight_absolute = 65 / 95,
        sample_size = 520
      ),
      entrance_2 = list(
        name = "Adele Mobilitätsbefragung Publikum Eingang 2 Sa., 10 August",
        user_weight_absolute = 30 / 95,
        sample_size = 220
      )
    )
  )
)

# get total group weights
groups_total_user_weight_absolute <- sum(sapply(groups, function(group) group$user_weight_absolute))
groups_mean_user_weight_absolute <- groups_total_user_weight_absolute / length(groups)

# Adjusted weight calculation to incorporate sample size
# Add normalised group weight to each group
for (group_name in names(groups)) {
  group <- groups[[group_name]]
  group$user_weight_normalised <- groups[[group_name]]$user_weight_absolute / groups_mean_user_weight_absolute

  # Add normalised survey weight to each survey, adjusted by sample size
  surveys <- group$surveys
  total_user_weight_absolute <- sum(unlist(lapply(surveys, function(survey) if (!is.null(survey$user_weight_absolute)) survey$user_weight_absolute else 0)))
  num_weighted_surveys <- sum(sapply(surveys, function(survey) !is.null(survey$user_weight_absolute) && survey$user_weight_absolute > 0))

  groups[[group_name]]$surveys <- lapply(surveys, function(survey) {
    if (!is.null(survey$user_weight_absolute) && !is.null(survey$sample_size)) {
      # Calculate adjusted survey weight considering both user weight and sample size
      survey$user_weight_normalised <- (survey$user_weight_absolute / total_user_weight_absolute) * num_weighted_surveys
      survey$sample_adjusted_weight <- survey$user_weight_normalised * (survey$sample_size / sum(sapply(surveys, function(s) s$sample_size)))
      survey$normalised_weight_product <- survey$sample_adjusted_weight * group$user_weight_normalised
    } else {
      survey$normalised_weight_product <- group$user_weight_normalised
    }
    return(survey)
  })
}

# Calculate weights for different entrances, based on estimated audience flow
survey_weights_tibbles <- function(surveys, group_weight) {
  return(
    tibble(
      name = unlist(lapply(surveys, function(survey) survey$name)),
      weight = unlist(lapply(surveys, function(survey) {
        if (!is.null(survey$user_weight_normalised)) {
          # Normalised weight is known: multiply with group weight
          return(survey$user_weight_normalised * group_weight)
        } else {
          # Weight is not set, use just group weight
          return(group_weight)
        }
      }))
    )
  )
}

files_and_metadata <- list(
  list(
    file_path = "data/2024-08-11-adele-2024-Publikumsumfrage-v1-4-0.csv",
    location = "Adele Stadion",
    population = population,
    groups = groups
  )
)

# Load and combine data from all locations, applying weights where necessary
all_data <- bind_rows(lapply(files_and_metadata, function(fl) {
  load_and_process_data(fl$file_path, fl$location, fl$groups)
}))

print(dim(all_data)) # Print dimensions of the data
print(head(all_data)) # Inspect the first few rows to ensure data integrity

# Save the loaded data to an RDS file
saveRDS(all_data, file = "output/all_data.rds")
