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
    strata_size_absolute = 74000,
    surveys = list(
      entrance_1 = list(
        name = "Adele Mobilitätsbefragung Publikum Eingang 1 Fr., 9. August",
        strata_size_absolute = 65 / 95,
        sample_size = 541
      ),
      entrance_2 = list(
        name = "Adele Mobiliätsbefragung Publikum Eingang 2 Fr. 9. August",
        strata_size_absolute = 30 / 95,
        sample_size = 203
      )
    )
  ),
  saturday = list(
    name = "Saturday Concert",
    strata_size_absolute = 74000,
    surveys = list(
      entrance_1 = list(
        name = "Adele Mobilitätsbefragung Publikum Eingang 1 Sa., 10. August",
        strata_size_absolute = 65 / 95,
        sample_size = 520
      ),
      entrance_2 = list(
        name = "Adele Mobilitätsbefragung Publikum Eingang 2 Sa., 10 August",
        strata_size_absolute = 30 / 95,
        sample_size = 220
      )
    )
  )
)

# get total group weights
groups_total_strata_size_absolute <- sum(sapply(groups, function(group) group$strata_size_absolute))
groups_mean_strata_size_absolute <- groups_total_strata_size_absolute / length(groups)

# Adjusted weight calculation to incorporate sample size
# Add normalised group weight to each group
for (group_name in names(groups)) {
  # Get the current group and calculate its normalised user weight relative to the group mean user weight
  group <- groups[[group_name]]
  group$strata_size_normalised <- group$strata_size_absolute / groups_mean_strata_size_absolute

  # Get the surveys within this group
  surveys <- group$surveys

  # Calculate the total user weight for all surveys in this group to normalise individual survey weights
  num_weighted_surveys <- sum(sapply(surveys, function(survey) {
    !is.null(survey$strata_size_absolute) && survey$strata_size_absolute > 0
  }))

  total_strata_size_weighted_surveys <- sum(unlist(lapply(surveys, function(survey) {
    if (!is.null(survey$strata_size_absolute)) survey$strata_size_absolute else 0
  })))
  mean_strata_size_weighted_surveys <- total_strata_size_weighted_surveys / num_weighted_surveys

  # Calculate the average sample size across all surveys in this group
  total_sample_size_weighted_surveys <- sum(sapply(surveys, function(survey) {
    if (!is.null(survey$strata_size_absolute)) survey$sample_size else 0
  }))
  mean_sample_size_weighted_surveys <- total_sample_size_weighted_surveys / num_weighted_surveys

  # Process each survey within the group to apply composite weighting
  groups[[group_name]]$surveys <- lapply(surveys, function(survey) {
    # Check that both user weight and sample size are defined for this survey
    if (!is.null(survey$strata_size_absolute) && !is.null(survey$sample_size)) {
      strata_name <- survey$name

      # The size of this subgroup of the population
      strata_size <- survey$strata_size_absolute

      # The percentage this subgroup of the population represents
      strata_percentage <- strata_size / total_strata_size_weighted_surveys

      # The size of the stratified sample for this subgroup (supposed to survey that many people to be representative)
      strata_sample_size_goal <- total_sample_size_weighted_surveys * strata_percentage

      # The actual sample size of this subgroup (how many people were actually surveyed)
      strata_sample_size_actual <- survey$sample_size

      # The equalisation factor to make the sample size representative of the subgroup
      strata_weight <- strata_sample_size_goal / strata_sample_size_actual

      print("")
      print(paste("strata_name: ", strata_name))
      print(paste("strata_size: ", strata_size))
      print(paste("strata_percentage: ", sprintf("%.1f %%", strata_percentage * 100)))
      print(paste("strata_sample_size_goal: ", strata_sample_size_goal))
      print(paste("strata_sample_size_actual: ", strata_sample_size_actual))
      print(paste("strata_weight: ", strata_weight))

      survey$normalised_weight_product <- strata_weight * group$strata_size_normalised
    } else {
      # If user weight or sample size is missing, fallback to the group's normalised weight
      survey$normalised_weight_product <- group$strata_size_normalised
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
        if (!is.null(survey$strata_size_normalised)) {
          # Normalised weight is known: multiply with group weight
          return(survey$strata_size_normalised * group_weight)
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
