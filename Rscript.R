# install.packages(c("httr", "jsonlite", "dyplr"))

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)


# Import the data from the CSV file
# data <- read.csv2(file = "./se_companies_1000.csv", header = TRUE)
# 500 fetch of companies (less data)
data <- read.csv2(file = "./se_companies_500.csv", header = TRUE)
# 200 000 + rows to fetch (around 13 hours, no recommended...)
# data <- read.csv2(file = "./se_companies.csv", header = TRUE)

# Create an empty data frame to store the new information
new_df <- data.frame()

# Define the base URL for the API
url_base <- "https://recherche-entreprises.api.gouv.fr/search?q="

time_estimation <- (nrow(data) / 6) / 60
sprintf("The estimate time is: %f", time_estimation)


get_info <- function(siren) {
  # Build the API URL with the SIREN number from each row
  api_url <- paste0(url_base, URLencode(as.character(siren)))
  # Get data from the API
  query <- GET(api_url)
  response <-fromJSON(rawToChar(query$content), flatten = TRUE)
  if (query$status_code == "429") response <- "429"
  if (length(str(response$results)) < 0) response <- "404"
  return(response)
}

for (i in 1:nrow(data)) {
  sprintf("Current fetch: %d", data$ID[i])
  fetch <- get_info(data$SIREN[i])
  # Check if fetch$total_results has non-zero length
  if (fetch$total_results > 0 ) {
    # Extract relevant information from the API response
    response <- fetch$results

    # if response is not data.frame, then there is a error code (see get_info function)  # nolint: line_length_linter.
    # so we exit
    if (class(response) == "data.frame") {
      # Extract the data if the elements exist
      category <- response$categorie_entreprise
      activity_principal <- response$activite_principale
      etablissements <- response$nombre_etablissements_ouverts
      employee_nb <- response$tranche_effectif_salarie
      location_department <- response$siege.departement
      creation_dates <- response$date_creation
      person_in_charge <- response$dirigeants
      j_nature <- response$nature_juridique
      is_ess <- response$complements.est_ess
      is_missions_driven <- response$complements.est_societe_mission
      # Create a new row with extracted information
      new_row <- data.frame(
        categorie_entreprise = ifelse(length(category) > 0, category, NA),
        principal_activity = ifelse(length(activity_principal) > 0, activity_principal, NA), # nolint: line_length_linter.
        employees_nb = ifelse(length(employee_nb) > 0, employee_nb, NA),
        etablissements = ifelse(etablissements > 0, etablissements, NA),
        department = ifelse(location_department > 0, location_department, NA),
        creation_date = ifelse(!is.null(creation_dates), creation_dates, NA),
        juridical_nature = ifelse(!is.null(j_nature), j_nature, NA),
        is_sse = ifelse(!is.null(is_ess), is_ess, NA),
        is_mission_driven = ifelse(!is.null(is_missions_driven), is_missions_driven, NA) # nolint: line_length_linter.
      )
      # Add the new row to the dataframe
      new_df <- rbind(new_df, new_row)
    } else {
      # If specific elements don't exist, add NA values to the new row
      new_row <- data.frame(
        categorie_entreprise = NA,
        principal_activity = NA,
        employees_nb = NA,
        etablissements = NA,
        department = NA,
        creation_date = NA,
        juridical_nature = NA,
        is_sse = NA,
        is_mission_driven = NA
      )
      # Add the new row with NA values to the dataframe
      new_df <- rbind(new_df, new_row)
    }
  } else {
    # If fetch$response doesn't exist or is empty, add NA values to the new row
    new_row <- data.frame(
      categorie_entreprise = "Not_found",
      principal_activity = "Not_found",
      employees_nb = "Not_found",
      etablissements = "Not_found",
      department = "Not_found",
      creation_date = "Not_found",
      juridical_nature = "Not_found",
      is_sse = "Not_found",
      is_mission_driven = "Not_found"
    )
    # Add the new row with NA values to the dataframe
    new_df <- rbind(new_df, new_row)
  }
  Sys.sleep(0.16) # Pause to avoid being kicked by the API
  # The api only takes 7 query per second, so => 1/6 = 0.16
}

# Append the data in the main data <- data_frame # nolint: line_length_linter.
data <- cbind(data, new_df)

# Everything is in a new CSV file
write.csv2(data, file = "./se_companies_updated.csv", row.names = FALSE)