# install.packages(c("httr", "jsonlite", "dyplr"))

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)


# Import the data from the CSV file
data <- read.csv2(file = "./se_companies.csv", header = TRUE)

# Create an empty data frame to store the new information
new_df <- data.frame()

# Define the base URL for the API
url_base <- "https://recherche-entreprises.api.gouv.fr/search?q="

get_info <- function(SIREN) {
  # Build the API URL with the SIREN number from each row
  api_url <- paste0(url_base, URLencode(as.character(SIREN)))
  # Get data from the API
  query <- GET(api_url)
  response <- fromJSON(rawToChar(query$content), flatten = TRUE)
  if (query$status_code == "429") response <- "429"
  if (length(str(response$results)) < 0) response <- "404"
  return(response)
}

for (i in 1:nrow(data)) {
  fetch <- get_info(data$SIREN[i])

  # Check if fetch$total_results has non-zero length
  if (fetch$total_results > 0) {
    # Extract relevant information from the API response
    response <- fetch$results
    # Check if the specific elements exist in the response
    if (class(response) == "data.frame") {
      # Extract the data if the elements exist
      category <- response$categorie_entreprise
      etablissements <- response$nombre_etablissements_ouverts
      creation_dates <- response$date_creation
      person_in_charge <- response$dirigeants
      j_nature <- response$nature_juridique
      is_ess <- response$complements.est_ess
      is_missions_driven <- response$complements.est_societe_mission
      # Create a new row with extracted information
      new_row <- data.frame(
        categorie_entreprise = ifelse(length(category) > 0, category, NA),
        etablissements = ifelse(etablissements > 0, etablissements, NA),
        creation_date = ifelse(!is.null(creation_dates), creation_dates, NA),
        juridical_nature = ifelse(!is.null(j_nature), j_nature, NA),
        is_sse = ifelse(!is.null(is_ess), is_ess, NA),
        is_mission_driven = ifelse(!is.null(is_missions_driven), is_missions_driven, NA)
      )
      
      # Add the new row to the dataframe
      new_df <- rbind(new_df, new_row)
    } else {
      # If specific elements don't exist, add NA values to the new row
      new_row <- data.frame(
        categorie_entreprise = NA,
        etablissements = NA,
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
      etablissements = "Not_found",
      creation_date = "Not_found",
      juridical_nature = "Not_found",
      is_sse = "Not_found",
      is_mission_driven = "Not_found"
    )
    # Add the new row with NA values to the dataframe
    new_df <- rbind(new_df, new_row)
  }
  Sys.sleep(0.3) # Pause to avoid being kicked by the API
}

# Append the data in the main data <- data_frame # nolint: line_length_linter.
data <- cbind(data, new_df)

# Everything is in a new CSV file
write.csv2(data, file = "./se_companies_updated.csv", row.names = FALSE)

# when
# Convert creation_dates to Date format
new_df$creation_dates <- as.Date(new_df$creation_dates, format = "%Y-%m-%d")

ggplot(new_df, aes(x=creation_dates)) +  
  geom_histogram(binwidth=1, fill="blue",color="black", stat = "identity") +
  labs(x="Date of creation")

# Histogram of 
ggplot(new_df, mapping = aes(creation_dates)) +
  geom_histogram(color = "black", fill = "blue") + # nolint
  labs(title = "Histogram of Creation Dates", x = "Creation Dates") # nolint



# Plot about the categories of company
table_categories <- table(new_df$categorie_entreprise)
barplot(table_categories, main = "Company Categories", xlab = "Category", ylab = "Frequency")
