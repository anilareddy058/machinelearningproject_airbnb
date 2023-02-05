library(httr)
install.packages("jsonlite")
library(jsonlite)
# Define the API endpoint
url <- "https://api.apify.com/v2/datasets/5GPP8IfMe144xmKjQ/items?token=apify_api_depdKl0wxd8ASvYDSqF1CVL78Fjndm02s8mr"

# Make the HTTP GET request to retrieve the data
response <- GET(url)

# Check the status code of the response
if (status_code(response) == 200) {
  # Parse the JSON data from the response
  data <- content(response, as = "text", encoding = "UTF-8")
  data_parsed <- fromJSON(data)
  # Convert the parsed data to a list
  data_list <- lapply(data_parsed, unlist)
  
  # Convert the list to a data frame
  df <- as.data.frame(data_list)
  print(df)
  
  # Write the data frame to a CSV file
  write.csv(df, "airbnb_api.csv", row.names = FALSE)
} else {
  # If the request fails, print an error message
  stop("Failed to retrieve data. Response code:", status_code(response))
}