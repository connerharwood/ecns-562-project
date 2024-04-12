library(httr)
library(jsonlite)

# run this code in console with FRED API key
#Sys.setenv(fred_api_key = "xxxxxxxxxxxxxxxxxxx")

# set FRED API url and endpoint
url = "https://api.stlouisfed.org/"
endpoint = "fred/series/observations"

# function to get data for a series id, to be run with for loop
get_series_data = function(series_id) {
  params = list(
    api_key = Sys.getenv("fred_api_key"), # FRED API key
    file_type = "json", # file type for response
    series_id = series_id # series id to retrieve data for
  )
  
  # request data
  response = httr::GET(url = url, path = endpoint, query = params)
  
  # check if response was successful
  if (response$status_code == 200) {
    # extract json content from response
    json = httr::content(response, "text") |> 
      jsonlite::fromJSON()
    
    # convert json observations to tibble
    data = as_tibble(json$observations)
    
    # add column for series id
    data$series_id = series_id
    
    return(data)
  } else {
    # warning if response was not successful
    warning(paste("Failed to retrieve data for", series_id))
    return(NULL)
  }
}

#------------------------------------------------------------------------------#
# Adams County ----

# series IDs for Adams County
series = c("WIADAM1POP", "MHIWI55001A052NCEN", "WIADAM1URN", "ENU5500120510", "BPPRIV055001")

# initialize empty list to store data
all_series_data = list()

# loop over each series id to get data
for (series_id in series) {
  # run function to retrieve data for each series
  series_data = get_series_data(series_id)
  
  # assign data to list if request was successful
  if (!is.null(series_data)) {
    all_series_data[[series_id]] = series_data
  }
}

# combine data from all series into single tibble
data = bind_rows(all_series_data)

# assign names to each series
series_names = c(
  "WIADAM1POP" = "adams_pop", 
  "MHIWI55001A052NCEN" = "adams_income", 
  "WIADAM1URN" = "adams_unemp", 
  "ENU5500120510" ="adams_establishments", 
  "BPPRIV055001" = "adams_housing"
)
 
# filter data for each series into separate lists
filtered_data = lapply(names(series_names), function(.x) {
  data |> 
    filter(series_id == .x)
}) 

# assign names to each filtered data list
names(filtered_data) = series_names

# convert each list to dataframe in global environment
list2env(filtered_data, envir = .GlobalEnv)

