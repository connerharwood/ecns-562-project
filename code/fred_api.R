library(tidyverse)
library(jsonlite)

# run this code in console with FRED API key
# Sys.setenv(fred_api_key = "xxxxxxxxxxxxxxxxxxx")

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
# retrieve data ----

series = c(
  # Adams County
  "WIADAM1POP", "MHIWI55001A052NCEN", "LAUCN550010000000003A", "ENU5500120510", "BPPRIV055001",
  # Ashland County
  "WIASHL3POP", "MHIWI55003A052NCEN", "LAUCN550030000000003A", "ENU5500320510", "BPPRIV055003",
  # Bayfield County
  "WIBAYF7POP", "MHIWI55007A052NCEN", "LAUCN550070000000003A", "ENU5500720510", "BPPRIV055007",
  # Columbia County
  "WICOLU1POP", "MHIWI55021A052NCEN", "LAUCN550210000000003A", "ENU5502120510", "BPPRIV055021",
  # Dodge County
  "WIDODG5POP", "MHIWI55027A052NCEN", "LAUCN550270000000003A", "ENU5502720510", "BPPRIV055027",
  # Douglas County
  "WIDOUG1POP", "MHIWI55031A052NCEN", "LAUCN550310000000003A", "ENU5503120510", "BPPRIV055031",
  # Fond du Lac County
  "WIFOND0POP", "MHIWI55039A052NCEN", "LAUCN550390000000003A", "ENU5503920510", "BPPRIV055039",
  # Forest County
  "WIFORE1POP", "MHIWI55041A052NCEN", "LAUCN550410000000003A", "ENU5504120510", "BPPRIV055041",
  # Green Lake County
  "WIGREE7POP", "MHIWI55047A052NCEN", "LAUCN550470000000003A", "ENU5504720510", "BPPRIV055047",
  # Iron County
  "WIIRON1POP", "MHIWI55051A052NCEN", "LAUCN550510000000003A", "ENU5505120510", "BPPRIV055051",
  # Kenosha County
  "WIKENO9POP", "MHIWI55059A052NCEN", "LAUCN550590000000003A", "ENU5505920510", "BPPRIV055059",
  # Langlade County
  "WILANG7POP", "MHIWI55067A052NCEN", "LAUCN550670000000003A", "ENU5506720510", "BPPRIV055067",
  # Marathon County
  "WIMARA0POP", "MHIWI55073A052NCEN", "LAUCN550730000000003A", "ENU5507320510", "BPPRIV055073",
  # Marquette County
  "WIMARQ7POP", "MHIWI55077A052NCEN", "LAUCN550770000000003A", "ENU5507720510", "BPPRIV055077",
  # Milwaukee County
  "WIMILW5POP", "MHIWI55079A052NCEN", "LAUCN550790000000003A", "ENU5507920510", "BPPRIV055079",
  # Oneida County
  "WIONEI5POP", "MHIWI55085A052NCEN", "LAUCN550850000000003A", "ENU5508520510", "BPPRIV055085",
  # Portage County
  "WIPORT0POP", "MHIWI55097A052NCEN", "LAUCN550970000000003A", "ENU5509720510", "BPPRIV055097",
  # Racine County
  "WIRACI1POP", "MHIWI55101A052NCEN", "LAUCN551010000000003A", "ENU5510120510", "BPPRIV055101",
  # Shawano County
  "WISHAW5POP", "MHIWI55115A052NCEN", "LAUCN551150000000003A", "ENU5511520510", "BPPRIV055115",
  # Vilas County
  "WIVILA5POP", "MHIWI55125A052NCEN", "LAUCN551250000000003A", "ENU5512520510", "BPPRIV055125",
  # Washington County
  "WIWASH7POP", "MHIWI55131A052NCEN", "LAUCN551310000000003A", "ENU5513120510", "BPPRIV055131",
  # Waukesha County
  "WIWAUK3POP", "MHIWI55133A052NCEN", "LAUCN551330000000003A", "ENU5513320510", "BPPRIV055133",
  # Waushara County
  "WIWAUS7POP", "MHIWI55137A052NCEN", "LAUCN551370000000003A", "ENU5513720510", "BPPRIV055137"
)

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
  # Adams County
  # annual resident population
  "WIADAM1POP" = "adams_pop", 
  # annual median household income
  "MHIWI55001A052NCEN" = "adams_income",
  # annual unemployment rate
  "LAUCN550010000000003A" = "adams_unemp", 
  # quarterly number of private establishments for all industries
  "ENU5500120510" = "adams_establishments", 
  # annual new private housing structures authorized by building permits
  "BPPRIV055001" = "adams_housing",

  # Ashland County
  "WIASHL3POP" = "ashland_pop", 
  "MHIWI55003A052NCEN" = "ashland_income", 
  "LAUCN550030000000003A" = "ashland_unemp", 
  "ENU5500320510" = "ashland_establishments", 
  "BPPRIV055003" = "ashland_housing",
  
  # Bayfield County
  "WIBAYF7POP" = "bayfield_pop", 
  "MHIWI55007A052NCEN" = "bayfield_income", 
  "LAUCN550070000000003A" = "bayfield_unemp", 
  "ENU5500720510" = "bayfield_establishments", 
  "BPPRIV055007" = "bayfield_housing",
  
  # Columbia County
  "WICOLU1POP" = "columbia_pop", 
  "MHIWI55021A052NCEN" = "columbia_income", 
  "LAUCN550210000000003A" = "columbia_unemp", 
  "ENU5502120510" = "columbia_establishments", 
  "BPPRIV055021" = "columbia_housing",
  
  # Dodge County
  "WIDODG5POP" = "dodge_pop", 
  "MHIWI55027A052NCEN" = "dodge_income", 
  "LAUCN550270000000003A" = "dodge_unemp", 
  "ENU5502720510" = "dodge_establishments", 
  "BPPRIV055027" = "dodge_housing",
  
  # Douglas County
  "WIDOUG1POP" = "douglas_pop", 
  "MHIWI55031A052NCEN" = "douglas_income", 
  "LAUCN550310000000003A" = "douglas_unemp", 
  "ENU5503120510" = "douglas_establishments", 
  "BPPRIV055031" = "douglas_housing",
  
  # Fond du Lac County
  "WIFOND0POP" = "fonddulac_pop", 
  "MHIWI55039A052NCEN" = "fonddulac_income", 
  "LAUCN550390000000003A" = "fonddulac_unemp", 
  "ENU5503920510" = "fonddulac_establishments", 
  "BPPRIV055039" = "fonddulac_housing",
  
  # Forest County
  "WIFORE1POP" = "forest_pop", 
  "MHIWI55041A052NCEN" = "forest_income", 
  "LAUCN550410000000003A" = "forest_unemp", 
  "ENU5504120510" = "forest_establishments", 
  "BPPRIV055041" = "forest_housing",
  
  # Green Lake County
  "WIGREE7POP" = "greenlake_pop", 
  "MHIWI55047A052NCEN" = "greenlake_income", 
  "LAUCN550470000000003A" = "greenlake_unemp", 
  "ENU5504720510" = "greenlake_establishments", 
  "BPPRIV055047" = "greenlake_housing",
  
  # Iron County
  "WIIRON1POP" = "iron_pop", 
  "MHIWI55051A052NCEN" = "iron_income", 
  "LAUCN550510000000003A" = "iron_unemp", 
  "ENU5505120510" = "iron_establishments", 
  "BPPRIV055051" = "iron_housing",
  
  # Kenosha County
  "WIKENO9POP" = "kenosha_pop", 
  "MHIWI55059A052NCEN" = "kenosha_income", 
  "LAUCN550590000000003A" = "kenosha_unemp", 
  "ENU5505920510" = "kenosha_establishments", 
  "BPPRIV055059" = "kenosha_housing",
  
  # Langlade County
  "WILANG7POP" = "langlade_pop", 
  "MHIWI55067A052NCEN" = "langlade_income", 
  "LAUCN550670000000003A" = "langlade_unemp", 
  "ENU5506720510" = "langlade_establishments", 
  "BPPRIV055067" = "langlade_housing",
  
  # Marathon County
  "WIMARA0POP" = "marathon_pop", 
  "MHIWI55073A052NCEN" = "marathon_income", 
  "LAUCN550730000000003A" = "marathon_unemp", 
  "ENU5507320510" = "marathon_establishments", 
  "BPPRIV055073" = "marathon_housing",
  
  # Marquette County
  "WIMARQ7POP" = "marquette_pop", 
  "MHIWI55077A052NCEN" = "marquette_income", 
  "LAUCN550770000000003A" = "marquette_unemp", 
  "ENU5507720510" = "marquette_establishments", 
  "BPPRIV055077" = "marquette_housing",
  
  # Milwaukee County
  "WIMILW5POP" = "milwaukee_pop", 
  "MHIWI55079A052NCEN" = "milwaukee_income", 
  "LAUCN550790000000003A" = "milwaukee_unemp", 
  "ENU5507920510" = "milwaukee_establishments", 
  "BPPRIV055079" = "milwaukee_housing",
  
  # Oneida County
  "WIONEI5POP" = "oneida_pop", 
  "MHIWI55085A052NCEN" = "oneida_income", 
  "LAUCN550850000000003A" = "oneida_unemp", 
  "ENU5508520510" = "oneida_establishments", 
  "BPPRIV055085" = "oneida_housing",
  
  # Portage County
  "WIPORT0POP" = "portage_pop", 
  "MHIWI55097A052NCEN" = "portage_income", 
  "LAUCN550970000000003A" = "portage_unemp", 
  "ENU5509720510" = "portage_establishments", 
  "BPPRIV055097" = "portage_housing",
  
  # Racine County
  "WIRACI1POP" = "racine_pop", 
  "MHIWI55101A052NCEN" = "racine_income", 
  "LAUCN551010000000003A" = "racine_unemp", 
  "ENU5510120510" = "racine_establishments", 
  "BPPRIV055101" = "racine_housing",
  
  # Shawano County
  "WISHAW5POP" = "shawano_pop", 
  "MHIWI55115A052NCEN" = "shawano_income", 
  "LAUCN551150000000003A" = "shawano_unemp", 
  "ENU5511520510" = "shawano_establishments", 
  "BPPRIV055115" = "shawano_housing",
  
  # Vilas County
  "WIVILA5POP" = "vilas_pop", 
  "MHIWI55125A052NCEN" = "vilas_income", 
  "LAUCN551250000000003A" = "vilas_unemp", 
  "ENU5512520510" = "vilas_establishments", 
  "BPPRIV055125" = "vilas_housing",
  
  # Washington County
  "WIWASH7POP" = "washington_pop", 
  "MHIWI55131A052NCEN" = "washington_income", 
  "LAUCN551310000000003A" = "washington_unemp", 
  "ENU5513120510" = "washington_establishments", 
  "BPPRIV055131" = "washington_housing",
  
  # Waukesha County
  "WIWAUK3POP" = "waukesha_pop", 
  "MHIWI55133A052NCEN" = "waukesha_income", 
  "LAUCN551330000000003A" = "waukesha_unemp", 
  "ENU5513320510" = "waukesha_establishments", 
  "BPPRIV055133" = "waukesha_housing",
  
  # Waushara County
  "WIWAUS7POP" = "waushara_pop", 
  "MHIWI55137A052NCEN" = "waushara_income", 
  "LAUCN551370000000003A" = "waushara_unemp", 
  "ENU5513720510" = "waushara_establishments", 
  "BPPRIV055137" = "waushara_housing"
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

#------------------------------------------------------------------------------#

# remove unwanted objects in environment to save all FRED data quickly as one .rda file
rm(all_series_data, data, filtered_data, series_data, endpoint, series, series_id, series_names, url, get_series_data)

# save all data as one .rda file
save.image("~/562-Project/raw-data/fred/fred_raw.rda")
