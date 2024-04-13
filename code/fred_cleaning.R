library(tidyverse)

load("~/562-Project/raw-data/fred/fred_raw.rda")
all_objects = ls()

#------------------------------------------------------------------------------#
# annual resident population ----

# create vector with all raw population datasets
raw_data = all_objects[grep("_pop$", all_objects)]

# initialize list for for-loop
clean_data = list()

for (county in raw_data) {
  # read in each county's annual population dataset
  data = get(county)
  # extract county name
  county_name = gsub("_pop", "", county)
  
  data = data |>
    # select and rename relevant variables
    select(year = date, population = value) |> 
    mutate(
      # extract year from date, convert to numeric
      year = as.numeric(str_extract(year, "\\d{4}")),
      # convert population in thousands to actual population
      population = as.numeric(population)*1000,
      # assign county name variable
      county = str_to_title(paste0(county_name, " County")),
      # manually correct 2 counties
      county = case_when(
        county == "Fonddulac County" ~ "Fond du Lac County", 
        county == "Greenlake County" ~ "Green Lake County",
        TRUE ~ county
      )
    ) |> 
    # reorder columns
    select(county, year, population)
  
  # store cleaned dataset in list
  clean_data[[county]] = data
}

# assign modified datasets back to global environment
list2env(clean_data, envir = .GlobalEnv)

# create list with all clean population datasets
population_datasets = mget(all_objects[grep("_pop$", all_objects)])

# append all clean population datasets into one
counties_pop = bind_rows(population_datasets)

#------------------------------------------------------------------------------#
# annual median household income ----

# create vector with all raw income datasets
raw_data = all_objects[grep("_income$", all_objects)]

# initialize list for for-loop
clean_data = list()

for (county in raw_data) {
  # read in each county's annual household income dataset
  data = get(county)
  # extract county name
  county_name = gsub("_income", "", county)
  
  data = data |>
    # select and rename relevant variables
    select(year = date, income = value) |> 
    mutate(
      # extract year from date, convert to numeric
      year = as.numeric(str_extract(year, "\\d{4}")),
      # convert median income to numeric
      income = as.numeric(income),
      # assign county name variable
      county = str_to_title(paste0(county_name, " County")),
      # manually correct 2 counties
      county = case_when(
        county == "Fonddulac County" ~ "Fond du Lac County", 
        county == "Greenlake County" ~ "Green Lake County",
        TRUE ~ county
      )
    ) |> 
    # reorder columns
    select(county, year, income)
  
  # store cleaned dataset in list
  clean_data[[county]] = data
}

# assign modified datasets back to global environment
list2env(clean_data, envir = .GlobalEnv)

# create list with all clean income datasets
income_datasets = mget(all_objects[grep("_income$", all_objects)])

# append all clean income datasets into one
counties_income = bind_rows(income_datasets)

#------------------------------------------------------------------------------#
# annual unemployment rate ----

# create vector with all raw unemployment datasets
raw_data = all_objects[grep("_unemp$", all_objects)]

# initialize list for for-loop
clean_data = list()

for (county in raw_data) {
  # read in each county's annual unemployment dataset
  data = get(county)
  # extract county name
  county_name = gsub("_unemp", "", county)
  
  data = data |>
    # select and rename relevant variables
    select(year = date, u_rate = value) |> 
    mutate(
      # extract year from date, convert to numeric
      year = as.numeric(str_extract(year, "\\d{4}")),
      # convert unemployment rate to decimal
      u_rate = as.numeric(u_rate)/100,
      # assign county name variable
      county = str_to_title(paste0(county_name, " County")),
      # manually correct 2 counties
      county = case_when(
        county == "Fonddulac County" ~ "Fond du Lac County", 
        county == "Greenlake County" ~ "Green Lake County",
        TRUE ~ county
      )
    ) |> 
    # reorder columns
    select(county, year, u_rate)
  
  # store cleaned dataset in list
  clean_data[[county]] = data
}

# assign modified datasets back to global environment
list2env(clean_data, envir = .GlobalEnv)

# create list with all clean unemployment datasets
unemp_datasets = mget(all_objects[grep("_unemp$", all_objects)])

# append all clean unemployment datasets into one
counties_unemp = bind_rows(unemp_datasets)

#------------------------------------------------------------------------------#
# quarterly number of private establishments for all industries ----

# create vector with all raw establishments datasets
raw_data = all_objects[grep("_establishments$", all_objects)]

# initialize list for for-loop
clean_data = list()

for (county in raw_data) {
  # read in each county's quarterly private establishments dataset
  data = get(county)
  # extract county name
  county_name = gsub("_establishments", "", county)
  
  data = data |>
    # select and rename relevant variables
    select(date, establishments = value) |> 
    # separate date variable into year, month, and day columns
    separate(date, into = c("year", "month", "day"), sep = "-") |> 
    # filter to only include July observations to align with how annual population estimates are given
    filter(month == "07") |> 
    mutate(
      # convert year, number of establishments to numeric
      year = as.numeric(year),
      establishments = as.numeric(establishments),
      # assign county name variable
      county = str_to_title(paste0(county_name, " County")),
      # manually correct 2 counties
      county = case_when(
        county == "Fonddulac County" ~ "Fond du Lac County", 
        county == "Greenlake County" ~ "Green Lake County",
        TRUE ~ county
      )
    ) |> 
    # reorder columns
    select(county, year, establishments)
  
  # store cleaned dataset in list
  clean_data[[county]] = data
}

# assign modified datasets back to global environment
list2env(clean_data, envir = .GlobalEnv)

# create list with all clean establishments datasets
establishments_datasets = mget(all_objects[grep("_establishments$", all_objects)])

# append all clean establishments datasets into one
counties_establishments = bind_rows(establishments_datasets)

#------------------------------------------------------------------------------#
# annual private housing structures authorized by building permits ----

# create vector with all raw housing permits datasets
raw_data = all_objects[grep("_housing$", all_objects)]

# initialize list for for-loop
clean_data = list()

for (county in raw_data) {
  # read in each county's annual housing permits dataset
  data = get(county)
  # extract county name
  county_name = gsub("_housing", "", county)
  
  data = data |>
    # select and rename relevant variables
    select(year = date, permits = value) |> 
    mutate(
      # extract year from date, convert to numeric
      year = as.numeric(str_extract(year, "\\d{4}")),
      # convert housing permits to numeric
      permits = as.numeric(permits),
      # assign county name variable
      county = str_to_title(paste0(county_name, " County")),
      # manually correct 2 counties
      county = case_when(
        county == "Fonddulac County" ~ "Fond du Lac County", 
        county == "Greenlake County" ~ "Green Lake County",
        TRUE ~ county
      )
    ) |> 
    # reorder columns
    select(county, year, permits)
  
  # store cleaned dataset in list
  clean_data[[county]] = data
}

# assign modified datasets back to global environment
list2env(clean_data, envir = .GlobalEnv)

# create list with all clean housing permits datasets
housing_datasets = mget(all_objects[grep("_housing$", all_objects)])

# append all clean housing permits datasets into one
counties_housing = bind_rows(housing_datasets)

#------------------------------------------------------------------------------#
# merge, check, clean, save

fred_merge1 = left_join(counties_pop, counties_income, by = c("county", "year"), relationship = "one-to-one")
fred_merge2 = left_join(fred_merge1, counties_unemp, by = c("county", "year"), relationship = "one-to-one")
fred_merge3 = left_join(fred_merge2, counties_housing, by = c("county", "year"), relationship = "one-to-one")
fred_merge4 = left_join(fred_merge3, counties_establishments, by = c("county", "year"), relationship = "one-to-one")

# check what years have no NAs for any variables to determine what years to filter from
fred_na = fred_merge4 |> 
  group_by(year) |> 
  summarize_all(~ all(!is.na(.))) |> 
  filter_all(all_vars(.))

# restrict data to 1997-2022 for most complete data
fred1997_2022 = fred_merge4 |> 
  filter(year <= 2022 & year >= 1997)

# check that all counties appear in all years
checkyears = fred1997_2022 |> 
  group_by(year) |> 
  summarize(n_counties = n_distinct(county))

# save
saveRDS(fred1997_2022, file = "~/562-Project/clean-data/fred.rds")
