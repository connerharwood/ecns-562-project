library(tidyverse)

# Wisconsin drinking water radium data
radium_raw = read_csv("~/562-Project/raw-data/radium.csv")

#------------------------------------------------------------------------------#
# cleaning ----

radium = radium_raw |> 
  select(
    # select and rename relevant variables
    county = County,
    year = `Sample Date`,
    contaminant = Contaminant,
    measurement = `Measured Amount`,
    units = Units,
    id = `Sample ID`
  ) |> 
  mutate(
    # add "County" to each county name
    county = paste(county, "County"),
    # extract year from date, convert to numeric
    year = as.numeric(substr(year, 7, 10))
  )

# calculate average measurement of each contaminant for each sample ID
radium2 = radium |> 
  group_by(id, contaminant, county, year, units) |> 
  summarize(
    measurement = mean(measurement)
  )

# measurements of Radium 226+228
radium_226_228 = radium2 |> 
  filter(contaminant == "RADIUM, (226 + 228)") |> 
  select(id, contaminant, county, year, measurement, units)

# sum Radium 226 + Radium 228 for samples without Radium 226+228 entry
radium_summed = radium2 |> 
  # sample IDs that don't have an entry for Radium 226+228
  filter(!id %in% radium_226_228$id) |> 
  group_by(id, county, year, units) |> 
  summarize(
    measurement = sum(measurement)
  ) |> 
  # add contaminant variable
  mutate(contaminant = "RADIUM, (226 + 228)") |> 
  select(id, contaminant, county, year, measurement, units)
  
# append Radium 226+228 data
radium3 = rbind(radium_226_228, radium_summed)

# county-year radium averages
radium4 = radium3 |> 
  group_by(county, year) |> 
  summarize(ra_average = mean(measurement))

#------------------------------------------------------------------------------#
# impute missing years to be minimum average value for each county ----

# grid of all year-county combinations
grid = expand_grid(
  county = unique(radium4$county),
  year = unique(radium4$year)
)

# minimum average radium value for each county
county_minimum = radium4 |> 
  group_by(county) |> 
  summarize(ra_minimum = min(ra_average, na.rm = TRUE)) |> 
  # change Waushara's negative value to zero
  mutate(
    ra_minimum = case_when(
      county == "Waushara County" ~ 0,
      TRUE ~ ra_minimum
    )
  )

# fill in missing values with respective county minimum
impute = grid |> 
  left_join(radium4, by = c("county", "year")) |> 
  left_join(county_minimum, by = c("county")) |> 
  mutate(ra_average = ifelse(is.na(ra_average), ra_minimum, ra_average))

radium = impute

#------------------------------------------------------------------------------#

# save cleaned radium data
saveRDS(radium, file = "~/562-Project/clean-data/radium.rds")
