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
  ) |> 
  # filter to only include detects
  filter(measurement > 0)

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

# set each county-year observation to be highest result of that county's samples in that year
radium4 = radium3 |> 
  group_by(county, year) |> 
  summarize(ra_max = max(measurement))

#------------------------------------------------------------------------------#
# impute missing years to be zero ----

# grid of all year-county combinations
grid = expand_grid(
  county = unique(radium3$county),
  year = unique(radium3$year)
)

radium = grid |> 
  left_join(radium4, by = c("county", "year")) |> 
  # replace NAs with zero
  mutate(
    ra_max = ifelse(is.na(ra_max), 0, ra_max)
  )

#------------------------------------------------------------------------------#

# save cleaned radium data
saveRDS(radium, file = "~/562-Project/clean-data/radium.rds")
