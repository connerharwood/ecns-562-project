library(tidyverse)
library(sf)

#------------------------------------------------------------------------------#

# load shapefiles
all_counties = st_read("~/562-Project/raw-data/counties-shapefile/wisconsin_counties.shp")
all_cities = st_read("~/562-Project/raw-data/cities-shapefile/wisconsin_cities.shp")
basin = st_read("~/562-Project/raw-data/basin-shapefile/subbasins.shp")

# make valid and transform CRS to WGS 84
all_counties = all_counties |> st_make_valid() |> st_transform(crs = 4326)
all_cities = all_cities |> st_make_valid() |> st_transform(crs = 4326)
basin = basin |> st_make_valid() |> st_transform(crs = 4326)

#------------------------------------------------------------------------------#
# find straddling counties ----

# select Lake Superior and Lake Michigan subbasins (Wisconsin only lies in these subbasins)
subbasins = basin |> 
  rename(subbasin = merge) |> 
  filter(subbasin == "lk_sup" | subbasin == "lk_mich")

# counties lying at least partially within Great Lakes Basin (straddling counties)
basin_counties = st_intersection(all_counties, subbasins)

# counties lying fully within Great Lakes Basin
fully_within_counties = all_counties[st_within(all_counties, subbasins, sparse = FALSE), ]

# filter out counties lying fully with basin to include only straddling counties
straddling_counties_partial = basin_counties |> 
  filter(!COUNTY_NAM %in% fully_within_counties$COUNTY_NAM)

# grab subbasin of each county for later
county_subbasin = straddling_counties_partial |> 
  select(county = COUNTY_NAM, subbasin) |>
  mutate(county = paste(county, "County")) |> 
  st_drop_geometry()

# full polygons of straddling counties
straddling_counties_full = all_counties |> 
  filter(COUNTY_NAM %in% straddling_counties_partial$COUNTY_NAM)

#------------------------------------------------------------------------------#
# percent of each county lying within basin ----

# calculate full area of each county
straddling_counties_full$area = st_area(straddling_counties_full)

# calculate area of each county lying within basin
straddling_counties_partial$area = st_area(straddling_counties_partial)

# sum up each county's partial area (some counties have multiple geometries)
straddling_counties_partial = straddling_counties_partial |> 
  group_by(COUNTY_NAM) |> 
  summarize(area = sum(area)) |> 
  # sort by county name
  arrange(COUNTY_NAM)

straddling_counties = straddling_counties_full |> 
  # sort by county name
  arrange(COUNTY_NAM) |> 
  # calculate percent of each county lying within Great Lakes Basin
  mutate(percent_within = straddling_counties_partial$area / area) |> 
  # select and rename relevant variables
  select(
    county = COUNTY_NAM,
    percent_within,
    geometry
  ) |> 
  mutate(
    # add "County" to each county
    county = paste(county, "County"),
    # remove the [1] at the end of percent_within entries, convert to numeric
    percent_within = as.numeric(gsub("\\[\\d+\\]", "", percent_within))
  )

#------------------------------------------------------------------------------#
# number of cities in each straddling county lying fully outside the basin ----

# filter cities data to only include straddling counties
straddling_cities = all_cities |>
  rename(county = CNTY_NAME, city = MCD_NAME) |> 
  mutate(
    # add "County" to each county
    county = paste(county, "County"),
    # convert city to title case
    city = str_to_title(city)
  ) |> 
  filter(county %in% straddling_counties$county)

# cities lying at least partially within Great Lakes Basin
cities_within = st_intersection(straddling_cities, subbasins)

# cities lying fully outside the basin, but within a straddling county
cities_outside = straddling_cities |> 
  filter(!city %in% cities_within$city)

# calculate number of cities lying fully outside basin in each straddling county
cities_per_county = cities_outside |> 
  # convert city names to title case
  distinct(county, city) |> 
  group_by(county) |> 
  summarize(n_cities_outside = n())

# merge with straddling counties sf object
straddling_counties = left_join(straddling_counties, cities_per_county, by = "county")
  
# replace NAs with 0
straddling_counties = straddling_counties |> 
  mutate(n_cities_outside = ifelse(is.na(n_cities_outside), 0, n_cities_outside))

#------------------------------------------------------------------------------#

# merge each county's subbasin
straddling_counties = left_join(straddling_counties, county_subbasin, by = "county")

# drop Vilas County observation with Lake Michigan subbasin (mostly irrelevant)
straddling_counties = straddling_counties |> 
  filter(!(county == "Vilas County" & subbasin == "lk_mich"))

#------------------------------------------------------------------------------#

st_write(
  straddling_counties, 
  "~/562-Project/clean-data/straddling-counties/straddling_counties.shp",
  append = FALSE
)
