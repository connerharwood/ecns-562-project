library(tidyverse)
library(sf)
library(units)
library(stringr)

straddlistringrstraddling_counties = st_read("~/562-Project/clean-data/straddling-counties/straddling_counties.shp")
cities = st_read("~/562-Project/raw-data/cities-shapefile/wisconsin_cities.shp")
basin = st_read("~/562-Project/raw-data/great-lakes-shapefiles/subbasins/subbasins.shp")

straddling_counties = straddling_counties |> st_make_valid() |> st_transform(crs = 4326)
cities = cities |> st_make_valid() |> st_transform(crs = 4326)
basin = basin |> st_make_valid() |> st_transform(crs = 4326)

#------------------------------------------------------------------------------#

straddling_cities = cities |> 
  select(
    fips = COUSUBFP,
    county = CNTY_NAME,
    city = MCD_NAME,
    CTV
  ) |> 
  mutate(
    county = paste(county, "County"),
    city = str_to_title(city)
  ) |> 
  filter(county %in% straddling_counties$county)

#------------------------------------------------------------------------------#
# percent of each city lying within basin ----

# cities lying within GL Basin
basin_cities = st_intersection(straddling_cities, basin)

# calculate area of each city
straddling_cities$area = set_units(st_area(straddling_cities), "miles^2")

# calculate area of basin cities lying within basin
basin_cities$area = set_units(st_area(basin_cities), "miles^2")

# sum up each city's partial area
basin_cities1 = basin_cities |> 
  group_by(fips) |> 
  summarize(area = sum(area)) |> 
  st_drop_geometry() |> 
  rename(basin_area = area)

straddling_cities1 = straddling_cities |>
  group_by(fips, city) |> 
  summarize(area = sum(area)) |> 
  left_join(basin_cities1, by = "fips") |> 
  mutate(
    basin_area = ifelse(is.na(basin_area), 0, basin_area),
    percent_within = basin_area / area,
    percent_within = as.numeric(str_extract(percent_within, "\\d+\\.?\\d*"))
  ) |> 
  select(fips, city, percent_within, geometry)

straddling_cities = straddling_cities1

#------------------------------------------------------------------------------#

saveRDS(straddling_cities, "~/562-Project/clean-data/straddling_cities.rds")
