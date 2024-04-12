library(tidyverse)
library(sf)
library(tmap)

#------------------------------------------------------------------------------#
# ---- load data

# load Wisconsin counties shapefile
all_counties = st_read("~/OneDrive - Montana State University/Metrics Project/wisconsin/data/counties shapefile/Wisconsin.shp")

# load Great Lakes Basin shapefile
basin = st_read("~/OneDrive - Montana State University/Metrics Project/wisconsin/data/basin shapefile/subbasins.shp")

# make valid
all_counties = st_make_valid(all_counties)
basin = st_make_valid(basin)

# transform CRS of shapefiles to WGS 84
all_counties = st_transform(all_counties, crs = 4326)
basin = st_transform(basin, crs = 4326)

#------------------------------------------------------------------------------#
# ---- find straddling counties

# select Lake Superior and Lake Michigan subbasins (Wisconsin only lies in these subbasins)
basins = basin |> 
  rename(subbasin = merge) |> 
  filter(subbasin == "lk_sup" | subbasin == "lk_mich")

# counties lying at least partially within Great Lakes Basin (straddling counties)
basin_counties = st_intersection(all_counties, basins)

# counties lying fully within Great Lakes Basin
fully_within_counties = all_counties[st_within(all_counties, basins, sparse = FALSE), ]

# filter out counties lying fully with basin to include only straddling counties
straddling_counties_partial = basin_counties |> 
  filter(!COUNTY_NAM %in% fully_within_counties$COUNTY_NAM)

#------------------------------------------------------------------------------#
# ---- percent of each county lying within basin

# full polygons of straddling counties
straddling_counties_full = all_counties |> 
  filter(COUNTY_NAM %in% straddling_counties_partial$COUNTY_NAM)

# calculate full area of each county
straddling_counties_full$area = st_area(straddling_counties_full)

# calculate area of each county lying within basin
straddling_counties_partial$area = st_area(straddling_counties_partial)

# sum up each county's partial area (some counties have multiple geometries)
straddling_counties_partial2 = straddling_counties_partial |> 
  group_by(COUNTY_NAM) |> 
  summarize(area = sum(area)) |> 
  # sort by county name
  arrange(COUNTY_NAM)

straddling_counties = straddling_counties_full |> 
  # sort by county name
  arrange(COUNTY_NAM) |> 
  # calculate percent of each county lying within Great Lakes Basin
  mutate(percent_within = straddling_counties_partial2$area / area) |> 
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

st_write(
  straddling_counties, 
  "~/OneDrive - Montana State University/Metrics Project/wisconsin/data/straddling counties shapefile/straddling_counties.shp",
  append = FALSE
)
