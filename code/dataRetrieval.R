library(dataRetrieval)
library(sf)
library(tidyverse)

counties_sf = st_read("~/562-Project/clean-data/straddling-counties/straddling_counties.shp")
counties_sf = counties_sf |> st_make_valid() |> st_transform(crs = 4326) |> st_simplify()

# water quality monitoring sites in Wisconsin
wi_sites = whatWQPsites(statecode = "US:55")

# filter out sites missing coordinates
wi_sites = wi_sites |>
  filter(!is.na(LongitudeMeasure) & !is.na(LatitudeMeasure))

# make sites data sf object
wi_sites_sf = st_as_sf(wi_sites, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326) |> st_make_valid() |> st_simplify()

# sites in straddling WI counties
straddling_sites = st_intersection(wi_sites_sf, counties_sf)

straddling_sites = straddling_sites |> 
  distinct(MonitoringLocationIdentifier) |> 
  pull(MonitoringLocationIdentifier)
#------------------------------------------------------------------------------#

wq_raw = readWQPdata(
  siteid = straddling_sites,
  startDateLo = "1997-01-01",
  startDateHi = "2022-12-31",
  characteristicName = c("Coliforms")
)
