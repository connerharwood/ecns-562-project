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

radium_raw = readWQPdata(
  
)

data_list = list()

for (site in straddling_sites) {
  data = readWQPdata(site, )
}

radium_test = readWQPdata(
  siteid = "USGS-04072657",
  characteristicName = "radium",
  "startDateLo" = "1997-01-01",
  "startDateHi" = "2022-12-31"
)

#------------------------------------------------------------------------------#
# Seunghyun method ----

pacman::p_load(
  dataRetrieval,
  tidyverse,
  data.table,
  furrr,
  tigris
)

wi = counties(state = "WI")

straddling_counties = wi |> 
  filter(NAMELSAD %in% counties_sf$county)

closeAllConnections()
plan("multisession", workers = 30)

df_wi_radium <- future_map(
  .progress = T,
  straddling_counties$NAME, function(nme) {
    library(dataRetrieval)
    library(data.table)
    
    tryCatch(
      readWQPdata(
        statecode = "WI",
        countycode = nme,
        startDateLo = "1997-01-01",
        parameterCd = "09501"
      ),
      error = function(e) {
        NULL
      }
    )
  }
) %>% rbindlist()




for(site in straddling_sites) {
  data = readWQPdata(siteid = site, characteristicType = "Radiochemical", startDateLo = "1997-01-01", startDateHi = "2022-12-31")
  data_list[[site]] <- data
}

adams_radium = readWQPdata(
  statecode = "WI",
  countycode = "Waukesha",
  parameterCd = 09501,
  startDateLo = "1997-01-01"
)
