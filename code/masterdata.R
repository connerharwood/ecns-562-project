library(tidyverse)
library(sf)

# load individual datasets
straddling_counties = st_read("~/562-Project/clean-data/straddling-counties/straddling_counties.shp")
fred1997_2022 = readRDS("~/562-Project/clean-data/fred.rds")
withdrawal_sources = readRDS("~/562-Project/clean-data/withdrawal_sources.rds")
radium = readRDS("~/562-Project/clean-data/radium.rds")

#------------------------------------------------------------------------------#

straddling_counties = straddling_counties |> 
  # rename shortened column names
  rename(
    percent_within = prcnt_w,
    n_cities_outside = n_cts_t,
    subbasin = subbasn
  )

# merge
merge1 = left_join(fred1997_2022, straddling_counties, by = "county", relationship = "many-to-one")
merge2 = left_join(merge1, withdrawal_sources, by = c("county", "year"), relationship = "one-to-one")
merge3 = left_join(merge2, radium, by = c("county", "year"), relationship = "one-to-one")

masterdata_raw = merge3 |> 
  mutate(
    # binary variable indicating when a county submitted a diversion proposal
    proposal = case_when(
      # City of Waukesha diversion
      county == "Waukesha County" & year == 2010 ~ 1,
      # City of New Berlin diversion
      county == "Waukesha County" & year == 2006 ~ 1,
      # City of Racine diversion
      county == "Racine County" & year == 2018 ~ 1,
      # Village of Somers diversion
      county == "Kenosha County" & year == 2021 ~ 1,
      # Village of Pleasant Prairie
      county == "Kenosha County" & year == 2010 ~ 1,
      
      TRUE ~ 0
    )
  )

masterdata = masterdata_raw |> 
  select(
    county,
    year,
    population,
    income,
    unemp = u_rate,
    permits,
    establishments,
    percent_within,
    cities_outside = n_cities_outside,
    sources,
    radium = ra_max,
    proposal,
    geometry
  )

#------------------------------------------------------------------------------#

# save masterdata
saveRDS(masterdata, file = "~/562-Project/clean-data/masterdata.rds")


#------------------------------------------------------------------------------#
# radium belt subset ----
# filter to only include counties in Wisconsin's radium belt, and only counties that are more than 1% or less than 99% in the basin 
masterdata_subset = masterdata |> 
  filter(
    county %in% c(
      "Kenosha County",
      "Racine County",
      "Waukesha County",
      "Washington County",
      "Green Lake County",
      "Fond du Lac County",
      "Waushara County"
    )
  ) |> 
  mutate(
    population = (population - lag(population)) / lag(population),
    permits = (permits - lag(permits)) / lag(permits),
    sources = (sources - lag(sources)) / lag(sources),
    establishments = (establishments - lag(establishments)) / lag(establishments)
  ) |> 
  filter(year > 1997)

saveRDS(masterdata_subset, file = "~/562-Project/clean-data/masterdata_subset.rds")
