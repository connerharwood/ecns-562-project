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
  ) |> 
  # calculate area in square miles of each county
  mutate(area = units::set_units(st_area(straddling_counties), mi^2))

# merge
merge1 = left_join(fred1997_2022, straddling_counties, by = "county", relationship = "many-to-one")
merge2 = left_join(merge1, withdrawal_sources, by = c("county", "year"), relationship = "one-to-one")
merge3 = left_join(merge2, radium, by = c("county", "year"), relationship = "one-to-one")

masterdata_raw = merge3 |> 
  mutate(
    # binary variable indicating when a county submitted a diversion proposal
    approval = case_when(
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
    ),
    # remove unit from area entries
    area = as.numeric(str_replace(area, "\\s*\\[mi\\^2\\]", "")),
    # calculate number of public drinking water sources per square mile
    sources_per_mi2 = sources / area
  )

masterdata = masterdata_raw |> 
  select(
    county,
    year,
    population,
    income,
    u_rate,
    housing_permits = permits,
    private_establishments = establishments,
    percent_within,
    cities_outside = n_cities_outside,
    sources_mi2 = sources_per_mi2,
    radium_ave = ra_average,
    approval,
    geometry
  )

#------------------------------------------------------------------------------#

# save masterdata
saveRDS(masterdata, file = "~/562-Project/clean-data/masterdata.rds")
