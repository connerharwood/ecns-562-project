library(tidyverse)
library(sf)
library(tmap)

straddling_counties = st_read("~/562-Project/clean-data/straddling-counties/straddling_counties.shp")
fred1997_2022 = readRDS("~/562-Project/clean-data/fred.rds")
withdrawal_sources = readRDS("~/562-Project/clean-data/withdrawal_sources.rds")

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

masterdata_raw = merge2 |> 
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
      
      TRUE ~ 0
    ),
    area = as.numeric(str_replace(area, "\\s*\\[mi\\^2\\]", "")),
    sources_per_mi2 = sources / area
  )

# other diversions (with POSSIBLE proposal dates):
# Menominee Falls: 2005
# City of Kenosha: 2007

firth = logistf(
  approval ~ population + income + u_rate + permits + establishments + percent_within + n_cities_outside + sources_per_mi2,
  data = masterdata_raw,
  control = logistf.control(maxit = 1000),
  flic = TRUE
)
summary(firth)

lk_mi_data = masterdata_raw |> 
  filter(subbasin != "lk_sup") |> 
  filter(year > 2000)

firth2 = logistf(
  approval ~ population + income + u_rate + percent_within + n_cities_outside + sources_per_mi2,
  data = lk_mi_data,
  control = logistf.control(maxit = 1000),
  flic = TRUE
)
summary(firth2)
