library(tidyverse)
library(sf)
library(tmap)

straddling_counties = st_read("~/562-Project/clean-data/straddling-counties/straddling_counties.shp")
fred1997_2022 = readRDS("~/562-Project/clean-data/fred.rds")
withdrawal_sources = readRDS("~/562-Project/clean-data/withdrawal_sources.rds")



# merge
masterdata_raw = left_join(fred1997_2022, straddling_counties, by = "county", relationship = "many-to-one")

