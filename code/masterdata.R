library(tidyverse)
library(sf)

straddling_counties = st_read("~/562-Project/clean-data/straddling-counties/straddling_counties.shp")
fred1997_2022 = readRDS("~/562-Project/clean-data/fred.rds")

# merge
masterdata_raw = left_join(fred1997_2022, straddling_counties, by = "county", relationship = "many-to-one")
