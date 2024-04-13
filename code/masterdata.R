library(tidyverse)
library(sf)

straddling_counties = st_read("~/562-Project/clean-data/straddling-counties/straddling_counties.shp")
fred1997_2022 = 