library(tidyverse)
library(lubridate)

sources_raw = read_csv("~/562-Project/raw-data/withdrawal_sources.csv")

#------------------------------------------------------------------------------#

sources = sources_raw |> 
  # select and rename relevant variables
  select(
    county, 
    hicap_num, 
    water_use, 
    approval_year = approval_date, 
    construction_year = construction_date, 
    abandoned_year, 
    well_num, 
    status, 
    source, 
    max_approved_gpm
  ) |> 
  mutate(
    # add "County" to each county entry
    county = paste0(county, " County"),
    # convert date variables to date format
    approval_year = as.Date(approval_year, format = "%m/%d/%y"),
    construction_year = as.Date(construction_year, format = "%m/%d/%y"),
    # extract year, convert to numeric
    approval_year = year(approval_year),
    construction_year = year(construction_year),
    # subtract 100 from years past 2024
    approval_year = ifelse(approval_year > 2024, approval_year - 100, approval_year),
    construction_year = ifelse(construction_year > 2024, construction_year - 100, construction_year)
  )

