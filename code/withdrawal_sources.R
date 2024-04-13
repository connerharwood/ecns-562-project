library(tidyverse)

sources_raw = read_csv("~/562-Project/raw-data/withdrawal_sources.csv")

#------------------------------------------------------------------------------#

sources = sources_raw |> 
  select(county, hicap_num, water_use, approval_date, construction_date, well_num, status, source, max_approved_gpm) |> 
  mutate(
    # add "County" to each county entry
    county = paste0(county, " County"),
    # convert date variables to date format
    approval_date = as.Date(approval_date, format = "%m/%d/%y"),
    construction_date = as.Date(construction_date, format = "%m/%d/%y"),
    # extract year, convert to numeric
    approval_year = year(approval_date),
    construction_year = year(construction_date)
  )
