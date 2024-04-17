library(tidyverse)

sources_raw = read_csv("~/562-Project/raw-data/withdrawal_sources.csv")

#------------------------------------------------------------------------------#

sources = sources_raw |> 
  # select variables
  select(
    county, 
    hicap_num, 
    water_use, 
    approval_year,
    construction_year,
    abandoned_year, 
    well_num, 
    status, 
    source, 
    max_approved_gpm = max_approved
  ) |> 
  mutate(
    # add "County" to each county entry
    county = paste0(county, " County"),
    # convert date variables to date format
    approval_year = as.Date(approval_year, format = "%A, %B %d, %Y"),
    construction_year = as.Date(construction_year, format = "%A, %B %d, %Y"),
    # extract year, convert to numeric
    approval_year = year(approval_year),
    construction_year = year(construction_year),
    # choose maximum between approval year and construction year as well's start date
    active_year = pmax(approval_year, construction_year, na.rm = TRUE),
    # fill missing unique well ID with their high capacity ID
    well_num = ifelse(is.na(well_num), hicap_num, well_num)
  ) |> 
  # filter out wells not yet drilled and those missing year started
  filter(status != "Not yet drilled" & !is.na(active_year)) |> 
  select(county, well_num, status, water_use, source, active_year, abandoned_year, max_approved_gpm)

# every combination of well and year
combos = expand_grid(active_year = 1997:2022, well_num = unique(sources$well_num))

# create long format panel dataset with well information
sources2 = left_join(combos, sources, by = "well_num") |> 
  rename(year = `active_year.x`, active_year = `active_year.y`)

# create variable indicating whether a well is active in a given year
sources3 = sources2 |> 
  mutate(is_active = ifelse(active_year <= year & (is.na(abandoned_year) | year < abandoned_year), 1, 0))

# calculate number of withdrawal sources per county per year
withdrawal_sources = sources3 |> 
  group_by(county, year) |> 
  summarize(sources = sum(is_active))

#------------------------------------------------------------------------------#

saveRDS(withdrawal_sources, "~/562-Project/clean-data/withdrawal_sources.rds")
