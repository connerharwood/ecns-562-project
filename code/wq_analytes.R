library(tidyverse)
library(readxl)

# raw well water quality data
wq1_raw = read_xlsx("~/562-Project/raw-data/wells/wq1.xlsx")
wq2_raw = read_xlsx("~/562-Project/raw-data/wells/wq2.xlsx")
wq3_raw = read_xlsx("~/562-Project/raw-data/wells/wq3.xlsx")
wq4_raw = read_xlsx("~/562-Project/raw-data/wells/wq4.xlsx")
wq5_raw = read_xlsx("~/562-Project/raw-data/wells/wq5.xlsx")

# append into one dataset
wq_raw = rbind(wq1_raw, wq2_raw, wq3_raw, wq4_raw, wq5_raw)

#------------------------------------------------------------------------------#
# cleaning ----

wq1 = wq_raw |> 
  # select and rename relevant variables
  select(
    well_id = `WI Unique Well #`,
    county = County,
    well_use = `Well Use`,
    well_status = `Well Status`,
    sample_id = `Labslip # / Sample ID`,
    year = `Sample Collection Date`,
    param = `Storet Parameter Description`,
    param_code = `Storet Parameter Code`,
    sample_result = `Sample Analytical Result Amount`,
    unit = Units,
    sample_qualifier = `Sample Analytical Qualifier`
  ) |> 
  mutate(
    # add "County" to each county name
    county = paste(county, "County"),
    well_use = str_to_sentence(well_use),
    well_status = str_to_sentence(well_status),
    # extract year from date, convert to numeric
    year = as.numeric(substr(year, 1, 4)),
    param = str_to_sentence(param),
    unit = str_to_lower(unit),
    sample_qualifier = str_to_sentence(sample_qualifier)
  ) |> 
  # remove NAs
  filter(!is.na(sample_result) & !is.na(unit) &!is.na(param))

# parameters with samples in all years for all counties
complete_params = wq1 |> 
  group_by(county, param, param_code) |> 
  summarize(n_years = n_distinct(year)) |> 
  filter(n_years >= 25)

complete_params2 = complete_params |> 
  group_by(param, param_code) |> 
  summarize(n_counties = n_distinct(county))

# parameters with samples in all years for all counties
complete_params = wq1 |> 
  group_by(county, param, param_code) |> 
  summarize(n = n_distinct(year)) |> 
  ungroup() |> 
  group_by(param, param_code) |> 
  summarize(n_counties = n_distinct(county))

# filter to include only potentially relevant parameters
wq2 = wq1 |> 
  filter(
    param_code %in% c(
      "410", "1002", "39033", "940", "99060", "95", "1042", "720", "951", "39941", "900", "74010", "1045", "1051", "1055",
      "71900", "618", "620", "39516", "403", "1501", "3501", "82303", "1147", "945", "1092", "11503"
    )
  )

# check how much each measurement unit for each sample parameter is used
units = wq2 |> 
  group_by(param, param_code, unit) |> 
  summarize(unit_count = n()) |> 
  arrange(param)

# filter out observations with inconsistent units better left dropped rather than converted
wq3 = wq2 |> 
  filter(
    !(param_code == "95" & unit == "fib/l") &
    !(param_code == "95" & unit == "mg/l") &
    !(param_code == "95" & unit == "ug/l") &
    !(param_code == "95" & unit == "units") &
    !(param_code == "403" & sample_result == 0)
  )

wq4 = wq3 |> 
  mutate(
    # convert between ug/l and mg/l for certain parameters
    sample_result = case_when(
      param_code == "1002" & unit == "mg/l" ~ sample_result*1000,
      param_code == "1042" & unit == "mg/l" ~ sample_result*1000,
      param_code == "720" & unit == "ug/l" ~ sample_result/1000,
      param_code == "900" & unit == "ug/l" ~ sample_result/1000,
      param_code == "74010" & unit == "ug/l" ~ sample_result/1000,
      param_code == "1045" & unit == "ug/l" ~ sample_result/1000,
      param_code == "1051" & unit == "mg/l" ~ sample_result*1000,
      param_code == "1055" & unit == "mg/l" ~ sample_result*1000,
      param_code == "71900" & unit == "ug/l" ~ sample_result/1000,
      param_code == "618" & unit == "ug/l" ~ sample_result/1000,
      param_code == "620" & unit == "ug/l" ~ sample_result/1000,
      param_code == "1147" & unit == "ug/l" ~ sample_result/1000,
      param_code == "1092" & unit == "ug/l" ~ sample_result/1000,
      TRUE ~ sample_result
    ),
    # align units for parameters
    unit = case_when(
      param_code == "1002" & unit == "mg/l" ~ "ug/l",
      param_code == "99060" & sample_result == 0 ~ "absent",
      param_code == "99060" & sample_result == 1 ~ "present",
      param_code == "95" & unit == "umhos/cm" ~ "umho/cm",
      param_code == "95" & unit == "us/cm" ~ "umho/cm",
      param_code == "1042" & unit == "mg/l" ~ "ug/l",
      param_code == "720" & unit == "ug/l" ~ "mg/l",
      param_code == "900" & unit == "ug/l" ~ "mg/l",
      param_code == "74010" & unit == "ug/l" ~ "mg/l",
      param_code == "1045" & unit == "ug/l" ~ "mg/l",
      param_code == "1051" & unit == "mg/l" ~ "ug/l",
      param_code == "1055" & unit == "mg/l" ~ "ug/l",
      param_code == "71900" & unit == "ug/l" ~ "mg/l",
      param_code == "618" & unit == "ug/l" ~ "mg/l",
      param_code == "620" & unit == "ug/l" ~ "mg/l",
      param_code == "620" & unit == "mg/l as n" ~ "mg/l",
      param_code == "403" ~ "su",
      param_code == "1147" & unit == "ug/l" ~ "mg/l",
      param_code == "1092" & unit == "ug/l" ~ "mg/l",
      TRUE ~ unit
    ),
    # change parameter names
    param = case_when(
      param_code == "410" ~ "Alkalinity total CaCO3",
      param_code == "99060" ~ "Coliform total Colilert absent/present",
      param_code == "39941" ~ "Glyphosate (Roundup)",
      param_code == "900" ~ "Hardness total CaCO3",
      param_code == "618" ~ "Nitrogen NO3-N diss",
      param_code == "620" ~ "Nitrogen NO3-N total",
      param_code == "39516" ~ "PCB total",
      param_code == "403" ~ "pH",
      TRUE ~ param
    )
  )

# check units again
units = wq4 |> 
  group_by(param, param_code, unit) |> 
  summarize(unit_count = n()) |> 
  arrange(param)

# most relevant parameters for Wisconsin: coliform (bacteria), nitrate (NO3), arsenic, copper, lead, radium
wq5 = wq4 |> 
  filter(param_code %in% c("99060", "620", "1002", "1042", "1051", "11503"))

# units again
units = wq5 |> 
  group_by(param, param_code, unit) |> 
  summarize(unit_count = n()) |> 
  arrange(param)

# number of samples for each parameter per year and county
obs = wq5 |> 
  group_by(year, county, param, param_code) |> 
  summarize(n = n())

# there are at least 30 samples of
#------------------------------------------------------------------------------#
# averages ----

# calculate year-county averages for coliform detection
coliform = wq5 |> 
  filter(param_code == "99060") |> 
  group_by(year, county) |> 
  summarize(mean_result = mean(sample_result == 1)) |> 
  mutate(param = "Coliform total Colilert absent/present", param_code = "99060")

# calculate year-county averages for non-coliform parameters
others = wq5 |> 
  filter(param_code != "99060") |> 
  group_by(year, county, param, param_code) |> 
  summarize(mean_result = mean(sample_result))

# combine averages into one dataset
wq_averages = rbind(others, coliform) |> mutate(mean_result = as.numeric(round(mean_result, digits = 6)))

# amount of rows with only 1 observation
# 99060: 53 coliform
# 620: 18 nitrate
# 1002: 42 arsenic
# 1042: 107 copper
# 1051: 112 lead
# 11503: 52 radium

radium = wq_averages |> 
  filter(param_code == 11503) |> 
  group_by(year) |> 
  summarize(n = n_distinct(county))
coliform = wq_averages |> 
  filter(param_code == 99060) |> 
  group_by(year) |> 
  summarize(n = n_distinct(county))
