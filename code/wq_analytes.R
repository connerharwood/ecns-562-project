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



# raw well water quality data for different contaminant types
bacteria_raw = read_xlsx("~/562-Project/raw-data/wells/bacteria.xlsx")
nitrate1_raw = read_xlsx("~/562-Project/raw-data/wells/nitrate1.xlsx")
nitrate2_raw = read_xlsx("~/562-Project/raw-data/wells/nitrate2.xlsx")
pesticides_raw = read_xlsx("~/562-Project/raw-data/wells/pesticides.xlsx")
voc1_raw = read_xlsx("~/562-Project/raw-data/wells/voc1.xlsx")
voc2_raw = read_xlsx("~/562-Project/raw-data/wells/voc2.xlsx")
other_raw = read_xlsx("~/562-Project/raw-data/wells/other.xlsx")

# append into one dataset
wq_raw = rbind(
  bacteria_raw,
  nitrate1_raw,
  nitrate2_raw,
  pesticides_raw,
  voc1_raw,
  voc2_raw,
  other_raw
)

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
  filter(!is.na(sample_result) & !is.na(unit))

# parameters with samples in all years for all counties
year_counts = wq1 |> 
  group_by(param, param_code) |> 
  summarize(n = n_distinct(year)) |> 
  filter(n == 26)
