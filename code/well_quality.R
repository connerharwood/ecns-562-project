library(tidyverse)
library(readxl)

# wells and well water quality data
well_quality_raw1 = read_xlsx("~/562-Project/raw-data/wells/well_quality1.xlsx")
well_quality_raw2 = read_xlsx("~/562-Project/raw-data/wells/well_quality2.xlsx")
well_quality_raw = rbind(well_quality_raw1, well_quality_raw2)

#------------------------------------------------------------------------------#
# cleaning ----

well_quality1 = well_quality_raw |> 
  # select and rename relevant variables
  select(
    well_id = `WI Unique Well #`,
    county = County,
    well_use = `Well Use`,
    well_status = `Well Status`,
    sample_id = `Labslip # / Sample ID`,
    sample_date = `Sample Collection Date`,
    param_description = `Storet Parameter Description`,
    param_code = `Storet Parameter Code`,
    sample_result = `Sample Analytical Result Amount`,
    unit = Units,
    sample_qualifier = `Sample Analytical Qualifier`,
    standard = `Enforcement Standard`,
    limit = `Preventive Action Limit`,
  ) |> 
  mutate(
    # add "County" to each county name
    county = paste(county, "County"),
    well_use = str_to_sentence(well_use),
    well_status = str_to_sentence(well_status),
    # extract year from date, convert to numeric
    sample_date = as.numeric(substr(sample_date, 1, 4)),
    param_description = str_to_sentence(param_description),
    unit = str_to_lower(unit),
    sample_qualifier = str_to_sentence(sample_qualifier)
  )

# check how much each measurement unit for each sample parameter is used
units = well_quality2 |> 
  group_by(param_description, unit) |> 
  summarize(unit_count = n()) |> 
  arrange(param_description)

# additional cleaning
well_quality2 = well_quality1 |> 
  # remove NAs
  filter(
    !is.na(unit) & 
    !is.na(sample_result) & 
    !(param_description == "Arsenic total" & unit == "ug/l") &
    !(param_description == "Copper total" & unit == "mg/l") &
    !(param_description == "Hardness total caco3" & unit == "ug/l") &
    !(param_description == "Iron total" & unit == "ug/l") &
    !(param_description == "Lead" & unit == "mg/l") &
    !(param_description == "Manganese" & unit == "mg/l") &
    !(param_description == "Ph field" & unit == "units") &
    !(param_description == "Phosphorus total" & unit == "ug/l")
  )

yearly = well_quality2 |> 
  group_by(county, sample_date, param_description) |> 
  summarize(n = n())

year_counts = well_quality2 |> 
  group_by(param_description, param_code) |> 
  summarize(n_years = n_distinct(sample_date))
