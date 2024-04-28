library(tidyverse)

# Wisconsin drinking water quality data
radium_raw = read_csv("~/562-Project/raw-data/water-quality/radium.csv")

wq_raw_raw = rbind(radium_raw, )

#------------------------------------------------------------------------------#
# cleaning ----

radium = radium_raw |> 
  select(
    county = County,
    year = `Sample Date`,
    contaminant = Contaminant,
    measurement = `Measured Amount`,
    units = Units,
    id = `Sample ID`,
    storet_code = `Storet Code`
  ) |> 
  mutate(
    county = paste(county, "County"),
    year = as.numeric(substr(year, 7, 10))
  )
  
yearcounty_ave = radium |> 
  group_by(county, year) |> 
  summarize(average = mean(measurement))
  
radium_226_228 = radium |> 
  
  
  
  
  
check = radium |> 
  group_by(year, id) |> 
  summarize(count = n_distinct(contaminant))
  


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
