library(tidyverse)
library(sf)
library("xtable")

masterdata = readRDS("~/562-Project/clean-data/masterdata.rds")
masterdata_changes = readRDS("~/562-Project/clean-data/masterdata_subset.rds")

#------------------------------------------------------------------------------#
masterdata_subset = masterdata |> 
  filter(
    county %in% c(
      "Kenosha County",
      "Racine County",
      "Waukesha County",
      "Washington County",
      "Green Lake County",
      "Fond du Lac County",
      "Waushara County"
    )
  )

summary = masterdata_subset |> 
  group_by(county) |> 
  summarize(
    population = mean(population),
    permits = mean(permits),
    establishments = mean(establishments),
    basin = mean(percent_within),
    sources = mean(sources),
    radium = mean(radium)
  )

changes_summary = masterdata_changes |> 
  group_by(county) |> 
  summarize(
    population = mean(population)*100,
    permits = mean(permits)*100,
    establishments = mean(establishments)*100,
    basin = mean(percent_within)*100,
    sources = mean(sources)*100,
    radium = mean(radium)
  ) |>
  mutate(
    proposal = case_when(
      county == "Kenosha County" ~ "2010, 2021",
      county == "Waukesha County" ~ "2006, 2010",
      county == "Racine County" ~ "2018",
      TRUE ~ "-"
    )
  ) |> 
  select(county, proposal, population, permits, establishments, basin, sources, radium) |> 
  arrange(county)

latex_code = xtable(changes_summary, caption = "1997-2022 Summary Statistics", digits = 3)

print(latex_code)
