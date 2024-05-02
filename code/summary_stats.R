library(tidyverse)
library(sf)
library(xtable)

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

#------------------------------------------------------------------------------#
# kenosha ----

kenosha = masterdata_changes |> 
  filter(county == "Kenosha County") |> 
  mutate(
    pop = population*100,
    permits = permits*100,
    establishments = establishments*100,
    percent_within = percent_within*100,
    sources = sources*100
  )

ggplot(kenosha, aes(x = year)) +
  geom_line(aes(y = pop, color = "Population")) +
  geom_line(aes(y = permits, color = "Housing Permits")) +
  geom_line(aes(y = establishments, color = "Private Establishments")) +
  geom_line(aes(y = sources, color = "Withdrawal Sources")) +
  geom_vline(xintercept = c(2010, 2021), linetype = "dashed", color = "black") +
  labs(title = "Kenosha County (2010, 2021)",
       x = "Year",
       y = "% Change") +
  scale_color_manual(values = c(
    "Population" = "blue",
    "Housing Permits" = "red",
    "Private Establishments" = "orange",
    "Withdrawal Sources" = "green"
  ), name = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------------------------#
# waukesha ----

waukesha = masterdata_changes |> 
  filter(county == "Waukesha County") |> 
  mutate(
    pop = population*100,
    permits = permits*100,
    establishments = establishments*100,
    percent_within = percent_within*100,
    sources = sources*100
  )

ggplot(waukesha, aes(x = year)) +
  geom_line(aes(y = pop, color = "Population")) +
  geom_line(aes(y = permits, color = "Housing Permits")) +
  geom_line(aes(y = establishments, color = "Private Establishments")) +
  geom_line(aes(y = sources, color = "Withdrawal Sources")) +
  geom_vline(xintercept = c(2006, 2010), linetype = "dashed", color = "black") +
  labs(title = "Waukesha County (2006, 2010)",
       x = "Year",
       y = "% Change") +
  scale_color_manual(values = c(
    "Population" = "blue",
    "Housing Permits" = "red",
    "Private Establishments" = "orange",
    "Withdrawal Sources" = "green"
  ), name = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------------------------#
# racine ----

racine = masterdata_changes |> 
  filter(county == "Racine County") |> 
  mutate(
    pop = population*100,
    permits = permits*100,
    establishments = establishments*100,
    percent_within = percent_within*100,
    sources = sources*100
  )

ggplot(racine, aes(x = year)) +
  geom_line(aes(y = pop, color = "Population")) +
  geom_line(aes(y = permits, color = "Housing Permits")) +
  geom_line(aes(y = establishments, color = "Private Establishments")) +
  geom_line(aes(y = sources, color = "Withdrawal Sources")) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  labs(title = "Racine County (2018)",
       x = "Year",
       y = "% Change") +
  scale_color_manual(values = c(
    "Population" = "blue",
    "Housing Permits" = "red",
    "Private Establishments" = "orange",
    "Withdrawal Sources" = "green"
  ), name = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
