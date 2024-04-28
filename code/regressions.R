library(tidyverse)
library(logistf)

firth = logistf(
  approval ~ population + income + u_rate + permits + establishments + percent_within + n_cities_outside + sources_per_mi2,
  data = masterdata_raw,
  control = logistf.control(maxit = 1000),
  flic = TRUE
)
summary(firth)

lk_mi_data = masterdata_raw |> 
  filter(subbasin != "lk_sup") |> 
  filter(year > 2000)

firth2 = logistf(
  approval ~ population + income + u_rate + percent_within + n_cities_outside + sources_per_mi2,
  data = lk_mi_data,
  control = logistf.control(maxit = 1000),
  flic = TRUE
)
summary(firth2)
