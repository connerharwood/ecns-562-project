library(tidyverse)
library(logistf)

masterdata = readRDS("~/562-Project/clean-data/masterdata.rds")

#------------------------------------------------------------------------------#

firth = logistf(
  approval ~ population + permits + establishments + percent_within + sources + radium + year,
  data = masterdata
)
summary(firth)

firth = logistf(
  approval ~ population + percent_within + permits + establishments + radium,
  data = masterdata,
  alpha = 0.90
)
summary(firth)


firth = logistf(
  approval ~ population + income + unemp + permits + establishments + percent_within + sources + radium,
  data = masterdata
)
summary(firth)

firth2 = logistf(
  approval ~ population + income + u_rate + percent_within + n_cities_outside + sources_per_mi2,
  data = lk_mi_data,
  control = logistf.control(maxit = 1000),
  flic = TRUE
)
summary(firth2)
