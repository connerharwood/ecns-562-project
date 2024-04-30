library(tidyverse)
library(logistf)

masterdata = readRDS("~/562-Project/clean-data/masterdata.rds")

#------------------------------------------------------------------------------#
# functional form ----

# histograms of explanatory variables
ggplot(masterdata, aes(x = population)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(population))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata, aes(x = permits)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(permits))) +
  geom_histogram(bins = 90) # transform

ggplot(masterdata, aes(x = establishments)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(establishments))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata, aes(x = percent_within)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(percent_within))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata, aes(x = cities_outside)) +
  geom_histogram(bins = 70)
ggplot(masterdata, aes(x = log(cities_outside))) +
  geom_histogram(bins = 70) # don't transform

ggplot(masterdata, aes(x = sources)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(sources))) +
  geom_histogram(bins = 90) # transform

ggplot(masterdata, aes(x = radium)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(radium + 1))) +
  geom_histogram(bins = 90) # transform

#------------------------------------------------------------------------------#
# regressions

firth = logistf(
  proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium,
  data = masterdata,
  control = logistf.control(maxit = 1000)
)
summary(firth)

firth = logistf(
  proposal ~ establishments + percent_within + cities_outside + log(sources) + log(radium + 1),
  data = masterdata,
  control = logistf.control(maxit = 1000)
)
summary(firth)











firth = logistf(
  proposal ~ log(permits) + establishments + percent_within + cities_outside + log(sources) + log(radium + 1),
  data = masterdata,
  control = logistf.control(maxit = 1000)
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
