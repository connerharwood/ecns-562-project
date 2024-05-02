library(tidyverse)
library(logistf)

masterdata_subset = readRDS("~/562-Project/clean-data/masterdata_subset.rds")

#------------------------------------------------------------------------------#
# functional form ----

# histograms of explanatory variables
ggplot(masterdata_subset, aes(x = population)) +
  geom_histogram(bins = 20)
ggplot(masterdata, aes(x = log(population))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata_subset, aes(x = permits)) +
  geom_histogram(bins = 90)
ggplot(masterdata_subset, aes(x = log(permits))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata_subset, aes(x = establishments)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(establishments))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata_subset, aes(x = percent_within)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(percent_within))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata_subset, aes(x = cities_outside)) +
  geom_histogram(bins = 70)
ggplot(masterdata, aes(x = log(cities_outside))) +
  geom_histogram(bins = 70) # don't transform

ggplot(masterdata_subset, aes(x = sources)) +
  geom_histogram(bins = 90)
ggplot(masterdata_subset, aes(x = log(sources + 1))) +
  geom_histogram(bins = 90) # don't transform

ggplot(masterdata_subset, aes(x = radium)) +
  geom_histogram(bins = 90)
ggplot(masterdata_subset, aes(x = log(radium + 1))) +
  geom_histogram(bins = 90) # don't transform

#------------------------------------------------------------------------------#

# change proposal to 1 for every year after initial proposal
masterdata_subset2 = masterdata_subset 
  mutate(
    # binary variable indicating when a county submitted a diversion proposal
    proposal = case_when(
      # Waukesha diversion (2010) and New Berlin diversion (2006)
      county == "Waukesha County" & year >= 2006 ~ 1,
      # City of Racine diversion
      county == "Racine County" & year >= 2018 ~ 1,
      # Somers diversion (2021) and Pleasant Prairie diversion (2010)
      county == "Kenosha County" & year >= 2010 ~ 1,
      TRUE ~ 0
    ),
  )

# cross-sectional data
masterdata_subset3 = masterdata_subset |> 
  filter(year %in% c(2006, 2010, 2018, 2021))

#------------------------------------------------------------------------------#
# regressions

# regular Firth penalized logistic regression
firth = logistf(
  proposal ~ population + permits + establishments + percent_within + sources + radium + factor(year) + factor(county),
  data = masterdata_subset,
  control = logistf.control(maxit = 1000)
)
summary(firth)

# FLAC modification of Firth
flac = flac(
  proposal ~ population + permits + establishments + percent_within + sources + radium,
  data = masterdata_subset,
  control = logistf.control(maxit = 1000)
)
summary(flac)

# FLIC modification of Firth
flic = flic(
  proposal ~ population + permits + establishments + percent_within + sources + radium,
  data = masterdata_subset,
  control = logistf.control(maxit = 1000)
)
summary(flic)

# regular logistic regression
logit = logistf(
  proposal ~ population + permits + establishments + percent_within + sources + radium,
  data = masterdata_subset,
  control = logistf.control(maxit = 1000),
  firth = FALSE
)
summary(logit)
