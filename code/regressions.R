library(tidyverse)
library(logistf)
library(stargazer)
library(texreg)

masterdata = readRDS("~/562-Project/clean-data/masterdata.rds")

#------------------------------------------------------------------------------#
# functional form ----

# histograms of explanatory variables
ggplot(masterdata, aes(x = population)) +
  geom_histogram(bins = 90)
ggplot(masterdata, aes(x = log(population))) +
  geom_histogram(bins = 90) # transform

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

# regular Firth penalized logistic regression
firth = logistf(
  proposal ~ lag(log(population)) + log(permits) + percent_within + log(sources) + radium,
  data = masterdata,
  control = logistf.control(maxit = 1000)
)
summary(firth)

# FLAC modification of Firth
flac = flac(
  proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium,
  data = masterdata,
  control = logistf.control(maxit = 1000)
)
summary(flac)

# FLIC modification of Firth
flic = flic(
  proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium,
  data = masterdata,
  control = logistf.control(maxit = 1000)
)
summary(flic)

# regular logistic regression
logit = logistf(
  proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium,
  data = masterdata,
  control = logistf.control(maxit = 1000),
  firth = FALSE
)
summary(logit)

#------------------------------------------------------------------------------#
# bootstrap robust standard errors ----

library(boot)
set.seed(123)  # for reproducibility

# define function to extract Firth regression's coefficients
firth_coef = function(data, indices) {
  fit <- logistf(proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium, data = data[indices, ], control = logistf.control(maxit = 1000))
  coef(fit)
}

# perform bootstrapping
firth_boot = boot(data = masterdata, statistic = firth_coef, R = 1000)

# compute robust standard errors
firth_se = apply(firth_boot$t, 2, sd)

#------------------------------------------------------------------------------#

# define function to extract Firth regression's coefficients
flac_coef = function(data, indices) {
  fit <- flac(proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium, data = data[indices, ], control = logistf.control(maxit = 1000))
  coef(fit)
}

# perform bootstrapping
flac_boot = boot(data = masterdata, statistic = flac_coef, R = 1000)

# compute robust standard errors
flac_se = apply(flac_boot$t, 2, sd)

#------------------------------------------------------------------------------#

# define function to extract Firth regression's coefficients
flic_coef = function(data, indices) {
  fit <- flic(proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium, data = data[indices, ], control = logistf.control(maxit = 1000))
  coef(fit)
}

# perform bootstrapping
flic_boot = boot(data = masterdata, statistic = flic_coef, R = 1000)

# compute robust standard errors
flic_se = apply(flic_boot$t, 2, sd)

#------------------------------------------------------------------------------#

# define function to extract Firth regression's coefficients
logit_coef = function(data, indices) {
  fit <- logistf(proposal ~ log(population) + log(permits) + percent_within + log(sources) + radium, data = data[indices, ], control = logistf.control(maxit = 1000), firth = FALSE)
  coef(fit)
}

# perform bootstrapping
logit_boot = boot(data = masterdata, statistic = logit_coef, R = 1000)

# compute robust standard errors
logit_se = apply(logit_boot$t, 2, sd)
