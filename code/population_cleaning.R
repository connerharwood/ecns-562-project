library(tidyverse)
library(readxl)

pop2000_2010_raw = read_csv("~/562-Project/raw-data/city-populations/population2000_2010.csv")
pop2010_2020_raw = read_csv("~/562-Project/raw-data/city-populations/population2010_2020.csv")
pop2020_2022_raw = read_xlsx("~/562-Project/raw-data/city-populations/population2020_2022.xlsx")

#------------------------------------------------------------------------------#
# 2000-2010 ----

pop1_raw = pop2000_2010_raw |> 
  filter(STATE == 55)
