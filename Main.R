rm(list = ls())

#install.packages("readxl")
library("readxl")
library("dplyr")
data <- data.frame(read_excel("data.xlsx"))
data <- data[order(data$countriesAndTerritories, data$dateRep),]
data <- data %>%
  group_by(countriesAndTerritories) %>%
  mutate("cumulative cases" = cumsum(cases))
data <- data %>%
  group_by(countriesAndTerritories) %>%
  mutate("cumulative deaths" = cumsum(deaths))

