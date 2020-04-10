rm(list = ls())

#install.packages("readxl","dplyr")
library("readxl")
library("dplyr")
library("ggplot2")

data <- data.frame(read_excel("data.xlsx"))
data <- data[order(data$countriesAndTerritories, data$dateRep),]
data <- data %>%
  group_by(countriesAndTerritories) %>%
  mutate("cumulative cases" = cumsum(cases))
data <- data %>%
  group_by(countriesAndTerritories) %>%
  mutate("cumulative deaths" = cumsum(deaths))

country = "Finland"

### Plot the new cases of the country
new_cases_per_country<-ggplot(data=data[data$countriesAndTerritories == country,], aes(x=dateRep, y=cases)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=cases))
new_cases_per_country
###
