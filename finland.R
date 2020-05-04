rm(list = ls())
setwd("~/GitHub/COVID-19-analysis")


library(tidyverse)
library(jsonlite)
library(ggplot2)

#Read in the material
tests_raw <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=dateweek2020010120201231-443686&row=hcdmunicipality2020-445222&column=measure-445356",sep=";", encoding = "UTF-8")
cases_raw <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=dateweek2020010120201231-443686&row=hcdmunicipality2020-445222&column=measure-444833",sep=";", encoding = "UTF-8")

tests_raw <- tests_raw %>%
  mutate("id" = paste(Time,Area, sep = " "))
cases_raw <- cases_raw %>%
  mutate("id" = paste(Time,Area, sep = " "))

data <- inner_join(tests_raw, cases_raw, by="id") %>%
  select(-ends_with("y"),"val.y", -"id", -"Measure.x") %>%
  rename(Tests = val.x) %>%
  rename(Cases = val.y) %>%
  rename(Time = Time.x) %>%
  rename(Area = Area.x) %>%
  filter(!is.na(Cases) | !is.na(Tests)) %>%
  filter(Time != "Time")
  
data_all <- data %>%
  filter(Area == "All areas") %>%
  select(-Area)

data_test <- data %>%
  filter(!is.na(Tests))

coeff = 1/0.03

tests_vs_cases <- ggplot(data=data_all, aes(x=Time, group = 1, ) ) +
                  geom_line(aes(y=Cases)) + geom_text(aes(label=Cases, y=Cases)) + 
                  geom_line(aes(y=Tests/coeff), col="Red") + geom_text(aes(label=Tests, y=Tests/coeff)) +
                  scale_y_continuous(name = "Cases",sec.axis = sec_axis( trans=~.*coeff, name="Tests")) +
                  geom_ribbon(data=subset(data_all, Cases >= Tests/coeff), aes(ymin=(Tests/coeff),ymax=Cases), fill="red", alpha=0.25) +
                  geom_ribbon(data=subset(data_all, Cases < Tests/coeff), aes(ymax=(Tests/coeff),ymin=Cases), fill="green", alpha=0.25)
tests_vs_cases


