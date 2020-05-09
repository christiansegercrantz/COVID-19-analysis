rm(list = ls())
setwd("~/GitHub/COVID-19-analysis")


library(tidyverse)
library(jsonlite)
library(ggplot2)
library(purrr)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Read in the material
tests_raw <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=dateweek2020010120201231-443686&row=hcdmunicipality2020-445222&column=measure-445356",sep=";", encoding = "UTF-8")
cases_raw <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=dateweek2020010120201231-443686&row=hcdmunicipality2020-445222&column=measure-444833",sep=";", encoding = "UTF-8")
genders <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?column=sex-444328",sep=";", encoding = "UTF-8")


tests_raw <- tests_raw %>%
  mutate("id" = paste(Time, Area, sep = " "))
cases_raw <- cases_raw %>%
  mutate("id" = paste(Time, Area, sep = " "))

data <- inner_join(tests_raw, cases_raw, by="id") %>%
  select(-ends_with("y"),"val.y", -"id", -"Measure.x") %>%
  rename(Tests = val.x) %>%
  rename(Cases = val.y) %>%
  rename(Time = Time.x) %>%
  rename(Area = Area.x) %>%
  filter(!is.na(Cases) | !is.na(Tests)) %>%
  filter(Time != "Time") %>%
  filter(Time != "Year 2020 Week 05")
  
  
  
  
data_all <- data %>%
  filter(Area == "All areas") %>%
  select(-Area) %>%
  mutate("Week" = as.integer((substrRight(as.character(Time), 2))))


data_test <- data %>%
  filter(!is.na(Tests))

coeff = 1/0.035

data_all.interp <- data_all %>% 
  map_df(~data.frame(tests = approx(data_all$Week, data_all$Tests, n = 80),
                     cases = approx(data_all$Week, data_all$Cases, n = 80)))

{tests_vs_cases <- ggplot(data=data_all, aes(x=Week, group = 1, ) ) +
                  geom_line(aes(y=Cases)) + geom_text(aes(label=Cases, y=Cases),hjust=0.6, vjust=-0.6) + geom_point(aes(y=Cases)) +
                  geom_line(aes(y=Tests/coeff), col="firebrick3") + geom_text(aes(label=Tests, y=Tests/coeff),hjust=0.6, vjust=-0.6) + geom_point(aes(y=Tests/coeff), col = "firebrick3") +
                  scale_y_continuous(name = "Cases",sec.axis = sec_axis( trans=~.*coeff, name="Tests")) +
                  geom_ribbon(data=data_all.interp, aes(x = tests.x, ymin = cases.y, ymax = pmin(cases.y, tests.y/coeff), fill = paste("Less than ",1/coeff*100,"%", sep = "")), alpha=0.5) +
                  geom_ribbon(data=data_all.interp, aes(x = cases.x, ymin = tests.y/coeff, ymax = pmin(cases.y, tests.y/coeff), fill = paste("More than ",1/coeff*100,"%", sep = "")), alpha=0.5) + 
                  labs(fill = "Positiv test procentage") +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = length(data_all[,1]))) +
                  scale_fill_manual(values=c("firebrick3", "#009E73"), name="fill")
tests_vs_cases}

#Women
women <- prop.test(genders$val[1], genders$val[3],p = 0.5, alternative = "two.sided")
paste("Womens are as likely to get corona as women with a 95-CI:",women$p.value>0.05)
#Men
men <- prop.test(genders$val[2], genders$val[3],p = 0.5, alternative = "two.sided")
paste("Men are as likely to get corona as women with a 95-CI:",men$p.value>0.05)

temp <- data_all %>% select(Week,Tests,Cases) %>%
  mutate("Week2" = Week) %>%
  select(-Cases,Cases) %>%
  rename_all(function(e){return (names(data_all.interp))})

data_all.interp <- bind_rows(data_all.interp,test) %>%
  arrange(tests.x)
