#rm(list = ls())
#setwd("~/GitHub/COVID-19-analysis")


library(tidyverse)
library(ggplot2)
library(purrr)
library(plotly)
library(jsonlite)
library(stringr)

theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Read in the material
#443686 for daily, 443702L for weekly
#445222L for hospital district, 445268L for municipality
tests_raw <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=hcdmunicipality2020-445222L&column=dateweek20200101-509030&filter=measure-445356",sep=";", encoding = "UTF-8")
cases_raw <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=hcdmunicipality2020-445222L&column=dateweek20200101-509030",sep=";", encoding = "UTF-8")
genders <- read.csv("https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?column=sex-444328",sep=";", encoding = "UTF-8")
self_made_tests_raw <- read.csv("https://sampo.thl.fi/pivot/prod/fi/epirapo/omaolosymp/fact_epirapo_omaolosymp.csv?column=dateweek2020010120201231-443686",sep=";", encoding = "UTF-8")
hospitalized_raw <- fromJSON("https://w3qa5ydb4l.execute-api.eu-west-1.amazonaws.com/prod/finnishCoronaHospitalData")[['hospitalised']]
# Data handling -----------------

tests_raw <- tests_raw %>%
  mutate("id" = paste(Time, Area, sep = " "))
cases_raw <- cases_raw %>%
  mutate("id" = paste(Time, Area, sep = " "))

finland <- inner_join(tests_raw, cases_raw, by="id") %>%
  select(-ends_with("y"),"val.y", -"id") %>%
  rename(Tests = val.x) %>%
  rename(Cases = val.y) %>%
  rename(Time = Time.x) %>%
  rename(Area = Area.x) %>%
  filter(!is.na(Cases) | !is.na(Tests)) %>%
  filter(Time != "Time") 
  
finland_all <- finland %>%
  filter(Area == "All areas" & Time != "All times") %>%
  select(-Area) %>%
  mutate(Week = paste0(substr(as.character(Time),6,9),".", substrRight(as.character(Time), 2))) %>%
  mutate(Tests = replace(Tests, is.na(Tests), 0)) %>%
  mutate(Index = row_number()) #%>%
  #filter(Index >= 7)


finland_test <- finland %>%
  filter(!is.na(Tests))

coeff = 1/0.03

finland_all.interp <- finland_all %>% 
  map_df(~data.frame(tests = approx(finland_all$Index, finland_all$Tests, n = 200),
                     cases = approx(finland_all$Index, finland_all$Cases, n = 200)))
# history <- 7
# future <- 2
# y <- head(tail(finland_all$Cases,history),-1)
# week <- head(tail(finland_all$Week,history),-1)
# model <- lm(y ~ log(week))
# newdata <- data.frame(week = append(week,nth(finland_all$Week, -2)+1:future))
# pred <- data.frame(predictions = predict(model,newdata=newdata), newdata)

#temp <- finland_all %>% select(Week,Tests,Cases) %>%
#  mutate(Week2 = Week) %>%
#  select(-Cases,Cases) %>%
#  rename_all(function(e){return (names(finland_all.interp))})

#finland_all.interp <- bind_rows(finland_all.interp,temp) %>%
#  arrange(tests.x)

# Plots ---------------

# tests_vs_cases <- ggplot(data=finland_all, aes(x=Week, group = 1, ) ) +
#                   geom_line(aes(y=Cases, col="Cases")) + geom_text(aes(label=Cases, y=Cases),hjust=1.2, vjust=-0.6) + geom_point(aes(y=Cases)) +
#                   geom_line(aes(y=Tests/coeff, col="Tests")) + geom_text(aes(label=Tests, y=Tests/coeff),hjust=1.2, vjust=-0.6) + geom_point(aes(y=Tests/coeff), col = "firebrick3") +
#                   scale_y_continuous(name = "Cases",sec.axis = sec_axis( trans=~.*coeff, name="Tests")) +
#                   geom_ribbon(data=finland_all.interp, aes(x = tests.x, ymin = cases.y, ymax = pmin(cases.y, tests.y/coeff), fill = paste("More than ",1/coeff*100,"%", sep = "")), alpha=0.5) +
#                   geom_ribbon(data=finland_all.interp, aes(x = cases.x, ymin = tests.y/coeff, ymax = pmin(cases.y, tests.y/coeff), fill = paste("Less than ",1/coeff*100,"%", sep = "")), alpha=0.5) + 
#                   labs(fill = "Positiv test procentage") +
#                   scale_x_continuous(breaks = scales::pretty_breaks(n = length(finland_all[,1]))) +
#                   scale_fill_manual(values=c("#009E73","firebrick3"), name="Amount of positive tests") +
#                   #geom_line(data=pred,aes(x = week, y = predictions, col="Prediction"), size=1.2, alpha = 0.7) + geom_text(data=tail(pred,future), aes(x = week, y = predictions, label=floor(predictions*coeff)), vjust=-0.8) +
#                   scale_colour_manual(name="Lines",breaks = c("Cases","Tests","Prediction"),values=c("009E73","firebrick3","purple"))
# tests_vs_cases
# Tests ----------------------------
#Women
#women <- prop.test(genders$val[1], genders$val[3],p = 0.5, alternative = "two.sided")
#paste("Womens are as likely to get corona as women with a 95-CI:",women$p.value>0.05)
#Men
#men <- prop.test(genders$val[2], genders$val[3],p = 0.5, alternative = "two.sided")
#paste("Men are as likely to get corona as women with a 95-CI:",men$p.value>0.05)

test <- ggplot(data = finland_all, aes(x = Week,Cases/Tests*100)) + geom_point(size=0.5) + geom_text(aes(label = paste0(round(Cases/Tests*100,1),"%")), vjust=-0.5)
#ggplotly(test)

# Hospitalized--------------------------
hospitalized <- hospitalized_raw %>%
  mutate(date = as.Date(date)) %>%
  filter(area == "Finland") %>%
  mutate(week = week_fun(date)) %>%
  gather("key", "value", c(inWard,inIcu)) %>%
  filter(date > max(date)-60)
