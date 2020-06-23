rm(list = ls())
#setwd("~/GitHub/COVID-19-analysis")
#install.packages("readxl","dplyr","plotly","tidyverse","htmlwidgets","countrycode","gganimate","tibbletime","remotes","utils", "stringr", "maps", "mapdata")
library("readxl")
library("dplyr")
library("ggplot2")
library("plotly")
library("tidyverse")
library("htmlwidgets")
library("countrycode")
library("tibbletime")
library("remotes")
library("utils")
library("stringr")
library("maps")
library("mapdata")


poly_est <- function(y){
  x <- 1:length(y)
  lm(y ~ poly(x, 4, raw = TRUE))
}
exp_est <- function(y){
  tryCatch({ 
    x <- 1:length(y)
    exp  <- lm(log(y) ~ x)
    exp$coefficients[["x"]]
      }, error = function(e){return(NaN)})
}
lin_est <- function(y){
  tryCatch({ 
    x <- 1:length(y)
    lin  <- lm(y ~ x)
    lin$coefficients[["x"]]
  }, error = function(e){return(NaN)})
}
est_comp <- function(data,column,tail_amount = length(eval(parse(text=paste0("data$",column))))){
  y <- tail(eval(parse(text=paste0("data$",column))),tail_amount)
  index <- tail(data$index,tail_amount)
  tryCatch({ 
    exp  <- lm(log(y) ~ index)
    plot(index,y)
  }, error = function(e){exp <- NaN})
  tryCatch({ 
    poly <- lm(y ~ poly(index, 4, raw = TRUE))
  }, error = function(e){poly <- NaN})
  tryCatch({ 
    lin <- lm(y~index)
  }, error = function(e){lin <- NaN})
  tryCatch({ 
    log <- lm(y~log(index))
  }, error = function(e){log <- NaN})
    rsqrd <- c(summary(exp)$r.squared,summary(poly)$r.squared,summary(lin)$r.squared,summary(log)$r.squared)
    df <- data.frame(t(rsqrd))
    names(df) <- c("exp","poly","lin","log")#(c(deparse(substitute(exp)),deparse(substitute(poly)),deparse(substitute(lin))))
    best <- names(df)[which(df==max(df))]
    ret <- get(as.character(best))
    return(ret)
}
roll_exp_est <- rollify(exp_est, window= 7, unlist = TRUE)
roll_lin_est <- rollify(lin_est, window= 7, unlist = TRUE)
week_fun <- function(date){
  as.integer(strftime(date, format = "%V"))
}

theme_set(
  theme_minimal() +
    theme(legend.position = "right"))

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
data$dateRep <- as.Date(data$dateRep, format = "%d/%m/%Y")
data <- data[order(data$countriesAndTerritories, data$dateRep),]
data <- data %>%
  group_by(countriesAndTerritories) %>%
  filter(n()>7) %>%
  mutate("index" = row_number()) %>%
  mutate("cumulative_cases" = cumsum(cases)) %>%
  mutate("cumulative_deaths" = cumsum(deaths)) %>%
  mutate("cases_per_100k" = (cumulative_cases/(popData2019/10^5))) %>%
  mutate("deaths_per_100k" = (cumulative_deaths/(popData2019/10^5))) %>%
  mutate("death_procentage_of_cases" = cumulative_deaths/cumulative_cases * 100) %>%
  mutate("real_cases" = (lag(cumulative_deaths,10)/0.01)*2^(10/5)) %>%
  mutate("doubling_time" =  log(2)/(roll_exp_est(cumulative_cases))) %>%
  mutate("index_since_100" = ifelse(cumulative_cases>100, 1:n(), 0)) %>%
  mutate("index_since_100" = ifelse(index_since_100>0, index_since_100 - min(index_since_100[index_since_100!=0])+1,0 )) %>%
  ungroup() %>%
  filter(continentExp != "Other")

#data %>% relocate(continent, .before = "countriesAndTerritories")


#Todays data
#data_today <- filter(data, dateRep == (Sys.Date())  & cumulative_cases >10)
#Change country data
#country <-  c("Finland")
#countries <- c("Finland", "Sweden", "Norway", "Denmark")
#country_data <- filter(data, countriesAndTerritories == country & cumulative_cases != 0)
#countries_data <- filter(data, countriesAndTerritories %in% countries)
#europe <- filter(data, continent == "Europe")

      