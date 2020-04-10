rm(list = ls())
setwd("~/GitHub/COVID-19-analysis")
#install.packages("readxl","dplyr","plotly","tidyverse","htmlwidgets","countrycode")
library("readxl")
library("dplyr")
library("ggplot2")
library("plotly")
library("tidyverse")
library("htmlwidgets")
library("countrycode")

poly_est <- function(data){
  model <- lm(cumulative_cases ~ poly(index, 4, raw = TRUE), data=country_data)
  future <- data.frame(index = tail(data$index,1))
}

exp_est <- function(data){
  tryCatch({ 
    exp  <- lm(log(cumulative_cases) ~ index, data=data)
    exp$coefficients[["index"]]
  }, error = function(e){return(NaN)})
}

theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

data <- data.frame(read_excel("data.xlsx"))
data <- data[order(data$countriesAndTerritories, data$dateRep),]
data$dateRep <- as.Date(data$dateRep)
data <- data %>%
  group_by(countriesAndTerritories) %>%
  mutate("index" = row_number()) %>%
  mutate("cumulative_cases" = cumsum(cases)) %>%
  mutate("cumulative_deaths" = cumsum(deaths)) %>%
  mutate("cases_per_100k" = (cases/10^6)) %>%
  mutate("deaths_per_100k" = (deaths/10^6)) %>%
  mutate("death_procentage_of_cases" = cumulative_deaths/cumulative_cases * 100) %>%
  mutate("continent" = countrycode(sourcevar = countriesAndTerritories,
                                   origin = "country.name",
                                   destination = "continent"))%>%
  ungroup()

data %>% relocate(continent, .before = "countriesAndTerritories")

data <- data %>%
  group_by(countriesAndTerritories) %>%
  mutate("Doubling_time" =  log(2)/exp_est(data[max(0,index-7):index,])) %>%
  mutate("Real_cases" = (lag(cumulative_deaths,10)/0.01)*2^(10/5)) %>%
  #transmute("Real_cases" = cumsum(Real_cases)) %>%
  ungroup()

#Todays data
data_today <- filter(data, dateRep == (Sys.Date()-1)  & cumulative_cases >10)
#Change country data
country <-  c("United_States_of_America")
countries <- c("Finland", "Sweden", "Norway", "Denmark")
country_data <- filter(data, countriesAndTerritories == country & cumulative_cases != 0)
countries_data <- filter(data, countriesAndTerritories %in% countries)
europe <- filter(data, continent == "Europe")


### Plot the new cases of the country
{new_cases_country_plot<-ggplot(data=country_data[which(country_data$'cumulative_cases'!=0),], aes(x=dateRep, y=cases)) +
                        geom_bar(stat="identity", fill="steelblue") +
                        geom_text(aes(label=cases), vjust=-0.3, size=3.5) +
                        theme_minimal() +
                        xlab("Date") + ylab("New cases / day")
ggplotly(new_cases_country_plot)}
###
### Plot the cumulative cases of the country
{cum_cases_country_plot <- ggplot(data=country_data[which(country_data$'cumulative_cases'!=0),], aes(x=dateRep, y=cumulative_cases, group=1)) +
                          geom_line(linetype = "dashed")+
                          geom_text(aes(label=cases), vjust=-0.3, size=3.5) +
                          geom_point()
ggplotly(cum_cases_country_plot)}
###

### Polynomial fitting
poly_model <- lm(cumulative_cases ~ poly(index, 4), data=country_data)
summary(poly_model)
future <- data.frame(index = tail(country_data$index,1)+c(0,1,2,3,4,5,6,7))
future$predictions <- predict(poly_model, future)
#Plot the fit
polynomfit_plot <- ggplot(country_data, aes(index,cumulative_cases))+
  geom_point(size=0.7) +
  geom_line() +
  geom_point(data=future, aes(index, predictions, col="red"), size=0.7)+
  geom_line(data=future, aes(index, predictions, col="red")) +
  xlab("Indices") + ylab("Cumulative Cases")
ggplotly(polynomfit_plot)
###

### Scatter plot of death rate
death_rate_sct <- ggplot(data_today, aes(cases_per_100k, death_procentage_of_cases, label = countriesAndTerritories, color = (death_procentage_of_cases*cases_per_100k))) +
  geom_point(size=1 ) +
  xlab("Caseas/100k") + ylab("Death % of cases") +
  geom_text(aes(label=countryterritoryCode),position = position_nudge(y = 0.7)) +
  scale_x_continuous(trans='log10') + 
  scale_color_gradient(low="blue", high="red")
ggplotly(death_rate_sct)
###

###Death rate for specified countries
death_rate <- 
  ggplot(countries_data[which(countries_data$'cumulative_cases'!=0),], aes(dateRep, death_procentage_of_cases, group = countriesAndTerritories, color = countriesAndTerritories )) +
  geom_line() + 
  geom_point(size = 0.5) +
  xlab("Date") + ylab("Death % of cases")
ggplotly(death_rate)


