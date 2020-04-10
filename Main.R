rm(list = ls())

#install.packages("readxl","dplyr")
library("readxl")
library("dplyr")
library("ggplot2")

data <- data.frame(read_excel("data.xlsx"))
data <- data[order(data$countriesAndTerritories, data$dateRep),]
data <- data %>%
  group_by(countriesAndTerritories) %>%
  mutate("cumulative cases" = cumsum(cases)) %>%
  mutate("cumulative deaths" = cumsum(deaths)) %>%
  mutate("cases per 100k" = (cases/10^6)) %>%
  mutate("deaths per 100k" = (deaths/10^6))


{ #Change country data
  country <-  c("Finland")
country_data <- data[data$countriesAndTerritories == country,]}

### Plot the new cases of the country
new_cases_country_plot<-ggplot(data=country_data[which(country_data$'cumulative cases'!=0),], aes(x=dateRep, y=cases)) +
                        geom_bar(stat="identity", fill="steelblue") +
                        geom_text(aes(label=cases), vjust=-0.3, size=3.5) +
                        theme_minimal() +
                        xlab("Date") + ylab("New cases / day")
new_cases_country_plot
###
### Plot the cumulative cases of the country
{cum_cases_country_plot <- ggplot(data=country_data[which(country_data$'cumulative cases'!=0),], aes(x=dateRep, y=`cumulative cases`, group=1)) +
                          geom_line(linetype = "dashed")+
                          geom_text(aes(label=cases), vjust=-0.3, size=3.5) +
                          geom_point()
cum_cases_country_plot}
###

### Polynomial fitting
poly_model <- lm(`cumulative cases` ~ poly(dateRep, 4), data=country_data)
summary(poly_model)
#Plot the fit
{ plot(country_data$dateRep,country_data$`cumulative cases`)
  points(country_data$dateRep, predict(poly_model), type= 'l', col="red")
  future <- data.frame(dateRep = as.Date(tail(country_data$dateRep,1))+c(1,2,3,4,5,6,7))
  plot(future, predict(poly_model, future), type= 'l', col="orange")
}

