source('Main.R')

### One country: New cases
{new_cases_country_plot<-ggplot(data=country_data[which(country_data$'cumulative_cases'!=0),], aes(x=dateRep, y=cases)) +
    geom_bar(stat="identity", fill="steelblue") +
    geom_text(aes(label=cases), vjust=-0.3, size=3.5) +
    theme_minimal() +
    xlab("Date") + ylab("New cases / day")
  ggplotly(new_cases_country_plot)}
###

### One Country: Cumulative cases
{cum_cases_country_plot <- ggplot(data=country_data[which(country_data$'cumulative_cases'!=0),], aes(x=dateRep, y=cumulative_cases, group=1)) +
    geom_line(linetype = "dashed")+
    geom_text(aes(label=cases), position = position_nudge(y = 2, x= -2), size=3.5) +
    geom_point(size = 1)
  ggplotly(cum_cases_country_plot)}
###

### One Country: Polynomial fitting
{history <- 30
  degree <- 4
  poly_model <- lm(cumulative_cases ~ poly(index, degree), data=tail(country_data,history))
  #summary(poly_model)
  future <- data.frame(index = tail(country_data$index,1)+c(0,1,2,3,4,5,6,7))
  poly_model_fit <- predict(poly_model, future, se.fit = TRUE)
  future$predictions <- poly_model_fit$fit
  future$lwr <- poly_model_fit$fit-1.99*poly_model_fit$se.fit
  future$upr <- poly_model_fit$fit+1.99*poly_model_fit$se.fit
  #Plot the fit
  polynomfit_plot <- ggplot(country_data, aes(index,cumulative_cases))+
    geom_point(size=0.7) +
    geom_line() +
    geom_point(data=future, aes(index, predictions, col="red"), size=0.7)+
    geom_line(data=future, aes(index, predictions, col="red")) +
    labs(title=paste0("Polynomial fitting of ",degree,"th using a history  of ",history, " days"),x = "Indices", y = "Cumulative Cases") +
    geom_ribbon(data=future, aes(x=index, y=predictions,  ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE)
  ggplotly(polynomfit_plot)
  rm("history","degree")}
###

### One Country: Best fitting
{history <- 30
  model <- est_comp(country_data,"cumulative_cases",history)
  #summary(model)
  future <- data.frame(index = tail(country_data$index,1)+c(0,1,2,3,4,5,6,7))
  model_fit <- predict(model, future, se.fit = TRUE)
  future$predictions <- model_fit$fit
  future$lwr <- model_fit$fit-1.99*model_fit$se.fit
  future$upr <- model_fit$fit+1.99*model_fit$se.fit
  
  #Plot the fit
  bestfit_plot <- ggplot(country_data, aes(index,cumulative_cases))+
    geom_point(size=0.7) +
    geom_line() +
    geom_point(data=future, aes(index, predictions, col="red"), size=0.7)+
    geom_line(data=future, aes(index, predictions, col="red")) +
    labs(title=paste0("Best fitting using a history  of ",history, " days"),x = "Indices", y = "Cumulative Cases")+
    geom_ribbon(data=future, aes(x=index, y=predictions,  ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE)
  rm("history")
  ggplotly(bestfit_plot)}
###

### One country: Real cases
{new_cases_country_plot<-ggplot(data=country_data[which(country_data$'cumulative_cases' > 10),], aes(x=dateRep, y=predict(poly_est(real_cases)))) +
    geom_bar(aes(x=dateRep, y=real_cases),stat="identity", fill="steelblue", alpha=0.5) +
    geom_point(stat="identity", col="steelblue") + geom_line(linetype = "dashed", col="steelblue") +
    #geom_text(aes(label=real_cases), position = position_nudge(y = 500), size=3.5) +
    theme_minimal() +
    xlab("Date") + ylab("Predicted real cases vs reported cases") +
    geom_point(aes(y=cumulative_cases)) + geom_line(aes(y=cumulative_cases),linetype = "dashed")
  ggplotly(new_cases_country_plot)}
###

### Specific countries: Cumulative cases/100k
{cumulative_cases_spec <- 
    ggplot(countries_data[which(countries_data$'cumulative_cases'!=0),], aes(dateRep, cases_per_100k, group = countriesAndTerritories, color = countriesAndTerritories )) +
    geom_line() + 
    geom_point(size = 0.5) +
    xlab("Date") + ylab("Cases/100k")# +
  #scale_y_continuous(trans='log10')
  ggplotly(cumulative_cases_spec)}
###

### Specific countries: Death rate
{death_rate <- 
    ggplot(countries_data[which(countries_data$'cumulative_deaths'!=0),], aes(dateRep, death_procentage_of_cases, group = countriesAndTerritories, color = countriesAndTerritories )) +
    geom_line() + 
    geom_point(size = 0.5) +
    xlab("Date") + ylab("Death % of cases")
  ggplotly(death_rate)}
###

###Specific countries: Doubling rate
{doubling_rate <- 
    ggplot(countries_data[which(countries_data$doubling_time < 100),], aes(dateRep, doubling_time, group = countriesAndTerritories, color = countriesAndTerritories )) +
    geom_line() + 
    geom_point(size = 0.5) +
    xlab("Date") + ylab("Doubling speed [Days]")
  ggplotly(doubling_rate)}
###

### All countries: Scatter plot of death rate
{death_rate_sct <- ggplot(data_today[which(data_today$popData2018 > 10^6 & data_today$cumulative_deaths > 10) ,], aes(cases_per_100k, death_procentage_of_cases, frame = dateRep, label = countriesAndTerritories, color = deaths_per_100k)) +
    geom_point(size=1 ) +
    xlab("Caseas/100k") + ylab("Death % of cases") +
    geom_text(aes(label=countryterritoryCode), position = position_nudge(y = 0.7)) +
    scale_x_continuous(trans='log10') + 
    scale_color_gradient(low="blue", high="red") + theme(legend.position = "none")
  ggplotly(death_rate_sct)}
###

### All countries: Scatter plot of doubling time
{doubling_sct <- ggplot(data_today[which(data_today$doubling_time < 100 & data_today$cumulative_cases > 1000),], aes(cumulative_cases, doubling_time, label = countriesAndTerritories, color = -doubling_time)) +
    geom_point(size=1) +
    xlab("Caseas/100k") + ylab("Doubling time in days") +
    geom_text(aes(label=countryterritoryCode), position = position_nudge(y = 0.05)) +
    scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') +
    guides(fill=FALSE) #+
  #scale_color_gradient(low="blue", high="red")
  ggplotly(doubling_sct)}
###

history <- 41
index <- append(tail(country_data$index,history),tail(country_data$index,1)+c(0,1,2,3,4,5,6,7))
plot(tail(country_data$index,history),tail(country_data$cases,history))
lines(index,predict(est_comp(country_data,"cases",history), data.frame(index)),type="l")

data_test <- country_data %>%
  group_by(week = week(dateRep)) %>%
  mutate("week_sum" = sum(cases)) %>%
  ungroup()

plot(data_test$week,data_test$week_sum, xlim=c(5,21))
weeks <-tail(unique(data_test$week),5)
test <- lm(tail(unique(data_test$week_sum),5)~log(weeks))
lines(append(weeks,c(20,21)),predict(test, newdata=data.frame("weeks" = append(weeks,c(20,21)))))
