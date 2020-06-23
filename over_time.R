output$daily_cum_cases <- renderPlotly({
  
  ggplotly(ggplot(current_data(), aes(dateRep, if(input$per100k){cases_per_100k}else{cumulative_cases}, group = countriesAndTerritories, color = countriesAndTerritories, text=paste("Date:", dateRep, "<br>Country:", countriesAndTerritories, "<br>New cases:", cases, "<br>Cumulative cases:", cumulative_cases) )) +
             geom_line() + 
             geom_point(size = 0.5) +
             xlab("Date") + ylab("Cumulative cases") + ggtitle("Cumulative cases per date") + labs(color="Country"), 
           tooltip=c("text")) %>% 
    config(displayModeBar = F) 
})


output$death_rate <- renderPlotly({
  
  ggplotly(ggplot(current_data(), aes(dateRep, death_procentage_of_cases, group = countriesAndTerritories, color = countriesAndTerritories, text=paste0("Date: ", dateRep, "<br>Country: ", countriesAndTerritories, "<br>Deaths procentage: ", round(death_procentage_of_cases,2), "%<br>Cumulative deaths: ", cumulative_deaths) )) +
             geom_line() + 
             geom_point(size = 0.5) +
             xlab("Date") + ylab("Death % of cases") + ggtitle("Death rate") + labs(color="Country"), 
           tooltip=c("text")) %>% 
    config(displayModeBar = F) 
})