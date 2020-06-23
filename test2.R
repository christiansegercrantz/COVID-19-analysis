hospitalized_raw <- fromJSON("https://w3qa5ydb4l.execute-api.eu-west-1.amazonaws.com/prod/finnishCoronaHospitalData")[['hospitalised']]

test <- hospitalized_raw %>%
  mutate(date = as.Date(date)) %>%
  filter(area == "Finland") %>%
  mutate(week = week(date)) #%>%
  #gather("key", "value", inWard:inIcu)

ggplotly(ggplot(test, aes(x=week, fill = key)) + 
  geom_bar(aes(y=value),position="stack", stat="identity"))
  
  
  plot_ly(data=test, x = ~date, y = ~inWard, type = 'bar', name = 'In ward', text=paste0("ICU: ", test$inIcu, "<br>Ward: ", test$inWard)) %>%
  add_trace(y = ~inIcu, name = 'In ICU') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
  