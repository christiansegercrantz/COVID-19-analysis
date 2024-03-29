#install.packages("shinyr","shinythemes","rsconnect")
library(shiny)
library(shinythemes)
library(rsconnect)

source('Main.R')
source('finland.R')
history <- 30

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  navbarPage("COVID-19 Analysis",
    # tabPanel("Time dependent",
    #   sidebarPanel( width = 3,
    #                 dateRangeInput(inputId = "ot_daterange", label = "Date range to show", min = min(data$dateRep), max = max(data$dateRep), start = min(data$dateRep), end = max(data$dateRep)),
    #                 radioButtons(inputId = "ot_continent", label = "Choose continent", choices = append(as.character(unique(data$continentExp)),"All"), selected = "All"),
    #                 uiOutput("ot_countries_of_continent", inline = TRUE),
    #                 checkboxInput(inputId = "ot_per100k", label = "Per 100k"),
    #                 checkboxInput(inputId = "ot_y_log", label = "Logarithmic y scale")),
    #   mainPanel(
    #             tabsetPanel(tabPanel("New cases",plotlyOutput("ot_cases")),
    #                         tabPanel("Cumulative cases",plotlyOutput("ot_cum_cases")),
    #                         tabPanel("Case rate",plotlyOutput("ot_case_rate"))),
    #             tabsetPanel(tabPanel("New deaths",plotlyOutput("ot_deaths")),
    #                         tabPanel("Cumulative deaths",plotlyOutput("ot_cumulative_deaths")),
    #                         tabPanel("Death rate",plotlyOutput("ot_death_rate"))))),
    # tabPanel("Per date",
    #   sidebarPanel( width = 3,
    #                 dateInput(inputId = "pd_date", label = "Date show", min = min(data$dateRep), max = max(data$dateRep), value = max(data$dateRep)),
    #                 radioButtons(inputId = "pd_continent", label = "Choose continent", choices = append(as.character(unique(data$continentExp)),"All"), selected = "All"),
    #                 checkboxInput(inputId = "pd_y_log", label = "Logarithmic y scale", value = TRUE),
    #                 uiOutput("pd_countries_of_continent", inline = TRUE)),
    #   mainPanel(
    #                plotlyOutput("pd_cumulative"),
    #                verbatimTextOutput("test"),
    #                plotlyOutput("pd_doubling_rate"),
    #                plotlyOutput("pd_death_rate")),
    # ),
    # tabPanel("Special",
    #  sidebarPanel( width = 3,
    #                radioButtons(inputId = "spec_continent", label = "Choose continent", choices = append(as.character(unique(data$continentExp)),"All"), selected = "All"),
    #                checkboxInput(inputId = "spec_per100k", label = "Per 100k", value = TRUE),
    #                checkboxInput(inputId = "spec_y_log", label = "Logarithmic y scale", value = TRUE),
    #                uiOutput("spec_countries_of_continent")),
    #  mainPanel(
    #               plotlyOutput("spec_since_100")
    #  )),
    tabPanel("Finland",
             plotOutput("finland_tests"),
             plotlyOutput("finland_hospitalized"))
  )
)


server <- function(input, output) {
# Data ---------------------------------
#   ot_contries_of_continents_data <- reactive({data %>% filter(continentExp == input$ot_continent | input$ot_continent == "All")})
#   output$ot_countries_of_continent <- renderUI({
#     selectInput(inputId = "ot_country", label = "Choose a country", choices = unique(ot_contries_of_continents_data()$countriesAndTerritories) , selected = head(unique(ot_contries_of_continents_data()$countriesAndTerritories),1) , multiple = TRUE)
#   })
#   
#   pd_contries_of_continents_data <- reactive({data %>% filter(continentExp == input$pd_continent | input$pd_continent == "All")})
#   output$pd_countries_of_continent <- renderUI({
#     selectInput(inputId = "pd_country", label = "Choose a country", choices = unique(pd_contries_of_continents_data()$countriesAndTerritories) , selected = unique(pd_contries_of_continents_data()$countriesAndTerritories) , multiple = TRUE)
#   })
#   
#   spec_contries_of_continents_data <- reactive({data %>% filter(continentExp == input$spec_continent | input$spec_continent == "All")})
#   output$spec_countries_of_continent <- renderUI({
#     selectInput(inputId = "spec_country", label = "Choose a country", choices = unique(spec_contries_of_continents_data()$countriesAndTerritories) , selected = head(spec_contries_of_continents_data()$countriesAndTerritories) , multiple = TRUE)
#   })
# 
#   data_over_time <- reactive({
#     data %>% filter(dateRep >= input$ot_daterange[1]) %>%
#       filter(dateRep <= input$ot_daterange[2]) %>%
#       filter(countriesAndTerritories %in% input$ot_country) %>%
#       filter(cumulative_cases != 0)
#   })
#   
#   data_per_date <- reactive({data %>%
#       filter(countriesAndTerritories %in% input$pd_country) %>%
#       filter(dateRep == input$pd_date) %>%
#       filter(cumulative_cases > 500) %>%
#       filter(popData2019 > 10^6) %>%
#       filter(doubling_time < 10000)
#                                  })
#   
#   data_spec <- reactive({
#     data %>% filter(index_since_100 > 0) %>%
#       filter(countriesAndTerritories %in% input$spec_country)
#   })
#   
#   # model <- reactive({est_comp(data_over_time(),"cumulative_cases",history)})
#   # future <- reactive({data.frame(index = tail(data_over_time()$index,1)+c(0,1,2,3,4,5,6,7))})
#   # model_fit <- reactive({predict(model(), future(), se.fit = TRUE)})
#   # future_new <- reactive({data.frame(index <- future(),
#   #                         predictions <- model_fit()$fit,
#   #                         lwr <- model_fit()$fit-1.99*model_fit()$se.fit,
#   #                         upr <- model_fit()$fit+1.99*model_fit()$se.fit)
#   #   })
# # Over time plots ---------------------------------
#   
#   output$ot_cases <- renderPlotly({
#     shiny::validate(
#       need(input$ot_country !="", "Please select at least one country")
#     )
#     ggplotly({temp <- ggplot(data_over_time(), aes(dateRep, if(!input$ot_per100k){rolling_avg_cases}else{rolling_avg_cases/(popData2019/10^5)}, group = countriesAndTerritories, color = countriesAndTerritories, text=paste("Date:", dateRep, "<br>Country:", countriesAndTerritories, "<br>New cases:", cases, "<br>Cumulative cases:", format(cumulative_cases, big.mark=",", trim=TRUE)) )) +
#                geom_line() + 
#                geom_point(size = 0.5) +
#                xlab("Date") + ylab("Cumulative cases") + ggtitle("7 day rolling average cases") + labs(color="Country")
#              if(input$ot_y_log){
#                temp <- temp + scale_y_continuous(trans='log10')
#              }
#              else{
#                temp
#              }}, 
#              tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
# 
#   output$ot_cum_cases <- renderPlotly({
#     shiny::validate(
#       need(input$ot_country !="", "Please select at least one country")
#     )
#     ggplotly({temp <- ggplot(data_over_time(), aes(dateRep, if(input$ot_per100k){cases_per_100k}else{cumulative_cases}, group = countriesAndTerritories, color = countriesAndTerritories, text=paste("Date:", dateRep, "<br>Country:", countriesAndTerritories, "<br>New cases:", cases, "<br>Cumulative cases:", format(cumulative_cases, big.mark=",", trim=TRUE)) )) +
#               geom_line() + 
#               geom_point(size = 0.5) +
#               xlab("Date") + ylab("Cumulative cases") + ggtitle("Cumulative cases per date") + labs(color="Country")
#               if(input$ot_y_log){
#                 temp <- temp + scale_y_continuous(trans='log10')
#               }
#               else{
#                 temp
#               }}, 
#             tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
#   
#   output$ot_case_rate <- renderPlotly({
#     shiny::validate(
#       need(input$ot_country !="", "Please select at least one country")
#     )
#     ggplotly(ggplot(data_over_time(), aes(dateRep, cases/popData2019*100, group = countriesAndTerritories, color = countriesAndTerritories, text=paste("Date:", dateRep, "<br>Country:", countriesAndTerritories, "<br>New cases:", cases, "<br>Cumulative cases:", format(cumulative_cases, big.mark=",", trim=TRUE)) )) +
#                geom_line() + 
#                geom_point(size = 0.5) +
#                xlab("Date") + ylab("Cases % of total population") + ggtitle("Case rate of population per date") + labs(color="Country"), 
#              tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
#   
# 
#   output$ot_deaths <- renderPlotly({
#     shiny::validate(
#       need(input$ot_country !="", "Please select at least one country")
#     )
#     ggplotly({temp <- ggplot(data_over_time(), aes(dateRep, if(!input$ot_per100k){rolling_avg_deaths}else{rolling_avg_deaths/(popData2019/10^5)}, group = countriesAndTerritories, color = countriesAndTerritories, text=paste0("Date: ", dateRep, "<br>Country: ", countriesAndTerritories, "<br>Deaths: ", deaths, "<br>Cumulative deaths: ", cumulative_deaths) )) +
#                geom_line() + 
#                geom_point(size = 0.5) +
#                xlab("Date") + ylab("Death") + ggtitle("7 day rolling average deaths") + labs(color="Country")
#              if(input$ot_y_log){
#                temp <- temp + scale_y_continuous(trans='log10')
#              }
#              else{
#                temp
#              }}, 
#              tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
#   
#   output$ot_cumulative_deaths <- renderPlotly({
#     shiny::validate(
#       need(input$ot_country !="", "Please select at least one country")
#     )
#     ggplotly({temp <- ggplot(data_over_time(), aes(dateRep, if(!input$ot_per100k){cumulative_deaths}else{deaths_per_100k}, group = countriesAndTerritories, color = countriesAndTerritories, text=paste0("Date: ", dateRep, "<br>Country: ", countriesAndTerritories, "<br>Deaths: ", deaths, "<br>Cumulative deaths: ", cumulative_deaths) )) +
#                geom_line() + 
#                geom_point(size = 0.5) +
#                xlab("Date") + ylab("Death") + ggtitle("Cumulative deaths") + labs(color="Country")
#              if(input$ot_y_log){
#                temp <- temp + scale_y_continuous(trans='log10')
#              }
#              else{
#                temp
#              }}, 
#              tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
#   
#   output$ot_death_rate <- renderPlotly({
#     shiny::validate(
#       need(input$ot_country !="", "Please select at least one country")
#     )
#     ggplotly({temp <- ggplot(data_over_time(), aes(dateRep, death_procentage_of_cases, group = countriesAndTerritories, color = countriesAndTerritories, text=paste0("Date: ", dateRep, "<br>Country: ", countriesAndTerritories, "<br>Deaths procentage: ", round(death_procentage_of_cases,2), "%<br>Cumulative deaths: ", cumulative_deaths) )) +
#                geom_line() + 
#                geom_point(size = 0.5) +
#                xlab("Date") + ylab("Death % of cases") + ggtitle("Death rate") + labs(color="Country")
#              if(input$ot_y_log){
#                temp <- temp + scale_y_continuous(trans='log10')
#              }
#              else{
#                temp
#              }}, 
#              tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
#   
# # Per date plots ---------------------------------
#   output$pd_cumulative <- renderPlotly({
#     shiny::validate(
#       need(input$pd_country !="", "Please select at least one country <br>")
#     )
#     ggplotly({temp <- ggplot(data_per_date(), aes(cases_per_100k, cumulative_cases, label = countriesAndTerritories, color = (log(cumulative_cases)*cases_per_100k), text=paste0("Country: ", countriesAndTerritories, "<br>Cumulative cases: ", format(cumulative_cases, big.mark=",", trim=TRUE), "<br>Cases per 100k: ", round(cases_per_100k,1)))) +
#                geom_point(size=1) +
#                geom_text(aes(label=countryterritoryCode, text=""), position = position_nudge(y = 0.12)) +
#                xlab("Cases per 100k") + ylab("Cumulative cases") + ggtitle("Cumulative cases") +
#                scale_color_gradient(low="blue", high="red") + theme(legend.position = "none")
#              if(input$pd_y_log){
#                temp <- temp + scale_y_continuous(trans='log10')
#              }
#              else{
#                temp
#              }},
#              tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
#   
#   output$test <- renderText(quantile(data_per_date()$doubling_time, na.rm = TRUE))
#   
#   output$pd_doubling_rate <- renderPlotly({
#     shiny::validate(
#       need(input$pd_country !="", "Please select at least one country <br>")
#     )
#     qants <- quantile(data_per_date()$doubling_time, na.rm = TRUE)
#      ggplotly({temp <- ggplot(data_per_date(), aes(cumulative_cases, doubling_time, label = countriesAndTerritories, color = -doubling_time, text=paste0("Country: ",countriesAndTerritories, "<br>Cumulative cases: ", format(cumulative_cases, big.mark=",", trim=TRUE), "<br>Doubling time: ", round(doubling_time,1)))) +
#               geom_point(size=1) +
#               xlab("Caseas/100k") + ylab("Doubling time in days") + ggtitle("Doubling rate calculated using exponential growth") +
#               geom_text(aes(label=countryterritoryCode, text=""), position = position_nudge(y = 0.05)) +
#               scale_x_continuous(trans='log10') +
#               guides(fill=FALSE)+ 
#               scale_color_gradientn(colors = c("blue","purple","orange","red"), values = scales::rescale(c(qants[[1]], qants[[2]], qants[[3]], qants[[4]], max(data_per_date()$doubling_time)))) + theme(legend.position = "none")
#              if(input$pd_y_log){
#                temp <- temp + scale_y_continuous(trans='log10')
#              }
#              else{
#                temp
#              }},
#               tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })
#   
#   output$pd_death_rate <- renderPlotly({
#     shiny::validate(
#       need(input$pd_country !="", "Please select at least one country <br>")
#     )
#     ggplotly({temp <- ggplot(data_per_date(), aes(cases_per_100k, death_procentage_of_cases, label = countriesAndTerritories, color = deaths_per_100k, text=paste0("Country: ", countriesAndTerritories, "<br>Deaths procentage: ", round(death_procentage_of_cases,2), "%<br>Cumulative deaths: ", cumulative_deaths, "<br>Deaths per 100k: ",round(deaths_per_100k,1)))) +
#       geom_point(size=1 ) +
#       geom_text(aes(label=countryterritoryCode, text=""), position = position_nudge(y = 0.7)) +
#       xlab("Caseas/100k") + ylab("Death % of cases") + ggtitle("Death rate of cases") +
#       scale_x_continuous(trans='log10') + 
#       scale_color_gradient(low="blue", high="red") + theme(legend.position = "none")
#       if(input$pd_y_log){
#         temp <- temp + scale_y_continuous(trans='log10')
#       }
#       else{
#         temp
#       }},
#       tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#     
#   })
#   
#   #output$cum_pred <- renderPlotly({
#   #  ggplot(data_over_time(), aes(index, cumulative_cases, group = countriesAndTerritories))+
#   #    geom_point(size=0.7) +
#   #    geom_line() +
#   #    geom_point(data=future_new(), aes(x = future_new()$index,y = future_new()$predictions, col="red"), size=0.7) +
#   #    geom_line(data=future_new(), aes(x = future_new()$index,y = future_new()$predictions, col="red")) +
#   #    labs(title=paste0("Best fitting using a history  of ",history, " days"),x = "Indices", y = "Cumulative Cases") +
#   #    geom_ribbon(data=future_new(), aes(x=future_new()$index, y=future_new()$predictions,  ymin=future_new()$lwr, ymax=future_new()$upr), alpha = 0.3, inherit.aes = FALSE)
#   #})
#   
#   # Special plots ---------------------------------------
#   output$spec_since_100 <- renderPlotly({
#     shiny::validate(
#       need(input$spec_country !="", "Please select at least one country <br>")
#     )
#     ggplotly({temp <- ggplot(data_spec(), aes(index_since_100, if(!input$spec_per100k){cumulative_cases}else{cases_per_100k}, group = countriesAndTerritories, color = countriesAndTerritories, text=paste("Date:", dateRep, "<br>Country:", countriesAndTerritories, "<br>New cases:", cases, "<br>Cumulative cases:", format(cumulative_cases, big.mark=",", trim=TRUE)) )) +
#       geom_line() + 
#       geom_point(size = 0.3) +
#       xlab("Days since 100 cases") + ylab("Cumulative cases") + ggtitle("Timeline after 100 cases") + labs(color="Country")
#     if(input$spec_y_log){
#       temp <- temp + scale_y_continuous(trans='log10')
#     }
#     else{
#       temp
#     }}, 
#     tooltip=c("text")) %>% 
#       config(displayModeBar = F) 
#   })    
  
# Finland plots ----------
  output$finland_tests <- renderPlot(
    ggplot(data=finland_all, aes( x= reorder(Week,Index), group = 1, ) ) +
      geom_line(aes(y=Cases, col="Cases")) + geom_text(aes(label=Cases, y=Cases),hjust=1.2, vjust=-0.6) + geom_point(aes(y=Cases)) +
      geom_line(aes(y=Tests/coeff, col="Tests")) + geom_text(aes(label=Tests, y=Tests/coeff),hjust=1.2, vjust=-0.6) + geom_point(aes(y=Tests/coeff), col = "firebrick3") +
      scale_y_continuous(name = "Cases",sec.axis = sec_axis( trans=~.*coeff, name="Tests")) +
      geom_ribbon(data=finland_all.interp, aes(x = tests.x, ymin = cases.y, ymax = pmin(cases.y, tests.y/coeff), fill = paste("More than ",1/coeff*100,"%", sep = "")), alpha=0.5) +
      geom_ribbon(data=finland_all.interp, aes(x = cases.x, ymin = tests.y/coeff, ymax = pmin(cases.y, tests.y/coeff), fill = paste("Less than ",1/coeff*100,"%", sep = "")), alpha=0.5) + 
      labs(fill = "Positiv test procentage") + 
      xlab("Week") +
      #scale_x_discrete(breaks = scales::pretty_breaks(n =length(finland_all[,1])), name = "Week") + #breaks = scales::pretty_breaks(n =length(finland_all[,1]))
      theme(axis.text.x = element_text(angle = 60)) +
      scale_fill_manual(values=c("#009E73","firebrick3"), name="Amount of positive tests") +
      #geom_line(data=pred,aes(x = week, y = predictions, col="Prediction"), size=1.2, alpha = 0.7) + geom_text(data=tail(pred,future), aes(x = week, y = predictions, label=floor(predictions*coeff)), vjust=-0.8) +
      scale_colour_manual(name="Lines",breaks = c("Cases","Tests","Prediction"),values=c("009E73","firebrick3","purple"))
  )
  output$finland_hospitalized <- renderPlotly({
    ggplotly({ggplot(hospitalized, aes(x=date, y=value, fill = key, text=paste0("Date: ",date,"<br>Total hospitalized: ", totalHospitalised))) + 
        geom_bar(position="stack", stat="identity") +
        geom_text(aes(label=value), position = position_stack(vjust = 0.5)) +
        xlab("Date") + ylab("Hospitalized") + ggtitle("Hospitalized last month in Finland")
    }, 
    tooltip=c("text")) %>% 
      config(displayModeBar = F) 
  })
    


  
}

shinyApp(ui = ui, server = server)
