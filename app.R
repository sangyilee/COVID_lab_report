# Setting
rm(list=ls())
## Library packages
# install.packages("shinydashboard")
# install.packages("fontawesome")
library(fontawesome)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(rnaturalearth)
library(RColorBrewer)
library(raster)
library(maptools)
library(sf)
library(rgeos)
library(rgdal)
library(DT)
library(lubridate)
library(ggplot2)
library(leaflet)
library(highcharter)

## Load covid-19 data from owid

covid <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv', fileEncoding = "utf-8")

covid$date <- as.Date(covid$date)

countries = read.csv("C:/Users/LSY/Dropbox/Shiny/countries.csv")

countries$iso_code <- countries$alpha3

## Create vectors to use in Shiny

all_countries <- unique(covid$location)

colname <- colnames(covid[,-(1:4)])

infect <- read.csv('data/dat_local_trans_by_age.csv',header = T, fileEncoding = 'UTF-8-BOM') %>% as.tibble
infect$Date <- infect$Date %>% as.Date
colnames(infect) <- c('Date', 'total', 'y0-9', 'y10-19', 'y20-29','y30-39','y40-49', 'y50-59', 'y60-69','y70-79','y80', 'import')
gathinf <- infect %>% gather(age_group, num, -c(1)) %>% arrange(Date)

death <-  read.csv('data/death.csv',header = T, fileEncoding = 'UTF-8-BOM') %>% as.tibble
death$Date <- death$Date %>% as.Date
colnames(death) <- c('Date', 'y0-9', 'y10-19', 'y20-29','y30-39','y40-49', 'y50-59', 'y60-69','y70-79','y80', 'total', 'check')
death <- death %>% na.omit
gathdeath <- death %>% gather(age_group, num, -c(1)) %>% arrange(Date)

severecase <-  read.csv('data/severecase.csv',header = T, fileEncoding = 'UTF-8-BOM') %>% as.tibble
severecase$Date <- severecase$Date %>% as.Date
colnames(severecase) <- rev(c('y0-9', 'y10-19', 'y20-29','y30-39','y40-49', 'y50-59', 'y60-69','y70-79','y80', 'Date'))
severecase <- severecase[,1:10]
severecase$total <- rowSums(severecase[,2:10])
severecase <- severecase %>% na.omit
gathsev <- severecase %>% gather(age_group, num, -c(1)) %>% arrange(Date)

vac1 <-  read.csv('data/vac1.csv',header = T, fileEncoding = 'UTF-8-BOM') %>% as.tibble
vac1$Date <- vac1$Date %>% as.Date
gatvac1 <- vac1 %>% gather(vaccine, num, -c(1)) %>% arrange(Date)

vac2 <-  read.csv('data/vac2.csv',header = T)
vac2$Date <- vac2$Date %>% as.Date
vac2 <- vac2 %>% as.tibble() %>% arrange(Date)
colnames(vac2) <- c('Date', 'vaccine', 'Dose', 'num')

vac3 <-  read.csv('data/vac3.csv',header = T)
vac3$Date <- vac3$Date %>% as.Date
vac3 <- vac3 %>% as.tibble() %>% arrange(Date)
colnames(vac3) <- c('Date', 'Dose', 'age_group', 'num')
gatvac3 <- vac3 %>% group_by(Date) %>% filter(Dose == '1차') %>% summarise('1차' = sum(num)) %>% gather(Dose, num, -c(1))
gatvac3 <- cbind(gatvac3[c(1,2)], as.data.frame(rep('total', nrow(gatvac3))), gatvac3[c(3)])
colnames(gatvac3) <- c('Date', 'Dose', 'age_group', 'num')

gatvac32 <- vac3 %>% group_by(Date) %>% filter(Dose == '완료') %>% summarise('완료' = sum(num)) %>% gather(Dose, num, -c(1))
gatvac32 <- cbind(gatvac32[c(1,2)], as.data.frame(rep('total', nrow(gatvac32))), gatvac32[c(3)])
colnames(gatvac32) <- c('Date', 'Dose', 'age_group', 'num')

gatvac33 <- vac3 %>% group_by(Date) %>% filter(Dose == '3차') %>% summarise('3차' = sum(num)) %>% gather(Dose, num, -c(1))
gatvac33 <- cbind(gatvac33[c(1,2)], as.data.frame(rep('total', nrow(gatvac33))), gatvac33[c(3)])
colnames(gatvac33) <- c('Date', 'Dose', 'age_group', 'num')


gatvac3 <- rbind(gatvac3, gatvac32, gatvac33, vac3) %>% arrange(Date)


## Load prediction data

setwd('C:/Users/LSY/Dropbox/Shiny')
rsconnect::setAccountInfo(name='sylee721', token='2B99706CFFDE543384D36DE94A14A94B', secret='YqB+qC/Ji5mCUldJ4iDgOaYQKECQ6RGKDaRAmAOe')

result <- read.csv('data/covid_prediction_022322_KM.csv',header = T, fileEncoding = 'UTF-8-BOM')
result <- Filter(function(x)!all(is.na(x)), result)
colnames(result)[4:ncol(result)] <- 
  substr(colnames(result)[4:ncol(result)], 2,nchar(colnames(result)[4:ncol(result)])) %>% 
  as.Date('%Y.%m.%d') %>% as.character()
result <- result %>% na.omit
gather <- result %>% gather(Date, num, -c(1,2,3)) 
total <- result %>% gather(Date, num, -c(1,2,3)) %>% group_by(type, scenario, Date) %>% summarise(total = sum(num))
total <- total %>% gather(age_group, num, -c(1,2,3))
result <- rbind(total, gather) %>% spread(Date, num)

Scenario <- c("1", "2", "3")
Ages <- c('y0-11', 'y12-19', 'y20-39','y40-59','y60-79','y80+', 'total')
Ages2 <- c('total','y0-9', 'y10-19', 'y20-29','y30-39','y40-49', 'y50-59', 'y60-69','y70-79','y80')
Options <- c('confirmed', 'critical')
Doses <- c('1차', '완료', '3차')

df <- result %>% 
  gather(key = 'date', value = 'num', -c( type, scenario, age_group )) %>% 
  na.omit %>% mutate(date=as.Date(date)) %>% 
  as.tibble %>% arrange(date)
head(df)

Vaccines <- unique(gatvac1$vaccine)




# Building Shiny app

## Build-up UI


sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              menuItem("COVID-19 in the World", tabName = "world", icon = icon("globe")),
                              menuItem("COVID-19 in Korea", tabName = "korea", icon = icon("dashboard")),
                              menuItem("Biweekly Report", tabName = "report", icon = icon("th"), 
                                       badgeLabel = "new", badgeColor = "green"),
                              menuItem("Our Model", tabName = "model", icon = icon("cube")),
                              menuItem("Questions", tabName = "questions", icon = icon("question"))
                            )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "world",
            h1('World COVID-19 Today'),
            
            mainPanel(
              leafletOutput("map", height="800px"),
            ),
            
            fluidRow(
              
              absolutePanel(
                top = 1000, left = 450, width = "auto", height = "auto",
                draggable = F,
                
                wellPanel(
                  sliderInput("date", "Date Range",
                              min = min(covid$date),
                              max = max(covid$date),
                              value = c(min(covid$date), max(covid$date)),
                              timeFormat = "%Y-%m-%d")
                ),
                selectInput("country", "Select countries:",
                            choices = all_countries,
                            multiple = T,
                            selected = c("South Korea", "France", "Germany", "Israel", "Japan", "United Kingdom", "United States")),
                
                selectInput("select_1", "Select variable:",
                            choices = colname,
                            multiple = F,
                            selected = "new_cases")
              )),
            
            fluidRow(
              absolutePanel(
                top = 1000, left = 900, width = 1500, height = "auto",
                draggable = F,
                helpText("Selected countries and variable will be shown in plot blew"),
                helpText("If you select variable with [total], size of circles in the map will be too large"),
                helpText("Don't select [rate variables] like reproduction rate and test positive rate"),
                plotlyOutput("plot_1"),
                
              ))
            
    ),
    
    tabItem(tabName = "korea",
            h1(paste0('COVID-19 Korea in ', tail(infect$Date,1))),
            
            fluidRow(
              infoBoxOutput("infecteeBox"),
              infoBoxOutput("severeBox"),
              infoBoxOutput("deathBox")
            ),
            
            
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("virus"), "Daily Confirmed"),
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "750px",
                tabPanel("Daily Confirmed",
                         sliderInput(inputId="date_range1", label="Date range", 
                                     value=c(head(gathinf$Date,1), tail(gathinf$Date,1)), 
                                     min=as.Date(head(gathinf$Date,1)), max=as.Date(tail(gathinf$Date,1)), step=1, width = 1000),
                         checkboxGroupInput(inputId="age_groups1", label="Age group", choices = Ages2,inline = TRUE),
                         highchartOutput(outputId="daily_confirmed_kor")
                )),
              
              tabBox(
                title = tagList(shiny::icon("hospital"), 'Death/Severe Cases'),
                side = "left", height = "750px",
                selected = 'Cumulative Death',
                
                tabPanel("Cumulative Death",
                         sliderInput(inputId="date_range2", label="Date range", 
                                     value=c(head(gathdeath$Date,1), tail(gathdeath$Date,1)), 
                                     min=as.Date(head(gathdeath $Date,1)), max=as.Date(tail(gathdeath $Date,1)), step=1, width = 1000),
                         checkboxGroupInput(inputId="age_groups2", label="Age group", choices = Ages2,inline = TRUE),
                         highchartOutput(outputId="daily_death_kor")),
                
                tabPanel("Daily Severe",
                         sliderInput(inputId="date_range3", label="Date range", 
                                     value=c(head(gathsev$Date,1), tail(gathsev$Date,1)), 
                                     min=as.Date(head(gathsev$Date,1)), max=as.Date(tail(gathsev$Date,1)), step=1, width = 1000),
                         checkboxGroupInput(inputId="age_groups3", label="Age group", choices = Ages2,inline = TRUE),
                         highchartOutput(outputId="daily_severe_kor"))
              )),
            
            fluidRow(
              tabBox(
                title = tagList(shiny::icon("syringe"), "Vaccination"),
                side = 'left', height = '750px',
                selected = '1st & 2nd Dose',
                
                tabPanel("1st & 2nd Dose",
                         sliderInput(inputId="date_range4", label="Date range", 
                                     value=c(head(gatvac1$Date,1), tail(gatvac1$Date,1)), 
                                     min=as.Date(head(gatvac1$Date,1)), max=as.Date(tail(gatvac1$Date,1)), step=1, width = 1000),
                         checkboxGroupInput(inputId="vaccine_types", label="Vaccine types", choices = Vaccines,inline = TRUE),
                         highchartOutput(outputId="firstsecond_vac")),
                
                tabPanel("3rd Dose",
                         sliderInput(inputId="date_range5", label="Date range", 
                                     value=c(head(vac2$Date,1), tail(vac2$Date,1)), 
                                     min=as.Date(head(vac2$Date,1)), max=as.Date(tail(vac2$Date,1)), step=1, width = 1000),
                         checkboxGroupInput(inputId="vaccine_types2", label="Vaccine types", choices = Vaccines,inline = TRUE),
                         highchartOutput(outputId="third_vac")
                )),
              
              tabBox(
                title = tagList(shiny::icon("bar-chart-o"), "Vaccination by Age Group"),
                side = "left", height = "750px",
                
                tabPanel("Age Groups Vaccination",
                         sliderInput(inputId="date_range6", label="Date range",
                                     value=c(head(gatvac3$Date,1), tail(gatvac3$Date,1)), 
                                     min=as.Date(head(gatvac3$Date,1)), max=as.Date(tail(gatvac3$Date,1)), step=1, width = 1000),
                         radioButtons("Dose", "Dose", Doses, inline=TRUE),
                         checkboxGroupInput(inputId="age_groups4", label="Age group", choices = Ages2,inline = TRUE),
                         highchartOutput(outputId="age_vac"))
              )
            )),
    
    tabItem(tabName = "report",
            h1(paste0('Predict COVID-19 Korea in ', head(df$date,1), ' ~ ', tail(df$date,1))),
            
            sliderInput(inputId="date_range", label="Date range", 
                        value=c(as.Date(colnames(result)[4]), as.Date(colnames(result)[ncol(result)])), 
                        min=as.Date(colnames(result)[4]), max=as.Date(colnames(result)[ncol(result)]), step=1, width = 1000),
            radioButtons("types", "Type", Options, inline = TRUE),
            radioButtons("age_groups", "Age group", Ages, inline = TRUE),
            checkboxGroupInput(inputId="scenarios", label="Scenario", choices = Scenario, inline = TRUE),
            mainPanel(
              highchartOutput(outputId="date_range_plot")
            )),
    
    tabItem(tabName = "model",
            h1('Our Model'),
            fluidPage(
              plotOutput("model"))),
    
    tabItem(tabName = "questions",
            h1(paste0('Questions')),
            helpText("Contact us if you have any questions"),
            helpText("Seoul National University, Building 220, Room 712 \n
          (08826) 1 Gwanak-ro, Gwanak-gu, Seoul, Republic of Korea \n
          (08826) 서울특별시 관악구 관악로 1 서울대학교 보건대학원 \n
                   만성병역학연구실 220동 712호"))
    
  ))


ui <- dashboardPage(skin = 'purple',
                    dashboardHeader(
                      title = "COVID-19 Workshop",
                      titleWidth = 300
                    ),
                    sidebar,
                    dashboardBody(
                      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                      body
                    ))

## Build-up Server

server <- function(input, output, session) {
  
  filtered <- reactive({
    covid %>% 
      filter(date >= input$date[1],
             date <= input$date[2]) %>% 
      filter(location %in% input$country)
  })
  
  output$plot_1 <- renderPlotly({
    
    plot <- ggplot(data = filtered(),
                   aes(x = date,
                       y = get(input$select_1),
                       colour = location)) +
      geom_line() +
      labs(x = "", y = "") +
      theme_bw()
    
    ggplotly(plot) %>% hide_legend()
  })
  
  output$map <- renderLeaflet({
    
    covid1 <- covid %>% filter(date == max(input$date))
    
    covid_final <- merge(countries, covid1, by = "iso_code", all.x = T, dupulicateGeom = T)
    
    covid_final$selected <- covid_final[, paste(input$select_1)]
    
    color_pal <- colorNumeric(palette = "inferno", domain = covid_final$selected, reverse = F)
    
    leaflet() %>%
      setView(lng = 2, lat = 46, zoom = 3.5) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addCircles(
        data = covid_final,
        lng = ~longitude,
        lat = ~latitude,
        weight = 1,
        radius = ~(60*log(covid_final$selected))^2,
        fillOpacity = 0.2,
        popup = paste0(covid_final$country,":", covid_final$selected),
        color = ~color_pal(covid_final$selected),
      )
  })
  
  output$infecteeBox <- renderInfoBox({
    infoBox(
      "CONFIRMED CASES", paste0(tail(infect$total,1), " 명"), icon = icon("virus"),
      color = "purple", fill = TRUE
    )
  })
  output$severeBox <- renderInfoBox({
    infoBox(
      "SEVERE CASES", paste0(tail(severecase$total,1)," 명"), icon = icon("hospital"),
      color = "red", fill = TRUE
    )
  })
  
  output$deathBox <- renderInfoBox({
    infoBox(
      "CUMULATIVE DEATH", paste0(tail(death$total,1)," 명"), icon = icon("remove"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$daily_confirmed_kor <- renderHighchart({
    highchart() %>%
      hc_add_series(data = gathinf %>% 
                      filter(age_group %in% input$age_groups1) %>%
                      filter(Date >= input$date_range1[1] & Date <= input$date_range1[2]),
                    mapping = hcaes(y = num, group = age_group),
                    type = 'line',
                    marker = list(symbol = 'circle') 
      ) %>%
      hc_xAxis( categories = unique(gathinf$Date) ) %>% 
      
      hc_add_theme(hc_theme_economist())
  })
  
  output$daily_severe_kor <- renderHighchart({
    highchart() %>%
      hc_add_series(data = gathsev %>% 
                      filter(age_group %in% input$age_groups3) %>%
                      filter(Date >= input$date_range3[1] & Date <= input$date_range3[2]),
                    mapping = hcaes(y = num, group = age_group),
                    type = 'line',
                    marker = list(symbol = 'circle') 
      ) %>%
      hc_xAxis( categories = unique(gathsev$Date) ) %>% 
      
      hc_add_theme(hc_theme_economist())
  })
  
  output$daily_death_kor <- renderHighchart({
    highchart() %>%
      hc_add_series(data = gathdeath %>% 
                      filter(age_group %in% input$age_groups2) %>%
                      filter(Date >= input$date_range2[1] & Date <= input$date_range2[2]),
                    mapping = hcaes(y = num, group = age_group),
                    type = 'line',
                    marker = list(symbol = 'circle') 
      ) %>%
      hc_xAxis( categories = unique(gathdeath$Date) ) %>% 
      
      hc_add_theme(hc_theme_economist())
  })
  
  # Vaccination
  output$firstsecond_vac <- renderHighchart({
    highchart() %>%
      hc_add_series(data = gatvac1 %>% 
                      filter(vaccine %in% input$vaccine_types) %>%
                      filter(Date >= input$date_range4[1] & Date <= input$date_range4[2]),
                    mapping = hcaes(y = num, group = vaccine),
                    type = 'line',
                    marker = list(symbol = 'circle') 
      ) %>%
      hc_xAxis( categories = unique(gatvac1$Date) ) %>% 
      
      hc_add_theme(hc_theme_superheroes())
  })
  
  output$third_vac <- renderHighchart({
    highchart() %>%
      hc_add_series(data = vac2 %>% 
                      filter(vaccine %in% input$vaccine_types2) %>%
                      filter(Date >= input$date_range5[1] & Date <= input$date_range5[2]),
                    mapping = hcaes(y = num, group = vaccine),
                    type = 'line',
                    marker = list(symbol = 'circle') 
      ) %>%
      hc_xAxis( categories = unique(vac2$Date) ) %>% 
      
      hc_add_theme(hc_theme_superheroes())
  })
  
  output$age_vac <- renderHighchart({
    highchart() %>%
      hc_add_series(data = gatvac3 %>% 
                      filter(Dose %in% input$Dose) %>%
                      filter(age_group %in% input$age_groups4) %>%
                      filter(Date >= input$date_range6[1] & Date <= input$date_range6[2]),
                    mapping = hcaes(y = num, group = age_group),
                    type = 'line',
                    marker = list(symbol = 'circle') 
      ) %>%
      hc_xAxis( categories = unique(gatvac3$Date) ) %>% 
      
      hc_add_theme(hc_theme_superheroes())
  })
  
  # Reports
  output$date_range_plot <-  renderHighchart({
    highchart() %>%
      hc_add_series( data = df %>% 
                       filter(type %in% input$types) %>%
                       filter(scenario %in% input$scenarios) %>%
                       filter(age_group %in% input$age_groups) %>%
                       filter(date >= input$date_range[1] & date <= input$date_range[2]),
                     mapping = hcaes(y = num, group = scenario),
                     type = 'line',
                     marker = list(symbol = 'circle') 
      ) %>%
      hc_xAxis( categories = unique(df$date) ) %>% 
      
      hc_add_theme(hc_theme_economist())
  })
  
  output$model <- renderImage({
    filename <- normalizePath(file.path('./fig',
                                        paste0('model', '.png')))
    list(src = filename)}, 
    deleteFile = FALSE)
}

## Run app

shinyApp(ui = ui, server = server)

