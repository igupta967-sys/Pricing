#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(shiny)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(    
  # Give the page a title
  #titlePanel("Monthly price of grain commodities in China"),
  sidebarPanel(
    selectInput("commosel", "Commodity:", 
                choices = c('rice', 'wheat', 'maize')),
    # selectInput("currency", "Currency:", 
    #             choices = c('US dollar', 'Chinese yuan')),
    hr(),
    helpText("Updated on April 01, 2020."),
    helpText("Contact: Ishaan Gupta,  (igupta967@gmail.com)")
  ),
  # Create a spot for the barplot
  mainPanel(
    plotOutput("pricePlot"),
    textOutput("text1"),
    hr(),
    plotOutput("ratioPlot"),
    textOutput("text2"),
    hr(),
    helpText("Suggested citation: Chen, B, Villoria, NB, Xia, T. Tariff quota administration in China's grain markets: An empirical assessment. Agricultural Economics. 2020; 51: 191- 206. https://doi.org/10.1111/agec.12549"),
    hr(),
    helpText('You can download the data and codes from this website: https://github.com/cbw1243/ChinaGrainPrice.')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  currencyData <- fread('Currency_IMF.csv') %>%
    mutate(Date = as.Date(Date, format = '%d-%b-%Y'),
           Year = year(Date), Month = month(Date)) %>%
    group_by(Year, Month) %>%
    summarise(Currency = mean(Currency))
  #input <- list(commosel = 'rice', currency = 'Chinese yuan')
  
  plotData <- reactive({
    outData <-  fread('china_month_price_moa_update.csv') %>%
      mutate(time = as.Date(time, format = '%m/%d/%Y'),
             Year = year(time), Month = month(time))  %>% 
      left_join(., currencyData, by = c('Year',  'Month')) %>%
      mutate(price = price/Currency) %>%
      filter(commo == input$commosel)
    
    return(outData)
  })
  
  output$text1 <- renderText({
    Notes <- ifelse(input$commosel == 'rice', 
                    paste0('Notes: Domestic price is average wholesale price of No.1 late Indica rice observed at Huangpu port in Southern China. ',
                           'International price is price of Thai white long grain rice (25% broken), after duties and taxes, observed at Huangpu port in Southern China. '),
                    ifelse(input$commosel == 'wheat', 
                           paste0('Notes: Domestic price is price of high quality wheat observed at Huangpu port in Southern China. ',
                                  'International price is price of U.S hard red winter wheat, after duties and taxes, observed at Huangpu port in Southern China. '),
                           paste0('Notes: Domestic price is exit price of No.2 yellow maize shipped from northeastern China observed at Huangpu port in Southern China. ',
                                  'International price is price of U.S. No.2 yellow maize, after duties and taxes, observed at Huangpu port in Southern China. ')
                    ))
    Notes <- paste0(Notes, 'Original price data are measured in Chinese yuan and are then converted to US dollars using IMF currency data. Data source: Ministry of Agriculture of China.')
  })
  
  output$text2 <- renderText({
    Notes <- c('Notes: Price gap ratios = 100*(domestic price - international price)/international price')
  })
  # Fill in the spot we created for a plot
  output$pricePlot <- renderPlot({
    
    plotData() %>%
      ggplot(data = . ) +
      geom_line(aes(time, price, col = note, group = note)) +
      scale_color_manual(values = c('red', 'deepskyblue4'),
                         labels = c('Domestic price', 'International price'),
                         name = '') +
      scale_y_continuous(breaks = seq(0.1, 0.4, by = 0.02)) +
      scale_x_date(date_breaks = '6 months', date_labels = '%Y/%m',
                   limits = as.Date(c('2012-12-01', '2020-03-01'))) +
      labs(x = '', y = 'Price, USD per kg',
           title = paste0('Figure 1. Monthly nominal ', input$commosel,  ' prices in China')) +
      theme_light() +
      theme(legend.position = c(0.9, 0.9),
            legend.direction = 'vertical',
            legend.background = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
            plot.caption = element_text(hjust = 0),
            axis.text = element_text(color = 'black', size = 11),
            legend.text = element_text(color = 'black', size = 14))
    
  })
  
  
  output$ratioPlot <- renderPlot({
    
    plotData() %>%
      select(time, price, note) %>%
      spread(note, price) %>%
      mutate(ratio = 100*(domestic - import)/import) %>%
      select(time, ratio) %>%
      mutate(label = ifelse(ratio >= 0, 'positive', 'negative')) %>%
      ggplot(data = . ) +
      geom_bar(aes(time, ratio, fill = label), stat = 'identity') +
      theme_light() +
      scale_fill_manual(values = c('deepskyblue4', 'red'),
                        name = '')  +
      scale_y_continuous(breaks = seq(-10, 60, by = 10)) +
      scale_x_date(date_breaks = '6 months', date_labels = '%Y/%m',
                   limits = as.Date(c('2012-12-01', '2020-03-01'))) +
      theme(legend.position = 'none',
            plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
            plot.caption = element_text(hjust = 0),
            axis.text = element_text(color = 'black', size = 11),
            legend.text = element_text(color = 'black', size = 14)) +
      labs(x = '', y = 'Price gap (%)',
           title = paste0('Figure 2. Gap between monthly nominal domestic and international ', input$commosel, ' prices in China'))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


