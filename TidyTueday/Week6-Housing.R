library(dplyr)
library(ggplot2)
library(readr)

housing = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv')

makeDate = function(month,year){
  month = ifelse(nchar(month) == 1, paste0('0',month),month)
  return(as.Date(paste0(year,'-',month,'-01')))
}

new_housing = housing %>% 
  mutate(aDate = makeDate(month,year)) %>% 
  group_by(state) %>% 
  mutate(change_from_last_year = (price_index - lag(price_index,n = 12))/lag(price_index,n = 12)*100)

graphit = function(times,states){
  new_housing %>% 
    filter(state %in% states & year %in% times) %>% 
    ggplot(aes (x = aDate, y = change_from_last_year, color = state)) + 
    geom_segment(aes(xend = aDate, y = 0, yend = change_from_last_year), size = .5, alpha = 0.7) +
    theme_light() + 
    geom_point(size = .5) + 
    xlab('Date') + 
    ylab('Percent channge from previous year') + 
    facet_grid(. ~ state)
}  
#graphit(times,all_states)
          
all_states = levels(factor(new_housing$state))
all_years  = as.numeric(levels(factor(new_housing$year)))

#lets make it all nice and shiny now 
library(shiny)

ui = fluidPage(
  fluidRow(sliderInput('dateRange','Select years to examine', min = min(all_years), max = max(all_years),sep = '', value = c(1990,2015))),
  sidebarLayout(
    sidebarPanel(width = 2,
      checkboxGroupInput(inputId = 'stateInput',label = 'select states to display',choices = all_states, selected = sample(all_states,2))
    )
    ,mainPanel(width = 10,
      plotOutput(outputId = 'bigPlot')
    )
  )
)

server = function(input,output) { 
  output$bigPlot = renderPlot(graphit(input$dateRange[1]:input$dateRange[2],input$stateInput))
  }

shinyApp(ui= ui, server = server)
