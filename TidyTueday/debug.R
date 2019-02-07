

#lets make it all nice and shiny now 
library(shiny)
all_states = c('tx','ca','fl')
ui = fluidPage(
  fluidRow(sliderInput('dateRange','Select years to examine', min = 1980, max =2018,sep = '', value = c(1990,2015))),
  sidebarLayout(
    sidebarPanel(width = 2,
                 checkboxGroupInput(inputId = 'stateInput',label = 'select states to display',choices = all_states, selected = states)
    )
    ,mainPanel(
      textOutput(outputId = 'debugText')
    )
  )
)

server = function(input,output) { 
  output$debugText  = renderText(input$dateRange[2])
}

shinyApp(ui= ui, server = server)