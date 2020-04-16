library(tidyverse)
library(shiny)

interpretGrades = function(path){
  path = as.character(path[4])
  inputList = path %>% 
    readxl::excel_sheets() %>% 
    set_names() %>% 
    map(readxl::read_excel, path = path)
  
  
  studentList = inputList[[1]] %>% 
    select(c(1,2)) %>% 
    slice(-1) %>% 
    rename(GradeLevel = "..2") %>% 
    rename(StudentID = Student)
  
  #sheet 2 is also kind of funky in that we need to pull TWO samples for it: both the trailing period and current period 
  #grab trailing from sheet 2 and instantiate grades List 
  gradesList = inputList[[2]] %>% 
    select(c(TermTested,Subject, StudentID, StartScore)) %>%  
    rename(Score = StartScore) %>% 
    mutate(TermTested = 'Fall 2017-2018') %>% 
    as.data.frame()
  
  #load in remaining sheets
  for(num in 2:length(inputList)){
    toAdd = inputList[[num]] %>% 
      select(c(TermTested,Subject, StudentID, EndScore)) %>% 
      rename(Score = EndScore)
    gradesList = rbind(gradesList, toAdd)
  }
  
  #ok sheet 4 is strange as well -- anyting in the fall to winter range gets funky...This step will need 
  #to be repeated if more sheets are added where theres a double count that has to occur
  
  toAdd = inputList[[4]] %>% 
    select(c(TermTested,Subject, StudentID, StartScore)) %>%  
    rename(Score = StartScore) %>% 
    mutate(TermTested = 'Fall 2018-2019') %>% 
    as.data.frame()
  gradesList = rbind(gradesList, toAdd)
  
  
  
  finalOutput = gradesList %>% 
    mutate(Period = paste(TermTested, Subject)) %>% 
    select(3:5) %>% 
    spread(Period, Score) %>% 
    right_join(studentList) %>% 
    select(c(StudentID, GradeLevel, #Reordering columns : \ 
             `Fall 2017-2018 Mathematics`, `Fall 2017-2018 Reading`, `Fall 2017-2018 Language Usage`, `Fall 2017-2018 Science - General Science`,
             `Winter 2017-2018 Mathematics`, `Winter 2017-2018 Reading`, `Winter 2017-2018 Language Usage`, `Winter 2017-2018 Science - General Science`,
             `Spring 2017-2018 Mathematics`, `Spring 2017-2018 Reading`, `Spring 2017-2018 Language Usage`, `Spring 2017-2018 Science - General Science`,
             `Fall 2018-2019 Mathematics`, `Fall 2018-2019 Reading`, `Fall 2018-2019 Language Usage`, `Fall 2018-2019 Science - General Science`,
             `Winter 2018-2019 Mathematics`, `Winter 2018-2019 Reading`, `Winter 2018-2019 Language Usage`, `Winter 2018-2019 Science - General Science`,
             `Spring 2018-2019 Mathematics`, `Spring 2018-2019 Reading`, `Spring 2018-2019 Language Usage`, `Spring 2018-2019 Science - General Science`
    )) 
  return(finalOutput)
}

##SHINY ###############

ui = fluidPage(
  fluidRow(
    fileInput(inputId = 'xlsx', label = 'Please upload the XLSX you wish to Transform.', multiple = FALSE, accept = c('.xlsx'))
  ),
  fluidRow(tableOutput(outputId = 'table1')),
  fluidRow(downloadButton("downloadData", "Download"))
)

server = function(input,output){
  
  myTable = reactive({
    inFile = input$xlsx 
    if(is.null(inFile)){return()}
    interpretGrades(inFile)
  })
  output$table1 = renderTable({
    myTable()
    
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(),"_gradesExport.csv", sep = "")
    },
    content = function(file) {
      write.csv(myTable(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
