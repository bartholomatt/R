library(tidyverse)
library(shiny)

sheets = readxl::excel_sheets('results.xlsx')
#sheet 1 is different format than all others  

nSheets = length(sheets)

studentList = readxl::read_xlsx('results.xlsx',sheet=sheets[1],col_names = TRUE) %>% 
  select(c(1,2)) %>% 
  slice(-1) %>% 
  rename(GradeLevel = ...2) %>% 
  rename(StudentID = Student)

#sheet 2 is also kind of funky in that we need to pull TWO samples for it: both the trailing period and current period 
#grab trailing from sheet 2 and instantiate grades List 
gradesList = readxl::read_xlsx('results.xlsx', sheet=sheets[2]) %>% 
  select(c(TermTested,Subject, StudentID, StartScore)) %>%  
  rename(EndScore = StartScore) %>% 
  mutate(TermTested = 'Fall 2017-2018') %>% 
  as.data.frame()

#load in remaining sheets
for(num in 2:length(sheets)){
  print(paste('loading tab ', sheets[num]))
  toAdd = readxl::read_xlsx('results.xlsx', sheet=sheets[num]) %>% 
    select(c(TermTested,Subject, StudentID, EndScore))
  gradesList = rbind(gradesList, toAdd)
}



gradesList %>% 
  mutate(Period = paste(TermTested, Subject)) %>% 
  select(3:5) %>% 
  spread(Period, EndScore) %>% 
  right_join(studentList) %>% 
  View()


