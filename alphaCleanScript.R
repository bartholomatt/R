library(tidyverse)

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
  rename(Score = StartScore) %>% 
  mutate(TermTested = 'Fall 2017-2018') %>% 
  as.data.frame()

#load in remaining sheets
for(num in 2:length(sheets)){
  print(paste('loading tab ', sheets[num]))
  toAdd = readxl::read_xlsx('results.xlsx', sheet=sheets[num]) %>% 
    select(c(TermTested,Subject, StudentID, EndScore)) %>% 
    rename(Score = EndScore)
  gradesList = rbind(gradesList, toAdd)
}

#ok sheet 4 is strange as well -- anyting in the fall to winter range gets funky...This step will need 
#to be repeated if more sheets are added where theres a double count that has to occur

toAdd = readxl::read_xlsx('results.xlsx', sheet=sheets[4]) %>% 
  select(c(TermTested,Subject, StudentID, StartScore)) %>%  
  rename(Score = StartScore) %>% 
  mutate(TermTested = 'Fall 2018-2019') %>% 
  as.data.frame()
gradesList = rbind(gradesList, toAdd)


gradesList %>% 
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
           )) %>% 
  View()


