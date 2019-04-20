library(dplyr)
library(ggplot2)
library(shiny)

#crap. We're gonna have to manually enter the scoring table. 

ones = c('','','','','','','')
twos = c('','2/2','3/2','4/2','5/2','6/2','7/2')
threes = c('','2/3','2/2','3/2','4/2','5/2','6/2')
fours = c('','2/4','2/3','3/3','4/3','5/3','5/2')
fives = c('','2/5','2/4','3/4','4/4','5/4','5/3')
sixes = c('','2/6','2/5','3/5','4/5','5/5','5/4')
sevens = c('','2/7','2/6','2/5','3/5','4/5','5/5')

score_table = data.frame(ones,twos,threes,fours,fives,sixes,sevens,stringsAsFactors = FALSE)


determineWinner = function(gamesToWin1, gamesToWin2, pVictory){
  gamesWonP1 = 0
  gamesWonP2 = 0 
  while ((gamesWonP1 < gamesToWin1) & (gamesWonP2 < gamesToWin2)){
    roll = runif(1)
    if (roll < pVictory){
      gamesWonP1 = gamesWonP1 + 1
    }else{
      gamesWonP2 = gamesWonP2 + 1}
  }
  results = (c(gamesWonP1,gamesWonP2))
  points1 = 0 
  points2 = 0
  if(gamesWonP2 == 0){
    points1 = 3
    points2 = 0
  }else if(gamesWonP1 == 0){
    points1 = 0
    points2 = 3
  }else if(gamesWonP1 == gamesToWin1){
    points1 = 2
  }else if(gamesWonP2 == gamesToWin2){
    points2 = 2
  }
  #hill checks
  if(gamesWonP1 == gamesToWin1-1){
    points1 = 1
  }
  if (gamesWonP2 == gamesToWin2 -1){
    points2 = 1
  }
  return(points1 - points2)
}

determineWinner(2,3,.5)
# 
# mean(results)
# sd(results)
# ggplot(data.frame(net_points=results) ,aes(x=net_points) ) +geom_histogram(bins = 13) + scale_x_discrete(limits = c(-3,-2,-1,0,1,2,3))
# 
 p1Skill = 2
 p2Skill = 3
 pVictory = .4

poolidge = function(p1Skill, p2Skill, pVictory){
  tableLookup = unlist(strsplit(score_table[p1Skill,p2Skill],'/'))
  p1ToWin = as.numeric(tableLookup[1])
  p2ToWin = as.numeric(tableLookup[2])
  nTrials = 10000
  results = rep(0,nTrials)
  for(x in 1:nTrials){
    results[x] = determineWinner(p1ToWin,p2ToWin,pVictory)
  }
  simMean = round(mean(results),2)
  simSD = sd(results)
  theplot = ggplot(data.frame(net_points=results) ,aes(x=net_points) ) + 
    geom_histogram(bins = 13) +
    scale_x_discrete(limits = c(-3,-2,-1,0,1,2,3)) + 
    labs(title = paste('After',nTrials, 'Simulations: Expected Net Points = ', simMean))
  retObject = list(simMean,simSD,theplot)
  return(retObject)
}


ui = fluidPage(
  titlePanel('Palvin Poolidge Top Secret'),
  sidebarLayout(
    sidebarPanel(numericInput(inputId = 'ourScore',
                              label = 'Input Our Team Player Rating',
                              value = 3,
                              min = 2, max = 7, step = 1),
                 numericInput(inputId = 'theirScore',
                  label = 'Input Their Team Player Rating',
                  value = 3,
                  min = 2, max = 7, step = 1),
                 numericInput(inputId = 'pWin',
                 label = 'Input %Probability of Our Player Winning',
                 value = .5,
                 min = 0, max = 1, step = .05
                )
                ),
    mainPanel(plotOutput(outputId = 'mainPlot')
              )
  )
)

server = function(input,output) {
  output$mainPlot = renderPlot({
    plotObj = poolidge(input$ourScore,input$theirScore,input$pWin)
    plotObj[[3]]
  })
  
  }

shinyApp(ui = ui, server = server)