# This file should contain the maximum likelihood functions
#ESCRIBA AQI Dr. TRUJANO!


source("src/Simulate_Game.R")
source("src/Bayes.R")

simulate_game_results <- simulate_game(matrixRow = m1,matrixCol = m2,pairs = 50,lambda = 2,belives_row = rep(.25,4),belives_col = rep(.25,4),trials = 100)

#### Bayes single lambda ####
Bayes_QRE_sl<-function(simulate_game_results,collapsed,inits){
  library(R2jags,quietly=T,warn.conflicts = F) # 1que hace?
  source("src/Simulate_Game.R")
  if(is.character(data)){
    data<-get(load(paste(c("results/",data),collapse="")))
  }
  else{
    data<-simulate_game_results
  }
  if(missing(collapsed)){
    collapsed<-F
  }
  if(collapsed==T){
    choice_r<-data$row$collapsed
    choice_c<-data$col$collapsed
  }
  else{
    choice_r<-data$row$bypair
    choice_c<-data$col$bypair
  }
  Es_row<-expected_payoffs(data$parameters$games$R,
                           as.vector(data$col$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  Es_col<-expected_payoffs(t(data$parameters$games$C),
                           as.vector(data$row$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  
}
