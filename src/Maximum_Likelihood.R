# This file should contain the maximum likelihood functions
#ESCRIBA AQI Dr. TRUJANO!

source("src/Simulate_Game.R")
source("src/Bayes.R")
library(stats4)
library(nleqslv)

n=4
# oneil
m1 <- matrix(c(-1,1,1,-1, 1,-1,1,-1,1,1,-1,-1, -1,-1,-1,1),nrow=n) 
m1 <- m1*5
m2 <- m1*(-1)
believes <- matrix(rep(c(.2,.2,.2,.4),2),nrow=2,byrow = T)

simulate_game_results <- simulate_game(matrixRow = m1,matrixCol = m2,
                                       pairs = 50,lambda = .5,trials = 100,
                                       belives_row = rep(.25,4),belives_col = rep(.25,4))

#### Stochastic Best Response ####
sigma_SBR <- function(Es_row,Es_col,lambda=.5){
  sigma_SBR <- matrix(nrow = length(Es_row),ncol = 2)
  sigma_SBR[,1] <- exp(lambda*Es_row)/sum(exp(lambda*Es_row))
  sigma_SBR[,2] <- exp(lambda*Es_col)/sum(exp(lambda*Es_col))
  return(sigma_SBR)
}

#### Belief error ####
belief_error <- function(sigma,matrixRow,matrixCol_t,lambda){ #sigma son las creencias de ambos jugadores
  expected_payoff1 <- expected_payoffs(matrixP = matrixRow,sigma_other = sigma[1:ncol(matrixRow)],lambda = lambda)
  expected_payoff2 <- expected_payoffs(matrixP = matrixCol_t,sigma_other = sigma[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))],lambda = lambda)
  SBR <- {}
  SBR[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))] <- exp(expected_payoff1)/(sum(exp(expected_payoff1)))
  SBR[1:ncol(matrixRow)] <- exp(expected_payoff2)/(sum(exp(expected_payoff2)))
  return(SBR-sigma)
}
# Remember that in output, col strategies go first

## QRE ####
QRE_sol <- function(matrixRow,matrixCol_t,lambda){
  QRE_sol<- nleqslv(fn = belief_error,
          x=c(rep(1/nrow(matrixRow),nrow(matrixRow)),rep(1/ncol(matrixRow),ncol(matrixRow))),
          matrixRow=matrixRow,matrixCol_t= matrixCol_t,lambda=lambda)[[1]]
  return(QRE_sol)
}
## neg log likelihood ####
### nleqslv
neg_log_L_collapsed_nleqslv <- function(lambda, matrixRow,matrixCol_t,choice_r,choice_c){
  QRE_sol <- QRE_sol(matrixRow,matrixCol_t,lambda)
  log_L_collapsed <- 
    dmultinom(choice_r,prob= QRE_sol[(1+ncol(matrixRow)):(ncol(matrixRow)+nrow(matrixRow))],log = T)+
    dmultinom(choice_c,prob= QRE_sol[1:ncol(matrixRow)],log = T)
  return(-log_L_collapsed)
}
### E_hat
neg_log_L_collapsed <- function(Es_row,Es_col,lambda,choice_r,choice_c){
  SBR = sigma_SBR(Es_row,Es_col,lambda)
  log_L_collapsed = dmultinom(choice_r,prob= SBR[,1],log = T)+
    dmultinom(choice_c,prob= SBR[,2],log = T)
  return(-log_L_collapsed)
}

#### MLE single lambda ####
MLE_QRE_sl<-function(simulate_game_results,collapsed=F){
  if(is.character(data)){
    data<-get(load(paste(c("results/",simulate_game_results),collapse="")))
  }
  else{
    data<-simulate_game_results
  }
  # Game payoffs used in sumulations
  matrixRow <- data$parameters$games$R
  matrixCol_t <- t(data$parameters$games$C)
  #Expected Payoffs given proportions collapsed
  Es_row<-expected_payoffs(matrixRow,
                           as.vector(data$col$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  Es_col<-expected_payoffs( matrixCol_t ,
                           as.vector(data$row$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  if(collapsed==T){
    choice_r<-data$row$collapsed
    choice_c<-data$col$collapsed
    # Expected payoffs
    MLE_collapsed_E_prop <- mle(minuslogl = neg_log_L_collapsed,start = list(lambda=1),
        fixed = list(Es_row=Es_row,Es_col=Es_col,choice_r=choice_r,choice_c=choice_c))
    # solving equations
    MLE_collapsed_nleqslv <- mle(minuslogl = neg_log_L_collapsed_nleqslv,start = list(lambda=1),
        fixed = list(matrixRow = matrixRow,matrixCol_t = matrixCol_t,choice_r = choice_r,choice_c = choice_c))
  }
  else{
    choice_r<-data$row$bypair
    choice_c<-data$col$bypair
    for(p in 1:length(choice_r)){
      
    }
  }
  return(list(Ehat=MLE_collapsed_E_prop@coef,nonlinear=MLE_collapsed_nleqslv@coef))
}


MLE_QRE_sl(simulate_game_results,collapsed=T)

