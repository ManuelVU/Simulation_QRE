# This file should contain the maximum likelihood functions
#ESCRIBA AQI Dr. TRUJANO!
rm(list=ls())
source("src/Simulate_Game.R")
source("src/Bayes.R")
library(stats4)
library(nleqslv)

n=4
# oneil
m1 <- matrix(c(-1,1,1,-1, 1,-1,1,-1,1,1,-1,-1, -1,-1,-1,1),nrow=n) 
m1 <- m1*5
m2 <- m1*(-1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
believes <- matrix(rep(c(.2,.2,.2,.4),2),nrow=2,byrow = T)

simulate_game_results <- simulate_game(matrixRow = m1,matrixCol = m2,
                                       pairs = 50,lambda = .5,trials = 10000,
                                       belives_row = rep(.25,4),belives_col = rep(.25,4))

#### Stadarization ####
Estandar_payoffs <- function(matrixPayoff){
  (max(matrixPayoff)-matrixPayoff)/(max(matrixPayoff)-min(matrixPayoff))
}

#### Stochastic Best Response ####
sigma_SBR <- function(Es_row,Es_col,lambda=.5){
  sigma_SBR_row <- exp(lambda*Es_row)/sum(exp(lambda*Es_row))
  sigma_SBR_col <- exp(lambda*Es_col)/sum(exp(lambda*Es_col))
  return(list(sigma_SBR_row,sigma_SBR_col))
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
neg_log_L_nleqslv <- function(lambda, matrixRow,matrixCol_t,choice_r,choice_c){
  QRE_sol <- QRE_sol(matrixRow,matrixCol_t,lambda)
  lob_L_row <- dmultinom(x = choice_r,
                         prob=QRE_sol[(1+ncol(matrixRow)):(ncol(matrixRow)+nrow(matrixRow))],
                          log = T)
  lob_L_col <- dmultinom(x = choice_c,
                         prob=QRE_sol[1:ncol(matrixRow)],
                         log = T)
  log_L <- lob_L_row + lob_L_col
  return(-log_L)
}
### E_hat
neg_log_L_Ehat <- function(Es_row,Es_col,lambda,choice_r,choice_c){
  SBR = sigma_SBR(Es_row,Es_col,lambda)
  log_L_Ehat = dmultinom(choice_r,prob= SBR[[1]],log = T)+
    dmultinom(choice_c,prob= SBR[[2]],log = T)
  return(-log_L_Ehat)
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
                           as.vector(data$col$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),lambda = 1)
  Es_col<-expected_payoffs( matrixCol_t ,
                           as.vector(data$row$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),lambda = 1)
  if(collapsed==T){
    choice_r<- data$row$collapsed
    choice_c<- data$col$collapsed
    MLE_collapsed_E_prop <- data.frame(matrix(ncol = 2,nrow = 1))
    MLE_collapsed_nleqslv <- data.frame(matrix(ncol = 2,nrow = 1))
    colnames(MLE_collapsed_E_prop) <- colnames(MLE_collapsed_nleqslv) <- c("Lambda","SD")
    # Expected payoffs
    MLE_E_collpased <- mle(minuslogl = neg_log_L_Ehat,start = list(lambda=data$parameters$lambda[1,1]),
        fixed = list(Es_row=Es_row,Es_col=Es_col,choice_r=choice_r,choice_c=choice_c),
        method = "L-BFGS-B",lower=0,upper=100)
    MLE_collapsed_E_prop["Lambda"] <- MLE_E_collpased@coef
    MLE_collapsed_E_prop["SD"] <- sqrt(MLE_E_collpased@vcov)
    # solving equations
    MLE_nleqslv <- mle(minuslogl = neg_log_L_nleqslv,start = list(lambda=data$parameters$lambda[1,1]),
                       fixed = list(matrixRow = matrixRow,matrixCol_t = matrixCol_t,
                                    choice_r = choice_r,choice_c = choice_c))
    MLE_collapsed_nleqslv["Lambda"] <-  MLE_nleqslv@coef
    MLE_collapsed_nleqslv["SD"] <-  sqrt(MLE_nleqslv@vcov)
    return(list(Collampsed_Ehat=MLE_collapsed_E_prop,
                Collapsed_nleqslv=MLE_collapsed_nleqslv))
  }
  else{
    choice_r<-data$row$bypair
    choice_c<-data$col$bypair
    pairs <- data$parameters$exp[1]
    MLE_bypair_Ehat <- data.frame(matrix(nrow = pairs,ncol = 2))
    colnames(MLE_bypair_Ehat) <-  c("Lambda","SD")
    MLE_bypair_nleqslv <- data.frame(matrix(nrow = pairs,ncol = 2))
    colnames(MLE_bypair_nleqslv) <- c("Lambda","SD")
    for(p in 1:pairs){
      # Expected payoffs
      Es_row<-expected_payoffs(matrixRow,
                               as.vector(data$col$bypair[p,]/(data$parameters$exp[2]))
                               ,lambda = 1)
      Es_col<-expected_payoffs( matrixCol_t ,
                                as.vector(data$row$bypair[p,]/(data$parameters$exp[2]))
                                ,lambda = 1)
      MLE_p <- mle(minuslogl = neg_log_L_Ehat,start = list(lambda=1),
          fixed = list(Es_row=Es_row,Es_col=Es_col,
                       choice_r=choice_r[p,],choice_c=choice_c[p,])
          ,method = "L-BFGS-B",lower=0,upper=100)
      MLE_bypair_Ehat[p,"Lambda"] <- MLE_p@coef
      MLE_bypair_Ehat[p,"SD"] <- sqrt(MLE_p@vcov)
      # solving equations
      MLE_nleqslv <- mle(minuslogl = neg_log_L_nleqslv,start = list(lambda=1),
                                   fixed = list(matrixRow = matrixRow,matrixCol_t = matrixCol_t,
                                                choice_r = choice_r[p,],choice_c = choice_c[p,]))
      MLE_bypair_nleqslv[p,"Lambda"] <- MLE_nleqslv@coef
      MLE_bypair_nleqslv[p,"SD"] <- sqrt(MLE_nleqslv@vcov)
    }
    return(list(Bypair_Ehat=MLE_bypair_nleqslv,
                Bypair_nleqslv=MLE_bypair_nleqslv))
  }

}


MLE_QRE_sl(simulate_game_results,collapsed=T)
MLE1 <- MLE_QRE_sl(simulate_game_results,collapsed=F)

