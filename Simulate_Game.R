# Pairs = How many pairs to be simulated for each value of lambda
simulate_game<-function(matrixRow,matrixCol,pairs,trials,lambda,belives_row,belives_col){
  library(nleqslv)
  belief_error <- function(sigma){
    expected_payoff1 <- matrixRow%*%sigma[1:ncol(matrixRow)]*lambda[l,1]
    expected_payoff2 <- matrixCol_t%*%sigma[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))]*lambda[l,2]
    SBR <- {}
    SBR[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))] <- exp(expected_payoff1)/(sum(exp(expected_payoff1)))
    SBR[1:ncol(matrixRow)] <- exp(expected_payoff2)/(sum(exp(expected_payoff2)))
    return(SBR-sigma)
  }
  choice_r<-{}
  choice_c<-{}
  matrixCol<-matrixCol
  matrixRow<-matrixRow
  matrixCol_t<-t(matrixCol)
  n_p<-pairs
  t<-trials
  if(missing(belives_row)){
    belives_row<-rep(1/nrow(matrixRow),nrow(matrixRow))
  }
  if(missing(belives_col)){
    belives_col<-rep(1/ncol(matrixRow),ncol(matrixRow))
  }
  belives<-c(belives_row,belives_col)
  if(is.vector(lambda)){
    lambda<-cbind(c(lambda),c(lambda))
  }
  for(l in 1:length(lambda[,1])){
    solution <-nleqslv(fn = belief_error,x=as.vector(belives))
    choice_r<-rbind(choice_r,t(rmultinom(n_p,t,solution$x[(ncol(matrixRow)+1):(ncol(matrixRow)+nrow(matrixRow))])))
    choice_c<-rbind(choice_c,
      t(rmultinom(n_p,t,solution$x[1:ncol(matrixRow)])))
  }
  Results<-list()
  Results$row<-choice_r
  Results$col<-choice_c
  return(Results)
  #under construction
}
datanew<-simulate_game(matrix(c(17,-1,1,-2,1,-1),nrow=3),-matrix(c(17,-1,1,-2,1,-1),nrow=3),
                       10,100,10,c(0.2,0.8),c(0.7,0.1,0.2))
