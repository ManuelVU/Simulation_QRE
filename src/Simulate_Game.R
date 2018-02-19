#### Expected payoff function ####
expected_payoffs <- function(matrixP,sigma_other,lambda){
  ep<-matrixP%*%sigma_other*lambda
  return(as.vector(ep))
}
#### Expected payoff for bayes NLEQSL ####
belief_error_bayes <- function(sigma,lambda){
  expected_payoff1 <- data$parameters$games$R%*%sigma[1:ncol(m1)]*lambda
  expected_payoff2 <- t(data$parameters$games$C)%*%sigma[(ncol(m1)+1):((ncol(m1))+(nrow(m1)))]*lambda
  SBR <- {}
  SBR[(ncol(m1)+1):((ncol(m1))+(nrow(m1)))] <- exp(expected_payoff1)/(sum(exp(expected_payoff1)))
  SBR[1:ncol(m1)] <- exp(expected_payoff2)/(sum(exp(expected_payoff2)))
  return(SBR-sigma)
}
#### Game Simulation function ####
# Pairs = How many pairs to be simulated for each value of lambda
simulate_game<-function(matrixRow,matrixCol,pairs,trials,lambda,belives_row,belives_col){
  library(nleqslv)
  belief_error <- function(sigma){
    expected_payoff1 <- expected_payoffs(matrixP = matrixRow,sigma_other = sigma[1:ncol(matrixRow)],lambda = lambda[l,1])
    #expected_payoff1 <- matrixRow%*%sigma[1:ncol(matrixRow)]*lambda[l,1]
    expected_payoff2 <- expected_payoffs(matrixP = matrixCol_t,sigma_other = sigma[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))],lambda = lambda[l,2])
    #expected_payoff2 <- matrixCol_t%*%sigma[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))]*lambda[l,2]
    SBR <- {}
    SBR[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))] <- exp(expected_payoff1)/(sum(exp(expected_payoff1)))
    SBR[1:ncol(matrixRow)] <- exp(expected_payoff2)/(sum(exp(expected_payoff2)))
    return(SBR-sigma)
  }
  name_eq<-c(rep("C",ncol(matrixRow)),rep("R",nrow(matrixRow)))
  choice_r<-{}
  choice_c<-{}
  equilibriums<-{}
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
    equilibriums<-rbind(equilibriums,solution$x)
    choice_r<-rbind(choice_r,t(rmultinom(n_p,t,solution$x[(ncol(matrixRow)+1):(ncol(matrixRow)+nrow(matrixRow))])))
    choice_c<-rbind(choice_c,
      t(rmultinom(n_p,t,solution$x[1:ncol(matrixRow)])))
  }
  Results<-list()
  Results$row$bypair<-choice_r
  Results$col$bypair<-choice_c
  Results$row$collapsed<-colSums(choice_r)
  Results$col$collapsed<-colSums(choice_c)
  Results$parameters$lambda<-lambda
  Results$parameters$exp<-c(pairs,trials)
  Results$parameters$games$R<-matrixRow
  Results$parameters$games$C<-matrixCol
  Results$equilibrium<-equilibriums
  colnames(Results$equilibrium)<-name_eq
  return(Results)
  #under construction
}