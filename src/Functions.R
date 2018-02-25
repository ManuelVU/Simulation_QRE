#### Expected Payoff function ####
expected_payoffs <- function(matrixP,sigma_other,lambda){
  ep<-matrixP%*%sigma_other*lambda
  return(as.vector(ep))
}
#### Belief Error ####
belief_error <- function(sigma,matrixRow,matrixCol_t,lambda){ #sigma son las creencias de ambos jugadores
  expected_payoff1 <- expected_payoffs(matrixP = matrixRow,sigma_other = sigma[1:ncol(matrixRow)],lambda = lambda)
  expected_payoff2 <- expected_payoffs(matrixP = matrixCol_t,sigma_other = sigma[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))],lambda = lambda)
  SBR <- {}
  SBR[(ncol(matrixRow)+1):((ncol(matrixRow))+nrow(matrixRow))] <- exp(expected_payoff1)/(sum(exp(expected_payoff1)))
  SBR[1:ncol(matrixRow)] <- exp(expected_payoff2)/(sum(exp(expected_payoff2)))
  return(SBR-sigma)
}