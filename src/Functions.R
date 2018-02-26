#### Expected Payoff function ####
expected_payoffs <- function(matrixP,sigma_other,lambda){
  ep<-matrixP%*%sigma_other*lambda
  return(as.vector(ep))
}

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

#### Belief Error ####
belief_error <- function(sigma,matrixRow,matrixCol_t,lambda, lambdaCol=lambda){ #sigma son las creencias de ambos jugadores
  n_Row <- nrow(matrixRow)
  n_Col <- nrow(matrixCol_t)
  expected_payoffsRow <- expected_payoffs(matrixP = matrixRow,sigma_other = sigma[1:n_Col],lambda = lambda)
  expected_payoffsCol <- expected_payoffs(matrixP = matrixCol_t,sigma_other = sigma[(n_Col+1):(n_Col+n_Row)],lambda = lambdaCol)
  SBR <- {}
  SBR[(n_Col+1):(n_Col+n_Row)] <- exp(expected_payoffsRow)/(sum(exp(expected_payoffsRow)))
  SBR[1:n_Col] <- exp(expected_payoffsCol)/(sum(exp(expected_payoffsCol)))
  print(SBR)
  return(SBR-sigma)
}


## QRE ####
QRE_sol <- function(matrixRow,matrixCol_t,lambda){
  QRE_sol<- nleqslv(fn = belief_error,
                    x=c(rep(1/nrow(matrixRow),nrow(matrixRow)),rep(1/ncol(matrixRow),ncol(matrixRow))),
                    matrixRow=matrixRow,matrixCol_t= matrixCol_t,lambda=lambda)
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
MLE_QRE_sl<-function(data,collapsed=F){
  if(is.character(data)){
    data<-get(load(paste(c("results/",data),collapse="")))
  }
  else{
    data<-data
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
    MLE_E_collpased <- mle(minuslogl = neg_log_L_Ehat,
                           start = list(lambda=rexp(n = 1,rate = 1/(data$parameters$lambda[1,1]))),
                           fixed = list(Es_row=Es_row,Es_col=Es_col,choice_r=choice_r,choice_c=choice_c),
                           method = "L-BFGS-B",lower=0,upper=100
    )
    MLE_collapsed_E_prop["Lambda"] <- MLE_E_collpased@coef
    MLE_collapsed_E_prop["SD"] <- sqrt(MLE_E_collpased@vcov)
    # solving equations
    MLE_nleqslv <- mle(minuslogl = neg_log_L_nleqslv,
                       start = list(lambda=rexp(n = 1,rate = 1/(data$parameters$lambda[1,1]))),
                       fixed = list(matrixRow = matrixRow,matrixCol_t = matrixCol_t,
                                    choice_r = choice_r,choice_c = choice_c),
                       method = "L-BFGS-B",lower=0,upper=100
    )
    MLE_collapsed_nleqslv["Lambda"] <-  MLE_nleqslv@coef
    MLE_collapsed_nleqslv["SD"] <-  sqrt(MLE_nleqslv@vcov)
    return(list(Collampsed_Ehat=MLE_collapsed_E_prop,
                Collapsed_nleqslv=MLE_collapsed_nleqslv,
                Es_row=Es_row, Es_col=Es_col,
                matrixRow=matrixRow,matrixCol_t=matrixCol_t,
                choice_r=choice_r,choice_c=choice_c
    )
    )
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
                                      choice_r = choice_r[p,],choice_c = choice_c[p,]),
                         method = "L-BFGS-B",lower=0,upper=100
      )
      MLE_bypair_nleqslv[p,"Lambda"] <- MLE_nleqslv@coef
      MLE_bypair_nleqslv[p,"SD"] <- sqrt(MLE_nleqslv@vcov)
    }
    return(list(Bypair_Ehat=MLE_bypair_nleqslv,
                Bypair_nleqslv=MLE_bypair_nleqslv,
                matrixRow=matrixRow,matrixCol_t=matrixCol_t,
                choice_r=choice_r,choice_c=choice_c
    )
    )
  }
  
}

#### Explore de likelihood ####
plot_L_lambda <- function(MLE_QRE_sl_collapsed,lambda_upper=10,lambda_by=1,E_hat=T){
  lambdas <- matrix(seq(from = 0,to = lambda_upper,by = lambda_by))
  if(E_hat==T){
    likelihoods <-apply(X = lambdas,MARGIN = 1,FUN = neg_log_L_Ehat,
                        Es_row = MLE_QRE_sl_collapsed$Es_row,
                        Es_col = MLE_QRE_sl_collapsed$Es_col,
                        choice_r = MLE_QRE_sl_collapsed$choice_r,
                        choice_c = MLE_QRE_sl_collapsed$choice_c
    )
    plot(lambdas,likelihoods,type="l",xlab="Lambda",ylab="Likelihood")
  }else{
    likelihoods <-apply(X = lambdas,MARGIN = 1,FUN = neg_log_L_nleqslv,
                        matrixRow = MLE_QRE_sl_collapsed$matrixRow, 
                        matrixCol_t = MLE_QRE_sl_collapsed$matrixCol_t,
                        choice_r = MLE_QRE_sl_collapsed$choice_r,
                        choice_c = MLE_QRE_sl_collapsed$choice_c)
    plot(lambdas,likelihoods,type="l",xlab="Lambda",ylab="Likelihood")
  }
}
