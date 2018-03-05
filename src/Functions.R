#### GENERAL FUNCTIONS ####
# Expected Payoff function
expected_payoffs <- function(matrixP,sigma_other,lambda){
  ep<-matrixP%*%sigma_other*lambda
  return(as.vector(ep))
}
# Stadarization 
Estandar_payoffs <- function(matrixPayoff,r=1){
  (matrixPayoff^r-min(matrixPayoff)^r)/(max(matrixPayoff)^r-min(matrixPayoff)^r)
}
# Belief Error 
belief_error <- function(sigma,matrixRow,matrixCol_t,lambda, lambdaCol=lambda){ #sigma son las creencias de ambos jugadores
  n_Row <- nrow(matrixRow)
  n_Col <- nrow(matrixCol_t)
  expected_payoffsRow <- expected_payoffs(matrixP = matrixRow,sigma_other = sigma[1:n_Col],lambda = lambda)
  expected_payoffsCol <- expected_payoffs(matrixP = matrixCol_t,sigma_other = sigma[(n_Col+1):(n_Col+n_Row)],lambda = lambdaCol)
  SBR <- {}
  SBR[(n_Col+1):(n_Col+n_Row)] <- exp(expected_payoffsRow)/(sum(exp(expected_payoffsRow)))
  SBR[1:n_Col] <- exp(expected_payoffsCol)/(sum(exp(expected_payoffsCol)))
  return(SBR-sigma)
}
# QRE 
QRE_sol <- function(matrixRow,matrixCol_t,lambda, lambdaCol=lambda){
  QRE_sol<- nleqslv(fn = belief_error,
                    x=c(rep(1/nrow(matrixRow),nrow(matrixRow)),rep(1/ncol(matrixRow),ncol(matrixRow))),
                    matrixRow=matrixRow,matrixCol_t= matrixCol_t,lambda=lambda,lambdaCol=lambdaCol)$x
  return(QRE_sol)
}
# Data Simulation
simulate_game<-function(matrixRow,matrixCol,pairs,trials,lambda){
  library(nleqslv)
  Results<-list()
  name_eq<-c(rep("C",ncol(matrixRow)),rep("R",nrow(matrixRow)))
  choice_r<-{}
  choice_c<-{}
  equilibriums<-{}
  matrixRow<-matrixRow
  matrixCol<-matrixCol
  matrixCol_t<-t(matrixCol)
  n_p<-pairs
  t<-trials
  if(is.vector(lambda)){
    lambda_s<-cbind(c(lambda),c(lambda))
  }
  count<-0
  Results$row$collapsed<-matrix(NA,ncol=nrow(matrixRow),nrow=length(lambda))
  Results$col$collapsed<-matrix(NA,ncol=ncol(matrixRow),nrow=length(lambda))
  for(l in 1:length(lambda_s[,1])){
    count<-count+1
    solution <- QRE_sol(matrixRow = matrixRow, matrixCol_t = matrixCol_t, lambda = lambda_s[l,1], lambdaCol = lambda_s[l,2])
    equilibriums<-rbind(equilibriums,solution)
    choice_r<-rbind(choice_r,t(rmultinom(n_p,t,solution[(ncol(matrixRow)+1):(ncol(matrixRow)+nrow(matrixRow))])))
    choice_c<-rbind(choice_c,
                    t(rmultinom(n_p,t,solution[1:ncol(matrixRow)])))
    Results$row$collapsed[count,]<-apply(choice_r[(1+(count*n_p-n_p)):(count*n_p),],2,sum)
    Results$col$collapsed[count,]<-apply(choice_c[(1+(count*n_p-n_p)):(count*n_p),],2,sum)
  }
  
  Results$row$bypair<-choice_r
  Results$col$bypair<-choice_c
  Results$parameters$lambda<-lambda
  Results$parameters$exp<-c(pairs,trials)
  Results$parameters$games$R<-matrixRow
  Results$parameters$games$C<-matrixCol
  Results$equilibrium<-equilibriums
  colnames(Results$equilibrium)<-name_eq
  return(Results)
}
# Stochastic Best Response
sigma_SBR <- function(Es_row,Es_col,lambda=1){
  sigma_SBR_row <- exp(lambda*Es_row)/sum(exp(lambda*Es_row))
  sigma_SBR_col <- exp(lambda*Es_col)/sum(exp(lambda*Es_col))
  return(list(sigma_SBR_row,sigma_SBR_col))
}

#### MAXIMUM LIKELIHOOD ESTIMATION FUNCTIONS ####
# Non-Linear Equation Negative Log-Likelihood
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
# E-Hat Negative Log Likelihood
neg_log_L_Ehat <- function(Es_row,Es_col,lambda,choice_r,choice_c){
  SBR = sigma_SBR(Es_row,Es_col,lambda)
  log_L_Ehat = dmultinom(choice_r,prob= SBR[[1]],log = T)+
    dmultinom(choice_c,prob= SBR[[2]],log = T)
  return(-log_L_Ehat)
}
# Maximum Likelihood Estimation Function Single Lambda
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

#### BAYESIAN ESTIMATION ####
# Bayes single lambda
Bayes_QRE_sl<-function(data,collapsed=F,parameters=c("lambda"),a_lambda = 1,
                       my_inits=list(list(lambda=rgamma(1,2,2)),list(lambda=rgamma(1,2,2))),
                       n_iter=15000,n_chains=2,n_burnin=5000,n_thin=1,
                       model.file="Bayesian_Models.R",model.name=gamma_sl,prior="gamma",
                       prior.val=c(0.001,0.001,0,0.001)){
  library(R2jags,quietly=T,warn.conflicts = F)
  if(is.character(data)){
    data<-get(load(paste(c("results/",data),collapse="")))
  }
  else{
    data<-data
  }
  if(collapsed==T){
    choice_r<-data$row$collapsed
    choice_c<-data$col$collapsed
  }
  else{
    choice_r<-data$row$bypair
    choice_c<-data$col$bypair
  }
  if(prior=="gamma"){
    ab<-prior.val[1:2]
  }
  if(prior=="lognorm"){
    ab<-prior.val[3:4]
  }
  Es_row<-expected_payoffs(data$parameters$games$R,
                           as.vector(data$col$collapsed[a_lambda,]/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  Es_col<-expected_payoffs(t(data$parameters$games$C),
                           as.vector(data$row$collapsed[a_lambda,]/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  n_pairs<-data$parameters$exp[1]
  trials<-data$parameters$exp[2]
  n_sr<-dim(data$parameters$games$R)[1]
  n_sc<-dim(data$parameters$games$R)[2]
  data_jags<-list("choice_r","choice_c","Es_row","Es_col","n_pairs","trials",
                  "n_sr","n_sc","ab","a_lambda")
  s<-jags(data=data_jags,
          inits = my_inits,
          parameters.to.save = c(paste(parameters)),
          model.file = model.name,
          n.chains = n_chains,
          n.iter = n_iter,
          n.burnin = n_burnin,
          n.thin = n_thin,DIC = T)
}
# Bayes Non-Linear Equation Solver
Bayes_sl_nleq<-function(data,collapsed=T,parameters=c("lambda"),a_lambda=1,
                        n_chains=2,n_iter=15000,n_burnin=5000,n_thin=1,
                        my_inits=c(rgamma(n_chains,1,1)),
                        prior="gamma",proposal.par=c(0,1),prior.v=c(0.001,0.001)){
  Results<-list()
  Results$chain<-matrix(NA,nrow=(n_iter),ncol=n_chains)
  Results$a<-c()
  library(nleqslv)
  if(is.character(data)){
    data<-get(load(paste(c("results/",data),collapse="")))
  }
  else{
    data<-data
  } 
  if(collapsed==T){
    choice_r<-data$row$collapsed[a_lambda,]
    choice_c<-data$col$collapsed[a_lambda,]
  }
  else{
    choice_r<-data$row$bypair
    choice_c<-data$col$bypair
  }
  n_pairs<-data$parameters$exp[1]
  trials<-data$parameters$exp[2]
  game_r<-data$parameters$games$R
  game_c<-t(data$parameters$games$C)
  n_sr<-dim(data$parameters$games$R)[1]
  n_sc<-dim(data$parameters$games$R)[2]
  likelihood<-{}
  for(k in 1:n_chains){
    init<-my_inits[k]
    lambda_s<-c()
    tuning<-c()
    countu<-1
    countd<-1
    count_alpha<-0
    accepted<-0
    for(i in 2:n_iter){
      if(i==2){
        lambda_s<-append(lambda_s,init)
        tuning<-append(tuning,proposal.par[2])
      }
      bandera<-1
      while(bandera==1){
        if(i>100&i<=n_burnin){
          alpha<-count_alpha/i
          if((alpha-0.44)<0){
            countu<-countu+1
            tuning<-append(tuning,(tuning[i-1]-(tuning[i-1]*1/countu)))
          }
          else{
            countd<-countd+1
            tuning<-append(tuning,(tuning[i-1]+(tuning[i-1]*1/countd)))
          }
        }
        else{
          tuning<-append(tuning,tuning[i-1])
        }
        prop<-lambda_s[i-1]+rnorm(n = 1,mean = proposal.par[1],sd = tuning[i-1])
        if(prop<=0){
          bandera<-1
        }
        else{
          bandera<-2
        }
      }
      if(i==2){
        solution<-QRE_sol(matrixRow=game_r,matrixCol_t=game_c,lambda=lambda_s[i-1])
        if(collapsed==T){
          likelihood[i-1]<-dmultinom(choice_r,prob=solution[(1+n_sc):(n_sc+n_sr)])*
                           dmultinom(choice_c,prob=solution[1:n_sc])
        }
        else{
          likelihood[i-1]<-prod(apply(choice_r,1,dmultinom(),size=trials,prob=solution[(1+n_sc):(n_sc+n_sr)])*
                                apply(choice_c,1,dmultinom(),size=trials,prob=solution[1:n_sc]))
        }
      }
      solution<-QRE_sol(matrixRow=game_r,matrixCol_t=game_c,lambda=prop)
      if(collapsed==T){
        likelihood.prop<-dmultinom(choice_r,prob=solution[(1+n_sc):(n_sc+n_sr)])*
                         dmultinom(choice_c,prob=solution[1:n_sc])
      }
      else{
        likelihood.prop<-prod(apply(choice_r,1,dmultinom(),size=trials,prob=solution[(1+n_sc):(n_sc+n_sr)])*
                              apply(choice_c,1,dmultinom(),size=trials,prob=solution[1:n_sc]))
      }
      if(prior=="gamma"){
        posterior.current<-likelihood[i-1]*dgamma(lambda_s[i-1],prior.v[1],prior.v[2])
        posterior.prop<-likelihood.prop*dgamma(prop,prior.v[1],prior.v[2])
      }
      else if(prior=="lnorm"){
        posterior.current<-likelihood[i-1]*dlnorm(lambda_s[i-1],prior.v[1],prior.v[2])
        posterior.prop<-likelihood.prop*dlnorm(prop,prior.v[1],prior.v[2])
      }
      if((posterior.prop/posterior.current)>=1){
        lambda_s<-append(lambda_s,prop)
        likelihood<-append(likelihood,likelihood.prop)
        count_alpha<-count_alpha+1
        if(i >n_burnin){
          accepted=accepted+1
        }
      }
      else if((posterior.prop/posterior.current)<1){
        accept.reject<-rbinom(n = 1,size = 1,prob = posterior.prop/posterior.current)
        if(accept.reject==1){
          lambda_s<-append(lambda_s,prop)
          likelihood<-append(likelihood,likelihood.prop)
          count_alpha<-count_alpha+1
          if(i >n_burnin){
            accepted=accepted+1
          }
        }
        else{
          lambda_s<-append(lambda_s,lambda_s[i-1])
          likelihood<-append(likelihood,likelihood[i-1])
        }
      }
    }
    Results$chain[,k]<-lambda_s
    Results$a[k]<-accepted/(n_iter-n_burnin)
  }
  Results$chain<-Results$chain[-c(1:n_burnin),]
  Results$chain<-Results$chain[seq(1,length(Results$chain[,1]),n_thin),]
  return(Results)
}
# Gamma Prior
gamma_sl<-function(){
  # Prior
  lambda~dgamma(ab[1],ab[2])
  # functions
  for(i in 1:n_sr){
    v_sr[i]<-exp(lambda*Es_row[i])
  }
  for(i in 1:n_sc){
    v_sc[i]<-exp(lambda*Es_col[i])
  }
  for(i in 1:n_sr){
    theta_r[i]<-v_sr[i]/sum(v_sr)
  }
  for(i in 1:n_sc){
    theta_c[i]<-v_sc[i]/sum(v_sc)
  }
  
  # likelihood
  for(n in (1+(a_lambda*n_pairs-n_pairs)):(a_lambda*n_pairs)){
    choice_r[n,]~dmulti(theta_r,trials)
    choice_c[n,]~dmulti(theta_c,trials)
  }
}
# Log-Normal Prior
lognormal_sl<-function(){
  # Prior
  lambda~dlnorm(ab[1],ab[2])
  # functions
  for(i in 1:n_sr){
    v_sr[i]<-exp(lambda*Es_row[i])
  }
  for(i in 1:n_sc){
    v_sc[i]<-exp(lambda*Es_col[i])
  }
  for(i in 1:n_sr){
    theta_r[i]<-v_sr[i]/sum(v_sr)
  }
  for(i in 1:n_sc){
    theta_c[i]<-v_sc[i]/sum(v_sc)
  }
  
  # likelihood
  for(n in (1+(a_lambda*n_pairs-n_pairs)):(a_lambda*n_pairs)){
    choice_r[n,]~dmulti(theta_r,trials)
    choice_c[n,]~dmulti(theta_c,trials)
  }
}

#### PLOT FUNCTIONS ####
# Explore de likelihood Plot
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
# Horizontal histograms
Hist<-function(x,border,color,lbreaks,CI,horizontal,add.p,xax,lwd.h,add,fill){
  hist.data<-list()
  hx<-c()
  if(missing(lbreaks)){
    lbreaks <- as.numeric(readline(prompt="Asignar un numero de particiones: "))
  }
  if(missing(add.p)){
    add.p=F
  }
  if(missing(border)){
    border=F
  }
  if(missing(color)){
    color="#000000"
  }
  if(missing(CI)){
    CI=F
  }
  if(missing(horizontal)){
    horizontal=F
  }
  if(missing(xax)){
    xax<-c(min(x),max(x))
  }
  if(missing(lwd.h)){
    lwd.h<-1
  }
  if(missing(add)){
    add=F
  }
  if(missing(fill)){
    fill=T
  }
  if(CI!=F){
    library(coda)
    ic<-HPDinterval(as.mcmc(x),prob=CI)
  }
  # Plots
  if(border==T){
    if(horizontal==F){
      hx<-hist(x,breaks=seq(min(x),max(x),length=lbreaks),axes=F,ann=F,
               border=paste(color),col=paste(c(color,"55"),collapse=""),
               freq=F)
      if(is.numeric(add.p)){
        points(x[add.p],rep(0,length(x[add.p])),pch="l",col=paste(color),cex=0.5)
      }
    }
    else{
      hx<-hist(x,breaks=seq(min(x),max(x),length=lbreaks),plot=F)
      plot(c(0,hx$density,0),c(hx$breaks,hx$breaks[length(hx$breaks)]),
           type="s",col=paste(color),axes=F,ann=F,
           lwd=lwd.h)
      polygon(c(0,rep(c(hx$density),each=2),0),
              rep(c(hx$breaks),each=2),
              col=paste(c(color,"55"),collapse=""),
              border=F)
      for(i in 1:length(hx$density)){
        segments(x0=0,x1=hx$density[i],y0=hx$breaks[i+1])
      }
      if(is.numeric(add.p)){
        points(length(x[add.p]),rep(0,length(x[add.p])),pch="l",col=paste(color),cex=0.5)
      }
    }
  }
  else{
    if(horizontal==F){
      hx<-hist(x,breaks=seq(min(x),max(x),length=lbreaks),plot=F)
      if(add==F){
        plot(c(hx$breaks[1],hx$breaks),c(0,hx$density,0),type="s",
             col=paste(color),axes=F,
             xlim=c(xax[1],xax[2]),ann=F,lwd=lwd.h)
      }
      else{
        lines(c(hx$breaks[1],hx$breaks),c(0,hx$density,0),type="s",
              col=paste(color),lwd=lwd.h)
      }
      if(fill==T){
        polygon(rep(c(hx$breaks),each=2),
                c(0,rep(c(hx$density),each=2),0),
                col=paste(c(color,"55"),collapse=""),
                border=F)
      }
      if(is.numeric(add.p)){
        points(x[add.p],rep(0,length(x[add.p])),pch="l",col=paste(color),cex=0.5)
      }
    }
    else{
      hx<-hist(x,breaks=seq(min(x),max(x),length=lbreaks),plot=F)
      if(add==F){
        plot(c(0,hx$density,0),c(hx$breaks,hx$breaks[length(hx$breaks)]),type="s",
             col=paste(color),axes=F,
             ylim=c(xax[1],xax[2]),ann=F,lwd=lwd.h)
      }
      else{
        lines(c(0,hx$density,0),c(hx$breaks,hx$breaks[length(hx$breaks)]),type="s",
              col=paste(color),lwd=lwd.h)
      }
      if(fill==T){
        polygon(c(0,rep(c(hx$density),each=2),0),
                rep(c(hx$breaks),each=2),
                col=paste(c(color,"55"),collapse=""),
                border=F)
      }
      if(is.numeric(add.p)){
        points(rep(0,length(x[add.p])),x[add.p],pch="l",col=paste(color),cex=0.5)
      }
    }
  }
  if(CI!=F){
    hist.data$CI<-ic
    if(horizontal==F){
      segments(x0=ic[1],x1=ic[2],y0=max(hx$density)+0.03*max(hx$density),
               y1=max(hx$density)+0.03*max(hx$density),lwd=3,col=color)
    }
    else{
      segments(y0=ic[1],y1=ic[2],x0=max(hx$density)+0.03*max(hx$density),
               x1=max(hx$density)+0.03*max(hx$density),lwd=3,col=color)
    }
  }
  hist.data$hist<-hx
  return(hist.data)
}
# Plot Simulation Results
plot_data<-function(data,add_hist=T,add_equilibrium=T,highlight_population=T,Oneil=F){
  library(nleqslv)
  if(is.character(data)){
    data<-get(load(paste(c("results/",data),collapse="")))
  }
  else{
    data<-data
  }
  if(add_equilibrium==T){
    x<-seq(0.01,data$parameters$lambda[,1],length=50)
    equilibriums<-matrix(NA,ncol=2,nrow=length(x))
    count<-0
    for(i in x){
      count<-count+1
      solution<-QRE_sol(matrixRow = data$parameters$games$R, matrixCol_t = t(data$parameters$games$C), 
                        lambda = i, lambdaCol = i)
      if(Oneil==T){
        equilibriums[count,1]<-sum(solution[5:7])
        equilibriums[count,2]<-sum(solution[1:3])
      }
      else{
        equilibriums[count,1]<-solution[3]
        equilibriums[count,1]<-solution[1]
      }
    }
  }
  par(fig=c(0,0.8,0,0.8))
  if(highlight_population==T){
    if(Oneil==T){
      plot(apply(data$row$bypair[,1:3],1,sum)/data$parameters$exp[2],
           apply(data$col$bypair[,1:3],1,sum)/data$parameters$exp[2],
           col="#00000066",pch=18,cex=1.3,ann=F,axes=F,
           ylim=c(0,1),xlim=c(0,1))
      points(sum(data$row$collapsed[1:3])/(data$parameters$exp[2]*data$parameters$exp[1]),
             sum(data$col$collapsed[1:3])/(data$parameters$exp[2]*data$parameters$exp[1]),
             col="#000000",pch=18,cex=1.8)
    }
    else{
      plot(data$row$bypair[,1]/data$parameters$exp[2],
           data$col$bypair[,1]/data$parameters$exp[2]
           ,col="#00000066",pch=18,cex=1.3,ann=F,axes=F,
           ylim=c(0,1),xlim=c(0,1))  
      points(data$row$collapsed[,1]/(data$parameters$exp[2]*data$parameters$exp[1]),
             data$col$collapsed[,1]/(data$parameters$exp[2]*data$parameters$exp[1]),
             col="#000000",pch=18,cex=1.8)
    }
  }
  else{
    if(Oneil==T){
      plot(apply(data$row$bypair[,1:3],1,sum)/data$parameters$exp[2],
           apply(data$col$bypair[,1:3],1,sum)/data$parameters$exp[2],
           col="#000000",pch=18,cex=1.3,ann=F,axes=F,
           ylim=c(0,1),xlim=c(0,1))
    }
    else{
      plot(data$row$bypair[,1]/data$parameters$exp[2],
           data$col$bypair[,1]/data$parameters$exp[2]
           ,col="#000000",pch=18,cex=1.3,ann=F,axes=F,
           ylim=c(0,1),xlim=c(0,1))  
    }
  }
  if(add_equilibrium==T){
    lines(equilibriums[,1],equilibriums[,2],lwd=1.2,col="#333333")
    points(equilibriums[length(equilibriums[,1]),1],equilibriums[length(equilibriums[,1]),2],
           pch="*",cex=1.6,col="#A43820")
  }
  box()
  axis(1)
  axis(2)
  mtext(paste("ROW"),1,line=2.3,cex=1.4,col="#00000099")
  mtext(paste("COLUMN"),2,line=2.2,cex=1.4,col="#00000099")
  par(fig=c(0,0.8,0.53,1), new=TRUE)
  if(Oneil==T){
    Hist(apply(data$row$bypair[,1:3],1,sum)/data$parameters$exp[2],color="#000000",lbreaks=10,
         xax=c(0,1))
  }
  else{
    Hist(data$row$bypair[,1]/data$parameters$exp[2],color="#000000",lbreaks=10,horizontal = T,
         xax=c(0,1))
  }
  par(fig=c(0.616,1,0,0.8),new=TRUE)
  if(Oneil==T){
    Hist(apply(data$row$bypair[,1:3],1,sum)/data$parameters$exp[2],
         color="#000000",lbreaks=10,horizontal = T,xax=c(0,1))  
  }
  else{
    Hist(data$col$bypair[,1]/data$parameters$exp[2],color="#000000",lbreaks=20,horizontal = T,
         xax=c(0,1))
  }
}
