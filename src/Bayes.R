#### Bayes single lambda ####
Bayes_QRE_sl<-function(data,collapsed=F,parameters=c("lambda"),
                       my_inits=list(list(lambda=rgamma(1,2,2)),list(lambda=rgamma(1,2,2))),
                       n_iter=15000,n_chains=2,n_burnin=5000,n_thin=1,
                       model.file="Bayesian_Models.R",model.name=gamma_sl,prior="gamma",
                       prior.val=c(0.001,0.001,0,0.001)){
  library(R2jags,quietly=T,warn.conflicts = F)
  source("src/Functions.R")
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
                           as.vector(data$col$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  Es_col<-expected_payoffs(t(data$parameters$games$C),
                           as.vector(data$row$collapsed/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  n_pairs<-data$parameters$exp[1]
  trials<-data$parameters$exp[2]
  n_sr<-dim(data$parameters$games$R)[1]
  n_sc<-dim(data$parameters$games$R)[2]
  data_jags<-list("choice_r","choice_c","Es_row","Es_col","n_pairs","trials","n_sr","n_sc","ab")
  s<-jags(data=data_jags,
          inits = my_inits,
          parameters.to.save = c(paste(parameters)),
          model.file = model.name,
          n.chains = n_chains,
          n.iter = n_iter,
          n.burnin = n_burnin,
          n.thin = n_thin,DIC = T)
}
#### Bayes single lambda NLEQ ####
# Agregar la funcion general de belief error
Bayes_sl_nleq<-function(data,collapsed=T,parameters=c("lambda"),
                        n_chains=2,
                        my_inits=c(rgamma(n_chains,1,1)),n_iter=15000,n_burnin=5000,n_thin=1,
                        prior="gamma",proposal.par=c(0,3),prior.v=c(0.001,0.001)){
  Results<-list()
  Results$chain<-matrix(NA,nrow=(n_iter),ncol=n_chains)
  Results$a<-c()
  library(nleqslv)
  source("src/Simulate_Game.R")
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
  belief_error_bayes <- function(sigma,lambda){
    expected_payoff1 <- game_r%*%sigma[1:n_sc]*lambda
    expected_payoff2 <- t(game_c)%*%sigma[(n_sc+1):(n_sc+n_sr)]*lambda
    SBR <- {}
    SBR[(n_sc+1):(n_sc+n_sr)] <- exp(expected_payoff1)/(sum(exp(expected_payoff1)))
    SBR[1:n_sc] <- exp(expected_payoff2)/(sum(exp(expected_payoff2)))
    return(SBR-sigma)
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
    lambda<-c()
    tuning<-c()
    countu<-1
    countd<-1
    count_alpha<-0
    accepted<-0
    for(i in 2:n_iter){
      if(i==2){
        lambda<-append(lambda,init)
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
        prop<-lambda[i-1]+rnorm(n = 1,mean = proposal.par[1],sd = tuning[i-1])
        if(prop<=0){
          bandera<-1
        }
        else{
          bandera<-2
        }
      }
      if(i==2){
        bandera<-1
        while(bandera==1){
          init_bel_r<-runif(n_sc,min = 0.01,max=0.99)
          init_bel_c<-runif(n_sr,min = 0.01,max=0.99)
          init_bel_r<-init_bel_r/sum(init_bel_r)
          init_bel_c<-init_bel_c/sum(init_bel_c)
          solution<-nleqslv(fn = belief_error_bayes,x=c(init_bel_r,init_bel_c),lambda=lambda[i-1],
                            global = "hook",method = "Newton")
          if(sum(solution$x<0)>0){
            bandera<-1
          }
          else{
            bandera<-2
          }
        }
        if(collapsed==T){
          likelihood[i-1]<-dmultinom(choice_r,prob=solution$x[(1+n_sc):(n_sc+n_sr)])*
            dmultinom(choice_c,prob=solution$x[1:n_sc])
        }
        else{
          likelihood[i-1]<-prod(apply(choice_r,1,dmultinom(),size=trials,prob=solution$x[(1+n_sc):(n_sc+n_sr)])*
                                  apply(choice_c,1,dmultinom(),size=trials,prob=solution$x[1:n_sc]))
        }
      }
      bandera<-1
      while(bandera==1){
        init_bel_r<-runif(n_sc,min = 0.01,max=0.99)
        init_bel_c<-runif(n_sr,min = 0.01,max=0.99)
        init_bel_r<-init_bel_r/sum(init_bel_r)
        init_bel_c<-init_bel_c/sum(init_bel_c)
        solution<-nleqslv(fn = belief_error_bayes,x=c(init_bel_r,init_bel_c),lambda=prop,
                          global = "hook",method = "Newton")
        if(sum(solution$x<0)>0){
          bandera<-1
        }
        else{
          bandera<-2
        }
      }
      if(collapsed==T){
        likelihood.prop<-dmultinom(choice_r,prob=solution$x[(1+n_sc):(n_sc+n_sr)])*
          dmultinom(choice_c,prob=solution$x[1:n_sc])
      }
      else{
        likelihood.prop<-prod(apply(choice_r,1,dmultinom(),size=trials,prob=solution$x[(1+n_sc):(n_sc+n_sr)])*
                                apply(choice_c,1,dmultinom(),size=trials,prob=solution$x[1:n_sc]))
      }
      if(prior=="gamma"){
        posterior.current<-likelihood[i-1]*dgamma(lambda[i-1],prior.v[1],prior.v[2])
        posterior.prop<-likelihood.prop*dgamma(prop,prior.v[1],prior.v[2])
        
      }
      else if(prior=="lnorm"){
        posterior.current<-likelihood[i-1]*dlnorm(lambda[i-1],prior.v[1],prior.v[2])
        posterior.prop<-likelihood.prop*dlnorm(prop,prior.v[1],prior.v[2])
      }
      if((posterior.prop/posterior.current)>=1){
        lambda<-append(lambda,prop)
        likelihood<-append(likelihood,likelihood.prop)
        count_alpha<-count_alpha+1
        if(i >n_burnin){
          accepted=accepted+1
        }
      }
      else if((posterior.prop/posterior.current)<1){
        accept.reject<-rbinom(n = 1,size = 1,prob = posterior.prop/posterior.current)
        if(accept.reject==1){
          lambda<-append(lambda,prop)
          likelihood<-append(likelihood,likelihood.prop)
          count_alpha<-count_alpha+1
          if(i >n_burnin){
            accepted=accepted+1
          }
        }
        else{
          lambda<-append(lambda,lambda[i-1])
          likelihood<-append(likelihood,likelihood[i-1])
        }
      }
    }
    Results$chain[,k]<-lambda
    Results$a[k]<-accepted/(n_iter-n_burnin)
  }
  Results$chain<-Results$chain[-c(1:n_burnin),]
  Results$chain<-Results$chain[seq(1,length(Results$chain[,1]),n_thin),]
  return(Results)
}
