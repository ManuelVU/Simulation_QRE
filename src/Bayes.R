#### Bayes single lambda ####
Bayes_QRE_sl<-function(data,collapsed,parameters,my_inits,n_iter,n_chains,n_burnin,n_thin,model.file,model.name){
  library(R2jags,quietly=T,warn.conflicts = F)
  source(paste(c("src/",model.file),collapse=""))
  source("src/Simulate_Game.R")
  if(is.character(data)){
    data<-load(paste(c("results/",data),collapse=""))
  }
  else{
    data<-data
  }
  if(missing(collapsed)){
    collapsed<-F
  }
  if(missing(parameters)){
    parameters<-c("lambda")
  }
  if(missing(my_inits)){
    my_inits<-list(
      list(lambda=rgamma(1,2,2)),
      list(lambda=rgamma(1,2,2)))
  }
  if(missing(n_iter)){
    n_iter<-15000
  }
  if(missing(n_chains)){
    n_chains<-2
  }
  if(missing(n_burnin)){
    n_burnin<-5000
  }
  if(missing(n_thin)){
    n_thin<-1
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
    prompt<-"Assign prior values to alpha and beta: "
    ab<-as.numeric(strsplit(readline(prompt), ",")[[1]])
    if(length(ab<2)){
      ab<-c(0.001,0.001)
    }
  }
  if(prior=="lognorm"){
    prompt<-"Assign prior values to mu and tau: "
    ab<-as.numeric(strsplit(readline(prompt), ",")[[1]])
    if(length(ab<2)){
      ab<-c(0,0.01)
    }
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