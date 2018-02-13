#### Bayes single lambda ####
Bayes_QRE_sl<-function(data,collapsed,parameters,my_inits,n_iter,n_chains,n_burnin,n_thin,model.file,model.name){
  library(R2jags,quietly=T,warn.conflicts = F)
  # source(paste(c("src/",model.file),collapse="")
  # source("src/Simulate_Game.R")
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
      list(lambda<-rgamma(1,2,2)),
      list(lambda<-rgamma(1,2,2)))             )
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
  Es_row<-expected_payoffs(data$parameters$games$R,as.vector(choice_c/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  Es_col<-expected_payoffs(data$parameters$games$C,as.vector(choice_r/(data$parameters$exp[2]*data$parameters$exp[1])),1)
  data_jags<-list(choice_r<-choice_r,choice_c<-choice_c,
                  Es_row<-Es_row,Es_col<-Es_col,
                  n_pairs<-data$parameters$exp[1],trials<-data$parameters$exp[2],
                  game_r<-data$parameters$games$R,game_c<-data$parameters$games$C)
  
}
