#### Single Lambda Bayes models ####
model_sl_gamma<-function(){
  # Prior
  lambda~dgamma(0.001,0.001)
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
  for(n in 1:n_pairs){
    choice_r[n,]~dmulti(theta_r,trials)
    choice_c[n,]~dmulti(theta_c,trials)
  }
}
model_sl_lognorm<-function(){
  # Prior
  lambda~dlnorm(0,0.01)
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
  for(n in 1:n_pairs){
    choice_r[n,]~dmulti(theta_r,trials)
    choice_c[n,]~dmulti(theta_c,trials)
  }
}