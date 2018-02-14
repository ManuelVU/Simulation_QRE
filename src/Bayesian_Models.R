#### Single Lambda Bayes models ####
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
  for(n in 1:n_pairs){
    choice_r[n,]~dmulti(theta_r,trials)
    choice_c[n,]~dmulti(theta_c,trials)
  }
}
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
  for(n in 1:n_pairs){
    choice_r[n,]~dmulti(theta_r,trials)
    choice_c[n,]~dmulti(theta_c,trials)
  }
}