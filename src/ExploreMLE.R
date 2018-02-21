
#### MLE test
rm(list=ls())

source("src/Simulate_Game.R")
source("src/Bayes.R")
source("src/Maximum_Likelihood.R")


n=4
# oneil
m1 <- matrix(c(-1,1,1,-1, 1,-1,1,-1,1,1,-1,-1, -1,-1,-1,1),nrow=n) 
m1 <- m1*5
m2 <- m1*(-1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
believes <- matrix(rep(c(.2,.2,.2,.4),2),nrow=2,byrow = T)

simulate_game_results <- simulate_game(matrixRow = m1,matrixCol = m2,
                                       pairs = 50,lambda = 4,trials = 10000,
                                       belives_row = rep(.25,4),belives_col = rep(.25,4))

MLE_QRE_sl("ON.RData",collapsed=F)

MLE1 <- MLE_QRE_sl(simulate_game_results,collapsed=T)
MLE_QRE_sl(simulate_game_results,collapsed=F)


#explore log likelihood of Ehat collapsed

plot_L_lambda(MLE_QRE_sl_collapsed = MLE1,lambda_upper = 10,lambda_by = 1,E_hat = T)
plot_L_lambda(MLE_QRE_sl_collapsed = MLE1,lambda_upper = 6,lambda_by = .001,E_hat = F)

