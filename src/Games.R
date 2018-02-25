source("src/Maximum_Likelihood.R")
# Games to use in Simulations
Games <- list()
#Conflict ####
## oneil ####
n=4
m1 <- matrix(c(-1,1,1,-1, 1,-1,1,-1,1,1,-1,-1, -1,-1,-1,1),nrow=n) 
m1 <- m1*5
m2 <- m1*(-1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
Oneil <- list(matrixRow=m1,matrixCol=m2) 
Games$Oneil <- Oneil

## tesis lic dario ####
n=2
m1 <- matrix(c(-9,1,9,-1),nrow=n)
m2 <- matrix(c(9,-9,-1,1),nrow=n)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
Dario <- list(matrixRow=m1,matrixCol=m2) 
Games$Dario <- Dario

#Coordination: sexos ####
n=2
m1<-matrix(c(2,0,0,1),nrow=n)
m2<-matrix(c(1,0,0,2),nrow=n)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
Sexes <- list(matrixRow=m1,matrixCol=m2) 
Games$Sexes <- Sexes

# asimetrico ####
n=2
m1 <- matrix(c(2,-2,-1,1),nrow=n)
m2 <- -t(m1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
Asymmetric <- list(matrixRow=m1,matrixCol=m2) 
Games$Asymmetric <- Asymmetric

# Prisioner's Dilemma ####
n=2
m1 <- matrix(c(-1,0,-3,-2),nrow=n)
m2 <- t(m1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
PDilemma <- list(matrixRow=m1,matrixCol=m2) 
Games$PDilemma <- PDilemma

