# Games to use in Simulations

#Conflict ####
## oneil ####
n=4
m1 <- matrix(c(-1,1,1,-1, 1,-1,1,-1,1,1,-1,-1, -1,-1,-1,1),nrow=n) 
m1 <- m1*5
m2 <- m1*(-1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)

## tesis lic dario ####
n=2
m1 <- matrix(c(-9,1,9,-1),nrow=n)
m2 <- matrix(c(9,-9,-1,1),nrow=n)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)

#Coordination: sexos ####
n=2
m1<-matrix(c(2,0,0,1),nrow=n)
m2<-matrix(c(1,0,0,2),nrow=n)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)

# asimetrico ####
n=2
m1 <- matrix(c(2,-2,-1,1),nrow=n)
m2 <- -t(m1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)

# Prisioner's Dilemma ####
n=2
m1 <- matrix(c(-1,0,-3,-2),nrow=n)
m2 <- t(m1)
m1 <- Estandar_payoffs(m1)
m2 <- Estandar_payoffs(m2)
