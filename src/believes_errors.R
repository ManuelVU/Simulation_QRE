
#Choose a game ####
n=4
# oneil
m1 <- matrix(c(-1,1,1,-1, 1,-1,1,-1,1,1,-1,-1, -1,-1,-1,1),nrow=n) 
m1 <- m1*5
m2 <- m1*(-1)
believes <- matrix(rep(c(.2,.2,.2,.4),2),nrow=2,byrow = T)

# tesis lic dario
n=2
m1 <- matrix(c(-9,1,9,-1),nrow=n)
m2 <- matrix(c(9,-9,-1,1),nrow=n)
m1;m2
believes <- matrix(rep(c(1/n,1/n),2),nrow=2,byrow = T)


# sexos
n=2
m1<-matrix(c(2,0,0,1),nrow=n)
m2<-matrix(c(1,0,0,2),nrow=n)
believes <- matrix(rep(c(1/n,1/n),2),nrow=2,byrow = T)

# asimetrico
n=2
m1 <- matrix(c(2,-2,-1,1),nrow=n)
m2 <- -t(m1)
m1;m2
believes <- matrix(rep(c(1/n,1/n),2),nrow=2,byrow = T)
believes <- matrix(c(1/3,2/3,1/2,1/2),nrow=2)

# function to optimize ####
sigma <- as.vector(believes)
lambda= 100


m1<-matrix(c(17,-1,1,-2,1,-1),nrow=3)
m2<- -m1
believes<-c(c(0.2,0.8),c(0.7,0.1,0.2))

belief_error <- function(sigma){
  expected_payoff1 <- m1%*%sigma[1:ncol(m1)]*lambda
  expected_payoff2 <- m2%*%sigma[(ncol(m1)+1):((ncol(m1))+(nrow(m1)))]*lambda
  SBR <- {}
  SBR[(ncol(m1)+1):((ncol(m1))+(nrow(m1)))] <- exp(expected_payoff1)/(sum(exp(expected_payoff1)))
  SBR[1:ncol(m1)] <- exp(expected_payoff2)/(sum(exp(expected_payoff2)))
  return(SBR-sigma)
}

belief_error(as.vector(believes))

res <-nleqslv(fn = belief_error,x=as.vector(believes))
res$x
 belief_error(res$x)
