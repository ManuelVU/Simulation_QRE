rm(list=ls())
source("src/Functions.R")
#### Bayesian Analysis of Players Data ####
colors<-c("#00293c","#1e656d","#f62a00","#68a225")
lambda_nleq<-{}
lambda_Ehat<-{}
pr<-c(1,2,4,6)
lm<-c(1,2,4,6)
for(i in 1:4){
  lambda_bayes<-Bayes_QRE_sl(data = "BS.Rdata",collapsed = F,parameters = c("lambda"),a_lambda = i,model.name = gamma_sl)
  lambda_Ehat<-cbind(lambda_Ehat,lambda_bayes$BUGSoutput$sims.list$lambda)
  lambda_bayes<-Bayes_sl_nleq(data = "BS.Rdata",proposal.par = c(0,pr[i]),a_lambda = i,my_inits = rep(lm[i],2),
                              n_iter = 30000,n_burnin = 20000)
  lambda_nleq<-cbind(lambda_nleq,c(lambda_bayes$chain[,1],lambda_bayes$chain[,2]))
  rm(lambda_bayes)
}

plot(0,0,ann=F,axes=F,type="n",xlim=c(0,10),ylim=c(0,5))
for(i in 1:4){
  lines(density(lambda_Ehat[,i]),lty=2,lwd=1.5,col=colors[i])
  lines(density(lambda_nleq[,i]),lty=1,lwd=1.5,col=colors[i])
}
axis(1,pos=c(0,0),col="#666666aa",col.ticks ="#666666aa",col.axis="#666666aa")
mtext(expression(paste(lambda)),col="#333333aa",line=2.3,cex=1.4,side=1)
#### Plot Participants Behaviur ####
pdf("results/PlayersChoices.pdf")
m<-matrix(seq(1,16),ncol=4,nrow=4,byrow=T)
layout(m)
par(mai=c(0,0,0,0),
    oma=c(5,5,0.2,0.2))
data_file<-c("BS.RData","PD.RData","ON.RData","AS.RData")
one<-c(F,F,T,F)
contar<-0
for(df in data_file){
  contar<-contar+1
  for(i in 1:4){
    plot_data(data = paste(df),add_hist = T,add_equilibrium = T,highlight_population = T,Oneil = one[contar],
              a_lambda = i,color=colors[i])  
    if(i==1){
      text(0.02,0.85,"ROW",col="#66666688",cex=1.5,srt=90)
      axis(2,at=seq(0,1,0.2),labels = c("0",seq(0.2,0.8,0.2),"1"),
           las=2,col="#666666aa",col.ticks ="#666666aa",col.axis="#666666aa")
    }
    if(contar==4){
      text(0.8,0.02,"Column",col="#66666688",cex=1.5)
      axis(1,at=seq(0,1,0.2),labels = c("0",seq(0.2,0.8,0.2),"1"),
           col="#666666aa",col.ticks ="#666666aa",col.axis="#666666aa")
    }
  }
  mtext("Battle of the sexes",2,outer=T,line=2.7,col="#555555aa",at=0.88)
  mtext("Prisoner's dilema",2,outer=T,line=2.7,col="#555555aa",at=0.63)
  mtext("O'Neill",2,outer=T,line=2.7,col="#555555aa",at=0.38)
  mtext("Asymetric",2,outer=T,line=2.7,col="#555555aa",at=0.12)
  
  mtext(expression(paste(lambda," = 1 ")),1,outer=T,line=2.7,col="#555555aa",at=0.88)
  mtext(expression(paste(lambda," = 4 ")),1,outer=T,line=2.7,col="#555555aa",at=0.63)
  mtext(expression(paste(lambda," = 2 ")),1,outer=T,line=2.7,col="#555555aa",at=0.38)
  mtext(expression(paste(lambda," = 1 ")),1,outer=T,line=2.7,col="#555555aa",at=0.12)
}
dev.off()

