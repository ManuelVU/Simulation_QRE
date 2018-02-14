# This script is to be used only for the main data analysis of the paper on simulation and QRE
source("src/Simulate_Game.R")
source("src/Bayes.R")
source("src/Maximum_Likelihood.R")
#### Game 1: Battle of the Sexes ####
Game_id<-"BS"
#Output path
OutPath<- "results/"
g_row<-matrix(c(3,0,0,2),nrow=2)
g_col<-matrix(c(2,0,0,3),nrow=2)
players<-50
t<-150
l<-2
belives_r<-rep(1/nrow(g_row),nrow(g_row))
belives_c<-rep(1/ncol(g_col),nrow(g_col))
battle_of_sexes<-simulate_game(matrixRow = g_row,matrixCol = g_col,pairs = players,trials = t,
                               lambda = l,belives_row = belives_r,belives_col = belives_c)
save(battle_of_sexes,file=paste(c(OutPath,Game_id,".RData"),collapse=""))

gamma_BS<-Bayes_QRE_sl(data = "BS.RData",collapsed = F,parameters = "lambda",
                       model.file = "Bayesian_Models.R",model.name = gamma_sl,
                       prior="gamma")

#### Game 2: Prisoners Dilema ####
Game_id<-"PD"
#Output path
OutPath<- "results/"
g_row<-matrix(c(-1,0,-3,-2),nrow=2)
g_col<-matrix(c(-1,-3,0,-2),nrow=2)
players<-50
t<-150
l<-2
belives_r<-rep(1/nrow(g_row),nrow(g_row))
belives_c<-rep(1/ncol(g_col),nrow(g_col))
prisoners_dilema<-simulate_game(matrixRow = g_row,matrixCol = g_col,pairs = players,trials = t,
                               lambda = l,belives_row = belives_r,belives_col = belives_c)
save(prisoners_dilema,file=paste(c(OutPath,Game_id,".RData"),collapse=""))
