# This script is to be used only for the main data analysis of the paper on simulation and QRE
source("src/Functions.R")
source("src/Games.R")
#### Game 1: Battle of the Sexes ####
Game_id<-"BS"
#Output path
OutPath<- "results/"
players<-50
t<-150
l<-2
belives_r<-rep(1/nrow(Games$Sexes$matrixRow),nrow(Games$Sexes$matrixCol))
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

#### Game 3: O'Neil's 4x4 ####
Game_id<-"ON"
#Output path
OutPath<- "results/"
g_row<-matrix(c(-1,1,1,-1, 1,-1,1,-1,1,1,-1,-1, -1,-1,-1,1),nrow=4) 
g_row <- g_row*5
g_col<-g_row*(-1)
players<-50
t<-150
l<-4
belives_r<-rep(1/nrow(g_row),nrow(g_row))
belives_c<-rep(1/ncol(g_col),nrow(g_col))
ONeill<-simulate_game(matrixRow = g_row,matrixCol = g_col,pairs = players,trials = t,
                                lambda = l,belives_row = belives_r,belives_col = belives_c)
save(ONeill,file=paste(c(OutPath,Game_id,".RData"),collapse=""))
