# This script is to be used only for the main data analysis of the paper on simulation and QRE
source("src/Functions.R")
source("src/Games.R")
# Experimental Prameters
players<-100
t<-100
#### Game 1: Battle of the Sexes ####
Game_id<-"BS"
#Output path
OutPath<- "results/"
l<-2
belives_r<-rep(1/nrow(Games$Sexes$matrixRow),nrow(Games$Sexes$matrixRow))
belives_c<-rep(1/ncol(Games$Sexes$matrixCol),nrow(Games$Sexes$matrixCol))
battle_of_sexes<-simulate_game(matrixRow = Games$Sexes$matrixRow,
                               matrixCol = Games$Sexes$matrixCol,
                               pairs = players,trials = t,
                               lambda = l,belives_row = belives_r,belives_col = belives_c)
save(battle_of_sexes,file=paste(c(OutPath,Game_id,".RData"),collapse=""))

#### Game 2: Prisoners Dilema ####
Game_id<-"PD"
#Output path
OutPath<- "results/"
l<-2
belives_r<-rep(1/nrow(Games$PDilemma$matrixRow),nrow(Games$PDilemma$matrixRow))
belives_c<-rep(1/ncol(Games$PDilemma$matrixCol),nrow(Games$PDilemma$matrixCol))
prisoners_dilema<-simulate_game(matrixRow = Games$PDilemma$matrixRow,
                                matrixCol = Games$PDilemma$matrixCol,
                                pairs = players,trials = t,
                               lambda = l,belives_row = belives_r,belives_col = belives_c)
save(prisoners_dilema,file=paste(c(OutPath,Game_id,".RData"),collapse=""))

#### Game 3: O'Neil's 4x4 ####
Game_id<-"ON"
#Output path
OutPath<- "results/"
belives_r<-rep(1/nrow(Games$Oneil$matrixRow),nrow(Games$Oneil$matrixRow))
belives_c<-rep(1/ncol(Games$Oneil$matrixCol),nrow(Games$Oneil$matrixCol))
l<-4
ONeill<-simulate_game(matrixRow = Games$Oneil$matrixRow,
                      matrixCol = Games$Oneil$matrixCol,
                      pairs = players,trials = t,
                      lambda = l,belives_row = belives_r,belives_col = belives_c)
save(ONeill,file=paste(c(OutPath,Game_id,".RData"),collapse=""))

#### Game 4: Asymetric 2x2 ####
Game_id<-"AS"
#Output path
OutPath<- "results/"
players<-100
belives_r<-rep(1/nrow(Games$Asymmetric$matrixRow),nrow(Games$Asymmetric$matrixRow))
belives_c<-rep(1/ncol(Games$Asymmetric$matrixCol),nrow(Games$Asymmetric$matrixCol))
Asymetric<-simulate_game(matrixRow = Games$Asymmetric$matrixRow,
                         matrixCol = Games$Asymmetric$matrixCol,
                         pairs = players,trials = t,
                         lambda = l,belives_row = belives_r,belives_col = belives_c)
save(Asymetric,file=paste(c(OutPath,Game_id,".RData"),collapse=""))
