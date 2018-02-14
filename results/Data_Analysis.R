# This script is to be used only for the main data analysis of the paper on simulation and QRE
# Game 1: Battle of the Sexes
source("src/Simulate_Game.R")
source("src/Bayes.R")
source("src/Maximum_Likelihood.R")
g_row<-matrix(c(3,0,0,2),nrow=2)
g_col<-matrix(c(2,0,0,3),nrow=2)
players<-50
t<-150
l<-2
belives_r<-rep(1/nrow(g_row),nrow(g_row))
belives_c<-rep(1/ncol(g_col),nrow(g_col))
battle_of_sexes<-simulate_game(matrixRow = g_row,matrixCol = g_col,pairs = players,trials = t,
                              lambda = l,belives_row = belives_r,belives_col = belives_c)
