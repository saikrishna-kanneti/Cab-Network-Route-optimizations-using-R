################################################################################
##
##Sai krishna kanneti
##Fantasy Football team selection using lpsolve
## April 27 2018
################################################################################

rm(list=ls())

require(ggplot2)
require(ggthemes)

require(plyr)
require(stringr)

library(lpSolve)

setwd("C:/Users/kanne/Desktop/STA_PROJECT")

df <- read.csv("com.csv", sep="|")

head(df)

dim(df)
names(df)

unique(df$Position)

## data 
df$Score <- as.numeric(df$Punkte)
df$MarketValue <- as.numeric(df$MW)

football_data <- df[c("Name","Position", "Score" , "MarketValue")]

football_data$Position <- ifelse(football_data$Position == "Torwart", "Keeper", football_data$Position)
football_data$Position <- ifelse(football_data$Position == "Abwehr", "Defense", football_data$Position)
football_data$Position <- ifelse(football_data$Position == "Mittelfeld", "Midfield", football_data$Position)
football_data$Position <- ifelse(football_data$Position == "Sturm", "Offense", football_data$Position)

football_data

x11()
## plot 
ggplot(football_data, aes(MarketValue / 1000, Score, color=Position)) + geom_point()+ geom_smooth()+ facet_wrap(~Position) + theme_economist() + xlab("Market Value in K$")

f.obj <- football_data$Score  ###objective max team score

f.con <- t(football_data$MarketValue)  ### constraints max MV <= Budget
player <- rep(1, nrow(football_data))  ## constraints max number of players!

f.con <- rbind(f.con, player)

## constrain that per postion can only be a certain number of players be set up. (e.g. just one keeper)
## define matrix   - as a one hot (dummy coding what position the player holds)
A <- as.data.frame(model.matrix(MarketValue ~ Position -1, football_data) )

f.con <- rbind(f.con, t(as.matrix(A)))

f.dir <- c("<=", "<=", "=", "<=", "<=" ,"<=")
f.rhs <- c(20000000, 13, 1, 5, 5, 3)  ## right hand side .. not more than Budget, Players, and players per position.

### solve the problem
solved<- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)  ## just binary variables!

###################output!
football_data$buy <- solved$solution

sum(df[df$buy == 1,]$MarketValue)  ## what is the Budget
sum(df[df$buy == 1,]$Score) ## what is the Score
sum(df[df$buy == 1,]$buy)   ## number of players bought
paste(df[df$buy == 1,]$Name, collapse=", ")  
############################

result<-paste(df[df$buy == 1,]$Name, collapse=", ")

result
