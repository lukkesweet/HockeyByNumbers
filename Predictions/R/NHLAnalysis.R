##################################################################
## Historical Analysis as a Function for Readability            ##
## No Overtimes between 1943 - 1983 for Regular Season          ##
## Shootouts instituted in 2005-2006 Season                     ##
## This analysis does not include preseason or exhibition games ##
##################################################################



memory.limit(size=70000)
start = Sys.time()

options(java.parameters = "-Xmx6g")
# Library
{
library(lubridate)
library(scales)
library(plyr)
library(rvest)
library(magrittr)
library(gridExtra)
library(grid)
suppressPackageStartupMessages(library(googleVis))
}

successrate <- c()

load(file = "C:/Users/User/Desktop/NHLPredictions/Data/Schedule1920.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/Standings1819.Rda")

# colnames(Schedule1920)[4] <- 'GH'

## Webscraping for Last Nights Scores

nhlscores <- read_html("https://www.hockey-reference.com/boxscores/")

nhlscores %<>%
  html_nodes(".teams td") %>%
  html_text()

nhlscores <- cbind.data.frame(split(nhlscores, rep(1:6, times = length(nhlscores)/6)), 
                              stringsAsFactors=F)

names(nhlscores) <- c("Visitor", "GA", "Final", "Home", "GH", "OT")

nhlscores <- nhlscores[c("Home", "Visitor", "GH", "GA", "OT")]

nhlscores <- cbind("Date" = rep(Sys.Date()-1, nrow(nhlscores)), nhlscores)

Schedule1920$Date <- as.Date(Schedule1920$Date, "%B %d, %Y")
nhlscores$Date <- as.Date(nhlscores$Date, "%B %d, %Y")

nhlscores$OT <- substr(nhlscores$OT, start = 1, stop = 2)

for(i in 1:nrow(Schedule1920)){
  for(j in 1:nrow(nhlscores)){
    if(Schedule1920[i, 1]==nhlscores[j, 1]){
      if(Schedule1920[i, 2]==nhlscores[j, 2]){
        Schedule1920[i, 4] = nhlscores[j, 4]
        Schedule1920[i, 5] = nhlscores[j, 5] 
        if(nhlscores[j, 6]=="OT"){
          Schedule1920[i, 7] = "Y"
          Schedule1920[i, 8] = "N"
        }
        else if(nhlscores[j, 6]=="SO"){
          Schedule1920[i, 7]="Y"
          Schedule1920[i, 8]="Y"
        }
        else{
          Schedule1920[i, 7]="N"
          Schedule1920[i, 8]="N"
        }
      }
    }
  }
}

Schedule1920$GA <- as.numeric(Schedule1920$GA)
Schedule1920$GH <- as.numeric(Schedule1920$GH)

## Save scores
save(Schedule1920, file = "C:/Users/User/Desktop/NHLPredictions/Data/Schedule1920.Rda")

# Set parameter Values
homeice <<- 20 # Home Ice Advantage
k <<- 8 # How far to move Ratings after each match
SC <<- 1/3 # How far to regress back to the mean between seasons

load(file = "C:/Users/User/Desktop/NHLPredictions/Data/ArizonaCoyotes.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/BostonBruins.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/BuffaloSabres.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/CalgaryFlames.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/CarolinaHurricanes.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/ChicagoBlackhawks.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/ColoradoAvalanche.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/DallasStars.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/DetroitRedWings.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/EdmontonOilers.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/LosAngelesKings.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/MontrealCanadiens.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/NewJerseyDevils.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/NewYorkIslanders.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/NewYorkRangers.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/PhiladelphiaFlyers.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/PittsburghPenguins.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/St.LouisBlues.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/TorontoMapleLeafs.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/VancouverCanucks.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/WashingtonCapitals.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/SanJoseSharks.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/OttawaSenators.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/TampaBayLightning.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/AnaheimDucks.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/FloridaPanthers.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/NashvillePredators.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/WinnipegJets.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/ColumbusBlueJackets.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/MinnesotaWild.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/ClevelandBarons.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/BrooklynAmericans.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/MontrealMaroons.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/MontrealWanderers.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/PhiladelphiaQuakers.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/St.LouisEagles.Rda")
load(file = "C:/Users/User/Desktop/NHLPredictions/Data/VegasGoldenKnights.Rda")

analysis <- function(Sch, PStand){
names(Sch)[names(Sch)=="ELO.1"] <- "ELO"
names(Sch)[names(Sch)=="ELO.CHANGE.HOME."] <- "ELO.CHANGE"
names(Sch)[names(Sch)=="ELO.CHANGE.AWAY."] <- "ELO.CHANGE"

load(file="C:/Users/User/Desktop/NHLPredictions/Data/Standings.Rda")
Sch[,1] = as.Date(Sch[,1])



for(i in c(1:nrow(Sch))){
if(Sch[i, 1] < Sys.Date()){
Sch[i, 6] = Sch[i,4] - Sch[i, 5]
}
else{Sch[i, 6] = 0}
}

for(i in c(1:nrow(Standings))){
if(PStand[i,4] != 0) {Standings[i,13] = PStand[i,13] + (SC)*(1500 - PStand[i,13])}
else{
  Standings[i,13] = 1400
}
}
# Populate the Standings Dataframe
for(i in c(1:nrow(Standings))){
for(j in c(1:nrow(Sch))){
  #Count Games Played (GP)
  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] !=0){
    Standings[i, 4] = Standings[i,4] + 1
  }
  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] !=0){
    Standings[i, 4] = Standings[i,4] + 1
  }
  #Count Games Won (W)
  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] > 0){
    Standings[i, 5] = Standings[i,5] + 1
  }
  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] < 0){
    Standings[i, 5] = Standings[i,5] + 1
  }
  # Counts Games Lost in Regulation Time (L)
  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] < 0 && Sch[j, 7] == "N"){
    Standings[i, 6] = Standings[i,6] + 1
  }
  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] > 0 && Sch[j, 7] == "N"){
    Standings[i, 6] = Standings[i,6] + 1
  }
  # Counts Games Lost in  Overime (L)
  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] < 0 && Sch[j, 7] == "Y"){
    Standings[i, 7] = Standings[i,7] + 1
  }
  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] > 0 && Sch[j, 7] == "Y"){
    Standings[i, 7] = Standings[i,7] + 1
  }
  # Counting Goals For
  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] !=0){
    Standings[i, 10] = Standings[i,10] + Sch[j, 4]
  }
  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] !=0){
    Standings[i, 10] = Standings[i,10] + Sch[j,5]
  }
  # Counting Goals Against
  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] !=0){
    Standings[i, 11] = Standings[i,11] + Sch[j,5]
  }
  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] !=0){
    Standings[i, 11] = Standings[i,11] + Sch[j,4]
  }
  # Calculating Regulation + Overtime Wins (ROW)
  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] > 0 && Sch[j,8] == "N"){
    Standings[i, 9] = Standings[i,9] + 1
  }
  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] < 0 && Sch[j, 8] == "N"){
    Standings[i, 9] = Standings[i,9] + 1
  }
}
# Calculate Points
Standings[i,8] = 2*Standings[i,5] + Standings[i,7]
# Calculate Goal Differential
Standings[i,12] = Standings[i,10] - Standings[i, 11]
}



# Build a function to calculate win probability for the home team
winprob = function(home, away){
for(i in c(1:nrow(Standings))){
  if(Standings[i,1]==home){
    ELOh = Standings[i,13]
  }
  else if(Standings[i,1]==away){
    ELOv = Standings[i,13]
  }
}

Eh = round(1 / (1 + 10 ^ (-(ELOh - ELOv + homeice)/400)), 2)
output = c(Eh, ELOh, ELOv)
return(output)
}

# Calculate Probability of Winning and ELO Change
for(i in c(1:nrow(Sch))){
if(Sch[i, 1] <= Sys.Date()){
  Sch[i,9] = winprob(Sch[i,2], Sch[i, 3])[1]
  Sch[i, 10] = 1 - Sch[i,9]
  Sch[i, 15] = winprob(Sch[i,2], Sch[i, 3])[2]
  Sch[i, 16] = winprob(Sch[i,2], Sch[i, 3])[3]
  Sch[i, 14] = Sch[i, 15] - Sch[i, 16]
  if(!is.na(Sch[i,6])){
    if(Sch[i,6]>0 && Sch[i,8]=="N"){
      outcome=1
    }
    else if(Sch[i,6] <0 && Sch[i,8]=="N"){
      outcome = 0
    }
    else{outcome = 0.5}
    if(Sch[i,13]=="N"){
      I = 1
    }
    else{I=1.5}
    M = max(1, log(abs(Sch[i,6] - 0.85*((winprob(Sch[i,2], Sch[i, 3])[2]-
                                          winprob(Sch[i,2], Sch[i, 3])[3]+homeice)/100)+exp(1)-1)))
    Sch[i,11] = k*M*I*(outcome-Sch[i,9])
    Sch[i,12] = -Sch[i,11]
    for(j in c(1:nrow(Standings))){
      if(Standings[j,1]==Sch[i,2]){
        Standings[j,13] = Standings[j,13] + Sch[i,11]
      }
      else if(Standings[j,1] == Sch[i,3]){
        Standings[j,13] = Standings[j,13] + Sch[i,12]
      }
    }
  } 
}







## This is the new CODE!!!!! MAY NOT WORK
# 1
if(Sch[i, 2] == "Toronto Maple Leafs"){
TorontoMapleLeafs <<- rbind(TorontoMapleLeafs, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Toronto Maple Leafs"){
TorontoMapleLeafs <<- rbind(TorontoMapleLeafs, Sch[i, c(1, 12, 16)])
}
# 2
else if(Sch[i, 2] == "Anaheim Ducks"){
AnaheimDucks <<- rbind(AnaheimDucks, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Anaheim Ducks"){
AnaheimDucks <<- rbind(AnaheimDucks, Sch[i, c(1, 12, 16)])
}
# 3
else if(Sch[i, 2] == "Arizona Coyotes"){
ArizonaCoyotes <<- rbind(ArizonaCoyotes, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Arizona Coyotes"){
ArizonaCoyotes <<- rbind(ArizonaCoyotes, Sch[i, c(1, 12, 16)])
}  
# 4
else if(Sch[i, 2] == "Boston Bruins"){
BostonBruins <<- rbind(BostonBruins, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Boston Bruins"){
BostonBruins <<- rbind(BostonBruins, Sch[i, c(1, 12, 16)])
}
#5
else if(Sch[i, 2] == "Brooklyn Americans"){
BrooklynAmericans <<- rbind(BrooklynAmericans, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Brooklyn Americans"){
BrooklynAmericans <<- rbind(BrooklynAmericans, Sch[i, c(1, 12, 16)])
}
#6
else if(Sch[i, 2] == "Buffalo Sabres"){
BuffaloSabres <<- rbind(BuffaloSabres, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Buffalo Sabres"){
BuffaloSabres <<- rbind(BuffaloSabres, Sch[i, c(1, 12, 16)])
}
# 7
else if(Sch[i, 2] == "Calgary Flames"){
CalgaryFlames <<- rbind(CalgaryFlames, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Calgary Flames"){
CalgaryFlames <<- rbind(CalgaryFlames, Sch[i, c(1, 12, 16)])
}
# 8
else if(Sch[i, 2] == "Carolina Hurricanes"){
CarolinaHurricanes <<- rbind(CarolinaHurricanes, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Carolina Hurricanes"){
CarolinaHurricanes <<- rbind(CarolinaHurricanes, Sch[i, c(1, 12, 16)])
}
# 9
else if(Sch[i, 2] == "Chicago Blackhawks"){
ChicagoBlackhawks <<- rbind(ChicagoBlackhawks, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Chicago Blackhawks"){
ChicagoBlackhawks <<- rbind(ChicagoBlackhawks, Sch[i, c(1, 12, 16)])
}
# 10
else if(Sch[i, 2] == "Cleveland Barons"){
ClevelandBarons <<- rbind(ClevelandBarons, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Cleveland Barons"){
ClevelandBarons <<- rbind(ClevelandBarons, Sch[i, c(1, 12, 16)])
}
# 11
else if(Sch[i, 2] == "Colorado Avalanche"){
ColoradoAvalanche <<- rbind(ColoradoAvalanche, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Colorado Avalanche"){
ColoradoAvalanche <<- rbind(ColoradoAvalanche, Sch[i, c(1, 12, 16)])
}
# 12
else if(Sch[i, 2] == "Columbus Blue Jackets"){
ColumbusBlueJackets <<- rbind(ColumbusBlueJackets, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Columbus Blue Jackets"){
ColumbusBlueJackets <<- rbind(ColumbusBlueJackets, Sch[i, c(1, 12, 16)])
}
# 13
else if(Sch[i, 2] == "Dallas Stars"){
DallasStars <<- rbind(DallasStars, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Dallas Stars"){
DallasStars <<- rbind(DallasStars, Sch[i, c(1, 12, 16)])
}
# 14
else if(Sch[i, 2] == "Detroit Red Wings"){
DetroitRedWings <<- rbind(DetroitRedWings, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Detroit Red Wings"){
DetroitRedWings <<- rbind(DetroitRedWings, Sch[i, c(1, 12, 16)])
}
# 15
else if(Sch[i, 2] == "Edmonton Oilers"){
EdmontonOilers <<- rbind(EdmontonOilers, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Edmonton Oilers"){
EdmontonOilers <<- rbind(EdmontonOilers, Sch[i, c(1, 12, 16)])
}
# 16
else if(Sch[i, 2] == "Florida Panthers"){
FloridaPanthers <<- rbind(FloridaPanthers, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Florida Panthers"){
FloridaPanthers <<- rbind(FloridaPanthers, Sch[i, c(1, 12, 16)])
}
# 17
else if(Sch[i, 2] == "Los Angeles Kings"){
LosAngelesKings <<- rbind(LosAngelesKings, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Los Angeles Kings"){
LosAngelesKings <<- rbind(LosAngelesKings, Sch[i, c(1, 12, 16)])
}
# 18
else if(Sch[i, 2] == "Minnesota Wild"){
MinnesotaWild <<- rbind(MinnesotaWild, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Minnesota Wild"){
MinnesotaWild <<- rbind(MinnesotaWild, Sch[i, c(1, 12, 16)])
}
# 19
else if(Sch[i, 2] == "Montreal Canadiens"){
MontrealCanadiens <<- rbind(MontrealCanadiens, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Montreal Canadiens"){
MontrealCanadiens <<- rbind(MontrealCanadiens, Sch[i, c(1, 12, 16)])
}
# 20
else if(Sch[i, 2] == "Montreal Maroons"){
MontrealMaroons <<- rbind(MontrealMaroons, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Montreal Maroons"){
MontrealMaroons <<- rbind(MontrealMaroons, Sch[i, c(1, 12, 16)])
}
# 21
else if(Sch[i, 2] == "Montreal Wanderers"){
MontrealWanderers <<- rbind(MontrealWanderers, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Montreal Wanderers"){
MontrealWanderers <<- rbind(MontrealWanderers, Sch[i, c(1, 12, 16)])
}
# 22
else if(Sch[i, 2] == "Nashville Predators"){
NashvillePredators <<- rbind(NashvillePredators, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Nashville Predators"){
NashvillePredators <<- rbind(NashvillePredators, Sch[i, c(1, 12, 16)])
}
# 23
else if(Sch[i, 2] == "New Jersey Devils"){
NewJerseyDevils <<- rbind(NewJerseyDevils, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "New Jersey Devils"){
NewJerseyDevils <<- rbind(NewJerseyDevils, Sch[i, c(1, 12, 16)])
}
# 24
else if(Sch[i, 2] == "New York Islanders"){
NewYorkIslanders <<- rbind(NewYorkIslanders, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "New York Islanders"){
NewYorkIslanders <<- rbind(NewYorkIslanders, Sch[i, c(1, 12, 16)])
}
# 25
else if(Sch[i, 2] == "New York Rangers"){
NewYorkRangers <<- rbind(NewYorkRangers, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "New York Rangers"){
NewYorkRangers <<- rbind(NewYorkRangers, Sch[i, c(1, 12, 16)])
}
# 26
else if(Sch[i, 2] == "Ottawa Senators"){
OttawaSenators <<- rbind(OttawaSenators, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Ottawa Senators"){
OttawaSenators <<- rbind(OttawaSenators, Sch[i, c(1, 12, 16)])
}
# 27
else if(Sch[i, 2] == "Philadelphia Flyers"){
PhiladelphiaFlyers <<- rbind(PhiladelphiaFlyers, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Philadelphia Flyers"){
PhiladelphiaFlyers <<- rbind(PhiladelphiaFlyers, Sch[i, c(1, 12, 16)])
}
# 28
else if(Sch[i, 2] == "Philadelphia Quakers"){
PhiladelphiaQuakers <<- rbind(PhiladelphiaQuakers, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Philadelphia Quakers"){
PhiladelphiaQuakers <<- rbind(PhiladelphiaQuakers, Sch[i, c(1, 12, 16)])
}
# 29
else if(Sch[i, 2] == "Pittsburgh Penguins"){
PittsburghPenguins <<- rbind(PittsburghPenguins, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Pittsburgh Penguins"){
PittsburghPenguins <<- rbind(PittsburghPenguins, Sch[i, c(1, 12, 16)])
}
#30
else if(Sch[i, 2] == "San Jose Sharks"){
SanJoseSharks <<- rbind(SanJoseSharks, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "San Jose Sharks"){
SanJoseSharks <<- rbind(SanJoseSharks, Sch[i, c(1, 12, 16)])
}
# 31
else if(Sch[i, 2] == "St. Louis Blues"){
St.LouisBlues <<- rbind(St.LouisBlues, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "St. Louis Blues"){
St.LouisBlues <<- rbind(St.LouisBlues, Sch[i, c(1, 12, 16)])
}
# 32
if(Sch[i, 2] == "St. Louis Eagles"){
St.LouisEagles <<- rbind(St.LouisEagles, Sch[i, c(1, 11, 15)])
}
if(Sch[i, 3] == "St. Louis Eagles"){
St.LouisEagles <<- rbind(St.LouisEagles, Sch[i, c(1, 12, 16)])
}
# 33
else if(Sch[i, 2] == "Tampa Bay Lightning"){
TampaBayLightning <<- rbind(TampaBayLightning, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Tampa Bay Lightning"){
TampaBayLightning <<- rbind(TampaBayLightning, Sch[i, c(1, 12, 16)])
}
# 34
else if(Sch[i, 2] == "Vancouver Canucks"){
VancouverCanucks <<- rbind(VancouverCanucks, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Vancouver Canucks"){
VancouverCanucks <<- rbind(VancouverCanucks, Sch[i, c(1, 12, 16)])
}
# 35
else if(Sch[i, 2] == "Washington Capitals"){
WashingtonCapitals <<- rbind(WashingtonCapitals, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Washington Capitals"){
WashingtonCapitals <<- rbind(WashingtonCapitals, Sch[i, c(1, 12, 16)])
}
# 36
else if(Sch[i, 2] == "Winnipeg Jets"){
WinnipegJets <<- rbind(WinnipegJets, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Winnipeg Jets"){
WinnipegJets <<- rbind(WinnipegJets, Sch[i, c(1, 12, 16)])
}
# 37
else if(Sch[i, 2] == "Vegas Golden Knights"){
VegasGoldenKnights <<- rbind(VegasGoldenKnights, Sch[i, c(1, 11, 15)])
}
else if(Sch[i, 3] == "Vegas Golden Knights"){
VegasGoldenKnights <<- rbind(VegasGoldenKnights, Sch[i, c(1, 12, 16)])
}
## This is the end of the new code!

              }



Year = year(as.POSIXlt(Sch[nrow(Sch),1]))
Year = rep(Year, nrow(Standings))
Standings = cbind(Standings, Year)

return(list(Standings=Standings, Schedule=Sch))
}


output <- analysis(Schedule1920, Standings1819)
Standings1920 <<- output$Standings
Schedule1920 <<- output$Schedule
rm(output)

       
## Dropping Defunct Teams
dropped <- c()
for(i in c(1:nrow(Standings1920))){
  if(Standings1920[i, 4]==0){
    dropped <- append(dropped, i)
  }
}
Standings1920 = Standings1920[-dropped,]
Standings1920 = Standings1920[order(-Standings1920[, 8], Standings1920[, 4], 
                                    -Standings1920[, 9]),]

# Creating a Pop-up Table for Tonight's Games
 TodaysPredictions = data.frame()
 
 for(i in c(1:nrow(Schedule1920))){
   if(Schedule1920[i, 1] == Sys.Date()){
     TodaysPredictions = rbind(TodaysPredictions, Schedule1920[i,])
   }
 }
 
 TodaysPredictions = TodaysPredictions[,-c(4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16)]
 TodaysPredictions[,4] = percent(TodaysPredictions[,4])
 TodaysPredictions[,5] = percent(TodaysPredictions[,5])
 TodaysPredictions = rename(TodaysPredictions, c("P.H.WINS." = "Probability Home Team Wins", "P.A.WINS." = "Probability Away Team Wins"))
 # Table <- gvisTable(TodaysPredictions, options=list(title="Today's Predictions Given the Complete History of the NHL"))
 # plot(Table)
 # grid.table(TodaysPredictions)
end = Sys.time()
end-start


library(mailR)
library(xtable)

TodaysPredictions$Date <- format(TodaysPredictions$Date, '%m/%d/%Y')

sender <- 'lukkerachelsweet@gmail.com'
recipient <- c('lukkerachelsweet@gmail.com', 'dcodysweet@gmail.com')

send.mail(from = sender,
          to = recipient,
          subject = 'NHL Predictions',
          body = print(xtable(TodaysPredictions), type = 'html'),
          html = T,
          smtp = list(host.name = 'smtp.gmail.com', port = 465,
                      user.name = 'lukkerachelsweet',
                      passwd = 'wgmot4oj',
                      ssl = T),
          authenticate = T,
          send = T)

save(ArizonaCoyotes, file = "C:/Users/User/Desktop/NHLPredictions/Data/ArizonaCoyotes.Rda")
save(BostonBruins, file = "C:/Users/User/Desktop/NHLPredictions/Data/BostonBruins.Rda")
save(BuffaloSabres, file = "C:/Users/User/Desktop/NHLPredictions/Data/BuffaloSabres.Rda")
save(CalgaryFlames, file = "C:/Users/User/Desktop/NHLPredictions/Data/CalgaryFlames.Rda")
save(CarolinaHurricanes, file = "C:/Users/User/Desktop/NHLPredictions/Data/CarolinaHurricanes.Rda")
save(ChicagoBlackhawks, file = "C:/Users/User/Desktop/NHLPredictions/Data/ChicagoBlackhawks.Rda")
save(ColoradoAvalanche, file = "C:/Users/User/Desktop/NHLPredictions/Data/ColoradoAvalanche.Rda")
save(DallasStars, file = "C:/Users/User/Desktop/NHLPredictions/Data/DallasStars.Rda")
save(DetroitRedWings, file = "C:/Users/User/Desktop/NHLPredictions/Data/DetroitRedWings.Rda")
save(EdmontonOilers, file = "C:/Users/User/Desktop/NHLPredictions/Data/EdmontonOilers.Rda")
save(LosAngelesKings, file = "C:/Users/User/Desktop/NHLPredictions/Data/LosAngelesKings.Rda")
save(MontrealCanadiens, file = "C:/Users/User/Desktop/NHLPredictions/Data/MontrealCanadiens.Rda")
save(NewJerseyDevils, file = "C:/Users/User/Desktop/NHLPredictions/Data/NewJerseyDevils.Rda")
save(NewYorkIslanders, file = "C:/Users/User/Desktop/NHLPredictions/Data/NewYorkIslanders.Rda")
save(NewYorkRangers, file = "C:/Users/User/Desktop/NHLPredictions/Data/NewYorkRangers.Rda")
save(PhiladelphiaFlyers, file = "C:/Users/User/Desktop/NHLPredictions/Data/PhiladelphiaFlyers.Rda")
save(PittsburghPenguins, file = "C:/Users/User/Desktop/NHLPredictions/Data/PittsburghPenguins.Rda")
save(St.LouisBlues, file = "C:/Users/User/Desktop/NHLPredictions/Data/St.LouisBlues.Rda")
save(TorontoMapleLeafs, file = "C:/Users/User/Desktop/NHLPredictions/Data/TorontoMapleLeafs.Rda")
save(VancouverCanucks, file = "C:/Users/User/Desktop/NHLPredictions/Data/VancouverCanucks.Rda")
save(WashingtonCapitals, file = "C:/Users/User/Desktop/NHLPredictions/Data/WashingtonCapitals.Rda")
save(SanJoseSharks, file = "C:/Users/User/Desktop/NHLPredictions/Data/SanJoseSharks.Rda")
save(OttawaSenators, file = "C:/Users/User/Desktop/NHLPredictions/Data/OttawaSenators.Rda")
save(TampaBayLightning, file = "C:/Users/User/Desktop/NHLPredictions/Data/TampaBayLightning.Rda")
save(AnaheimDucks, file = "C:/Users/User/Desktop/NHLPredictions/Data/AnaheimDucks.Rda")
save(FloridaPanthers, file = "C:/Users/User/Desktop/NHLPredictions/Data/FloridaPanthers.Rda")
save(NashvillePredators, file = "C:/Users/User/Desktop/NHLPredictions/Data/NashvillePredators.Rda")
save(WinnipegJets, file = "C:/Users/User/Desktop/NHLPredictions/Data/WinnipegJets.Rda")
save(ColumbusBlueJackets, file = "C:/Users/User/Desktop/NHLPredictions/Data/ColumbusBlueJackets.Rda")
save(MinnesotaWild, file = "C:/Users/User/Desktop/NHLPredictions/Data/MinnesotaWild.Rda")
save(ClevelandBarons, file = "C:/Users/User/Desktop/NHLPredictions/Data/ClevelandBarons.Rda")
save(BrooklynAmericans, file = "C:/Users/User/Desktop/NHLPredictions/Data/BrooklynAmericans.Rda")
save(MontrealMaroons, file = "C:/Users/User/Desktop/NHLPredictions/Data/MontrealMaroons.Rda")
save(MontrealWanderers, file = "C:/Users/User/Desktop/NHLPredictions/Data/MontrealWanderers.Rda")
save(PhiladelphiaQuakers, file = "C:/Users/User/Desktop/NHLPredictions/Data/PhiladelphiaQuakers.Rda")
save(St.LouisEagles, file = "C:/Users/User/Desktop/NHLPredictions/Data/St.LouisEagles.Rda")
save(VegasGoldenKnights, file = "C:/Users/User/Desktop/NHLPredictions/Data/VegasGoldenKnights.Rda")
save(Schedule1920, file = 'C:/Users/User/Desktop/NHLPredictions/Data/Schedule1920.Rda')

