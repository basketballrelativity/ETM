rm(list=ls())

# Relevant libraries
library(ggplot2)

PlayByPlay = read.csv("newPBP_1.csv",sep=",") # Loading in play-by-play ETM data
GameByGame = read.csv("newGBG_1.csv",sep=",") # Loading in game aggregate ETM data

GameByGame$AetmTOT = GameByGame$AwayETM+GameByGame$AwayDETM # Away ETM + dETM
GameByGame$HetmTOT = GameByGame$HomeETM+GameByGame$HomeDETM # Home ETM + dETM

uniqueTeams = unique(PlayByPlay$AwayTeam) # Gets a list of all 30 NBA teams

# Initialize variables
etmTotal = numeric(length=0)
etmRate = numeric(length=0)
detmTotal = numeric(length=0)
detmRate = numeric(length=0)
etmBoth = numeric(length=0)
etmBothRate = numeric(length=0)
WP = numeric(length=0)
Games = numeric(length=0)
WP_init = numeric(length=0)
L = numeric(length=0)
makePer = numeric(length=0)
makePerO = numeric(length=0)
Diff = numeric(length=0)
TO = numeric(length=0)
MM = numeric(length=0)
FF = numeric(length=0)

WPBegin = numeric(length=0)
etmPTotal = numeric(length=0)
etmPlayRate = numeric(length=0)
meanScore = numeric(length=0)
correctOplay = numeric(length=0)
detmPlayRate = numeric(length=0)
bothPlayRate = numeric(length=0)
totalPlays = numeric(length=0)

for (i in 1:length(uniqueTeams)){

 Awhere = which(GameByGame$AwayTeam==uniqueTeams[i]) # Away games for team i
 Hwhere = which(GameByGame$HomeTeam==uniqueTeams[i]) # Home games for team i

 # ETM by game for team i
 etm = GameByGame$AwayETM[Awhere] 
 etm = append(etm,GameByGame$HomeETM[Hwhere])

 # dETM by game for team i
 detm = GameByGame$AwayDETM[Awhere]
 detm = append(detm,GameByGame$HomeDETM[Hwhere])
 
 # Starting win probability by game for team i
 WPstart = GameByGame$AwayWPstart[Awhere]
 WPstart = append(WPstart,GameByGame$HomeWPstart[Hwhere])
 
 # Mean starting win probability for team i
 WP_init = append(WP_init,mean(WPstart))

 # Total close games for team i
 totalGames = length(etm)
 
 # Game ETM rates for team i
 etmPerGame = sum(etm)/totalGames
 detmPerGame = sum(detm)/totalGames
 
 # Store rates and totals
 etmRate = append(etmRate,etmPerGame)
 etmTotal = append(etmTotal,sum(etm))

 detmRate = append(detmRate,detmPerGame)
 detmTotal = append(detmTotal,sum(detm))

 # ETM and dETM total
 etmBoth = append(etmBoth,sum(etm)+sum(detm))

 # Total ETM rate
 etmBothVector = etm+detm
 etmBothRate = append(etmBothRate,sum(etmBothVector)/totalGames)

 AW = length(which(GameByGame$Win[Awhere]==-1)) # Away wins
 HW = length(which(GameByGame$Win[Hwhere]==1)) # Home wins
 TW = length(which(GameByGame$Win[append(Awhere,Hwhere)]==0)) # Ties

 # Percentage of games where team i had the lower total ETM (ETM + dETM)
 LowerProb = (length(which(GameByGame$AetmTOT[Awhere]<=GameByGame$HetmTOT[Awhere])) + length(which(GameByGame$HetmTOT[Hwhere]<=GameByGame$AetmTOT[Hwhere])))/(length(etm))
 
 # Game by game total ETM difference relative to team i
 diff = GameByGame$AetmTOT[Awhere]-GameByGame$HetmTOT[Awhere]
 diff = append(diff,GameByGame$HetmTOT[Hwhere]-GameByGame$AetmTOT[Hwhere])
 
 # Store total ETM difference
 Diff = append(Diff,mean(diff))
 
 # Store lower ETM percentage
 L = append(L,LowerProb)
 
 # Win probability in close games (ties excluded)
 WP = append(WP,(AW+HW)/(length(etm)-TW))
 Games = append(Games,totalGames) # Total close games played by team i

 ### Play by Play Analysis

 AwhereP = which(PlayByPlay$AwayTeam==uniqueTeams[i]) # Away plays of team i
 HwhereP = which(PlayByPlay$HomeTeam==uniqueTeams[i]) # Home plays of team i

 uniqueGameIndA = unique(PlayByPlay$GameIndex[AwhereP]) # Unique away game indices for team i
 uniqueGameIndH = unique(PlayByPlay$GameIndex[HwhereP]) # Unique home game indices for team i

 numberWin = which(GameByGame$Win[uniqueGameIndA]==-1) # Away wins for team i
 numberWin = append(numberWin,which(GameByGame$Win[uniqueGameIndH]==1)) # Appending home wins for team i

 AwherePmake = which(PlayByPlay$AwayTeam==uniqueTeams[i] & PlayByPlay$AwayPlay==1) # Away team i actions
 HwherePmake = which(PlayByPlay$HomeTeam==uniqueTeams[i] & PlayByPlay$HomePlay==1) # Home team i actions

 AwherePmakeO = which(PlayByPlay$AwayTeam==uniqueTeams[i] & PlayByPlay$AwayPlay==0) # Team i opponent actions
 HwherePmakeO = which(PlayByPlay$HomeTeam==uniqueTeams[i] & PlayByPlay$HomePlay==0) # Team i opponent actions

 anyWhere = append(AwherePmake,HwherePmake) # Total actions for team i

 # ETM on team i actions
 etmP = PlayByPlay$ETM[AwherePmake]
 etmP = append(etmP,PlayByPlay$ETM[HwherePmake])
 
 # dETM on team i actions
 detmP = PlayByPlay$DETM[AwherePmake]
 detmP = append(detmP,PlayByPlay$DETM[HwherePmake])

 totalOPlays = length(which(PlayByPlay$PlayType[anyWhere]!=5)) # Number of offensive plays for team i
 totalOPlaysO = length(which(PlayByPlay$PlayType[append(AwherePmakeO,HwherePmakeO)]!=5)) # Team i opponent offensive plays
 totalTO = length(which(PlayByPlay$PlayType[anyWhere]==0))/totalGames # Turnovers per game for team i
 totalTOO = length(which(PlayByPlay$PlayType[append(AwherePmakeO,HwherePmakeO)]==0))/totalGames # Team i opponent turnovers per game

 totalFPlays = length(PlayByPlay$PlayType[anyWhere]) - totalOPlays # Number of team i fouls
 totalFPlaysO = length((PlayByPlay$PlayType[append(AwherePmakeO,HwherePmakeO)])) - totalOPlaysO # Team i opponent fouls
 
 # Team i and opponent foul rates
 tF = totalFPlays/totalGames
 tFO = totalFPlaysO/totalGames

 # Foul rate difference relative to team i opponent
 FF = append(FF,tFO - tF)
 
 # Turnover rate difference relative to team i opponent
 TO = append(TO,totalTOO-totalTO)

 totalDPlays = length(etmP)-totalOPlays # Team i defensive actions
 totalPlays = append(totalPlays,length(etmP)) # Team i offensive actions

 # Correct decision percentage. Correct decision occurs if Actual 
 correct = length(which(PlayByPlay$ActualDecisionO[anyWhere]==PlayByPlay$OptimalDecisionO[anyWhere] & PlayByPlay$PlayType[anyWhere]!=5))/totalOPlays
 correctOplay = append(correctOplay,correct)

 # Gathering mean score difference over all plays
 score = PlayByPlay$ScoreDifference[anyWhere]
 meanScore = append(meanScore,mean(score))
 
 # Field goals made by team i
 make = PlayByPlay$Make[append(AwherePmake,HwherePmake)]
 makePer = append(makePer,sum(make)/totalOPlays)
 
 # Field goals made by opponent of team i
 makeO = PlayByPlay$Make[append(AwherePmakeO,HwherePmakeO)]
 makePerO = append(makePerO,sum(makeO)/totalOPlaysO)
 
 # Difference in field goal percentage in close games relative to team i
 MM = append(MM,sum(make)/totalOPlays-sum(makeO)/totalOPlaysO)
 
 # ETM on team i actions 
 etmPTotal = append(etmPTotal,sum(etmP))
 
 # ETM rates
 etmPRate = sum(etmP)/totalOPlays
 detmPRate = sum(detmP)/totalDPlays

 bothPRate = (sum(etmP)+sum(detmP))/(length(etmP))

 etmPlayRate = append(etmPlayRate,etmPRate)
 detmPlayRate = append(detmPlayRate,detmPRate)
 bothPlayRate = append(bothPlayRate,bothPRate)

}

### VISUALIZATION ###

AwherePmake = which(PlayByPlay$AwayTeam=="MIN" & PlayByPlay$AwayPlay==1)
HwherePmake = which(PlayByPlay$HomeTeam=="MIN" & PlayByPlay$HomePlay==1)

etmP = PlayByPlay$ETM[AwherePmake]#/PlayByPlay$OptimalWP[AwherePmake]
etmP = append(etmP,PlayByPlay$ETM[HwherePmake])#/PlayByPlay$OptimalWP[HwherePmake])

hist(etmP)

etmSummary = data.frame(uniqueTeams,Games,WP,M=makePer-makePerO,Diff,TO,WP_init,L,FF,etmTotal,etmRate,detmTotal,detmRate,etmBoth,etmBothRate)

etmPSummary = data.frame(uniqueTeams,WP,WP_init,etmPlayRate,detmPlayRate,bothPlayRate)

etmSummary = etmSummary[order(Diff),]
etmPSummary = etmPSummary[order(bothPlayRate),]

A = which(etmP>=0)
h = hist(PlayByPlay$ETM[A],breaks=12,plot=FALSE)
h$counts = h$counts/sum(h$counts)
 plot(h,xaxt="n",xlab = "Play ETM", ylab = "Density", main="Minnesota Timberwolves ETM by Play",xlim=c(0.0057,0.15))
 axis(side=1,at=h$breaks)
 
 G = append(GameByGame$AwayETM,GameByGame$HomeETM)
 
B =  ggplot(GameByGame,aes(x =AetmTOT - HetmTOT,fill=factor(Win))) + geom_histogram() + labs(x= "ETM (Away Team - Home Team)", y="Games") + scale_fill_discrete(name="Result",labels = c("Away Win", "Overtime", "Home Win"))
