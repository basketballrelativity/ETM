rm(list=ls())

library(ggplot2)

PlayByPlay = read.csv("newPBP_1.csv",sep=",")
GameByGame = read.csv("newGBG_1.csv",sep=",")

GameByGame$AetmTOT = GameByGame$AwayETM+GameByGame$AwayDETM
GameByGame$HetmTOT = GameByGame$HomeETM+GameByGame$HomeDETM

uniqueTeams = unique(PlayByPlay$AwayTeam)

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

whereLosing = which(PlayByPlay$OptimalWP<0.5)
#PlayByPlay$OptimalWP[whereLosing] = 1 - PlayByPlay$OptimalWP[whereLosing]
for (i in 1:length(uniqueTeams)){

Awhere = which(GameByGame$AwayTeam==uniqueTeams[i])# & GameByGame$Win==1)

Hwhere = which(GameByGame$HomeTeam==uniqueTeams[i])# & GameByGame$Win==-1)

etm = GameByGame$AwayETM[Awhere]
etm = append(etm,GameByGame$HomeETM[Hwhere])

detm = GameByGame$AwayDETM[Awhere]
detm = append(detm,GameByGame$HomeDETM[Hwhere])

WPstart = GameByGame$AwayWPstart[Awhere]
WPstart = append(WPstart,GameByGame$HomeWPstart[Hwhere])

WP_init = append(WP_init,mean(WPstart))

totalGames = length(etm)

etmPerGame = sum(etm)/totalGames

detmPerGame = sum(detm)/totalGames

etmRate = append(etmRate,etmPerGame)
etmTotal = append(etmTotal,sum(etm))

detmRate = append(detmRate,detmPerGame)
detmTotal = append(detmTotal,sum(detm))

etmBoth = append(etmBoth,sum(etm)+sum(detm))

etmBothVector = etm+detm

etmBothRate = append(etmBothRate,sum(etmBothVector)/totalGames)

AW = length(which(GameByGame$Win[Awhere]==-1))
HW = length(which(GameByGame$Win[Hwhere]==1))
TW = length(which(GameByGame$Win[append(Awhere,Hwhere)]==0))

LowerProb = (length(which(GameByGame$AetmTOT[Awhere]<=GameByGame$HetmTOT[Awhere])) + length(which(GameByGame$HetmTOT[Hwhere]<=GameByGame$AetmTOT[Hwhere])))/(length(etm))

diff = GameByGame$AetmTOT[Awhere]-GameByGame$HetmTOT[Awhere]
diff = append(diff,GameByGame$HetmTOT[Hwhere]-GameByGame$AetmTOT[Hwhere])

Diff = append(Diff,mean(diff))

L = append(L,LowerProb)

WP = append(WP,(AW+HW)/(length(etm)-TW))
Games = append(Games,totalGames)

### Play by Play Analysis

AwhereP = which(PlayByPlay$AwayTeam==uniqueTeams[i])
HwhereP = which(PlayByPlay$HomeTeam==uniqueTeams[i])

uniqueGameIndA = unique(PlayByPlay$GameIndex[AwhereP])
uniqueGameIndH = unique(PlayByPlay$GameIndex[HwhereP])

numberWin = which(GameByGame$Win[uniqueGameIndA]==-1)
numberWin = append(numberWin,which(GameByGame$Win[uniqueGameIndH]==1))

AwherePmake = which(PlayByPlay$AwayTeam==uniqueTeams[i] & PlayByPlay$AwayPlay==1)
HwherePmake = which(PlayByPlay$HomeTeam==uniqueTeams[i] & PlayByPlay$HomePlay==1)

AwherePmakeO = which(PlayByPlay$AwayTeam==uniqueTeams[i] & PlayByPlay$AwayPlay==0)
HwherePmakeO = which(PlayByPlay$HomeTeam==uniqueTeams[i] & PlayByPlay$HomePlay==0)

anyWhere = append(AwherePmake,HwherePmake)

etmP = PlayByPlay$ETM[AwherePmake]#/PlayByPlay$OptimalWP[AwherePmake]
etmP = append(etmP,PlayByPlay$ETM[HwherePmake])#/PlayByPlay$OptimalWP[HwherePmake])

detmP = PlayByPlay$DETM[AwherePmake]
detmP = append(detmP,PlayByPlay$DETM[HwherePmake])

totalOPlays = length(which(PlayByPlay$PlayType[anyWhere]!=5))
totalOPlaysO = length(which(PlayByPlay$PlayType[append(AwherePmakeO,HwherePmakeO)]!=5))
totalTO = length(which(PlayByPlay$PlayType[anyWhere]==0))/totalGames
totalTOO = length(which(PlayByPlay$PlayType[append(AwherePmakeO,HwherePmakeO)]==0))/totalGames

totalFPlays = length(PlayByPlay$PlayType[anyWhere]) - totalOPlays
totalFPlaysO = length((PlayByPlay$PlayType[append(AwherePmakeO,HwherePmakeO)])) - totalOPlaysO

tF = totalFPlays/totalGames
tFO = totalFPlaysO/totalGames


FF = append(FF,tFO - tF)

TO = append(TO,totalTOO-totalTO)

totalDPlays = length(etmP)-totalOPlays

totalPlays = append(totalPlays,length(etmP))

correct = length(which(PlayByPlay$ActualDecisionO[anyWhere]==PlayByPlay$OptimalDecisionO[anyWhere] & PlayByPlay$PlayType[anyWhere]!=5))/totalOPlays

correctOplay = append(correctOplay,correct)

score = PlayByPlay$ScoreDifference[anyWhere]
meanScore = append(meanScore,mean(score))

make = PlayByPlay$Make[append(AwherePmake,HwherePmake)]
makePer = append(makePer,sum(make)/totalOPlays)

makeO = PlayByPlay$Make[append(AwherePmakeO,HwherePmakeO)]
makePerO = append(makePerO,sum(makeO)/totalOPlaysO)

MM = append(MM,sum(make)/totalOPlays-sum(makeO)/totalOPlaysO)


etmPTotal = append(etmPTotal,sum(etmP))

etmPRate = sum(etmP)/totalOPlays
detmPRate = sum(detmP)/totalDPlays

bothPRate = (sum(etmP)+sum(detmP))/(length(etmP))

etmPlayRate = append(etmPlayRate,etmPRate)
detmPlayRate = append(detmPlayRate,detmPRate)
bothPlayRate = append(bothPlayRate,bothPRate)

}

#plot(WPstart,etm,type='n')
#points(WPstart[(GameByGame$Win[append(Awhere,Hwhere)]==-1)],etm[(GameByGame$Win[append(Awhere,Hwhere)]==-1)],col="red")
#points(WPstart[(GameByGame$Win[append(Awhere,Hwhere)]==0)],etm[(GameByGame$Win[append(Awhere,Hwhere)]==0)],col="black")
#points(WPstart[(GameByGame$Win[append(Awhere,Hwhere)]==1)],etm[(GameByGame$Win[append(Awhere,Hwhere)]==1)],col="green")
#legend("topleft", c('Loss','Tie','Win'),col=c("red","black","green"),pch=c(1,1))

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
### MUST DISCOUNT ETM WHEN ACTION IS 5 ###
### MUST DISCOUNT dETM WHEN ACTION IS NOT 5 ###