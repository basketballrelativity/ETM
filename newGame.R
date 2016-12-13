rm(list=ls())

# Relevant libraries

library(glmnet)
library(data.table)
library(R.matlab)

# newPBP_1.csv is the 2015-2016 NBA play-by-play data with win probability and ETM values added. The 20 columnds are:
# Column 1: GameIndex, unique number for each game
# Column 2: Away team, unique three letter designator
# Column 3: Home team, unique three letter designator
# Column 4: AwayPlay, 1 if away team commits action, 0 OW
# Column 5: HomePlay, 1 if home team commits action, 0 OW
# Column 6: PlayStartTime, time remaining in seconds
# Column 7: PlayEndTime, time remaining in seconds
# Column 8: Make, 1 if shot is made, 0 OW
# Column 9: PlayType, 0 is turnover, 2 is two-point FG, 3 is three-point field goal, 5 is foul
# Column 10: ScoreDifference, relative to team commiting the action
# Column 11: ActualDecision, 2 is two-point FG, 3 is three-point FG. If turnover or foul, actual decision is assumed to be the more optimal of a two-point or three-point FG
# Column 12: OptimalDecision, 2 is two-point FG, 3 is three-point FG
# Column 13: OptimalWP, win probability of optimal decision executed at the optimal time. 
# Column 14: ActualWP, win probability of actual decision executed at the actual time.
# Column 15: ETM, difference between OptimalWP and ActualWP
# Column 16: ActualDecisionD, 0 is no-foul, 5 is foul
# Column 17: OptimalDecisionD, 0 is no-foul, 5 is foul
# Column 18: OptimalWPD, win probability of team with possession after optimal defensive decision executed at optimal time
# Column 19: ActualWPD, win probability of team with possession after actual defensive decision executed at actual time (ONLY VALID IF ACTUAL DECISION OR OPTIMAL DECISION IS A FOUL)
# Column 20: DETM, ActualWPD minus OptimalWPD (ONLY VALID IF OPTIMAL DECISION IS A FOUL)
summary1516 = fread('newPBP_1.csv',data.table=F)

uniqueIndex = unique(summary1516$GameIndex) # Gets the number of periods in the dataset

# Initializing variables
GameIndex = numeric(length=length(uniqueIndex))
AwayTeam = numeric(length=length(uniqueIndex))
HomeTeam = numeric(length=length(uniqueIndex))
Win = numeric(length=length(uniqueIndex))
InitPoss = numeric(length=length(uniqueIndex))
AwayETM = numeric(length=length(uniqueIndex))
HomeETM = numeric(length=length(uniqueIndex))
AwayDETM = numeric(length=length(uniqueIndex))
HomeDETM = numeric(length=length(uniqueIndex))
AwayWPstart = numeric(length=length(uniqueIndex))
HomeWPstart = numeric(length=length(uniqueIndex))
AwayWPend = numeric(length=length(uniqueIndex))
HomeWPend = numeric(length=length(uniqueIndex))
AwayCorrectO = numeric(length=length(uniqueIndex))
HomeCorrectO = numeric(length=length(uniqueIndex))
AwayCorrectD = numeric(length=length(uniqueIndex))
HomeCorrectD = numeric(length=length(uniqueIndex))
AwayPoss = numeric(length=length(uniqueIndex))
HomePoss = numeric(length=length(uniqueIndex))

for (i in 1:length(uniqueIndex)) {
	print(i)
	GameIndex[i] = i 
	ind = which(summary1516$GameIndex==uniqueIndex[i]) # Grabs indices of plays in period i
	awayInd = ind[which(summary1516$AwayPlay[ind]==1 & summary1516$PlayType[ind]!=5)] # Away offensive possessions that do not involve fouls
	homeInd = ind[which(summary1516$HomePlay[ind]==1 & summary1516$PlayType[ind]!=5)] # Home offensive possesions that do not involve fouls
	lastInd = ind[length(ind)] # Last play of period i
	AwayTeam[i] = summary1516$AwayTeam[ind[1]] 
	HomeTeam[i] = summary1516$HomeTeam[ind[1]]
	AwayETM[i] = sum(summary1516$ETM[awayInd]) # Total away ETM of offensive plays for period i
	HomeETM[i] = sum(summary1516$ETM[homeInd]) # Total away ETM of offensive plays for period i
	awayIndD = ind[which(summary1516$AwayPlay[ind]==1 & summary1516$OptimalDecisionD[ind]==5)] # Away defensive possessions that involve fouls
	homeIndD = ind[which(summary1516$HomePlay[ind]==1 & summary1516$OptimalDecisionD[ind]==5)] # Home defensive possessions that involve fouls
	AwayDETM[i] = sum(summary1516$DETM[awayInd]) # Total away dETM for period i
	HomeDETM[i] = sum(summary1516$DETM[homeInd]) # Total home dETM for period i
	indB = ind[min(which(summary1516$PlayType[ind]!=5))] # First possession of period i that ends without a foul
	
	# Initial possession and win probability for first play without foul
	if (summary1516$AwayPlay[ind[1]]==1){
		if (summary1516$PlayType[ind[1]]!=5){
			prob = summary1516$ActualWP[ind[1]]
			InitPoss[i] = 0
		} else {
			prob = 1 - summary1516$ActualWP[ind[1]]
			InitPoss[i] = 1
		}
		# Away possession
		AwayWPstart[i] = prob
		HomeWPstart[i] = 1 - prob
	} else {
		if (summary1516$PlayType[ind[1]]!=5){
			prob = 1 - summary1516$ActualWP[ind[1]]
			InitPoss[i] = 1
		} else {
			prob = summary1516$ActualWP[ind[1]]
			InitPoss[i] = 0
		}
		# Home possession
		AwayWPstart[i] = prob
		HomeWPstart[i] = 1 - prob
	}
	# Final win probability
	if (summary1516$AwayPlay[lastInd]==1){
		if (summary1516$PlayType[lastInd]==5){
			prob = 1 - summary1516$ActualWP[lastInd]
		} else {
			prob = summary1516$ActualWP[lastInd]
		}
		AwayWPend[i] = prob
		HomeWPend[i] = 1 - prob
	} else {
		if (summary1516$PlayType[lastInd]==5){
			prob = summary1516$ActualWP[lastInd]
		} else {
			prob = 1 - summary1516$ActualWP[lastInd]
		}
		AwayWPend[i] = 1 - prob
		HomeWPend[i] = prob
	}
	
	AwayCorrectO[i] = length(which(summary1516$ActualDecisionO[awayInd]==summary1516$OptimalDecisionO[awayInd])) # Number of correct offensive decisions for away team
	HomeCorrectO[i] = length(which(summary1516$ActualDecisionO[homeInd]==summary1516$OptimalDecisionO[homeInd])) # Number of correct offensive decisions for home team
	
	AwayCorrectD[i] = length(which(summary1516$ActualDecisionD[awayIndD]==summary1516$OptimalDecisionD[awayIndD] & summary1516$OptimalDecisionD[awayIndD]==5)) # Number of correct defensive decisions for away team
	HomeCorrectD[i] = length(which(summary1516$ActualDecisionD[homeIndD]==summary1516$OptimalDecisionD[homeIndD] & summary1516$OptimalDecisionD[homeIndD]==5)) # Number of correct offensive decisions for home team
	
	AwayPoss[i] = length(awayInd) # Away offensive possessions w/o foul
	HomePoss[i] = length(homeInd) # Home offensive possessions w/o foul
	
	# Assigning winner of period. -1 is away team, 0 is tie, 1 is home team
	if (summary1516$ScoreDifference[lastInd]==0 & summary1516$Make[lastInd]==0){
		Win[i] = 0
	} else if (summary1516$ScoreDifference[lastInd]<0 & summary1516$Make[lastInd]==0) {
		if (summary1516$AwayPlay[lastInd]==1) {
			Win[i] = 1
		} else {
			Win[i] = -1
		}
	} else if (summary1516$ScoreDifference[lastInd]>0 & summary1516$Make[lastInd]==0) {
		if (summary1516$AwayPlay[lastInd]==1) {
			Win[i] = -1
		} else {
			Win[i] = 1
		}

	} else if (summary1516$ScoreDifference[lastInd]<0 & summary1516$Make[lastInd]==1) {
		if (summary1516$AwayPlay[lastInd]==1) {
			if (summary1516$PlayType[lastInd]==abs(summary1516$ScoreDifference[lastInd])) {
				Win[i] == 0
			} else if (summary1516$PlayType[lastInd]>abs(summary1516$ScoreDifference[lastInd])) {
				Win[i] = -1
			} else {
				Win[i] = 1
			}
		} else {
			if (summary1516$PlayType[lastInd]==abs(summary1516$ScoreDifference[lastInd])) {
				Win[i] == 0
			} else if (summary1516$PlayType[lastInd]>abs(summary1516$ScoreDifference[lastInd])) {
				Win[i] = 1
			} else {
				Win[i] = -1
			}
		}
	}
}
# Organizing data frame for export
newGBG = data.frame(GameIndex,AwayTeam,HomeTeam,Win,InitPoss,AwayETM,HomeETM,AwayDETM,HomeDETM,AwayWPstart,HomeWPstart,AwayWPend,HomeWPend,AwayCorrectO,HomeCorrectO,AwayCorrectD,HomeCorrectD,AwayPoss,HomePoss)
write.table(newGBG,'newGBG_1.csv',sep=',',row.names=F,col.names=T)