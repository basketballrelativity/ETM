rm(list=ls())

# Relevant libraries
library(glmnet)
library(data.table)
library(R.matlab)

load('nbawp.RData') # Win probablity logistic regression model

summary1516 = fread('PlayByPlay3.csv',data.table=F) # 2015-2016 NBA play-by-play data
stats = fread('leagues_NBA_2016_team.csv',data.table=F) # 2015-2016 NBA team statistics
shooting = readMat('shootingcoeff.mat') # Coefficients of shooting percentage weights
shooting = shooting$b

# keepRel is the main data frame for this script. The 10 columns are:
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
keepRel = summary1516[,1:10]

# Creating placeholders for relevant variables
ActualDecision = numeric(length=nrow(keepRel))
OptimalDecision = numeric(length=nrow(keepRel))
OptimalWP = numeric(length=nrow(keepRel))
ActualWP = numeric(length=nrow(keepRel))
ETM_col = numeric(length=nrow(keepRel))
ActualDecisionD = numeric(length=nrow(keepRel))
OptimalDecisionD = numeric(length=nrow(keepRel))
OptimalWPD = numeric(length=nrow(keepRel))
ActualWPD = numeric(length=nrow(keepRel))
DETM_col = numeric(length=nrow(keepRel))
foulWP = numeric(length=nrow(keepRel))
no_foulWP = numeric(length=nrow(keepRel))

# This loop goes through each play in close games of the 2015-2016 NBA season and computes the optimal win probability for a team after the best choice of tactic given the score, time remaining, and possession. This is then compared with the actual win probability after a team's choice of tactic to find ETM. 
for (i in 1:nrow(keepRel)){
	print(i)
	# Timing bounds of a possession. 
	beginning_time = keepRel$PlayStartTime[i] 
	end_time = beginning_time - 24
	if (end_time<0){
		end_time = 0
	}
	# This ensures the beginning time is greater than the end time
	if (beginning_time == 0){
		beginning_time = 0.1
	}
	# The tactics are evaluated at 1000 increments between beginning_time and ending_time
	t = seq(end_time,beginning_time,min(c(24,beginning_time))/999)
	s = keepRel$ScoreDifference[i]
	
	awayTeam = keepRel$AwayTeam[i]
	homeTeam = keepRel$HomeTeam[i]
	
	# This grabs the indices of the home and away team in the stats data.frame
	whereA = which(stats$Team==awayTeam)
	whereH = which(stats$Team==homeTeam)
	
	statsA = stats[whereA,]
	statsH = stats[whereH,]
	
	# Position indicator. 0 for away team, 1 for home team. If the action committed is a foul, position switches to the opposite team and the score is relative to the team fouled
	if (keepRel$AwayPlay[i]==1 & keepRel$PlayType[i]!=5){
		poss = 0
	} else if (keepRel$AwayPlay[i]==0 & keepRel$PlayType[i]!=5) {
		poss = 1
	} else if (keepRel$AwayPlay[i]==1 & keepRel$PlayType[i]==5) {
		poss = 1
		s = -s
	} else if (keepRel$AwayPlay[i]==0 & keepRel$PlayType[i]==5) {
		poss = 0
		s = -s
	}

	
	current = data.frame(StartTime = beginning_time, ScoreDiff = s,
		Line = 0) # Current state of the game assuming a point spread of 0
	
	# Generating model variables	
	current$var1 = (1/(current$StartTime/2880 + 0.0001))^(0.32)*current$ScoreDiff
	current$var2 = exp(-current$StartTime/2880) 
	
	WP_current = predict(model,current,type='response') # Current win probability for team with possession
	
	# Initializing win probability vectors 
	PW_f = numeric(length=length(t)) # Win probability if fouled
	PW_2 = numeric(length=length(t)) # Win probability after 2-point FG attemped
	PW_3 = numeric(length=length(t)) # Win probability after 3-point attemped
	
	# Evaluating each offensive and defensive choice for the length of the shot clock
	for (j in 1:length(t)){
		# The shooting weight adjusts the shooting percentages of a team throughout the shot clock. These weights were found by fitting a quadratic function to team shooting percentage given time remaining on shot clock. The shooting weight below for possessions beginning with 36 seconds or less is assumed to be 1. 
		shooting_weight = shooting[1] + shooting[2]*(beginning_time-t[j]) + shooting[3]*(beginning_time-t[j])^2
		if (beginning_time<=36){
			shooting_weight = 1
		}
		
		# Assigning approriate team statistics to team with possession
		if (poss==1) {
			two_point = statsH$P2 # 2-point FG percentage
			three_point = statsH$P3 # 3-point FG percentage
			dreb = 1 - statsH$ORB # Opponent's defensive rebound percentage (assumed to be the 1 - offensive rebound percentage)
			oreb = statsH$ORB # Offensive rebound percentage
			ft = statsH$FT # Free throw percentage
			to = statsH$TOV # Turnover percentage
			foul = statsA$F # Opponent's foul percentage
		} else {
			two_point = statsA$P2
			three_point = statsA$P3
			dreb = 1 - statsA$ORB
			oreb = statsA$ORB
			ft = statsA$FT
			to = statsA$TOV
			foul = statsH$F 
		}
		# Applying shooting weight 
		two_point_p = two_point*shooting_weight
		three_point_p = three_point*shooting_weight
		
		## SCENARIOS ##
		
		s_mi_o = data.frame(var1 = s*(1/(t[j]/2880 + 0.0001))^(0.32),var2 = exp(-t[j]/2880),Line = 0) # Score remains the same and team maintains possession
		s1_ma_o = data.frame(var1 = (s+1)*(1/(t[j]/2880 + 0.0001))^(0.32),var2 = exp(-t[j]/2880),Line = 0) # Score differential increases by 1 and team maintains possession
		s_mi_d = data.frame(var1 = -s*(1/(t[j]/2880 + 0.0001))^(0.32),var2 = exp(-t[j]/2880),Line = 0) # Score remains the same and team loses possession
		s1_ma_d = data.frame(var1 = -(s+1)*(1/(t[j]/2880 + 0.0001))^(0.32),var2 = exp(-t[j]/2880),Line = 0) # Score differential increases by 1 and team loses possession
		s2_ma = data.frame(var1 = -(s+2)*(1/(t[j]/2880 + 0.0001))^(0.32),var2 = exp(-t[j]/2880),Line = 0) # Score differential increases by 2 and team loses possession
		s3_ma = data.frame(var1 = -(s+3)*(1/(t[j]/2880 + 0.0001))^(0.32),var2 = exp(-t[j]/2880),Line = 0) # Score differential increases by 3 and team loses possession
		
		# Team win probablity after the above scenarios occur
		w_mi_o = predict(model,s_mi_o,type='response')
		w1_ma_o = predict(model,s1_ma_o,type='response')
		w_mi_d = predict(model,s_mi_d,type='response')
		w1_ma_d = predict(model,s1_ma_d,type='response')
		w2_ma = predict(model,s2_ma,type='response')
		w3_ma = predict(model,s3_ma,type='response')
		
		# Chapman-Kolmogorov for each decision. Possibilities of and-1 fouls and getting fouled on a three-point attempt are ignored in this analysis. 
		
		PW_f[j] = ((ft^2)*(1-w2_ma) + (1-ft)*ft*(1-w1_ma_d) + dreb*(1-ft)^2 * (1 - w_mi_d) + dreb*(1-ft)*ft*(1 - w1_ma_d) + oreb*(1-ft)*ft*(w1_ma_o) + oreb*(1-ft)^2 * (w_mi_o)) # Win probability after defensive team fouls at time t[j]
		
		PW_2[j] = (1-to)*((1-foul)*(two_point_p*(1-w2_ma) + dreb*(1-two_point_p)*(1-w_mi_d) + oreb*(1-two_point_p)*(w_mi_o)) + foul*(((ft^2)*(1-w2_ma) + (1-ft)*ft*(1-w1_ma_d) + dreb*(1-ft)^2 * (1-w_mi_d) + dreb*(1-ft)*ft*(1-w1_ma_d) + oreb*(1-ft)*ft*(w1_ma_o) + oreb*(1-ft)^2 * (w_mi_o)))) + to*(1 - w_mi_d) # Win probability if offensive team shoots a 2-point field goal at time t[j]
		
		PW_3[j] = (1-to)*(three_point_p*(1-w3_ma) + dreb*(1-three_point_p)*(1-w_mi_d) + oreb*(1-three_point_p)*(w_mi_o)) + to*(1-w_mi_d) # Win probability if offensive team shoots a 3-point field goal at time t[j]
	}
	max_c = 1 - w_mi_d # Initial possession win probability
	max_c = append(max_c,max(PW_2)) # Appending best 2-point FG WP
	max_c = append(max_c,max(PW_3)) # Appending best 3-point FG WP
	max_d = PW_f[1000] # WP if defensive team intentionally fouls. This assumes that defensive team fouls immediately.
	
	choice_o = which(max_c==max(max_c))[1] # This outputs a 2 if a 2-point fields goal has a higher win probability than shooting a 3-point field goal. This outpus a 3 if the opposite is true
	
	# Finds optimal choice of the defense, If the win probability of fouling is less than the team shooting a shot, fouling is optimal. If a team is up by three and the win probability of fouling is less than the offsenive team shooting a 3-point field goal, fouling is optimal. 
	if (max_d<max_c[2] & max_d<max_c[3]) {
		choice_d = 5
	} else if (max_d<max_c[3] && s == -3) {
		choice_d = 5
	} else {
		choice_d = 0
	}
	
	optimal_decision_prob_offense = max_c[choice_o] # Win probability of the optimal offensive decision
	ADT = keepRel$PlayEndTime[i] # Actual decision time
	actual_decision = keepRel$PlayType[i] # Actual decision
	
	# If a team is fouled or the ball is turned over, the model assumes a team was going to choose the optimal field-goal.
	if (actual_decision!=2 & actual_decision!=3){
		actual_decision = choice_o #
	}
	
	actual_decision_d = keepRel$PlayType[i] # Actual defensive decision
	
	if (actual_decision_d==2 | actual_decision_d==3 | actual_decision_d==0){
		actual_decision_d = 0 # Did not foul
	} else {
		actual_decision_d = 5 # Did foul
	}
	if (choice_d==actual_decision_d){
		if (actual_decision_d==5){
			optimal_decision_prob_defense = max_d # If team fouled and it was the optimal decision, the optimal offensive WP is max_d
		} else {
			optimal_decision_prob_defense = optimal_decision_prob_offense # If team did not foul and this was the optimal decision, then the optimal offensive WP is optimal_decision_prob_offense
		}
	} else {
		if (actual_decision_d==5){
			optimal_decision_prob_defense = optimal_decision_prob_offense # If team fouled and it was not optimal, the optimal offensive WP is optimal_decision_prob_offense
		} else {
			optimal_decision_prob_defense = max_d # If team did not foul and it was optimal, the optimal offensive WP is max_d

		}
	}
	
	### EVALUATE ACTUAL WP UPDATES ###
	
	# Shooting weight at actual decision time
	shooting_weight = shooting[1] + shooting[2]*(beginning_time-ADT) + shooting[3]*(beginning_time-ADT)^2
	if (beginning_time<=36){
		shooting_weight = 1
	}
	
	### SCENARIOS AT ACTUAL DECISION TIME ###
	
	s_mi_o = data.frame(var1 = s*(1/(ADT/2880 + 0.0001))^(0.32),var2 = exp(-ADT/2880),Line = 0) # Score differential remains the same and team maintains possession
	s1_ma_o = data.frame(var1 = (s+1)*(1/(ADT/2880 + 0.0001))^(0.32),var2 = exp(-ADT/2880),Line = 0) # Score differential increases by 1 and team maintains possession
	s_mi_d = data.frame(var1 = -s*(1/(ADT/2880 + 0.0001))^(0.32),var2 = exp(-ADT/2880),Line = 0) # Score differential remains the same and team loses possession
	s1_ma_d = data.frame(var1 = -(s+1)*(1/(ADT/2880 + 0.0001))^(0.32),var2 = exp(-ADT/2880),Line = 0) # Score differential increases by 1 and team loses possession
	s2_ma = data.frame(var1 = -(s+2)*(1/(ADT/2880 + 0.0001))^(0.32),var2 = exp(-ADT/2880),Line = 0)# Score differential increases by 2 and team loses possession
	s3_ma = data.frame(var1 = -(s+3)*(1/(ADT/2880 + 0.0001))^(0.32),var2 = exp(-ADT/2880),Line = 0)# Score differential increases by 3 and team loses possession
	
	# Team win probablity after the above scenarios occur
	w_mi_o = predict(model,s_mi_o,type='response')
	w1_ma_o = predict(model,s1_ma_o,type='response')
	w_mi_d = predict(model,s_mi_d,type='response')
	w1_ma_d = predict(model,s1_ma_d,type='response')
	w2_ma = predict(model,s2_ma,type='response')
	w3_ma = predict(model,s3_ma,type='response')
		
	PW_f_actual = ((ft^2)*(1-w2_ma) + (1-ft)*ft*(1-w1_ma_d) + dreb*(1-ft)^2 * (1 - w_mi_d) + dreb*(1-ft)*ft*(1 - w1_ma_d) + oreb*(1-ft)*ft*(w1_ma_o) + oreb*(1-ft)^2 * (w_mi_o))
		
	PW_2_actual = (1-to)*((1-foul)*(two_point_p*(1-w2_ma) + dreb*(1-two_point_p)*(1-w_mi_d) + oreb*(1-two_point_p)*(w_mi_o)) + foul*(((ft^2)*(1-w2_ma) + (1-ft)*ft*(1-w1_ma_d) + dreb*(1-ft)^2 * (1-w_mi_d) + dreb*(1-ft)*ft*(1-w1_ma_d) + oreb*(1-ft)*ft*(w1_ma_o) + oreb*(1-ft)^2 * (w_mi_o)))) + to*(1 - w_mi_d)
		
	PW_3_actual = (1-to)*(three_point_p*(1-w3_ma) + dreb*(1-three_point_p)*(1-w_mi_d) + oreb*(1-three_point_p)*(w_mi_o)) + to*(1-w_mi_d)	
	
	# Vector of actual decisions
	max_c_actual = PW_f_actual
	max_c_actual = append(max_c_actual,PW_2_actual)
	max_c_actual = append(max_c_actual,PW_3_actual)	
	
	# ETM is the difference between optimal and actual win probabilities
	ETM = optimal_decision_prob_offense - max_c_actual[actual_decision]
	
	if (choice_d == 0 & actual_decision_d == 0){
		# If team does not foul and fouling is not optimal, defensive ETM is set to 0
		dETM = 0 
		D_choice = 0
	} else if (choice_d == 5 & actual_decision_d == 5) {
		# If team fouls and that is optimal, dETM is the difference between WP of fouling at optimal time and actual time
		dETM = abs(max_c_actual[1] - optimal_decision_prob_defense)
		D_choice = max_c_actual[1] # WP after fouling at ADT
	} else if (choice_d ==5 & actual_decision_d==0) {
		# If a team does not foul but fouling is optimal, dETM is the differnce between the optimal offensive WP and the win probability of fouling.
		dETM = abs(max_c[choice_o] - optimal_decision_prob_defense)
		D_choice = max_c[choice_o]
	} else if (choice_d == 0 & actual_decision_d == 5) {
		# If fouling is not optimal, dETM is set to 0
		dETM = 0
		D_choice = 0
	}
	
	# This stores the interesting variables for analysis or saving. 
	
	ActualDecision[i] = actual_decision
	OptimalDecision[i] = choice_o
	OptimalWP[i] = optimal_decision_prob_offense
	ActualWP[i] = max_c_actual[actual_decision]
	if (actual_decision_d!=5){
		ETM_col[i] = ETM
	} else {
		ETM_col[i] = 0 # ETM set to zero if defensive team fouls
	}
	ActualDecisionD[i] = actual_decision_d
	OptimalDecisionD[i] = choice_d
	OptimalWPD[i] = optimal_decision_prob_defense
	ActualWPD[i] = D_choice # D_choice is only a valid probability if the optimal decision is for the defense to foul
	foulWP[i] = max_d
	no_foulWP[i] = max_c[choice_o]
	DETM_col[i] = dETM
}