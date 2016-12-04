rm(list=ls())

# Relevant libraries
library(glmnet)
library(data.table)
library(R.matlab)

# Reading in play-by-play data. Numeric suffix of file name indicates NBA season, i.e. '0506' is the 2005-2006 NBA season
season1 = readMat('situation0506.mat')
season2 = readMat('situation0607.mat')
season3 = readMat('situation0708.mat')
season4 = readMat('situation0809.mat')

# Newest situation is the cleaned and manipulated versoin of the play-by-play data this analysis uses. Games w/ OT periods are removed. The 7 columns are:
# Column 1: Away team (1 if away team is acting on this play, 0 OW)
# Column 2: Home team (1 if home team is acting on this play, 0 OW)
# Column 3: Start time of posession in seconds remaining
# Column 4: End time of possession in seconds remaining
# Column 5: Score difference relative to the team with possession
# Column 6: Vegas point spread relative to the team with possession
# Column 7: Win (1 if team with possession won, 0 if they lost)
season1 = season1$newest.situation
season2 = season2$newest.situation
season3 = season3$newest.situation
season4 = season4$newest.situation

# Create training data frame
training = rbind(season1,season2)
training = data.frame(training)
names(training) = c('Away','Home','StartTime','EndTime','ScoreDiff','Line','Win')

# Create validation data frame
validation = data.frame(season3)
names(validation) = c('Away','Home','StartTime','EndTime','ScoreDiff','Line','Win')

# Create test data frame
test = data.frame(season4)
names(test) = c('Away','Home','StartTime','EndTime','ScoreDiff','Line','Win')

# Select plays from each data frame that occur within the last 3 minutes of each game. 
training = training[which(training$StartTime<=180),]
validation = validation[which(validation$StartTime<=180),]
test = test[which(test$StartTime<=180),]

# 'ex' is a sequence of exponents for a term in the win probability model. The best exponent is found using the validation set. 
ex = seq(0.01,1,0.01)

R = numeric(length=length(ex)) # Stores the log-likelihood of each validation model

TP = numeric(length=length(ex)) # Stores the true positives of the classifier

# This loop performs model validation to find the exponent of the first term in the classifier. Previous feature engineering and validation efforts went into the decision to use the following three features in the model. 

for (i in 1:length(ex)){

	training$var1 = (1/(training$StartTime/2880 + 0.0001))^(ex[i])*training$ScoreDiff # First feature, function of possession start time and score difference. Small value added to denominator of term to avoid division by 0
	training$var2 = exp(-training$StartTime/2880) # Second feature, function of possession start time

	# Replicating features for validation set
	validation$var1 = (1/(validation$StartTime/2880 + 0.0001))^(ex[i])*validation$ScoreDiff
	validation$var2 = exp(-validation$StartTime/2880)
	
	# Build logistic regression model
	model = glm(Win ~ var1 + 
		var2 + Line,family='binomial',data=training)  
	
	# Predict win probabilities in the validation set
	eval = predict(model,validation,type='response')
	
	# Convert probabilities into binary classification
	type = numeric(length=length(eval))
	type[which(eval>=0.5)] = 1
	type[which(eval<0.5)] = 0
	
	# Get indices of plays from winning and losing teams
	inder = which(validation$Win==1)
	inder_0 = which(validation$Win==0)
	
	# Create vector to hold win probabilites
	L = numeric(length=length(eval))

	L[inder] = eval[inder]
	L[inder_0] = 1 - eval[inder_0]
	
	# Overall accuracy of model
	accuracy =  (length(eval) - sum(abs(type-validation$Win)))/length(eval)
	
	# Confusion matrix and validation statistics
	conf_m = table(predicted = type, actual = validation$Win)
	precision = conf_m[2,2]/sum(conf_m[2,])
	recall = conf_m[2,2]/sum(conf_m[,2])
	
	# Store the true positive count and the sum of the log-likelihood of validation set
	TP[i] = conf_m[2,2]
	R[i] = sum(log(L))
	
	# Printing summary statistics
	print(conf_m)
	print(paste('Precision: ',precision,sep=''))
	print(paste('Recall: ',recall,sep=''))	
	print(paste('Accuracy: ',accuracy,sep=''))
	print(sum(log(L)))
	print(ex[i])
}

# Picking the first feature exponent that maximizes the log-likelihood
expToUse = ex[which(R==max(R))] # This value is 0.32 if original training and validation sets are used

# Resetting training features
training$var1 = (1/(training$StartTime/2880 + 0.0001))^(expToUse)*training$ScoreDiff
training$var2 = exp(-training$StartTime/2880)

# Build final model
model = glm(Win ~ var1 + 
		var2 + Line,family='binomial',data=training) 

# Creating features for test set
test$var1 = (1/(test$StartTime/2880 + 0.0001))^(expToUse)*test$ScoreDiff
test$var2 = exp(-test$StartTime/2880)

# Predict test set win probabilities 
testing = predict(model,test,type='response')

# Convert to binary classification 
class = numeric(length=length(testing))
class[which(testing>=0.5)] = 1
class[which(testing<0.5)] = 0

# Create test summary statistics
confusion_matrix = table(predicted = class,actual = test$Win)
precision = confusion_matrix[2,2]/sum(confusion_matrix[2,])
recall = confusion_matrix[2,2]/sum(confusion_matrix[,2])

# Below creates win probability values to create a summary illustration of the win probability model 

t = seq(0,180,0.1)
s = seq(-5,5,1)
a = 0
for (i in 1:length(s)){
	a = a + 1
	df = data.frame(StartTime = t, ScoreDiff = rep(s[i],length(t)), 
		Line = rep(0,length(t)))
	df$var1 = (1/(df$StartTime/2880 + 0.0001))^(expToUse)*df$ScoreDiff
	df$var2 = exp(-df$StartTime/2880)
	assign(paste('score',a,sep=''),predict(model,df,type='response'))
}

plot(0,0,type='n',xlim=c(0,200),ylim=c(0,1),xlab='Game Time Remaining (s)',ylab='Win Probability')
lines(t,score1,col='black')
text(190,score1[length(t)],'S = -5')
lines(t,score2,col='black')
text(190,score2[length(t)],'S = -4')
lines(t,score3,col='black')
text(190,score3[length(t)],'S = -3')
lines(t,score4,col='black')
text(190,score4[length(t)],'S = -2')
lines(t,score5,col='black')
text(190,score5[length(t)],'S = -1')
lines(t,score6,col='black')
text(190,score6[length(t)],'S = 0')
lines(t,score7,col='black')
text(190,score7[length(t)],'S = 1')
lines(t,score8,col='black')
text(190,score8[length(t)],'S = 2')
lines(t,score9,col='black')
text(190,score9[length(t)],'S = 3')
lines(t,score10,col='black')
text(190,score10[length(t)],'S = 4')
lines(t,score11,col='black')
text(190,score11[length(t)],'S = 5')