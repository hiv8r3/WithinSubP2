library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(export)


# 1. Preprocess ERP fix data ----------------------------------------------

ERPfix = read.delim("./1 ERP-fix/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2.txt")

# Dummy code
ERPfix$Race.d = 0
ERPfix$Race.d[ERPfix$Race == "White"] = 1

ERPfix$Fix.d = 0
ERPfix$Fix.d[ERPfix$Fix == "forehead"] = 1


# Calculate intercepts and slopes -----------------------------------------

# fit model to plot simple slopes
plot2.d = lmer(RT ~ P2.meancent.scale*Race.d*Fix.d + (1|Subject), dat = ERPfix)

parms <- fixef(plot2.d) # fixed parameters from model
vcmat <- vcov(plot2.d) # variance/covariance matrix of fixed effects

# First calculate slopes

## each row of this matrix defines entries of parms that we want to combine:
STmat <- matrix(NA,4,8)
STmat[1,] <- c(0,1,rep(0,2),0,0,0,0) # slope for Race = 0, Fix = 0
STmat[2,] <- c(0,1,rep(0,2),0,1,0,0) # slope for Race = 0, Fix = 1
STmat[3,] <- c(0,1,rep(0,2),1,0,0,0) # slope for Race = 1, Fix = 0
STmat[4,] <- c(0,1,rep(0,2),1,1,0,1) # slope for Race = 1, Fix = 1

Sparest <- STmat %*% parms          # see above for notation
newvc <- STmat %*% vcmat %*% t(STmat)
ses <- sqrt(diag(newvc))

slopes = cbind(Sparest, ses) %>% as.data.frame()
names(slopes) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE
slopes$lbnd = slopes$est - 2*slopes$ses
slopes$ubnd = slopes$est + 2*slopes$ses

# Next calculate intercepts

## each row of this matrix defines entries of parms that we want to combine:
ITmat <- matrix(NA,4,8)
ITmat[1,] <- c(1,0,0,0,0,0,0,0) # intercept for Race = 0, Fix = 0
ITmat[2,] <- c(1,0,0,1,0,0,0,0) # intercept for Race = 0, Fix = 1
ITmat[3,] <- c(1,0,1,0,0,0,0,0) # intercept for Race = 1, Fix = 0
ITmat[4,] <- c(1,0,1,1,0,0,1,0) # intercept for Race = 1, Fix = 1

Iparest <- ITmat %*% parms          # see above for notation

ERPfix.plotting = cbind(Iparest, slopes[c(1,3:4)]) %>% as.data.frame()
names(ERPfix.plotting) = c("Intercept", "Slope", "lower_95CI", "upper_95CI")

# label for understanding
ERPfix.plotting$Target_Race = c(rep("Black",2), rep("White",2))
ERPfix.plotting$Target_Gender = "male"
ERPfix.plotting$Fix = rep(c("eyes", "forehead"),2)
ERPfix.plotting$Task = "RaceTask"
ERPfix.plotting$Study = "Study1"

ERPfix.plotting$Condition = paste(ERPfix.plotting$Target_Race, ERPfix.plotting$Fix, sep="_")

# Zoom in, add SEs

# Calculate SEs -----------------------------------------------------------

# since P2 amp is continuous, don't have discrete values like Trial (in N170 and ERN examples)
# Try with 9 levels of P2 amp (-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)

SETmat <- matrix(NA,36,8) # 36 rows because 9 levels of x variable x 4 conditions, 8 columns because 8 levels of predictors in vcmat
# fill columns with appropriate weights (columns correspond to order model is specified, i.e., order of predictors in output of model)

P2levels = c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)
# weights for intercept
SETmat[,1] = 1 
# weights for P2.meancent.scale
SETmat[,2] = rep(P2levels, 4) # 9 levels of continuous variable of P2.meancent.scale, repeated 4 times because 4 conditions
# weights for Race main effect
SETmat[,3] = rep(c(0, 0, 1, 1), each = 9) # race = 0 for first two conditions, race = 1 for second two conditions
# weights for Fix main effect
SETmat[,4] = rep(c(0, 1, 0, 1), each = 9) # fix = 0 for first and third conditions, fix = 0 for second and fourth conditions
# weights for P2 x Race condition
SETmat[,5] = c(rep(c(0, 0), each = 9), P2levels, P2levels) 
# weights for P2 x Fix condition
SETmat[,6] = c(rep(0,9), P2levels, rep(0,9), P2levels) 
# weights for Race x Fix condition
SETmat[,7] = rep(c(0, 0, 0, 1), each = 9) 
# weights for 3 way interaction
SETmat[,8] = c(rep(c(0, 0, 0), each = 9), P2levels)

newvc <- SETmat %*% vcmat %*% t(SETmat)
ses <- sqrt(diag(newvc)) # long string of SEs for each level of P2 amp, first 9 are for Black-eyes, second 9 are for Black-forehead, third 9 are for White-eyes, fourth 9 are for White-forehead


ERPfix.SE = data.frame(Race = rep(c("Black", "White"), each=18), 
                       Gender = "male",
                       Fix = rep(c("eyes", "forehead"), each = 9),
                       Task = "RaceTask",
                       Study = "Study1",
                       P2.meancent.scale = rep(c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1), 4),
                       SE = ses)
ERPfix.SE$Condition = paste(SE.dat$Race, SE.dat$Fix, sep="_")


# Combine SEs and slopes/intercepts ---------------------------------------

ERPfix.SE$Point = NA
for (c in c("Black_eyes", "Black_forehead", "White_eyes", "White_forehead")) {
  forPlottingtemp = ERPfix.plotting[ERPfix.plotting$Condition == c,]
  ERPfix.SE$Point[ERPfix.SE$Condition == c] = 
    forPlottingtemp$Intercept + P2levels*forPlottingtemp$Slope
}

# calculate upper and lower bound (+/- 1 SE)
ERPfix.SE$upper = ERPfix.SE$Point + ERPfix.SE$SE
ERPfix.SE$lower = ERPfix.SE$Point - ERPfix.SE$SE


# 2. Preprocess RaceGen data ----------------------------------------------

RaceGen = read.delim("./2 Race-Gen/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2.txt")

# A. Race Task ---------------------------------------------------------------
RG_race = filter(RaceGen, Task == "Race")

RG_race$TarRace.d = 0
RG_race$TarRace.d[RG_race$TarRace == "White"] = 1

RG_race$TarGender.d = 0
RG_race$TarGender.d[RG_race$TarGender == "male"] = 1

RG_race$Fix.d = 0
RG_race$Fix.d[RG_race$Fix == "forehead"] = 1

RG_race$ParRace.d = 0
RG_race$ParRace.d[RG_race$ParRace == "White"] = 1


# Calculate intercepts and slopes -----------------------------------------

plot2.d = lmer(RT ~ P2.meancent.scale*TarRace.d*TarGender.d*Fix.d + (1|Subject), data = RG_race)

parms <- fixef(plot2.d) # fixed parameters from model
vcmat <- vcov(plot2.d) # variance/covariance matrix of fixed effects

# First calculate slopes

## each row of this matrix defines entries of parms that we want to combine:
STmat <- matrix(NA,8,16)
STmat[1,] <- c(0,1,rep(0,3),0,0,0,0,0,0,0,0,0,0,0) # slope for tarRace = 0, tarGender = 0, Fix = 0
STmat[2,] <- c(0,1,rep(0,3),0,0,0,1,0,0,0,0,0,0,0) # slope for tarRace = 0, tarGender = 0, Fix = 1
STmat[3,] <- c(0,1,rep(0,3),0,1,0,0,0,0,0,0,0,0,0) # slope for tarRace = 0, tarGender = 1, Fix = 0
STmat[4,] <- c(0,1,rep(0,3),0,1,0,1,0,0,0,0,1,0,0) # slope for tarRace = 0, tarGender = 1, Fix = 1
STmat[5,] <- c(0,1,rep(0,3),1,0,0,0,0,0,0,0,0,0,0) # slope for tarRace = 1, tarGender = 0, Fix = 0
STmat[6,] <- c(0,1,rep(0,3),1,0,0,1,0,0,0,1,0,0,0) # slope for tarRace = 1, tarGender = 0, Fix = 1
STmat[7,] <- c(0,1,rep(0,3),1,1,0,0,0,0,1,0,0,0,0) # slope for tarRace = 1, tarGender = 1, Fix = 0
STmat[8,] <- c(0,1,rep(0,3),1,1,0,1,0,0,1,1,1,0,1) # slope for tarRace = 1, tarGender = 1, Fix = 1

Sparest <- STmat %*% parms          # see above for notation
newvc <- STmat %*% vcmat %*% t(STmat)
ses <- sqrt(diag(newvc))

slopes = cbind(Sparest, ses) %>% as.data.frame()
names(slopes) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE
slopes$lbnd = slopes$est - 2*slopes$ses
slopes$ubnd = slopes$est + 2*slopes$ses

# Next calculate intercepts

## each row of this matrix defines entries of parms that we want to combine:
ITmat <- matrix(NA,8,16)
ITmat[1,] <- c(1,0,0,0,0,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 0, Fix = 0
ITmat[2,] <- c(1,0,0,0,1,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 0, Fix = 1
ITmat[3,] <- c(1,0,0,1,0,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 1, Fix = 0
ITmat[4,] <- c(1,0,0,1,1,0,0,0,0,0,1,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 1, Fix = 1
ITmat[5,] <- c(1,0,1,0,0,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 1, tarGender = 0, Fix = 0
ITmat[6,] <- c(1,0,1,0,1,0,0,0,0,1,0,rep(0,3),0,0) # intercept for tarRace = 1, tarGender = 0, Fix = 1
ITmat[7,] <- c(1,0,1,1,0,0,0,1,0,0,0,rep(0,3),0,0) # intercept for tarRace = 1, tarGender = 1, Fix = 0
ITmat[8,] <- c(1,0,1,1,1,0,0,1,0,1,1,rep(0,3),1,0) # intercept for tarRace = 1, tarGender = 1, Fix = 1

Iparest <- ITmat %*% parms          # see above for notation

RGrace.plotting = cbind(Iparest, slopes[c(1,3:4)]) %>% as.data.frame()
names(RGrace.plotting) = c("Intercept", "Slope", "lower_95CI", "upper_95CI")

# label for understanding
RGrace.plotting$Target_Race = c(rep("Black",4), rep("White",4))
RGrace.plotting$Target_Gender = rep(c(rep("female",2), rep("male",2)),2)
RGrace.plotting$Fix = rep(c("eyes", "forehead"),4)
RGrace.plotting$Task = "RaceTask"
RGrace.plotting$Study = "Study2"

RGrace.plotting$Condition = paste(RGrace.plotting$Target_Race, RGrace.plotting$Target_Gender, RGrace.plotting$Fix, sep="_")


# Calculate SEs -----------------------------------------------------------

# Zoom in, add SEs

# since P2 amp is continuous, don't have discrete values (like Trial in N170 and ERN examples)
# Try with 9 levels of P2 amp (-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)
SETmat = NULL
SETmat = matrix(NA,72,16) # 72 rows because 9 levels of x variable x 8 conditions, 16 columns because 8 levels of predictors in vcmat
# fill columns with appropriate weights (columns correspond to order model is specified, i.e., order of predictors in output of model)

P2levels = c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)

# weights for intercept
SETmat[,1] = 1 

# weights for P2.meancent.scale
SETmat[,2] = rep(P2levels, 8) # 9 levels of continuous variable of P2.meancent.scale, repeated 8 times because 4 conditions

# weights for Race main effect
SETmat[,3] = rep(c(0, 0, 0, 0, 1, 1, 1, 1), each = 9) # race = 0 for first four conditions, race = 1 for second four conditions

# weights for Gender main effect
SETmat[,4] = rep(c(0, 0, 1, 1, 0, 0, 1, 1), each = 9) 

# weights for Fix main effect
SETmat[,5] = rep(c(0, 1, 0, 1, 0, 1, 0, 1), each = 9) 

# weights for P2 x Race condition
SETmat[,6] = c(rep(c(0, 0, 0, 0), each = 9), P2levels, P2levels, P2levels, P2levels) 

# weights for P2 x Gender condition
SETmat[,7] = c(rep(0,18), P2levels, P2levels, rep(0,18), P2levels, P2levels)

# weights for Race x Gender interaction
SETmat[,8] = rep(c(0, 0, 0, 0, 0, 0, 1, 1), each = 9) 

# weights for P2 x Fix condition
SETmat[,9] = c(rep(0,9), P2levels, rep(0,9), P2levels, rep(0,9), P2levels, rep(0,9), P2levels)

# weights for Race x Fix condition
SETmat[,10] = rep(c(0, 0, 0, 0, 0, 1, 0, 1), each = 9) 

# weights for Gender x Fix condition
SETmat[,11] = rep(c(0, 0, 0, 1, 0, 0, 0, 1), each = 9)

# weights for P2 x Race x Gender
SETmat[,12] = c(rep(c(0, 0, 0, 0, 0, 0), each = 9), P2levels, P2levels)

# weights for P2 x Race x Fix
SETmat[,13] = c(rep(c(0, 0, 0, 0, 0), each = 9), P2levels, rep(0,9), P2levels)

# weights for P2 x Gender x Fix
SETmat[,14] = c(rep(c(0, 0, 0), each = 9), P2levels, rep(c(0, 0, 0), each = 9), P2levels)

# weights for Race x Gender x Fix
SETmat[,15] = rep(c(0, 0, 0, 0, 0, 0, 0, 1), each = 9)

# weights for P2 x Race x Gender x Fix
SETmat[,16] = c(rep(c(0, 0, 0, 0, 0, 0, 0), each = 9), P2levels)


plot2.d = lmer(RT ~ P2.meancent.scale*TarRace.d*TarGender.d*Fix.d + (1|Subject), data = RG_race)

vcmat <- vcov(plot2.d) # variance/covariance matrix of fixed effects

newvc <- SETmat %*% vcmat %*% t(SETmat)
ses <- sqrt(diag(newvc)) # long string of SEs for each level of P2 amp
# first 9 are for Black-female-eyes
# second 9 are for Black-female-forehead
# third 9 are for Black-male-eyes
# fourth 9 are for Black-male-forehead
# fifth 9 are for White-female-eyes
# sixth 9 are for White-female-forehead
# seventh 9 are for White-male-eyes
# eighth 9 are for White-male-forehead


RGrace.SE = data.frame(Race = c(rep("Black", 36), rep("White", each=36)), 
                       Gender = rep(c("female", "female","male", "male"), each=9, 2),
                       Fix = rep(c("eyes", "forehead"), each = 9, 4),
                       Task = "RaceTask",
                       Study = "Study2",
                       P2.meancent.scale = rep(c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1), 4),
                       SE = ses)
RGrace.SE$Condition = paste(RGrace.SE$Race, RGrace.SE$Gender, RGrace.SE$Fix, sep="_")

# Add in point data
RGrace.SE$Point = NA
for (c in unique(RGrace.SE$Condition)) {
  forPlottingtemp = RGrace.plotting[RGrace.plotting$Condition == c,]
  RGrace.SE$Point[RGrace.SE$Condition == c] = 
    forPlottingtemp$Intercept + P2levels*forPlottingtemp$Slope
}

# Combine SEs and slopes/intercepts ---------------------------------------

# calculate upper and lower bound (+/- 1 SE)
RGrace.SE$upper = RGrace.SE$Point + RGrace.SE$SE
RGrace.SE$lower = RGrace.SE$Point - RGrace.SE$SE


# B. Gen Task ----------------------------------------------------------------


RG_gen = filter(RaceGen, Task == "Gender")

RG_gen$TarRace.d = 0
RG_gen$TarRace.d[RG_gen$TarRace == "White"] = 1

RG_gen$TarGender.d = 0
RG_gen$TarGender.d[RG_gen$TarGender == "male"] = 1

RG_gen$Fix.d = 0
RG_gen$Fix.d[RG_gen$Fix == "forehead"] = 1

RG_gen$ParRace.d = 0
RG_gen$ParRace.d[RG_gen$ParRace == "White"] = 1


# Calculate intercepts and slopes -----------------------------------------

# fit model to plot simple slopes 
plot2.d = lmer(RT ~ P2.meancent.scale*TarRace.d*TarGender.d*Fix.d + (1|Subject), data = RG_gen)

parms <- fixef(plot2.d) # fixed parameters from model
vcmat <- vcov(plot2.d) # variance/covariance matrix of fixed effects

# First calculate slopes

## each row of this matrix defines entries of parms that we want to combine:
STmat <- matrix(NA,8,16)
STmat[1,] <- c(0,1,rep(0,3),0,0,0,0,0,0,0,0,0,0,0) # slope for tarRace = 0, tarGender = 0, Fix = 0
STmat[2,] <- c(0,1,rep(0,3),0,0,0,1,0,0,0,0,0,0,0) # slope for tarRace = 0, tarGender = 0, Fix = 1
STmat[3,] <- c(0,1,rep(0,3),0,1,0,0,0,0,0,0,0,0,0) # slope for tarRace = 0, tarGender = 1, Fix = 0
STmat[4,] <- c(0,1,rep(0,3),0,1,0,1,0,0,0,0,1,0,0) # slope for tarRace = 0, tarGender = 1, Fix = 1
STmat[5,] <- c(0,1,rep(0,3),1,0,0,0,0,0,0,0,0,0,0) # slope for tarRace = 1, tarGender = 0, Fix = 0
STmat[6,] <- c(0,1,rep(0,3),1,0,0,1,0,0,0,1,0,0,0) # slope for tarRace = 1, tarGender = 0, Fix = 1
STmat[7,] <- c(0,1,rep(0,3),1,1,0,0,0,0,1,0,0,0,0) # slope for tarRace = 1, tarGender = 1, Fix = 0
STmat[8,] <- c(0,1,rep(0,3),1,1,0,1,0,0,1,1,1,0,1) # slope for tarRace = 1, tarGender = 1, Fix = 1

Sparest <- STmat %*% parms          # see above for notation
newvc <- STmat %*% vcmat %*% t(STmat)
ses <- sqrt(diag(newvc))

slopes = cbind(Sparest, ses) %>% as.data.frame()
names(slopes) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE
slopes$lbnd = slopes$est - 2*slopes$ses
slopes$ubnd = slopes$est + 2*slopes$ses

# Next calculate intercepts

## each row of this matrix defines entries of parms that we want to combine:
ITmat <- matrix(NA,8,16)
ITmat[1,] <- c(1,0,0,0,0,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 0, Fix = 0
ITmat[2,] <- c(1,0,0,0,1,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 0, Fix = 1
ITmat[3,] <- c(1,0,0,1,0,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 1, Fix = 0
ITmat[4,] <- c(1,0,0,1,1,0,0,0,0,0,1,rep(0,3),0,0) # intercept for tarRace = 0, tarGender = 1, Fix = 1
ITmat[5,] <- c(1,0,1,0,0,0,0,0,0,0,0,rep(0,3),0,0) # intercept for tarRace = 1, tarGender = 0, Fix = 0
ITmat[6,] <- c(1,0,1,0,1,0,0,0,0,1,0,rep(0,3),0,0) # intercept for tarRace = 1, tarGender = 0, Fix = 1
ITmat[7,] <- c(1,0,1,1,0,0,0,1,0,0,0,rep(0,3),0,0) # intercept for tarRace = 1, tarGender = 1, Fix = 0
ITmat[8,] <- c(1,0,1,1,1,0,0,1,0,1,1,rep(0,3),1,0) # intercept for tarRace = 1, tarGender = 1, Fix = 1

Iparest <- ITmat %*% parms          # see above for notation

RGgen.plotting = cbind(Iparest, slopes[c(1,3:4)]) %>% as.data.frame()
names(RGgen.plotting) = c("Intercept", "Slope", "lower_95CI", "upper_95CI")

# label for understanding
RGgen.plotting$Target_Race = c(rep("Black",4), rep("White",4))
RGgen.plotting$Target_Gender = rep(c(rep("female",2), rep("male",2)),2)
RGgen.plotting$Fix = rep(c("eyes", "forehead"),4)
RGgen.plotting$Task = "GenTask"
RGgen.plotting$Study = "Study2"

RGgen.plotting$Condition = paste(RGgen.plotting$Target_Race, RGgen.plotting$Target_Gender, RGgen.plotting$Fix, sep="_")

# Calculate SEs -----------------------------------------------------------

# Zoom in, add SEs

# since P2 amp is continuous, don't have discrete values (like Trial in N170 and ERN examples)
# Try with 9 levels of P2 amp (-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)
SETmat = NULL
SETmat = matrix(NA,72,16) # 72 rows because 9 levels of x variable x 8 conditions, 16 columns because 8 levels of predictors in vcmat
# fill columns with appropriate weights (columns correspond to order model is specified, i.e., order of predictors in output of model)

P2levels = c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)

# weights for intercept
SETmat[,1] = 1 

# weights for P2.meancent.scale
SETmat[,2] = rep(P2levels, 8) # 9 levels of continuous variable of P2.meancent.scale, repeated 8 times because 4 conditions

# weights for Race main effect
SETmat[,3] = rep(c(0, 0, 0, 0, 1, 1, 1, 1), each = 9) # race = 0 for first four conditions, race = 1 for second four conditions

# weights for Gender main effect
SETmat[,4] = rep(c(0, 0, 1, 1, 0, 0, 1, 1), each = 9) 

# weights for Fix main effect
SETmat[,5] = rep(c(0, 1, 0, 1, 0, 1, 0, 1), each = 9) 

# weights for P2 x Race condition
SETmat[,6] = c(rep(c(0, 0, 0, 0), each = 9), P2levels, P2levels, P2levels, P2levels) 

# weights for P2 x Gender condition
SETmat[,7] = c(rep(0,18), P2levels, P2levels, rep(0,18), P2levels, P2levels)

# weights for Race x Gender interaction
SETmat[,8] = rep(c(0, 0, 0, 0, 0, 0, 1, 1), each = 9) 

# weights for P2 x Fix condition
SETmat[,9] = c(rep(0,9), P2levels, rep(0,9), P2levels, rep(0,9), P2levels, rep(0,9), P2levels)

# weights for Race x Fix condition
SETmat[,10] = rep(c(0, 0, 0, 0, 0, 1, 0, 1), each = 9) 

# weights for Gender x Fix condition
SETmat[,11] = rep(c(0, 0, 0, 1, 0, 0, 0, 1), each = 9)

# weights for P2 x Race x Gender
SETmat[,12] = c(rep(c(0, 0, 0, 0, 0, 0), each = 9), P2levels, P2levels)

# weights for P2 x Race x Fix
SETmat[,13] = c(rep(c(0, 0, 0, 0, 0), each = 9), P2levels, rep(0,9), P2levels)

# weights for P2 x Gender x Fix
SETmat[,14] = c(rep(c(0, 0, 0), each = 9), P2levels, rep(c(0, 0, 0), each = 9), P2levels)

# weights for Race x Gender x Fix
SETmat[,15] = rep(c(0, 0, 0, 0, 0, 0, 0, 1), each = 9)

# weights for P2 x Race x Gender x Fix
SETmat[,16] = c(rep(c(0, 0, 0, 0, 0, 0, 0), each = 9), P2levels)


plot2.d = lmer(RT ~ P2.meancent.scale*TarRace.d*TarGender.d*Fix.d + (1|Subject), data = RG_gen)

vcmat <- vcov(plot2.d) # variance/covariance matrix of fixed effects

newvc <- SETmat %*% vcmat %*% t(SETmat)
ses <- sqrt(diag(newvc)) # long string of SEs for each level of P2 amp
# first 9 are for Black-female-eyes
# second 9 are for Black-female-forehead
# third 9 are for Black-male-eyes
# fourth 9 are for Black-male-forehead
# fifth 9 are for White-female-eyes
# sixth 9 are for White-female-forehead
# seventh 9 are for White-male-eyes
# eighth 9 are for White-male-forehead


RGgen.SE = data.frame(Race = c(rep("Black", 36), rep("White", each=36)), 
                      Gender = rep(c("female", "female","male", "male"), each=9, 2),
                      Fix = rep(c("eyes", "forehead"), each = 9, 4),
                      Task = "GenTask", 
                      Study = "Study2",
                      P2.meancent.scale = rep(c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1), 4),
                      SE = ses)
RGgen.SE$Condition = paste(RGgen.SE$Race, RGgen.SE$Gender, RGgen.SE$Fix, sep="_")

# Add in point data
RGgen.SE$Point = NA
for (c in unique(RGgen.SE$Condition)) {
  forPlottingtemp = RGgen.plotting[RGgen.plotting$Condition == c,]
  RGgen.SE$Point[RGgen.SE$Condition == c] = 
    forPlottingtemp$Intercept + P2levels*forPlottingtemp$Slope
}

# Combine SEs and slopes/intercepts ---------------------------------------

# calculate upper and lower bound (+/- 1 SE)
RGgen.SE$upper = RGgen.SE$Point + RGgen.SE$SE
RGgen.SE$lower = RGgen.SE$Point - RGgen.SE$SE


# 3. Preprocess Diss data -------------------------------------------------

diss = read.delim("./3 Dissertation/AllSubs_bothTasks_acceptedTrials_long_nobs_nobe_withRT_meancenteredP2.txt")

# Dummy code
diss$Race.d = 0
diss$Race.d[diss$TarRace == "White"] = 1

diss$Gen.d = 0
diss$Gen.d[diss$TarGender == "Male"] = 1

# separate by task
diss_race = filter(diss, Task == "RaceTask")
diss_gen = filter(diss, Task == "GenTask")


# A. Race Task ------------------------------------------------------------


# Calculate intercepts and slopes -----------------------------------------

# fit model to plot simple slopes
plot2.d = lmer(RT ~ P2.meancent.scale*Race.d*Gen.d + (1|Subject), data = diss_race)

parms <- fixef(plot2.d) # fixed parameters from model
vcmat <- vcov(plot2.d) # variance/covariance matrix of fixed effects

# First calculate slopes

## each row of this matrix defines entries of parms that we want to combine:
STmat <- matrix(NA,4,8)
STmat[1,] <- c(0,1,rep(0,2),0,0,0,0) # slope for Race = 0, Gen = 0
STmat[2,] <- c(0,1,rep(0,2),0,1,0,0) # slope for Race = 0, Gen = 1
STmat[3,] <- c(0,1,rep(0,2),1,0,0,0) # slope for Race = 1, Gen = 0
STmat[4,] <- c(0,1,rep(0,2),1,1,0,1) # slope for Race = 1, Gen = 1

Sparest <- STmat %*% parms          # see above for notation
newvc <- STmat %*% vcmat %*% t(STmat)
ses <- sqrt(diag(newvc))

slopes = cbind(Sparest, ses) %>% as.data.frame()
names(slopes) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE
slopes$lbnd = slopes$est - 2*slopes$ses
slopes$ubnd = slopes$est + 2*slopes$ses

# Next calculate intercepts

## each row of this matrix defines entries of parms that we want to combine:
ITmat <- matrix(NA,4,8)
ITmat[1,] <- c(1,0,0,0,0,0,0,0) # intercept for Race = 0, Gen = 0
ITmat[2,] <- c(1,0,0,1,0,0,0,0) # intercept for Race = 0, Gen = 1
ITmat[3,] <- c(1,0,1,0,0,0,0,0) # intercept for Race = 1, Gen = 0
ITmat[4,] <- c(1,0,1,1,0,0,1,0) # intercept for Race = 1, Gen = 1

Iparest <- ITmat %*% parms          # see above for notation

dissrace.plotting = cbind(Iparest, slopes[c(1,3:4)]) %>% as.data.frame()
names(dissrace.plotting) = c("Intercept", "Slope", "lower_95CI", "upper_95CI")

# label for understanding
dissrace.plotting$Target_Race = c(rep("Black",2), rep("White",2))
dissrace.plotting$Target_Gender = rep(c("female", "male"),2)
dissrace.plotting$Fix = "eyes"
dissrace.plotting$Task = "RaceTask"
dissrace.plotting$Study = "Study3"

dissrace.plotting$Condition = paste(dissrace.plotting$Target_Race, dissrace.plotting$Target_Gender, sep="_")

# Calculate SEs -----------------------------------------------------------

# since P2 amp is continuous, don't have discrete values like Trial (in N170 and ERN examples)
# Try with 9 levels of P2 amp (-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)

SETmat <- matrix(NA,36,8) # 36 rows because 9 levels of x variable x 4 conditions, 8 columns because 8 levels of predictors in vcmat
# fill columns with appropriate weights (columns correspond to order model is specified, i.e., order of predictors in output of model)

P2levels = c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)
# weights for intercept
SETmat[,1] = 1 
# weights for P2.meancent.scale
SETmat[,2] = rep(P2levels, 4) # 9 levels of continuous variable of P2.meancent.scale, repeated 4 times because 4 conditions
# weights for Race main effect
SETmat[,3] = rep(c(0, 0, 1, 1), each = 9) # race = 0 for first two conditions, race = 1 for second two conditions
# weights for Fix main effect
SETmat[,4] = rep(c(0, 1, 0, 1), each = 9) # fix = 0 for first and third conditions, fix = 0 for second and fourth conditions
# weights for P2 x Race condition
SETmat[,5] = c(rep(c(0, 0), each = 9), P2levels, P2levels) 
# weights for P2 x Fix condition
SETmat[,6] = c(rep(0,9), P2levels, rep(0,9), P2levels) 
# weights for Race x Fix condition
SETmat[,7] = rep(c(0, 0, 0, 1), each = 9) 
# weights for 3 way interaction
SETmat[,8] = c(rep(c(0, 0, 0), each = 9), P2levels)


newvc <- SETmat %*% vcmat %*% t(SETmat)
ses <- sqrt(diag(newvc)) # long string of SEs for each level of P2 amp, first 9 are for Black-female, second 9 are for Black-male, third 9 are for White-female, fourth 9 are for White-male


dissrace.SE = data.frame(Race = rep(c("Black", "White"), each=18), 
                         Gender = rep(c("female", "male"), each = 9),
                         Fix = "eyes",
                         Task = "RaceTask",
                         Study = "Study3",
                         P2.meancent.scale = rep(c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1), 4),
                         SE = ses)
dissrace.SE$Condition = paste(dissrace.SE$Race, dissrace.SE$Gender, sep="_")


# Combine SEs and intercept/slopes ----------------------------------------

dissrace.SE$Point = NA
for (c in unique(dissrace.SE$Condition)) {
  forPlottingtemp = dissrace.plotting[dissrace.plotting$Condition == c,]
  dissrace.SE$Point[dissrace.SE$Condition == c] = 
    forPlottingtemp$Intercept + P2levels*forPlottingtemp$Slope
}

# calculate upper and lower bound (+/- 1 SE)
dissrace.SE$upper = dissrace.SE$Point + dissrace.SE$SE
dissrace.SE$lower = dissrace.SE$Point - dissrace.SE$SE

# B. Gen Task -------------------------------------------------------------


# Calculate intercepts and slopes -----------------------------------------

# fit model to plot simple slopes
plot2.d = lmer(RT ~ P2.meancent.scale*Race.d*Gen.d + (1|Subject), data = diss_gen)

parms <- fixef(plot2.d) # fixed parameters from model
vcmat <- vcov(plot2.d) # variance/covariance matrix of fixed effects

# First calculate slopes

## each row of this matrix defines entries of parms that we want to combine:
STmat <- matrix(NA,4,8)
STmat[1,] <- c(0,1,rep(0,2),0,0,0,0) # slope for Race = 0, Gen = 0
STmat[2,] <- c(0,1,rep(0,2),0,1,0,0) # slope for Race = 0, Gen = 1
STmat[3,] <- c(0,1,rep(0,2),1,0,0,0) # slope for Race = 1, Gen = 0
STmat[4,] <- c(0,1,rep(0,2),1,1,0,1) # slope for Race = 1, Gen = 1

Sparest <- STmat %*% parms          # see above for notation
newvc <- STmat %*% vcmat %*% t(STmat)
ses <- sqrt(diag(newvc))

slopes = cbind(Sparest, ses) %>% as.data.frame()
names(slopes) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE
slopes$lbnd = slopes$est - 2*slopes$ses
slopes$ubnd = slopes$est + 2*slopes$ses

# Next calculate intercepts

## each row of this matrix defines entries of parms that we want to combine:
ITmat <- matrix(NA,4,8)
ITmat[1,] <- c(1,0,0,0,0,0,0,0) # intercept for Race = 0, Gen = 0
ITmat[2,] <- c(1,0,0,1,0,0,0,0) # intercept for Race = 0, Gen = 1
ITmat[3,] <- c(1,0,1,0,0,0,0,0) # intercept for Race = 1, Gen = 0
ITmat[4,] <- c(1,0,1,1,0,0,1,0) # intercept for Race = 1, Gen = 1

Iparest <- ITmat %*% parms          # see above for notation

dissgen.plotting = cbind(Iparest, slopes[c(1,3:4)]) %>% as.data.frame()
names(dissgen.plotting) = c("Intercept", "Slope", "lower_95CI", "upper_95CI")

# label for understanding
dissgen.plotting$Target_Race = c(rep("Black",2), rep("White",2))
dissgen.plotting$Target_Gender = rep(c("female", "male"),2)
dissgen.plotting$Fix = "eyes"
dissgen.plotting$Task = "GenTask"
dissgen.plotting$Study = "Study3"

dissgen.plotting$Condition = paste(dissgen.plotting$Target_Race, dissgen.plotting$Target_Gender, sep="_")

# Calculate SEs -----------------------------------------------------------

# since P2 amp is continuous, don't have discrete values like Trial (in N170 and ERN examples)
# Try with 9 levels of P2 amp (-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)

SETmat <- matrix(NA,36,8) # 36 rows because 9 levels of x variable x 4 conditions, 8 columns because 8 levels of predictors in vcmat
# fill columns with appropriate weights (columns correspond to order model is specified, i.e., order of predictors in output of model)

P2levels = c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1)
# weights for intercept
SETmat[,1] = 1 
# weights for P2.meancent.scale
SETmat[,2] = rep(P2levels, 4) # 9 levels of continuous variable of P2.meancent.scale, repeated 4 times because 4 conditions
# weights for Race main effect
SETmat[,3] = rep(c(0, 0, 1, 1), each = 9) # race = 0 for first two conditions, race = 1 for second two conditions
# weights for Fix main effect
SETmat[,4] = rep(c(0, 1, 0, 1), each = 9) # fix = 0 for first and third conditions, fix = 0 for second and fourth conditions
# weights for P2 x Race condition
SETmat[,5] = c(rep(c(0, 0), each = 9), P2levels, P2levels) 
# weights for P2 x Fix condition
SETmat[,6] = c(rep(0,9), P2levels, rep(0,9), P2levels) 
# weights for Race x Fix condition
SETmat[,7] = rep(c(0, 0, 0, 1), each = 9) 
# weights for 3 way interaction
SETmat[,8] = c(rep(c(0, 0, 0), each = 9), P2levels)


newvc <- SETmat %*% vcmat %*% t(SETmat)
ses <- sqrt(diag(newvc)) # long string of SEs for each level of P2 amp, first 9 are for Black-female, second 9 are for Black-male, third 9 are for White-female, fourth 9 are for White-male


dissgen.SE = data.frame(Race = rep(c("Black", "White"), each=18), 
                        Gender = rep(c("female", "male"), each = 9),
                        Fix = "eyes",
                        Task = "GenTask",
                        Study = "Study3",
                        P2.meancent.scale = rep(c(-1, -.75, -.5, -.25, 0, .25, .50, .75, 1), 4),
                        SE = ses)

dissgen.SE$Condition = paste(dissgen.SE$Race, dissgen.SE$Gender, sep="_")


# Combine SEs and intercept/slopes ----------------------------------------

dissgen.SE$Point = NA
for (c in unique(dissgen.SE$Condition)) {
  forPlottingtemp = dissgen.plotting[dissgen.plotting$Condition == c,]
  dissgen.SE$Point[dissgen.SE$Condition == c] = 
    forPlottingtemp$Intercept + P2levels*forPlottingtemp$Slope
}

# calculate upper and lower bound (+/- 1 SE)
dissgen.SE$upper = dissgen.SE$Point + dissgen.SE$SE
dissgen.SE$lower = dissgen.SE$Point - dissgen.SE$SE




# 4. Plots -----------------------------------------------------------------

all.plotting = rbind(ERPfix.plotting,
                     RGgen.plotting,
                     RGrace.plotting,
                     dissgen.plotting,
                     dissrace.plotting)

all.SE = rbind(ERPfix.SE,
               RGgen.SE,
               RGrace.SE,
               dissgen.SE,
               dissrace.SE)

#  set elements of plots --------------------------------------------------------------------------

zoom.ribbon = geom_ribbon(aes(ymin=lower, ymax=upper, x = P2.meancent.scale),
                          linetype = "dotted",
                          alpha = .1)
zoom.labs = labs(x = "P2 amplitude", y = "Reaction time")

zoom.theme = theme(plot.title = element_text(hjust = 0.5, size = 20),# center title
                   axis.title = element_text(size = 20),
                   axis.text.y = element_text(size = 16),
                   axis.text.x = element_text(size = 12),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.text = element_text(size = 16))
zoom.scalex = scale_x_continuous(limits=c(-1,1),expand=c(0,0))
zoom.scaley = scale_y_continuous(limits=c(410, 480), expand=c(0,0))


# A. Black male (eyes) -----------------------------------------------------

BME.plotting = filter(all.plotting, Target_Race == "Black", Target_Gender == "male", Fix == "eyes")
BME.plotting$Condition = paste(BME.plotting$Task, BME.plotting$Study, sep = "_")

BME.SE = filter(all.SE, Race == "Black", Gender == "male", Fix == "eyes")
BME.SE$Condition = paste(BME.SE$Task, BME.SE$Study, sep = "_")

BMEcolors = c("RaceTask_Study1" = "forestgreen",
              "GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet",
              "GenTask_Study3" = "red",
              "RaceTask_Study3" = "goldenrod2")

ggplot(BME.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = BME.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("Black male faces (eyes)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = BMEcolors)

ggsave("./4 Figures/P2->RT/Black_Male_Eyes.tiff", height=6, width = 6, unit="in")

# B. Black female (eyes) -----------------------------------------------------

BFE.plotting = filter(all.plotting, Target_Race == "Black", Target_Gender == "female", Fix == "eyes")
BFE.plotting$Condition = paste(BFE.plotting$Task, BFE.plotting$Study, sep = "_")

BFE.SE = filter(all.SE, Race == "Black", Gender == "female", Fix == "eyes")
BFE.SE$Condition = paste(BFE.SE$Task, BFE.SE$Study, sep = "_")

BFEcolors = c("GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet",
              "GenTask_Study3" = "red",
              "RaceTask_Study3" = "goldenrod2")

ggplot(BFE.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = BFE.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("Black female faces (eyes)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = BFEcolors)

ggsave("./4 Figures/P2->RT/Black_Female_Eyes.tiff", height=6, width = 6, unit="in")

# C. White male (eyes) -----------------------------------------------------

WME.plotting = filter(all.plotting, Target_Race == "White", Target_Gender == "male", Fix == "eyes")
WME.plotting$Condition = paste(WME.plotting$Task, WME.plotting$Study, sep = "_")

WME.SE = filter(all.SE, Race == "White", Gender == "male", Fix == "eyes")
WME.SE$Condition = paste(WME.SE$Task, WME.SE$Study, sep = "_")

WMEcolors = c("RaceTask_Study1" = "forestgreen",
              "GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet",
              "GenTask_Study3" = "red",
              "RaceTask_Study3" = "goldenrod2")

ggplot(WME.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = WME.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("White male faces (eyes)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = WMEcolors)

ggsave("./4 Figures/P2->RT/White_Male_Eyes.tiff", height=6, width = 6, unit="in")

# D. White female (eyes) -----------------------------------------------------

WFE.plotting = filter(all.plotting, Target_Race == "White", Target_Gender == "female", Fix == "eyes")
WFE.plotting$Condition = paste(WFE.plotting$Task, WFE.plotting$Study, sep = "_")

WFE.SE = filter(all.SE, Race == "White", Gender == "female", Fix == "eyes")
WFE.SE$Condition = paste(WFE.SE$Task, WFE.SE$Study, sep = "_")

WFEcolors = c("GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet",
              "GenTask_Study3" = "red",
              "RaceTask_Study3" = "goldenrod2")

ggplot(WFE.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = WFE.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("White female faces (eyes)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = WFEcolors)

ggsave("./4 Figures/P2->RT/White_Female_Eyes.tiff", height=6, width = 6, unit="in")

# E. Black male (forehead) -----------------------------------------------------

BMF.plotting = filter(all.plotting, Target_Race == "Black", Target_Gender == "male", Fix == "forehead")
BMF.plotting$Condition = paste(BMF.plotting$Task, BMF.plotting$Study, sep = "_")

BMF.SE = filter(all.SE, Race == "Black", Gender == "male", Fix == "forehead")
BMF.SE$Condition = paste(BMF.SE$Task, BMF.SE$Study, sep = "_")

BMFcolors = c("RaceTask_Study1" = "forestgreen",
              "GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet")

ggplot(BMF.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = BMF.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("Black male faces (forehead)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = BMFcolors)

ggsave("./4 Figures/P2->RT/Black_Male_Forehead.tiff", height=6, width = 6, unit="in")

# F. Black female (forehead) -----------------------------------------------------

BFF.plotting = filter(all.plotting, Target_Race == "Black", Target_Gender == "female", Fix == "forehead")
BFF.plotting$Condition = paste(BFF.plotting$Task, BFF.plotting$Study, sep = "_")

BFF.SE = filter(all.SE, Race == "Black", Gender == "female", Fix == "forehead")
BFF.SE$Condition = paste(BFF.SE$Task, BFF.SE$Study, sep = "_")

BFFcolors = c("GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet")

ggplot(BFF.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = BFF.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("Black female faces (forehead)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = BFFcolors)

ggsave("./4 Figures/P2->RT/Black_Female_Forehead.tiff", height=6, width = 6, unit="in")

# G. White male (forehead) -----------------------------------------------------

WMF.plotting = filter(all.plotting, Target_Race == "White", Target_Gender == "male", Fix == "forehead")
WMF.plotting$Condition = paste(WMF.plotting$Task, WMF.plotting$Study, sep = "_")

WMF.SE = filter(all.SE, Race == "White", Gender == "male", Fix == "forehead")
WMF.SE$Condition = paste(WMF.SE$Task, WMF.SE$Study, sep = "_")

WMFcolors = c("RaceTask_Study1" = "forestgreen",
              "GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet")

ggplot(WMF.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = WMF.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("White male faces (forehead)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = WMFcolors)

ggsave("./4 Figures/P2->RT/White_Male_Forehead.tiff", height=6, width = 6, unit="in")

# H. White female (forehead) -----------------------------------------------------

WFF.plotting = filter(all.plotting, Target_Race == "White", Target_Gender == "female", Fix == "forehead")
WFF.plotting$Condition = paste(WFF.plotting$Task, WFF.plotting$Study, sep = "_")

WFF.SE = filter(all.SE, Race == "White", Gender == "female", Fix == "forehead")
WFF.SE$Condition = paste(WFF.SE$Task, WFF.SE$Study, sep = "_")

WFFcolors = c("GenTask_Study2" = "blue",
              "RaceTask_Study2" = "blueviolet")

ggplot(WFF.SE, aes(Point, P2.meancent.scale, color = Condition)) +
  geom_abline(data = WFF.plotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  ggtitle("White female faces (forehead)") +
  zoom.ribbon +
  zoom.labs +
  zoom.scalex +
  zoom.scaley +
  theme_bw() +
  zoom.theme +
  scale_color_manual(values = WFFcolors)

ggsave("./4 Figures/P2->RT/White_Female_Forehead.tiff", height=6, width = 6, unit="in")

