library(dplyr)

RTdat = read.delim("./3 Dissertation/Diss_categorization_allSubjects.txt") %>% 
  filter(Trial == 2)
# Trial tells whether practice or experimental (1 = practice, 2 = experimental)
# SubTrial tells trial number (4 practice trials, 128 experimental trials in each task)
# faceRace, faceGender tell trial condition
# Procedure[Block] tells which task (RaceTask, GenderTask)
# responseAccData tells accuracy (1 = incorrect, 2 = correct, 3 = no response)
# TargetFaceRace.RT tells RT in Race Task
# TargetFaceGen.RT tells RT in Gen Task
# TargetPortCode tells trigger code

# Read in file with P2 quantified (accepted trials only) and add RT
ERPdat = read.delim("./3 Dissertation/AllSubs_acceptedTrials_long_nobs_nobe.txt", stringsAsFactors=F)

ERPdat$TarGender[ERPdat$TarGender == "male"] = "Male"
ERPdat$TarGender[ERPdat$TarGender == "female"] = "Female"
ERPdat$TarRace[ERPdat$TarRace == "white"] = "White"
ERPdat$TarRace[ERPdat$TarRace == "black"] = "Black"

# subjects with < 50% trials have already been taken out (36, 52, 69)
# only includes correct and no-miss trials
# bad electrodes already taken out 

# # ERP dat is missing some trials, results in mismatch with EPrime data
mismatch = read.delim("./3 Dissertation/MismatchTrials.txt", stringsAsFactor=F)
mismatch = mutate(mismatch,
                  Difference1 = Eprime - ERP)

# # For checking
# View(ERPdat[ERPdat$Subject == i & ERPdat$Electrode == "CZ",])
# View(RTdat[RTdat$Subject == i,c("SubTrial", "Procedure.Block.", "faceGender", "faceRace")])
# 
# numTrialsDat = NULL
# for (i in unique(ERPdat$Subject)) {
#   tempERP = ERPdat[ERPdat$Subject == i,]
#   tempRT = RTdat[RTdat$Subject == i,]
#   ERP = max(tempERP$Trial)
#   RT = nrow(filter(tempRT, Trial != 1))
#   numTrialsDat = rbind(numTrialsDat, data.frame(Subject = i,
#                                                 numTrialsERP = ERP,
#                                                 numTrialsRT = RT))
# }
# 
# filter(numTrialsDat, numTrialsERP != 256)
# filter(numTrialsDat, Subject == i)
# notes:
# sub 39: last trial is rejected, but everything matches up
# sub 60: no gen task 
# in mismatch file: 40, 42, 50, 54, 70, 88, 93, 96, 100, 103, 113


# Loop to merge RT data into ERP data -------------------------------------

newdat = NULL
for (i in unique(ERPdat$Subject)) { 
  # Race task
  EPrimetemp = RTdat[RTdat$`Procedure.Block.` == "RaceTask" & RTdat$Subject == i,] # only take trials for race task for that sub
  ERPtemp = ERPdat[ERPdat$Task == "RaceTask" & ERPdat$Subject == i,]
  
  # set mismatch value
  # if sub isn't in mismatch dat, value = 0
  # if sub is in mismatch dat but for other task, value = 0
  # if sub is in mismatch dat for race task, value = mismatch
  mismatch.value = ifelse(i %in% mismatch$Subject,
                          ifelse(sum(mismatch$Task[mismatch$Subject == i] == "RaceTask") == 1, mismatch$Difference1[mismatch$Subject == i & mismatch$Task == "RaceTask"], 0),
                          0)
  
  # adjust trial numbers in ERPdat if race task was presented 2nd (trial ranges from 129-256 for second task)
  if (i %in% mismatch$Subject) {
    # for subjects with mismatches
    if(max(ERPtemp$Trial > 128)) ERPtemp$Trial = ERPtemp$Trial - (min(ERPtemp$Trial -1))
  } else {
    # for subjects with no mismatches
    if(max(ERPtemp$Trial > 128)) ERPtemp$Trial = ERPtemp$Trial - 128
  }
  
  if(mismatch.value == 0) { # for subjects with no mismatches
    for (t in unique(ERPtemp$Trial)) {
      RT = EPrimetemp$TargetFaceRace.RT[EPrimetemp$SubTrial == t]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t])
      EPrime_gen = as.character(EPrimetemp$faceGender[EPrimetemp$SubTrial == t])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_gen[ERPtemp$Trial == t] = EPrime_gen
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$TarRace[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$gen_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_gen,
                                                           as.character(ERPtemp$TarGender[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
    }
  } else if (mismatch.value > 0) {
    for (t in unique(ERPtemp$Trial)) {
      RT = EPrimetemp$TargetFaceRace.RT[EPrimetemp$SubTrial == t + mismatch.value]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t + mismatch.value] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t + mismatch.value])
      EPrime_gen = as.character(EPrimetemp$faceGender[EPrimetemp$SubTrial == t + mismatch.value])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_gen[ERPtemp$Trial == t] = EPrime_gen
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$TarRace[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$gen_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_gen,
                                                           as.character(ERPtemp$TarGender[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
    }
  }
  
  newdat = rbind(newdat, ERPtemp)
  
  # Gender Task
  EPrimetemp = RTdat[RTdat$`Procedure.Block.` == "GenderTask" & RTdat$Subject == i,] # only take trials for race task for that sub
  ERPtemp = ERPdat[ERPdat$Task == "GenTask" & ERPdat$Subject == i,]
  
  # set mismatch value
  # if sub isn't in mismatch dat, value = 0
  # if sub is in mismatch dat but for other task, value = 0
  # if sub is in mismatch dat for race task, value = mismatch
  mismatch.value = ifelse(i %in% mismatch$Subject,
                          ifelse(sum(mismatch$Task[mismatch$Subject == i] == "GenTask") == 1, mismatch$Difference1[mismatch$Subject == i & mismatch$Task == "GenTask"], 0),
                          0)
  
  # adjust trial numbers in ERPdat if race task was presented 2nd (trial ranges from 129-256 for second task)
  if (i %in% mismatch$Subject) {
    # for subjects with mismatches
    if(max(ERPtemp$Trial > 128)) ERPtemp$Trial = ERPtemp$Trial - (min(ERPtemp$Trial -1))
  } else {
    # for subjects with no mismatches
    if(max(ERPtemp$Trial > 128)) ERPtemp$Trial = ERPtemp$Trial - 128
  }
  
  if(mismatch.value == 0) { # for subjects with no mismatches
    for (t in unique(ERPtemp$Trial)) {
      RT = EPrimetemp$TargetFaceGen.RT[EPrimetemp$SubTrial == t]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t])
      EPrime_gen = as.character(EPrimetemp$faceGender[EPrimetemp$SubTrial == t])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_gen[ERPtemp$Trial == t] = EPrime_gen
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$TarRace[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$gen_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_gen,
                                                           as.character(ERPtemp$TarGender[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
    }
  } else if (mismatch.value > 0) {
    for (t in unique(ERPtemp$Trial)) {
      RT = EPrimetemp$TargetFaceGen.RT[EPrimetemp$SubTrial == t + mismatch.value]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t + mismatch.value] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t + mismatch.value])
      EPrime_gen = as.character(EPrimetemp$faceGender[EPrimetemp$SubTrial == t + mismatch.value])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_gen[ERPtemp$Trial == t] = EPrime_gen
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$TarRace[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$gen_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_gen,
                                                           as.character(ERPtemp$TarGender[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
    }
  }
  newdat = rbind(newdat, ERPtemp)
} 

# error message is because sub 60 doesn't have data for genTask
# loop is still fine

# test to make sure all trials match up
unique(newdat$Subject[newdat$race_mismatch == 1])
unique(newdat$Subject[newdat$gen_mismatch == 1])

write.table(newdat, "./3 Dissertation/AllSubs_bothTasks_acceptedTrials_long_nobs_nobe_withRT.txt", sep="\t", row.names=F)


# add subject mean and mean-centered values for P2

dat = read.delim("./3 Dissertation/AllSubs_bothTasks_acceptedTrials_long_nobs_nobe_withRT.txt")

# subject mean calculated separately for each electrode
electrode = as.character(unique(dat$Electrode))
for (i in unique(dat$Subject)) {
  for (e in electrode) {
    # calculate subject mean
    temp.e = dat[dat$Subject == i & dat$Electrode == e,]
    sub.mean = mean(temp.e$meanAmp)
    dat$P2.submean[dat$Subject == i & dat$Electrode == e] = sub.mean
  }
}

# calculate subject mean-centered variable
dat$P2.meancent = dat$meanAmp - dat$P2.submean

# scale subject mean-centered variable
dat$P2.meancent.scale = scale(dat$P2.meancent)

write.table(dat, "./3 Dissertation/AllSubs_bothTasks_acceptedTrials_long_nobs_nobe_withRT_meancenteredP2.txt", sep="\t", row.names=F)

