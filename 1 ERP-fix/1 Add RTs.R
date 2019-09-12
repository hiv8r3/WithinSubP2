require(dplyr)

RTdat = read.delim("./1 ERP-fix/catDat.txt")
# Trial tells whether practice or experimental (1 = practice, 2 = first block of experimental, 3 = second block of experimental)
# SubTrial tells trial number (8 practice trials, 256 experimental trials)
# faceRace, FixArea tell trial condition
# responseAccData tells accuracy (1 = incorrect, 2 = correct, 3 = no response)
# TargetFace.RT tells RT

# Read in file with P2 quantified (accepted trials only) and add RT
ERPdat = read.delim("./1 ERP-fix/Cat_AllSubs_TBTaverages_noBS_groupP2.txt") %>% 
  filter(Subject != 32) %>% # too few trials, RT doesn't match up - less than 50% of trials
  filter(Subject != 51) %>% 
  filter(Subject != 62)

# ##### For testing
# View(ERPdat[ERPdat$Subject == 46 & ERPdat$Electrode == "CZ",])
# View(RTdat[RTdat$Subject == 46,c("SubTrial", "TrialType")])
# 
# #####

# ERP dat is missing some trials, results in mismatch with EPrime data
# IMPORTANT: Trials with no response were not included in event file, resulting in mismatches
# This was especially the case for sub 46

mismatch = read.delim("./1 ERP-fix/MismatchTrials.txt")
mismatch = mutate(mismatch, 
                  Difference1 = Eprime - ERP, 
                  Difference2 = Eprime2 - ERP2, 
                  Difference3 = Eprime3 - ERP3,
                  Difference4 = Eprime4 - ERP4,
                  Difference5 = Eprime5 - ERP5,
                  Difference6 = Eprime6 - ERP6,
                  Difference7 = Eprime7 - ERP7,
                  Difference8 = Eprime8 - ERP8,
                  Difference9 = Eprime9 - ERP9,
                  Difference10 = Eprime10 - ERP10,
                  Difference11 = Eprime11 - ERP11,
                  Difference12 = Eprime12 - ERP12,
                  Difference13 = Eprime13 - ERP13,
                  Difference14 = Eprime14 - ERP14,
                  Difference15 = Eprime15 - ERP15,
                  Difference16 = Eprime16 - ERP16,
                  Difference17 = Eprime17 - ERP17,
                  Difference18 = Eprime18 - ERP18,
                  Difference19 = Eprime19 - ERP19,
                  Difference20 = Eprime20 - ERP20,
                  Difference21 = Eprime21 - ERP21)

# loop merges RT dat from EPrime files into ERP data
# separate else if statement for each section, depending on mismatch
# vomit-inducing, but I don't want to spend the time to make it more efficient

newdat = NULL
for (i in unique(ERPdat$Subject)) { #unique(ERPdat$Subject) 
  # Race task
  EPrimetemp = RTdat[RTdat$Subject == i & RTdat$Trial != 1,] # only take experimental trials
  ERPtemp = ERPdat[ERPdat$Subject == i,]
  mismatchtemp = mismatch[mismatch$Subject == i,]
  for (t in unique(ERPtemp$Trial)) {
    if (t < mismatchtemp$ERP) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                              as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                              nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                             as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                             nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP & t < mismatchtemp$ERP2) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference1[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference1[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference1[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference1[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP2 & t < mismatchtemp$ERP3) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference2[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference2[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference2[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference2[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP3 & t < mismatchtemp$ERP4) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference3[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference3[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference3[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference3[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP4 & t < mismatchtemp$ERP5) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference4[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference4[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference4[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference4[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP5 & t < mismatchtemp$ERP6) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference5[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference5[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference5[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference5[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP6 & t < mismatchtemp$ERP7) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference6[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference6[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference6[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference6[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP7 & t < mismatchtemp$ERP8) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference7[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference7[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference7[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference7[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP8 & t < mismatchtemp$ERP9) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference8[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference8[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference8[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference8[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP9 & t < mismatchtemp$ERP10) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference9[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference9[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference9[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference9[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP10 & t < mismatchtemp$ERP11) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference10[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference10[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference10[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference10[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    }  else if (t >= mismatchtemp$ERP11 & t < mismatchtemp$ERP12) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference11[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference11[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference11[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference11[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP12 & t < mismatchtemp$ERP13) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference12[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference12[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference12[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference12[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP13 & t < mismatchtemp$ERP14) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference13[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference13[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference13[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference13[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP14 & t < mismatchtemp$ERP15) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference14[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference14[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference14[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference14[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP15 & t < mismatchtemp$ERP16) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference15[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference15[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference15[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference15[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP16 & t < mismatchtemp$ERP17) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference16[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference16[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference16[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference16[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP17 & t < mismatchtemp$ERP18) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference17[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference17[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference17[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference17[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP18 & t < mismatchtemp$ERP19) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference18[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference18[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference18[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference18[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP19 & t < mismatchtemp$ERP20) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference19[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference19[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference19[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference19[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP20 & t < mismatchtemp$ERP21) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference20[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference20[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference20[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference20[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    } else if (t >= mismatchtemp$ERP21) {
      RT = EPrimetemp$TargetFace.RT[EPrimetemp$SubTrial == t+mismatch$Difference21[mismatch$Subject == i]]
      ACC = EPrimetemp$responseAccDat[EPrimetemp$SubTrial == t+mismatch$Difference21[mismatch$Subject == i]] # 2 is correct, 1 is incorrect, 3 is miss
      EPrime_race = as.character(EPrimetemp$faceRace[EPrimetemp$SubTrial == t+mismatch$Difference21[mismatch$Subject == i]])
      EPrime_fix = as.character(EPrimetemp$FixArea[EPrimetemp$SubTrial == t+mismatch$Difference21[mismatch$Subject == i]])
      
      # add new columns
      ERPtemp$RT[ERPtemp$Trial == t] = RT
      ERPtemp$EPrime_race[ERPtemp$Trial == t] = EPrime_race
      ERPtemp$EPrime_fix[ERPtemp$Trial == t] = EPrime_fix
      ERPtemp$EPrime_acc[ERPtemp$Trial == t] = ACC
      
      # check if condition matches up
      ERPtemp$race_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_race,
                                                            as.character(ERPtemp$Race[ERPtemp$Trial == t])[1],
                                                            nomatch = 2) - 1 # match = 0, nomatch = 1
      ERPtemp$fix_mismatch[ERPtemp$Trial == t] = charmatch(EPrime_fix,
                                                           as.character(ERPtemp$Fix[ERPtemp$Trial == t])[1],
                                                           nomatch = 2) - 1 # match = 0, nomatch = 1
      
    }
  } 
  newdat = rbind(newdat, ERPtemp)
  
}

unique(newdat$Subject[newdat$race_mismatch == 1])
unique(newdat$Subject[newdat$fix_mismatch == 1])
# # for testing
# test = newdat[newdat$Subject == 40,]
# head(test[test$race_mismatch == 1 | test$fix_mismatch == 1,])
# View(test[test$race_mismatch == 1 | test$fix_mismatch == 1,])



# Take out incorrect and missed trials -------------------------------

# not sure why there are missed and incorrect trials in some subs but not others
# for some, missed weren't recorded in event files, but I guess not all?
unique(newdat$EPrime_acc)
View(newdat[newdat$EPrime_acc != 2,])

newdat = filter(newdat, EPrime_acc == 2)

newdat[newdat$RT == 0,]



# Look at percentage of trials included -----------------------------------

numTrials = NULL
for (i in unique(newdat$Subject)) {
  trials = length(unique(newdat$Trial[newdat$Subject == i]))
  numTrials = rbind(numTrials,
                    data.frame(Subject = i, numTrials = trials))
}

numTrials$percentage = numTrials$numTrials/256
numTrials[order(numTrials$percentage),]

# subs 32, 51 and 62 aren't included because < 50% trials
# leaves 62 subs

write.table(newdat, "./1 ERP-fix/AllSubs_acceptedTrials_long_nobe_nobs_withRT.txt", sep="\t", row.names=F)

# add subject mean and mean-centered values for P2

dat = read.delim("./1 ERP-fix/AllSubs_acceptedTrials_long_nobe_nobs_withRT.txt")

# mean calculated separately for each subject and electrode
electrode = as.character(unique(dat$Electrode))
for (i in unique(dat$Subject)) {
  for (e in electrode) {
  # calculate mean
    temp.race = dat[dat$Subject == i & dat$Electrode == e,]
    dat$P2.mean[dat$Subject == i & dat$Electrode == e] = mean(temp.race$MeanAmp)

    # calculate subject mean-centered variable
    dat$P2.meancent = dat$MeanAmp - dat$P2.mean
    
    # scale subject mean-centered variable
    dat$P2.meancent.scale = scale(dat$P2.meancent)
    
  }
}

write.table(dat, "./1 ERP-fix/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2.txt", sep="\t", row.names=F)

