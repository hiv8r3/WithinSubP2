require(dplyr)

RTdat = read.delim("./2 Race-Gen/RaceGen_allSubs_EPrime.txt")
# Trial tells whether practice or experimental (1 = practice, 2 = experimental)
# Block tells task order
# Procedure[Block] tells task
# SubTrial tells trial number (8 practice trials, 256 experimental trials per task)
# faceGender, faceRace, FixArea tell trial condition
# responseAccData tells accuracy (1 = incorrect, 2 = correct, 3 = no response)
# TargetFaceGen.RT, TargetFaceRace.RT tells RT
# TargetPortCode tells what trigger code was


# Read in file with P2 quantified (accepted trials only) and add RT
ERPdat = read.delim("./2 Race-Gen/AllSubs_acceptedTrials_long_nobe_nobs.txt")


newdat = NULL
for (i in c(1:54, 56:66)) {
  # Gen task
  RTtemp = RTdat[RTdat$Subject == i & RTdat$Procedure.Block. == "GenderTask",]
  genTemp = ERPdat[ERPdat$Subject == i & ERPdat$Task == "Gender",]
  for (t in unique(genTemp$Trial)) {
    RT = RTtemp$TargetFaceGen.RT[RTtemp$Trial == 2 & RTtemp$SubTrial == t]
    EPrime_gender = as.character(RTtemp$faceGender[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
      EPrime_gender[EPrime_gender == "Female"] = "female"
      EPrime_gender[EPrime_gender == "Male"] = "male"
    EPrime_race = as.character(RTtemp$faceRace[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
    EPrime_fix = as.character(RTtemp$FixArea[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
    
    # add new columns
    genTemp$RT[genTemp$Trial == t] = RT
    genTemp$EPrime_gender[genTemp$Trial == t] = EPrime_gender
    genTemp$EPrime_race[genTemp$Trial == t] = EPrime_race
    genTemp$EPrime_fix[genTemp$Trial == t] = EPrime_fix
    
    # check if condition matches up
    genTemp$gen_mismatch[genTemp$Trial == t] = charmatch(EPrime_gender,
                                                      as.character(genTemp$TarGender[genTemp$Trial == t])[1],
                                                      nomatch = 2) - 1 # match = 0, nomatch = 1
    genTemp$race_mismatch[genTemp$Trial == t] = charmatch(EPrime_race,
                                                      as.character(genTemp$TarRace[genTemp$Trial == t])[1],
                                                      nomatch = 2) - 1 # match = 0, nomatch = 1
    genTemp$fix_mismatch[genTemp$Trial == t] = charmatch(EPrime_fix,
                                                      as.character(genTemp$Fix[genTemp$Trial == t])[1],
                                                      nomatch = 2) - 1 # match = 0, nomatch = 1
    
  }
  newdat = rbind(newdat, genTemp)
  
  # Race task
  RTtemp = RTdat[RTdat$Subject == i & RTdat$Procedure.Block. == "RaceTask",]
  raceTemp = ERPdat[ERPdat$Subject == i & ERPdat$Task == "Race",]
  for (t in unique(raceTemp$Trial)) {
    RT = RTtemp$TargetFaceRace.RT[RTtemp$Trial == 2 & RTtemp$SubTrial == t]
    EPrime_gender = as.character(RTtemp$faceGender[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
    EPrime_gender[EPrime_gender == "Female"] = "female"
    EPrime_gender[EPrime_gender == "Male"] = "male"
    EPrime_race = as.character(RTtemp$faceRace[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
    EPrime_fix = as.character(RTtemp$FixArea[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
    
    # add new columns
    raceTemp$RT[raceTemp$Trial == t] = RT
    raceTemp$EPrime_gender[raceTemp$Trial == t] = EPrime_gender
    raceTemp$EPrime_race[raceTemp$Trial == t] = EPrime_race
    raceTemp$EPrime_fix[raceTemp$Trial == t] = EPrime_fix
    
    # check if condition matches up
    raceTemp$gen_mismatch[raceTemp$Trial == t] = charmatch(EPrime_gender,
                                                         as.character(raceTemp$TarGender[raceTemp$Trial == t])[1],
                                                         nomatch = 2) - 1 # match = 0, nomatch = 1
    raceTemp$race_mismatch[raceTemp$Trial == t] = charmatch(EPrime_race,
                                                          as.character(raceTemp$TarRace[raceTemp$Trial == t])[1],
                                                          nomatch = 2) - 1 # match = 0, nomatch = 1
    raceTemp$fix_mismatch[raceTemp$Trial == t] = charmatch(EPrime_fix,
                                                         as.character(raceTemp$Fix[raceTemp$Trial == t])[1],
                                                         nomatch = 2) - 1 # match = 0, nomatch = 1
    
  }
  newdat = rbind(newdat, raceTemp)
  
}

# do separately for 55 because missed first 4 trials in race task
# Gen task is regular
RTtemp = RTdat[RTdat$Subject == 55 & RTdat$Procedure.Block. == "GenderTask",]
genTemp = ERPdat[ERPdat$Subject == 55 & ERPdat$Task == "Gender",]
for (t in unique(genTemp$Trial)) {
  RT = RTtemp$TargetFaceGen.RT[RTtemp$Trial == 2 & RTtemp$SubTrial == t]
  EPrime_gender = as.character(RTtemp$faceGender[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
  EPrime_gender[EPrime_gender == "Female"] = "female"
  EPrime_gender[EPrime_gender == "Male"] = "male"
  EPrime_race = as.character(RTtemp$faceRace[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
  EPrime_fix = as.character(RTtemp$FixArea[RTtemp$Trial == 2 & RTtemp$SubTrial == t])
  
  # add new columns
  genTemp$RT[genTemp$Trial == t] = RT
  genTemp$EPrime_gender[genTemp$Trial == t] = EPrime_gender
  genTemp$EPrime_race[genTemp$Trial == t] = EPrime_race
  genTemp$EPrime_fix[genTemp$Trial == t] = EPrime_fix
  
  # check if condition matches up
  genTemp$gen_mismatch[genTemp$Trial == t] = charmatch(EPrime_gender,
                                                       as.character(genTemp$TarGender[genTemp$Trial == t])[1],
                                                       nomatch = 2) - 1 # match = 0, nomatch = 1
  genTemp$race_mismatch[genTemp$Trial == t] = charmatch(EPrime_race,
                                                        as.character(genTemp$TarRace[genTemp$Trial == t])[1],
                                                        nomatch = 2) - 1 # match = 0, nomatch = 1
  genTemp$fix_mismatch[genTemp$Trial == t] = charmatch(EPrime_fix,
                                                       as.character(genTemp$Fix[genTemp$Trial == t])[1],
                                                       nomatch = 2) - 1 # match = 0, nomatch = 1
  
}
newdat = rbind(newdat, genTemp)

# Race task (need to account for first four missed trials)
RTtemp = RTdat[RTdat$Subject == 55 & RTdat$Procedure.Block. == "RaceTask",]
raceTemp = ERPdat[ERPdat$Subject == 55 & ERPdat$Task == "Race",]
for (t in unique(raceTemp$Trial)) {
  RT = RTtemp$TargetFaceRace.RT[RTtemp$Trial == 2 & RTtemp$SubTrial == t+4]
  EPrime_gender = as.character(RTtemp$faceGender[RTtemp$Trial == 2 & RTtemp$SubTrial == t+4])
  EPrime_gender[EPrime_gender == "Female"] = "female"
  EPrime_gender[EPrime_gender == "Male"] = "male"
  EPrime_race = as.character(RTtemp$faceRace[RTtemp$Trial == 2 & RTtemp$SubTrial == t+4])
  EPrime_fix = as.character(RTtemp$FixArea[RTtemp$Trial == 2 & RTtemp$SubTrial == t+4])
  
  # add new columns
  raceTemp$RT[raceTemp$Trial == t] = RT
  raceTemp$EPrime_gender[raceTemp$Trial == t] = EPrime_gender
  raceTemp$EPrime_race[raceTemp$Trial == t] = EPrime_race
  raceTemp$EPrime_fix[raceTemp$Trial == t] = EPrime_fix
  
  # check if condition matches up
  raceTemp$gen_mismatch[raceTemp$Trial == t] = charmatch(EPrime_gender,
                                                         as.character(raceTemp$TarGender[raceTemp$Trial == t])[1],
                                                         nomatch = 2) - 1 # match = 0, nomatch = 1
  raceTemp$race_mismatch[raceTemp$Trial == t] = charmatch(EPrime_race,
                                                          as.character(raceTemp$TarRace[raceTemp$Trial == t])[1],
                                                          nomatch = 2) - 1 # match = 0, nomatch = 1
  raceTemp$fix_mismatch[raceTemp$Trial == t] = charmatch(EPrime_fix,
                                                         as.character(raceTemp$Fix[raceTemp$Trial == t])[1],
                                                         nomatch = 2) - 1 # match = 0, nomatch = 1
}
newdat = rbind(newdat, raceTemp)


# check for mismatches
problem = newdat[newdat$gen_mismatch == 1,] %>% 
  rbind(newdat[newdat$race_mismatch == 1,]) %>% 
  rbind(newdat[newdat$fix_mismatch == 1,])





write.table(newdat, "./2 Race-Gen/AllSubs_acceptedTrials_long_nobe_nobs_withRT.txt", sep="\t", row.names=F)


# add subject mean and mean-centered values for P2

dat = read.delim("./2 Race-Gen/AllSubs_acceptedTrials_long_nobe_nobs_withRT.txt")

# mean calculated separately for each subject and electrode
electrode = as.character(unique(dat$Electrode))
for (i in unique(dat$Subject)) {
  for (e in electrode) {
  # calculate mean
    temp.gen = dat[dat$Subject == i & dat$Electrode == e & dat$Task == "Gender",]
    temp.race = dat[dat$Subject == i & dat$Electrode == e & dat$Task == "Race",]
    dat$P2.mean[dat$Subject == i & dat$Electrode == e & dat$Task == "Gender"] = mean(temp.gen$value)
    dat$P2.mean[dat$Subject == i & dat$Electrode == e & dat$Task == "Race"] = mean(temp.race$value)

    # calculate mean-centered variable
    dat$P2.meancent = dat$value - dat$P2.mean
    
    # scale subject mean-centered variable
    dat$P2.meancent.scale = scale(dat$P2.meancent)
  }
}

write.table(dat, "./2 Race-Gen/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2.txt", sep="\t", row.names=F)

