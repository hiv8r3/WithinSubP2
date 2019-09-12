# Probe interactions

require(lme4)
require(lmerTest)
require(dplyr)
require(knitr)


dat = read.delim("AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2.txt")

# deprecated, hasn't been run with corrected data #

# Gender task -------------------------------------------------------------

gendat = filter(dat, Task == "Gender")

# Add predictors, effect coding
gendat$TarRace.e = -1
gendat$TarRace.e[gendat$TarRace == "White"] = 1

gendat$TarGender.e = -1
gendat$TarGender.e[gendat$TarGender == "male"] = 1

gendat$ParRace.e = -1
gendat$ParRace.e[gendat$ParRace == "White"] = 1

# just include within and between
b1 = lmer(RT ~ P2.mean+P2.meancent.scale + (1|Subject) + (1|Electrode), data = gendat)
sum.b1 = summary(b1)

# just include within and condition
b1b = lmer(RT ~ P2.meancent.scale*TarRace.e*TarGender.e*ParRace.e + (1|Subject) + (1|Electrode), data = gendat)
sum.b1b = summary(b1b)

### Summary:
# 1) Significant Slope x ParRace interaction
# 2) Significant Slope x ParRace x TarGender interaction
# 3) Significant Slope x ParRace x TarRace interaction
# 4) Significant Slope x ParRace x TarGender x TarRace interaction

# Use dummy coding to probe
gendat$TarRace.d = 0
gendat$TarRace.d[gendat$TarRace == "White"] = 1

gendat$TarGender.d = 0
gendat$TarGender.d[gendat$TarGender == "male"] = 1

gendat$ParRace.d = 0
gendat$ParRace.d[gendat$ParRace == "White"] = 1

lmer(RT ~ P2.meancent.scale*TarRace.d*TarGender.d*ParRace.d + (1|Subject) + (1|Electrode), data = gendat) %>% 
  summary()


# Race task ---------------------------------------------------------------

racedat = filter(dat, Task == "Race")

# Add predictors, effect coding
racedat$TarRace.e = -1
racedat$TarRace.e[racedat$TarRace == "White"] = 1

racedat$TarGender.e = -1
racedat$TarGender.e[racedat$TarGender == "male"] = 1

racedat$ParRace.e = -1
racedat$ParRace.e[racedat$ParRace == "White"] = 1


# just include within and between
b1b = lmer(RT ~ P2.meancent.scale*TarRace.e*TarGender.e*ParRace.e + (1|Subject) + (1|Electrode), data = racedat)

sum.b1b = summary(b1b)
sum.b1b$call
sum.b1b$varcor
kable(round(sum.b1b$coefficients, digits=3)) %>% 
  kable_styling(full_width = F, position = "left")

