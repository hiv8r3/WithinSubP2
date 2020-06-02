# Positive plotted up
library(ggplot2)
library(colorspace)
library(grid)
library(dplyr)
library(tidyr)

# Set basic aspects of plots ----------------------------------------------

P2box = annotate("rect",    #P2
                 xmin=130, xmax=190, ymin=-Inf, ymax=Inf, 
                 alpha=0,
                 #fill="#F0E0FF",
                 color="black",
                 linetype="dashed") 

ERPline = geom_line(lwd=1.1,
                    aes(color = Condition, linetype = Condition))

none = element_blank() 


# 1. ERP-fix --------------------------------------------------------------

# Read in data ------------------------------------------------------------
ERPfix = read.delim("4 Figures/Grand averages/1 ERP-fix/For_plotting_Cat_P2_noBS.txt")


# Plots -------------------------------------------------------------------

condColors4 <- c("Black_eyes" = "blueviolet",
                 "Black_fore" = "blueviolet",
                 "White_eyes" = "darkblue",
                 "White_fore" = "darkblue")

condLinetype4 <- c("Black_eyes" = "solid",
                   "Black_fore" = "longdash",
                   "White_eyes" = "solid",
                   "White_fore" = "longdash")

# CPZ (Race Task)
ggplot(ERPfix, aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition, linetype = Condition)) +
  P2box + 
  # geom_ribbon(aes(ymax=CPZ+CPZ.SE, ymin=CPZ-CPZ.SE, color = Condition),
  #             alpha = .14,
  #             linetype = "dotted",
  #             size = .3) +
  theme_bw() + 
  theme(panel.grid.major = none,
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-100, 1000),
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors4) +
  scale_linetype_manual(values=condLinetype4)

ggsave("4 Figures/Grand averages/1 ERP-fix/RaceXFix_RaceTask_CPZ_fullepoch.jpg", width=10, height=5, units="in")


# CPZ (Race Task) - no fixation
condColors2 <- c("Black_eyes" = "blueviolet",
                 "White_eyes" = "darkblue")

ggplot(filter(ERPfix, Fix == "eyes"), aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition)) +
  P2box + 
  # geom_ribbon(aes(ymax=CPZ+CPZ.SE, ymin=CPZ-CPZ.SE, color = Condition),
  #             alpha = .14,
  #             linetype = "dotted",
  #             size = .3) +
  theme_bw() + 
  theme(panel.grid.major = none,
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) +
  scale_x_continuous("Time (ms)",
                     limits=c(-100, 1000),
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors2) 

ggsave("4 Figures/Grand averages/1 ERP-fix/RaceXFix_RaceTask_CPZ_fullepoch_nofix.jpg", width=10, height=5, units="in")

# 2. Race-Gen -------------------------------------------------------------

# Read in data ------------------------------------------------------------

#separate by participant race
gentask = rbind(read.delim("4 Figures/Grand averages/2 Race-Gen/Gender_RaceXGenXFix_whiteSubjects.txt"),
                read.delim("4 Figures/Grand averages/2 Race-Gen/Gender_RaceXGenXFix_blackSubjects.txt"))

racetask = rbind(read.delim("4 Figures/Grand averages/2 Race-Gen/Race_RaceXGenXFix_whiteSubjects.txt"),
                 read.delim("4 Figures/Grand averages/2 Race-Gen/Race_RaceXGenXFix_blackSubjects.txt"))

# add SE for CPZ
# sd(x)/sqrt(length(x)) where x is individual averages
# for white Ps, length = 32
# for black Ps, length = 34

# SD.gen = gentask %>%  
#   select(Time, Condition, ParRace, CPZ) %>% 
#   group_by(Time, Condition, ParRace) %>% 
#   summarise(CPZ.sd = sd(CPZ, na.rm=TRUE)) %>% #THIS ISN'T WORKING AND I DON'T KNOW WHY
#   as.data.frame() 
# 
# gentask$CPZ.SE = NA
# gentask$CPZ.SE[gentask$ParRace == "White"] = gentask$CPZ.sd[gentask$ParRace == "White"]/sqrt(32) 
# gentask$CPZ.SE[gentask$ParRace == "Black"] = gentask$CPZ.sd[gentask$ParRace == "Black"]/sqrt(34) 
# 
# racetask$CPZ.SE = NA
# racetask$CPZ.SE[racetask$ParRace == "White"] = racetask$CPZ.sd[racetask$ParRace == "White"]/sqrt(32) 
# racetask$CPZ.SE[racetask$ParRace == "Black"] = racetask$CPZ.sd[racetask$ParRace == "Black"]/sqrt(34) 

# Plots  --------------------------------------
labels <- c(White = "White Participants", Black = "Black Participants")

condColors8 <- c("Black_male_eyes" = "blueviolet",
                 "Black_male_forehead" = "blueviolet",
                 "Black_female_eyes" = "darkgoldenrod2",
                 "Black_female_forehead" = "darkgoldenrod2",
                 "White_male_eyes" = "darkblue",
                 "White_male_forehead" = "darkblue",
                 "White_female_eyes" = "forestgreen",
                 "White_female_forehead" = "forestgreen")

condLinetype8 <- c("Black_male_eyes" = "solid",
                   "Black_male_forehead" = "longdash",
                   "Black_female_eyes" = "solid",
                   "Black_female_forehead" = "longdash",
                   "White_male_eyes" = "solid",
                   "White_male_forehead" = "longdash",
                   "White_female_eyes" = "solid",
                   "White_female_forehead" = "longdash")

# CPZ (Gender Task)
ggplot(gentask, aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition, linetype = Condition)) +
  P2box + 
  facet_grid(ParRace~., labeller=labeller(ParRace = labels)) +
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14.5), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors8) +
  scale_linetype_manual(values=condLinetype8)

ggsave("4 Figures/Grand averages/2 Race-Gen/RaceXGenXFix_GenderTask_CPZ_fullepoch.jpg", width=10, height=8, units="in")

# CPZ (Race Task)
ggplot(racetask, aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition, linetype = Condition)) +
  P2box + 
  facet_grid(ParRace~., labeller=labeller(ParRace = labels)) +
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14.5), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors8) +
  scale_linetype_manual(values=condLinetype8)

ggsave("4 Figures/Grand averages/2 Race-Gen/RaceXGenXFix_RaceTask_CPZ_fullepoch.jpg", width=10, height=8, units="in")

# All electrodes (Race Task)
long = gather(racetask, Electrode, amp, CZ:CP4)

ggplot(filter(long, Condition == "Black_female_eyes" & ParRace == "Black"), aes(Time, amp, group = Electrode)) + 
  geom_line(lwd=1,aes(color = Electrode)) +
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") 

ggsave("4 Figures/Grand averages/2 Race-Gen/RaceXGenXFix_RaceTask_allelec_fullepoch.jpg", width=14, height=8, units="in")

# CPZ (Gender Task) - no fixation
condColors4 <- c("Black_male_eyes" = "blueviolet",
                 "Black_female_eyes" = "darkgoldenrod2",
                 "White_male_eyes" = "darkblue",
                 "White_female_eyes" = "forestgreen")

ggplot(filter(gentask, Fix == "eyes"), aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition)) +
  P2box + 
  facet_grid(ParRace~., labeller=labeller(ParRace = labels)) +
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14.5), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors4) 

ggsave("4 Figures/Grand averages/2 Race-Gen/RaceXGenXFix_GenderTask_CPZ_fullepoch_nofix.jpg", width=10, height=8, units="in")

# CPZ (Race Task) - no fixation
ggplot(filter(racetask, Fix == "eyes"), aes(Time, CPZ, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition)) +
  P2box + 
  facet_grid(ParRace~., labeller=labeller(ParRace = labels)) +
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14.5), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors4)

ggsave("4 Figures/Grand averages/2 Race-Gen/RaceXGenXFix_RaceTask_CPZ_fullepoch_nofix.jpg", width=10, height=8, units="in")

# 3. Dissertation -------------------------------------------------------------

# Read in data ------------------------------------------------------------

dissdata = read.delim("4 Figures/Grand averages/3 Dissertation/BothTasks_grandaverages_noBS.txt")
dissdata$Condition = paste(dissdata$TarRace, dissdata$TarGender, sep="_")

# Plots  --------------------------------------
condColorsDiss <- c("black_male" = "blueviolet",
                    "black_female" = "darkgoldenrod2",
                    "white_male" = "darkblue",
                    "white_female" = "forestgreen")

condLinetypeDiss <- c("black_male" = "solid",
                      "black_female" = "solid",
                      "white_male" = "solid",
                      "white_female" = "solid")

# CPZ (Gender Task)
ggplot(dissdata[dissdata$Task == "GenTask",], aes(Time, CPZ_ind.avg_grand.avg, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition, linetype = Condition)) +
  P2box + 
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColorsDiss) +
  scale_linetype_manual(values=condLinetypeDiss)

ggsave("4 Figures/Grand averages/3 Dissertation/RaceXGen_GenderTask_CPZ_fullepoch.jpg", width=10, height=6, units="in")

# CPZ (Race Task)
ggplot(dissdata[dissdata$Task == "RaceTask",], aes(Time, CPZ_ind.avg_grand.avg, group = Condition)) + 
  geom_line(lwd=1,aes(color = Condition, linetype = Condition)) +
  P2box + 
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColorsDiss) +
  scale_linetype_manual(values=condLinetypeDiss)

ggsave("4 Figures/Grand averages/3 Dissertation/RaceXGen_RaceTask_CPZ_fullepoch.jpg", width=10, height=6, units="in")



# Grand-grand averages comparing tasks ------------------------------------


# 2. Race-Gen -------------------------------------------------------------

s2dat = rbind(read.delim("4 Figures/Grand averages/2 Race-Gen/Gender_RaceXGenXFix_whiteSubjects.txt"),
              read.delim("4 Figures/Grand averages/2 Race-Gen/Gender_RaceXGenXFix_blackSubjects.txt"),
              read.delim("4 Figures/Grand averages/2 Race-Gen/Race_RaceXGenXFix_whiteSubjects.txt"),
              read.delim("4 Figures/Grand averages/2 Race-Gen/Race_RaceXGenXFix_blackSubjects.txt"))

grandgrand = select(s2dat, Time, Task, CZ:CP4) %>% 
  group_by(Task, Time) %>% 
  summarise_all(list(mean = mean), na.rm=T)

# CPZ
ggplot(grandgrand, aes(Time, CPZ_mean, group = Task)) + 
  ggtitle("Study 2- CPZ") +
  geom_line(lwd=1,aes(color = Task)) +
  P2box + 
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)")

ggsave("4 Figures/Grand averages/2 Race-Gen/GrandGrand/Study2_CPZ_fullepoch.jpg", width=14, height=8, units="in")

# PZ
ggplot(grandgrand, aes(Time, PZ_mean, group = Task)) + 
  ggtitle("Study 2- PZ") +
  geom_line(lwd=1,aes(color = Task)) +
  P2box + 
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)")

ggsave("4 Figures/Grand averages/2 Race-Gen/GrandGrand/Study2_PZ_fullepoch.jpg", width=14, height=8, units="in")


# 3. Dissertation ---------------------------------------------------------

dissdata = read.delim("4 Figures/Grand averages/3 Dissertation/BothTasks_grandaverages_noBS.txt")

grandgrand = select(dissdata, Time, Task, CPZ_ind.avg_grand.avg, PZ_ind.avg_grand.avg) %>% 
  group_by(Task, Time) %>% 
  summarise_all(list(mean = mean), na.rm=T) %>% 
  rename(CPZ = CPZ_ind.avg_grand.avg_mean,
         PZ = PZ_ind.avg_grand.avg_mean)

# CPZ
ggplot(grandgrand, aes(Time, CPZ, group = Task)) + 
  ggtitle("Study 3- CPZ") +
  geom_line(lwd=1,aes(color = Task)) +
  P2box + 
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)")

ggsave("4 Figures/Grand averages/3 Dissertation/GrandGrand/Study3_CPZ_fullepoch.jpg", width=14, height=8, units="in")

# PZ
ggplot(grandgrand, aes(Time, PZ, group = Task)) + 
  ggtitle("Study 3- PZ") +
  geom_line(lwd=1,aes(color = Task)) +
  P2box + 
  theme_bw() + 
  theme(panel.grid.major = none, 
        panel.grid.minor = none,
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 18)) + 
  scale_x_continuous("Time (ms)", 
                     limits=c(-100, 1000), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_continuous(breaks=c(-4, -2, 0, 2, 4, 6, 8, 10, 12, 14))+
  coord_cartesian(ylim =c(-4.5, 14), expand= T) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)")

ggsave("4 Figures/Grand averages/3 Dissertation/GrandGrand/Study3_PZ_fullepoch.jpg", width=14, height=8, units="in")

