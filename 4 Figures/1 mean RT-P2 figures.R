library(dplyr)
library(ggplot2)

theme_hve = function (base_size = 20, 
                      base_family = "", 
                      base_line_size = base_size/22, 
                      base_rect_size = base_size/22) {
  theme_grey(base_size = base_size, 
             base_family = base_family, 
             base_line_size = base_line_size, 
             base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey20"), 
          panel.grid = element_blank(), 
          panel.grid.minor = element_blank(), 
          strip.background = element_rect(fill = "grey85", colour = "grey20"), 
          legend.key = element_rect(fill = "white", colour = NA), 
          plot.title = element_text(hjust = 0.5),
          complete = TRUE)
}


# Study 1 -----------------------------------------------------------------

ERPfix = read.delim("./1 ERP-fix/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2.txt")

# bar graph
# RT
win.metafile("./4 Figures/Study1_RT_bar.wmf")

ggplot(ERPfix, aes(Race, RT, fill = Fix)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1) +
  labs(y = "Reaction Time", x = "Target Race") +
  scale_fill_manual(values=c("white","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(425, 525))

dev.off()


# P2
win.metafile("./4 Figures/Study1_P2_point.wmf")

ggplot(ERPfix, aes(Race, MeanAmp, shape = Fix)) +
  stat_summary(fun.y = mean, geom = "point", size=4, position = position_dodge(width=.9), color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1, color = "blue") + 
  labs(y = "P2 amplitude", x = "Target Race") +
  scale_shape_manual(values=c(1, 16)) +
  #scale_fill_manual(values=c("white","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  theme(legend.position = "bottom") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue"))

dev.off()

# Study 2 -----------------------------------------------------------------

RaceGen = read.delim("./2 Race-Gen/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2.txt")

RG_racetask = filter(RaceGen, Task == "Race")
RG_gentask = filter(RaceGen, Task == "Gender")

facet_labels = c(eyes = "Eyes fixation", forehead = "Forehead fixation")

#RT

# Race Task
win.metafile("./4 Figures/Study2_RT_RaceTask_bar.wmf")

ggplot(RG_racetask, aes(TarRace, RT, fill = TarGender)) +
  facet_wrap(~Fix, labeller=labeller(Fix = facet_labels)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  coord_cartesian(ylim = c(410, 510)) +
  theme(legend.position = "bottom") 

dev.off()

# Gen Task
win.metafile("./4 Figures/Study2_RT_GenTask_bar.wmf")

ggplot(RG_gentask, aes(TarRace, RT, fill = TarGender)) +
  facet_wrap(~Fix, labeller=labeller(Fix = facet_labels)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  coord_cartesian(ylim = c(410, 510)) +
  theme(legend.position = "bottom") 

dev.off()

# P2

# Race Task
win.metafile("./4 Figures/Study2_P2_RaceTask_point.wmf")

ggplot(RG_racetask, aes(TarRace, value, fill = TarGender, shape = TarGender)) +
  facet_wrap(~Fix, labeller=labeller(Fix = facet_labels)) +
  stat_summary(fun.y = mean, geom = "point", size=4, position = position_dodge(width=.9), color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1, color = "blue") + 
  scale_shape_manual(values=c(1, 16)) +
  theme_hve() +
  theme(legend.position = "bottom") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue"))

dev.off()

# Gender Task
win.metafile("./4 Figures/Study2_P2_GenderTask_point.wmf")

ggplot(RG_gentask, aes(TarRace, value, fill = TarGender, shape = TarGender)) +
  facet_wrap(~Fix, labeller=labeller(Fix = facet_labels)) +
  stat_summary(fun.y = mean, geom = "point", size=4, position = position_dodge(width=.9), color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1, color = "blue") + 
  scale_shape_manual(values=c(1, 16)) +
  theme_hve() +
  theme(legend.position = "bottom") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue"))

dev.off()

# Study 3 -----------------------------------------------------------------

Diss = read.delim("./3 Dissertation/AllSubs_bothTasks_acceptedTrials_long_nobs_nobe_withRT_meancenteredP2.txt")

Diss_racetask = filter(Diss, Task == "RaceTask")
Diss_gentask = filter(Diss, Task == "GenTask")

# Race Task

ggplot(Diss_racetask, aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  coord_cartesian(ylim = c(400, 500)) +
  ggtitle("Race task")

ggplot(Diss_racetask, aes(TarRace, meanAmp, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "P2 amplitude", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  #  coord_cartesian(ylim = c(400, 500)) +
  ggtitle("Race task")

# Gen Task

ggplot(Diss_gentask, aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  coord_cartesian(ylim = c(400, 500)) +
  ggtitle("Race task")

ggplot(Diss_gentask, aes(TarRace, meanAmp, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "P2 amplitude", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Fixation")) +
  theme_hve() +
  #  coord_cartesian(ylim = c(400, 500)) +
  ggtitle("Race task")



temp = ERPfix[ERPfix$Electrode %in% c("CZ", "CPZ", "PZ"),c(1:4,6)]
