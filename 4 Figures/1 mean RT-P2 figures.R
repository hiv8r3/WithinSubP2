library(dplyr)
library(ggplot2)
library(export)

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

ERPfix = read.delim("./1 ERP-fix/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2_withP3_meancenteredP3.txt")

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

# no fixation
ggplot(ERPfix, aes(Race, RT, fill = Race)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1) +
  labs(y = "Reaction Time (ms)", x = "Target Race") +
  scale_fill_manual(values=c("white","grey70")) +
  theme_hve() +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(450, 500))


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

ggplot(ERPfix, aes(Race, P2amp)) +
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



# Study 2 -----------------------------------------------------------------

RaceGen = read.delim("./2 Race-Gen/AllSubs_acceptedTrials_long_nobe_nobs_withRT_meancenteredP2_meancenteredP3_withStimRatings.txt")

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

# no fixation
ggplot(RG_racetask, aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time (ms)", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70")) +
  theme_hve() +
  coord_cartesian(ylim = c(410, 510)) +
  theme(legend.position = "bottom") 

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

# no fixation
ggplot(RG_gentask, aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time (ms)", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70")) +
  theme_hve() +
  coord_cartesian(ylim = c(410, 510)) +
  theme(legend.position = "bottom") 


# Separate by participant race (RT) --------------------------------------------

# Race Task
RaceTask_BlackPs = 
  ggplot(filter(RG_racetask, ParRace == "Black"), aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time (ms)", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70")) +
  theme_hve() +
  ggtitle("Black Ps") +
  coord_cartesian(ylim = c(405, 510)) +
  theme(legend.position = "bottom") 

RaceTask_WhitePs = 
  ggplot(filter(RG_racetask, ParRace == "White"), aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time (ms)", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70")) +
  theme_hve() +
  ggtitle("White Ps") +
  coord_cartesian(ylim = c(405, 510)) +
  theme(legend.position = "bottom") 

RaceTask_BlackPs
RaceTask_WhitePs
# ggsave(RaceTask_WhitePs, file="./4 Figures/Stim type/vector files/Study2_RT_RaceTask_WhitePs.tiff", width=7, height=5.5)
# ggsave(RaceTask_BlackPs, file="./4 Figures/Stim type/vector files/Study2_RT_RaceTask_BlackPs.tiff", width=7, height=5.5)

# Gender task
GenTask_BlackPs = 
  ggplot(filter(RG_gentask, ParRace == "Black"), aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time (ms)", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70")) +
  theme_hve() +
  ggtitle("Black Ps") +
  coord_cartesian(ylim = c(405, 510)) +
  theme(legend.position = "bottom") 

GenTask_WhitePs = 
  ggplot(filter(RG_gentask, ParRace == "White"), aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time (ms)", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70")) +
  theme_hve() +
  ggtitle("White Ps") +
  coord_cartesian(ylim = c(405, 510)) +
  theme(legend.position = "bottom") 

GenTask_BlackPs
GenTask_WhitePs

# ggsave(GenTask_WhitePs, file="./4 Figures/Stim type/vector files/Study2_RT_GenTask_WhitePs.tiff", width=7, height=5.5)
# ggsave(GenTask_BlackPs, file="./4 Figures/Stim type/vector files/Study2_RT_GenTask_BlackPs.tiff", width=7, height=5.5)

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


# Separate by participant race (P2)--------------------------------------------

# Race Task 
RaceTask_BlackPs = 
  ggplot(filter(RG_racetask, ParRace == "Black"), aes(TarRace, P2amp, fill = TarGender, shape = TarGender)) +
  stat_summary(fun.y = mean, geom = "point", size=4, position = position_dodge(width=.9), color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1, color = "blue") + 
  scale_shape_manual(values=c(1, 16)) +
  theme_hve() +
  ggtitle("Black Ps") +
  theme(legend.position = "bottom") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue"))

RaceTask_WhitePs = 
  ggplot(filter(RG_racetask, ParRace == "White"), aes(TarRace, P2amp, fill = TarGender, shape = TarGender)) +
  stat_summary(fun.y = mean, geom = "point", size=4, position = position_dodge(width=.9), color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1, color = "blue") + 
  scale_shape_manual(values=c(1, 16)) +
  theme_hve() +
  ggtitle("White Ps") +
  theme(legend.position = "bottom") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue"))

# Export figures to powerpoint as layered images (can ungroup)
# Each time you export something new, it adds it as a slide
# font sizes don't translate for some reason
graph2ppt(RaceTask_WhitePs, file="./4 Figures/Stim type/vector files/Study2_RT_RaceTask.pptx", append = FALSE, width=6.5, height=5)
graph2ppt(RaceTask_BlackPs, file="./4 Figures/Stim type/vector files/Study2_RT_RaceTask.pptx", append = TRUE, width=6.5, height=5)

# Gender Task
GenTask_BlackPs = 
  ggplot(filter(RG_gentask, ParRace == "Black"), aes(TarRace, P2amp, fill = TarGender, shape = TarGender)) +
  stat_summary(fun.y = mean, geom = "point", size=4, position = position_dodge(width=.9), color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1, color = "blue") + 
  scale_shape_manual(values=c(1, 16)) +
  theme_hve() +
  ggtitle("Black Ps") +
  theme(legend.position = "bottom") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue"))

GenTask_WhitePs = 
  ggplot(filter(RG_gentask, ParRace == "White"), aes(TarRace, P2amp, fill = TarGender, shape = TarGender)) +
  stat_summary(fun.y = mean, geom = "point", size=4, position = position_dodge(width=.9), color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2, size = 1, color = "blue") + 
  scale_shape_manual(values=c(1, 16)) +
  theme_hve() +
  ggtitle("White Ps") +
  theme(legend.position = "bottom") +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(1, 5)) +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue"))

# Export figures to powerpoint as layered images (can ungroup)
# Each time you export something new, it adds it as a slide
# font sizes don't translate for some reason
graph2ppt(GenTask_WhitePs, file="./4 Figures/Stim type/vector files/Study2_RT_GenTask.pptx", append = FALSE, width=6.5, height=5)
graph2ppt(GenTask_BlackPs, file="./4 Figures/Stim type/vector files/Study2_RT_GenTask.pptx", append = TRUE, width=6.5, height=5)


# Study 3 -----------------------------------------------------------------

Diss = read.delim("./3 Dissertation/AllSubs_bothTasks_acceptedTrials_long_nobs_nobe_withRT_meancenteredP2.txt")

Diss_racetask = filter(Diss, Task == "RaceTask")
Diss_gentask = filter(Diss, Task == "GenTask")

# RT

# Race Task
win.metafile("./4 Figures/Study3_RT_RaceTask_bar.wmf")

ggplot(Diss_racetask, aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Target Gender")) +
  theme_hve() +
  coord_cartesian(ylim = c(410, 510)) +
  theme(legend.position = "bottom") 

dev.off()

# Gen Task
win.metafile("./4 Figures/Study3_RT_GenTask_bar.wmf")

ggplot(Diss_gentask, aes(TarRace, RT, fill = TarGender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  labs(y = "Reaction Time", x = "Target Race") +
  scale_fill_manual(values=c("black","grey70"), guide = guide_legend(title = "Target Gender")) +
  theme_hve() +
  coord_cartesian(ylim = c(410, 510)) +
  theme(legend.position = "bottom") 

dev.off()

# P2

# Race Task
win.metafile("./4 Figures/Study3_P2_RaceTask_point.wmf")

ggplot(Diss_racetask, aes(TarRace, meanAmp, fill = TarGender, shape = TarGender)) +
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

# Race Task
win.metafile("./4 Figures/Study3_P2_GenTask_point.wmf")

ggplot(Diss_gentask, aes(TarRace, meanAmp, fill = TarGender, shape = TarGender)) +
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

