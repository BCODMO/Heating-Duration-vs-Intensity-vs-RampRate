#Code for Figure S2 of "A universal, empirically derived proxy for coral bleaching susceptibility"

#setwd to source file location

library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)

#Load file
KAUST_PR<-read.csv("KAUST_PR_clean.csv")
View(KAUST_PR)
str(KAUST_PR)

KAUST_PR$Incubation_temp<-as.factor(KAUST_PR$Incubation_temp)
KAUST_PR$Temp<-as.factor(KAUST_PR$Temp)
KAUST_PR$Date<-as.factor(KAUST_PR$Date)

#Subset data
Acr_PR<-subset(KAUST_PR, Species=="Acropora")
Por_PR<-subset(KAUST_PR, Species=="Porites")

#Acropora Net PS
Prolonged_Acr_PS<-lmer(NetPS ~ Ramp.point*Site + (1|Geno),data=Heatwave_Acr)
sjPlot::plot_model(Prolonged_Acr_PS, type="diag")
step(Prolonged_Acr_PS, reduce.random = F)

Prolonged_Acr_PS_final<-lmer(NetPS ~ Ramp.point+Site + (1|Geno),data=Heatwave_Acr)
anova(Prolonged_Acr_PS_final)

#Do full comparisons or set pairwise comparisons here
print(emmeans(Prolonged_Acr_PS_final, list(pairwise ~ Ramp.point)), adjust = c("mvt"))
print(emmeans(Prolonged_Acr_PS_final, list(pairwise ~ Site)), adjust = c("mvt"))

#Acropora Resp
Prolonged_Acr_Resp<-lmer(Resp ~ Ramp.point*Site + (1|Geno),data=Heatwave_Acr)
sjPlot::plot_model(Prolonged_Acr_Resp, type="diag")
step(Prolonged_Acr_Resp, reduce.random = F)

Prolonged_Acr_Resp_final<-lmer(Resp ~ Ramp.point + (1|Geno),data=Heatwave_Acr)
anova(Prolonged_Acr_Resp_final)

print(emmeans(Prolonged_Acr_Resp_final, list(pairwise ~ Ramp.point)), adjust = c("mvt"))

#Porites Net PS
Prolonged_Por_PS<-lmer(NetPS ~ Ramp.point*Site + (1|Geno),data=Heatwave_Por)
sjPlot::plot_model(Prolonged_Por_PS, type="diag")
step(Prolonged_Por_PS, reduce.random = F)

Prolonged_Por_PS_final<-lmer(NetPS ~ Ramp.point + (1|Geno),data=Heatwave_Por)
anova(Prolonged_Por_PS_final)

print(emmeans(Prolonged_Por_PS_final, list(pairwise ~ Ramp.point)), adjust = c("mvt"))

#Porites Resp
Prolonged_Por_Resp<-lmer(Resp ~ Ramp.point*Site + (1|Geno),data=Heatwave_Por)
sjPlot::plot_model(Prolonged_Por_Resp, type="diag")
step(Prolonged_Por_Resp, reduce.random = F) #N.S.

#### Plot Acropora ####

Acr_PR$Ramp<-as.factor(Acr_PR$Ramp)
print(levels(Acr_PR$Ramp))
Acr_PR$Ramp = factor(Acr_PR$Ramp,levels(Acr_PR$Ramp)[c(1,3,2)])

NetPS_Acr <- ggplot(data=Acr_PR, 
                    aes(x=Timepoint, y=NetPS, label=Temp, fill=interaction(Ramp,Site,sep="-",lex.order=TRUE))) +
  scale_fill_manual(values = c("dodgerblue", "royalblue3", "sienna1", "darkorange3", "red","red3"), name = "Ramp rate") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0) +
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 15, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature") + ylab(label = "Net Photosynthesis (µmol O2 cm-2 h-1)") + ggtitle("Acropora") + theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom") + scale_y_continuous(limits = c(-0.8,1.4), breaks = c(-0.5,0.0,0.5,1.0)) +
        geom_hline(yintercept=0, linetype="dashed", color = "black")
NetPS_Acr

Resp_Acr <- ggplot(data=Acr_PR, 
                   aes(x=Timepoint, y=-Resp, label=Temp, fill=interaction(Ramp,Site,sep="-",lex.order=TRUE))) +
  scale_fill_manual(values = c("dodgerblue", "royalblue3", "sienna1", "darkorange3", "red","red3"), name = "Ramp rate") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0) +
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 15, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature") + ylab(label = "Respiration (µmol O2 cm-2 h-1)") + ggtitle("Acropora") + theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom") + scale_y_continuous(limits = c(0,1.5), breaks = c(0.0,0.5,1.0,1.5))
Resp_Acr

#### Plot Porites ####

Por_PR$Ramp<-as.factor(Por_PR$Ramp)
print(levels(Por_PR$Ramp))
Por_PR$Ramp = factor(Por_PR$Ramp,levels(Por_PR$Ramp)[c(1,3,2)])

NetPS_Por <- ggplot(data=Por_PR, 
                    aes(x=Timepoint, y=NetPS, label=Temp, fill=interaction(Ramp,Site,sep="-",lex.order=TRUE))) +
  scale_fill_manual(values = c("dodgerblue", "royalblue3", "sienna1", "darkorange3", "red","red3"), name = "Ramp rate") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0) +
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 15, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature") + ylab(label = "Net Photosynthesis (µmol O2 cm-2 h-1)") + ggtitle("Porites") + theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom") + scale_y_continuous(limits = c(-0.8,1.4), breaks = c(-0.5,0.0,0.5,1.0)) +
        geom_hline(yintercept=0, linetype="dashed", color = "black")
NetPS_Por

Resp_Por <- ggplot(data=Por_PR, 
                   aes(x=Timepoint, y=-Resp, label=Temp, fill=interaction(Ramp,Site,sep="-",lex.order=TRUE))) +
  scale_fill_manual(values = c("dodgerblue", "royalblue3", "sienna1", "darkorange3", "red","red3"), name = "Ramp rate") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0) +
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 15, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature") + ylab(label = "Respiration (µmol O2 cm-2 h-1)") + ggtitle("Porites") + theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom") + scale_y_continuous(limits = c(0,1.5), breaks = c(0.0,0.5,1.0,1.5))
Resp_Por

#END


