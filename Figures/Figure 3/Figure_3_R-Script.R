#Code for Figure 3 of "Assessing the roles of heating rate and intensity on the response of corals to thermal stress"

#setwd to source file location


library(lmerTest)
library(emmeans)
library(sjPlot)

#Load file
KAUST_2019<-read.csv("KAUST_Final_clean.csv")
View(KAUST_2019)
str(KAUST_2019)

#Subset data
Prolonged_final<-subset(KAUST_2019,Experiment=="Prolonged")
Prolonged_Acr<-subset(Prolonged_final, Species=="Acropora")
Prolonged_Por<-subset(Prolonged_final, Species=="Porites")

#### GLMMs for Protein, Chl a, and Sym counts
# Response variables named: Sym_density / Pro_cm2 / Chla_cm2

###############################
######### Protein data ########
###############################

#Acropora
Pro_log<-log(Prolonged_Acr$Pro_cm2)
Protein_Acr<-lmer(Pro_log ~ Treatment*Site + (1|Tank) + (1|Geno),data=Prolonged_Acr) 
sjPlot::plot_model(Protein_Acr, type="diag") # Model diagnostics - checking model assumptions
step(Protein_Acr,reduce.random=FALSE)

Protein_Acr_final<-lmer(Pro_log ~ Treatment + (1|Tank) + (1|Geno), data=Prolonged_Acr) #final model selected based on 'step' output
anova(Protein_Acr_final)

#For custom a priori comparisons - define where each treatment is in vector of treatment levels (order based on sjplot line)
F33.5 = c(1, 0, 0, 0)
Control = c(0, 1, 0, 0)
S33.5 = c(0, 0, 1, 0)
S35 = c(0, 0, 0, 1)

emm1 = emmeans(Protein_Acr_final, specs = ~ Treatment) #produce list emmeans
emm1 #view the list

#produce the list of contrasts - comparing treatments within slow and fast ramps, and same temperatures across ramps
#if there are multiple temp treatments within each ramp, also want to compare overall slow vs. overall fast ramps
contrast(emm1, method = list("S33.5 - F33.5" = S33.5 - F33.5,
                             "S33.5 - S35" = S33.5 - S35,
                             "Control - S33.5" = Control - S33.5,
                             "Control - S35" = Control - S35,
                             "Control - F33.5" = Control - F33.5), adjust = c("mvt"))

#Porites
Pro_log<-log(Prolonged_Por$Pro_cm2)
Protein_Pro<-lmer(Pro_log ~ Treatment*Site + (1|Tank) + (1|Geno),data=Prolonged_Por) 
sjPlot::plot_model(Protein_Pro, type="diag")
step(Protein_Pro, reduce.random = F)

Protein_Pro_final<-lmer(Pro_log ~ Treatment*Site + (1|Tank) + (1|Geno),data=Prolonged_Por)
anova(Protein_Pro_final)

#A priori comparisons
F33.5 = c(1, 0, 0, 0, 0, 0)
F35 = c(0, 1, 0, 0, 0, 0)
Control = c(0, 0, 1, 0, 0, 0)
S33.5 = c(0, 0, 0, 1, 0, 0)
S35 = c(0, 0, 0, 0, 1, 0)
S36.5 = c(0, 0, 0, 0, 0, 1)

Slow = c(0, 0, 0, 1, 1, 0)
Fast = c(1, 1, 0, 0, 0, 0)

emm2 = emmeans(Protein_Pro_final, specs = ~ Treatment|Site)
emm2

#Above are the contrasts between treatments that we're interested in, separated by Site, as there was 
#an interactive effect of treatment x site.
contrast(emm2, method = list("S33.5 - F33.5" = S33.5 - F33.5,
                             "S35 - F35" = S35 - F35,
                             "S33.5 - S35" = S33.5 - S35,
                             "S35 - S36.5" = S35 - S36.5,
                             "S33.5 - S36.5" = S33.5 - S36.5,
                             "F33.5 - F35" = F33.5 - F35,
                             "Slow - Fast" = Slow - Fast,
                             "Control - F33.5" = Control - F33.5,
                             "Control - F35" = Control - F35,
                             "Control - S33.5" = Control - S33.5,
                             "Control - S35" = Control - S35,
                             "Control - S36.5" = Control - S36.5), by = "Site", adjust = c("mvt"))

###############################
######### Chl a data ##########
###############################

#Acropora
Chla_log<-log(Prolonged_Acr$Chla_cm2)
Chla_Acr<-lmer(Chla_log ~ Treatment*Site + (1|Tank) + (1|Geno),data=Prolonged_Acr)
sjPlot::plot_model(Chla_Acr, type="diag")
step(Chla_Acr, reduce.random = F)

Chla_Acr_final<-lmer(Chla_log ~ Treatment + (1|Tank) + (1|Geno),data=Prolonged_Acr)
anova(Chla_Acr_final)

#a priori comparisons
F33.5 = c(1, 0, 0, 0)
Control = c(0, 1, 0, 0)
S33.5 = c(0, 0, 1, 0)
S35 = c(0, 0, 0, 1)

emm3 = emmeans(Chla_Acr_final, specs = ~ Treatment)
emm3

contrast(emm3, method = list("S33.5 - F33.5" = S33.5 - F33.5,
                             "S33.5 - S35" = S33.5 - S35,
                             "Control - F33.5" = Control - F33.5,
                             "Control - S33.5" = Control - S33.5,
                             "Control - S35" = Control - S35), adjust = c("mvt"))

#CPorites
Chla_log<-log(Prolonged_Por$Chla_cm2)
Chla_Por<-lmer(Chla_log ~ Treatment*Site + (1|Tank) + (1|Geno),data=Prolonged_Por)
sjPlot::plot_model(Chla_Por, type="diag")
step(Chla_Por, reduce.random = F)

Chla_Por_final<-lmer(Chla_log ~ Treatment + (1|Tank) + (1|Geno),data=Prolonged_Por)
anova(Chla_Por_final)

F33.5 = c(1, 0, 0, 0, 0, 0)
F35 = c(0, 1, 0, 0, 0, 0)
Control = c(0, 0, 1, 0, 0, 0)
S33.5 = c(0, 0, 0, 1, 0, 0)
S35 = c(0, 0, 0, 0, 1, 0)
S36.5 = c(0, 0, 0, 0, 0, 1)

Slow = c(0, 0, 0, 1, 1, 0)
Fast = c(1, 1, 0, 0, 0, 0)

emm4 = emmeans(Chla_Por_final, specs = ~ Treatment)
emm4

contrast(emm4, method = list("S33.5 - F33.5" = S33.5 - F33.5,
                             "S35 - F35" = S35 - F35,
                             "S33.5 - S35" = S33.5 - S35,
                             "S35 - S36.5" = S35 - S36.5,
                             "S33.5 - S36.5" = S33.5 - S36.5,
                             "F33.5 - F35" = F33.5 - F35,
                             "Slow - Fast" = Slow - Fast,
                             "Control - F33.5" = Control - F33.5,
                             "Control - F35" = Control - F35,
                             "Control - S33.5" = Control - S33.5,
                             "Control - S35" = Control - S35,
                             "Control - S36.5" = Control - S36.5), adjust = c("mvt"))

#####################################
######### Sym Density data ##########
#####################################

#Acropora
Sym_density_log<-log(Prolonged_Acr$Sym_density)
Sym_density_Acr<-lmer(Sym_density_log ~ Treatment*Site + (1|Tank) + (1|Geno),data=Prolonged_Acr)
sjPlot::plot_model(Sym_density_Acr, type="diag")
step(Sym_density_Acr, reduce.random = F) #N.S.

#Porites
Sym_density_Por<-lmer(Sym_density ~ Treatment*Site + (1|Tank) + (1|Geno),data=Prolonged_Por)
sjPlot::plot_model(Sym_density_Por, type="diag")
step(Sym_density_Por, reduce.random = F)

Sym_density_Por_final<-lmer(Sym_density ~ Treatment + (1|Tank) + (1|Geno),data=Prolonged_Por)
anova(Sym_density_Por_final)

#a priori comparisons
F33.5 = c(1, 0, 0, 0, 0, 0)
F35 = c(0, 1, 0, 0, 0, 0)
Control = c(0, 0, 1, 0, 0, 0)
S33.5 = c(0, 0, 0, 1, 0, 0)
S35 = c(0, 0, 0, 0, 1, 0)
S36.5 = c(0, 0, 0, 0, 0, 1)

Slow = c(0, 0, 0, 1, 1, 0)
Fast = c(1, 1, 0, 0, 0, 0)

emm5 = emmeans(Sym_density_Por_final, specs = ~ Treatment)
emm5

contrast(emm5, method = list("S33.5 - F33.5" = S33.5 - F33.5,
                             "S35 - F35" = S35 - F35,
                             "S33.5 - S35" = S33.5 - S35,
                             "S35 - S36.5" = S35 - S36.5,
                             "S33.5 - S36.5" = S33.5 - S36.5,
                             "F33.5 - F35" = F33.5 - F35,
                             "Slow - Fast" = Slow - Fast,
                             "Control - F33.5" = Control - F33.5,
                             "Control - F35" = Control - F35,
                             "Control - S33.5" = Control - S33.5,
                             "Control - S35" = Control - S35,
                             "Control - S36.5" = Control - S36.5), adjust = c("mvt"))

###################################################
######### Plot Prolonged exp. final data ##########
###################################################

library(ggplot2)
Prolonged_final$Temp<-as.factor(Prolonged_final$Temp)
print(levels(Prolonged_final$Temp))

Prolonged_final$Ramp<-as.factor(Prolonged_final$Ramp)
print(levels(Prolonged_final$Ramp))
Prolonged_final$Ramp = factor(Prolonged_final$Ramp,levels(Prolonged_final$Ramp)[c(1,3,2)])

Protein <- ggplot(data=Prolonged_final, 
                  aes(x=Temp, y=Pro_cm2, label=Temp, fill=interaction(Ramp,Site,sep="-",lex.order=TRUE))) +
  scale_fill_manual(values = c("dodgerblue", "royalblue3", "sienna1", "darkorange3", "red","red3"), drop = FALSE, name = "Ramp rate") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ #this can also be used but rows and columns have to be specified -> facet_grid(. ~ experiment)
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
        strip.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(3, "lines"))

Protein + xlab(label = "Temperature treatment") + ylab(label = "Protein (µg cm-2)")+
  theme(axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=18, face="plain"),
        legend.position="bottom")

Chla <- ggplot(data=Prolonged_final, 
               aes(x=Temp, y=Chla_cm2, label=Temp, fill=interaction(Ramp,Site,sep="-",lex.order=TRUE))) +
  scale_fill_manual(values = c("dodgerblue", "royalblue3", "sienna1", "darkorange3", "red","red3"), name = "Ramp rate") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7)+
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ #this can also be used but rows and columns have to be specified -> facet_grid(. ~ experiment)
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
        strip.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(3, "lines"))

Chla + xlab(label = "Temperature treatment") + ylab(label = "Chl a (µg cm-2)")+
  theme(axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=18, face="plain"),
        legend.position="bottom")

Symbiont <- ggplot(data=Prolonged_final, 
                   aes(x=Temp, y=Sym_density, label=Temp, fill=interaction(Ramp,Site,sep="-",lex.order=TRUE))) +
  scale_fill_manual(values = c("dodgerblue", "royalblue3", "sienna1", "darkorange3", "red","red3"), name = "Ramp rate") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7)+
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ #this can also be used but rows and columns have to be specified -> facet_grid(. ~ experiment)
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
        strip.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(3, "lines"))

Symbiont + xlab(label = "Temperature treatment") + ylab(label = "Symbiont density (per cm2)")+
  theme(axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=18, face="plain"),
        legend.position="bottom")

library(cowplot)
plot_grid(Protein, Symbiont, Chla, rows=3, labels = c('A', 'B', 'C'))

#END
