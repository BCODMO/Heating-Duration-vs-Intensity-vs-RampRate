#Code for Figure 6 of "Assessing the roles of heating rate and intensity on the response of corals to thermal stress"

#setwd to source file location

library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)

#Load file
KAUST_2019<-read.csv("KAUST_Final_clean.csv")
View(KAUST_2019)
str(KAUST_2019)

#Subset data
CBASS_final<-subset(KAUST_2019,Experiment=="CBASS")
CBASS_final$Temp<-as.factor(CBASS_final$Temp)
CBASS_final$Tank<-as.factor(CBASS_final$Tank)
CBASS_final$Site<-as.factor(CBASS_final$Site)

CBASS_Acr<-subset(CBASS_final, Species=="Acropora")
CBASS_Por<-subset(CBASS_final, Species=="Porites")

#### GLMMs for Protein, Chl a, and Sym counts

###############################
######### Protein data ########
###############################

#Acropora
Protein_Acr<-lmer(Pro_cm2 ~ Temp*Site + (1|Tank) + (1|Geno), data=CBASS_Acr)
sjPlot::plot_model(Protein_Acr, type="diag") 
step(Protein_Acr, reduce.random = F)

Protein_Acr_final<-lmer(Pro_cm2 ~ Temp + (1|Tank) + (1 | Geno), data=CBASS_Acr)
anova(Protein_Acr_final)

print(emmeans(Protein_Acr_final, list(pairwise ~ Temp)), adjust = c("mvt"))

#Porites
Protein_Por<-lmer(Pro_cm2 ~ Temp*Site + (1|Tank) + (1|Geno),data=CBASS_Por)
sjPlot::plot_model(Protein_Por, type="diag") 
step(Protein_Por, reduce.random = F)

Protein_Por_final<-lmer(Pro_cm2 ~ Temp*Site + (1|Tank) + (1|Geno), data=CBASS_Por)
anova(Protein_Por_final)

print(emmeans(Protein_Por_final, list(pairwise ~ Temp|Site)), adjust = c("mvt"))
print(emmeans(Protein_Por_final, list(pairwise ~ Site|Temp)), adjust = c("mvt"))

#############################
######### Chl a data ########
#############################

#Acropora
Chla_Acr<-lmer(Chla_cm2 ~ Temp*Site + (1|Tank) + (1|Geno),data=CBASS_Acr)
sjPlot::plot_model(Chla_Acr, type="diag") 
step(Chla_Acr, reduce.random = F)

Chla_Acr_final<-lmer(Chla_cm2 ~ Temp + (1|Tank) + (1|Geno), data=CBASS_Acr)
anova(Chla_Acr_final)

print(emmeans(Chla_Acr_final, list(pairwise ~ Temp)), adjust = c("mvt"))

#Porites
Chla_Por<-lmer(Chla_cm2 ~ Temp*Site + (1|Tank) + (1|Geno),data=CBASS_Por)
sjPlot::plot_model(Chla_Por, type="diag") 
step(Chla_Por, reduce.random = F)

Chla_Por_final<-lmer(Chla_cm2 ~ Temp + (1|Tank) + (1|Geno), data=CBASS_Por)
anova(Chla_Por_final)

print(emmeans(Chla_Por_final, list(pairwise ~ Temp)), adjust = c("mvt"))

###################################
######### Sym density data ########
###################################

#Acropora
Sym_density_log<-log(CBASS_Acr$Sym_density)
Sym_density_Acr<-lmer(Sym_density_log ~ Temp*Site + (1|Tank) + (1|Geno),data=CBASS_Acr)
sjPlot::plot_model(Sym_density_Acr, type="diag") 
step(Sym_density_Acr, reduce.random = F) # N.S.

#Porites
Sym_density_log<-log(CBASS_Por$Sym_density)
Sym_density_Por<-lmer(Sym_density_log ~ Temp*Site + (1|Tank) + (1|Geno),data=CBASS_Por)
sjPlot::plot_model(Sym_density_Por, type="diag") 
step(Sym_density_Por, reduce.random = F)

Sym_density_Por_final<-lmer(Sym_density_log ~ Temp + (1|Tank) + (1|Geno), data=CBASS_Por)
anova(Sym_density_Por_final)

print(emmeans(Sym_density_Por_final, list(pairwise ~ Temp)), adjust = c("mvt"))

#####################################################
######### Plot CBASS Final Time point data ##########
#####################################################

print(levels(CBASS_final$Temp))

Protein <- ggplot(data=CBASS_final, 
                  aes(x=Temp, y=Pro_cm2, label=Temp, fill=Site)) +
  scale_fill_manual(values = c("grey60", "grey30"), name = "Site") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+
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
        strip.text.x = element_text(color = "black", size = 18, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines"))

Protein + xlab(label = "Temperature treatment") + ylab(label = "Protein (µg cm-2)")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom")

Chla <- ggplot(data=CBASS_final, 
               aes(x=Temp, y=Chla_cm2, label=Temp, fill=Site)) +
  scale_fill_manual(values = c("grey60", "grey30"), name = "Site") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7)+
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
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
        strip.text.x = element_text(color = "black", size = 18, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines"))

Chla + xlab(label = "Temperature treatment") + ylab(label = "Chl a (µg cm-2)")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom")

Symbiont <- ggplot(data=CBASS_final, 
                   aes(x=Temp, y=Sym_density, label=Temp, fill=Site)) +
  scale_fill_manual(values = c("grey60", "grey30"), name = "Site") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7)+
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
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
        strip.text.x = element_text(color = "black", size = 18, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines"))

Symbiont + xlab(label = "Temperature treatment") + ylab(label = "Symbiont density (per cm2)")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom")

#END
