#Code for Figure 2 of "Assessing the roles of heating rate and intensity on the response of corals to thermal stress"

#setwd to source file location

library(reshape2)
library(plyr)
library(dplyr)
library(openair)
library(ggplot2)

#### Organising and plotting tank temperature profiles for panels A and B ####

###################################################
######## Sorting Acropora CBASS Temps #############
###################################################

#merge and clean temp files
boing1<-read.csv("2019-08-05_KAUST_ACR_CBASS_1-4.txt")
boing1$time<-strptime(paste(boing1$Date,paste(boing1$Th,boing1$Tm,boing1$Ts,sep=":"),sep=" "),format="%Y_%B_%d %H:%M:%S")
boing1<-boing1[order(boing1$time),]
boing1<-subset(boing1, select=c(24,8,12,16,20))

boing2<-read.csv("2019-08-05_KAUST_ACR_CBASS_5-8.txt")
boing2$time<-strptime(paste(boing2$Date,paste(boing2$Th,boing2$Tm,boing2$Ts,sep=":"),sep=" "),format="%Y_%B_%d %H:%M:%S")
boing2<-boing2[order(boing2$time),]
boing2<-subset(boing2, select=c(24,8,12,16,20))

boing1<-plyr::rename(boing1, c("T1inT"="32A", "T2inT"="32B", "T3inT"="35A", "T4inT"="35B"))
boing2<-plyr::rename(boing2, c("T1inT"="36.5A", "T2inT"="36.5B", "T3inT"="38A", "T4inT"="38B"))

boing1$time<-as.POSIXct(boing1$time, tz="UTC")
boing2$time<-as.POSIXct(boing2$time, tz="UTC")

spoing<-merge(boing1, boing2, all=T)
CBASS_full <- melt(spoing, id="time")

names(CBASS_full)[names(CBASS_full) == "time"] <- "date"
names(CBASS_full)[names(CBASS_full) == "variable"] <- "Tank"
names(CBASS_full)[names(CBASS_full) == "value"] <- "Temp"

#Select for experimental period within larger file, average data to 15-min intervals for plotting
CBASS_full <- na.omit(CBASS_full)

#Acropora assay temp data
CBASS_Acr1<-selectByDate(CBASS_full, start = "2019-08-04", end = "2019-08-04",hour = 12:23)
CBASS_Acr2<-selectByDate(CBASS_full, start = "2019-08-05", end = "2019-08-05",hour = 00:08)

CBASS_Acr<-bind_rows(CBASS_Acr1, CBASS_Acr2)
CBASS_Acr$Temp<-as.numeric(CBASS_Acr$Temp)
CBASS_Acr_av<-timeAverage(CBASS_Acr, avg.time = "15 min", type = "Tank")

#calculate mean temp during the hold
CBASS_Acr_hold<-selectByDate(CBASS_full, start = "2019-08-04", end = "2019-08-04",hour = 16:18)
CBASS_Acr_hold$Temp<-as.numeric(CBASS_Acr_hold$Temp)
CBASS_Acr_hold_summary <- ddply(CBASS_Acr_hold, c("Tank"), summarise, N= length(Temp), mean = mean(Temp),
                                sd   = sd(Temp),
                                se   = sd / sqrt(N))

##################################################
######## Sorting Porites CBASS Temps #############
##################################################

#merge and clean temp files
boing3<-read.csv("2019-08-01_KAUST_POR_CBASS_1-4.txt")
boing3$time<-strptime(paste(boing3$Date,paste(boing3$Th,boing3$Tm,boing3$Ts,sep=":"),sep=" "),format="%Y_%B_%d %H:%M:%S")
boing3<-boing3[order(boing3$time),]
boing3<-subset(boing3, select=c(24,8,12,16,20))

boing4<-read.csv("2019-08-01_KAUST_POR_CBASS_5-8.txt")
boing4$time<-strptime(paste(boing4$Date,paste(boing4$Th,boing4$Tm,boing4$Ts,sep=":"),sep=" "),format="%Y_%B_%d %H:%M:%S")
boing4<-boing4[order(boing4$time),]
boing4<-subset(boing4, select=c(24,8,12,16,20))

boing3<-plyr::rename(boing3, c("T1inT"="32A", "T2inT"="32B", "T3inT"="35A", "T4inT"="35B"))
boing4<-plyr::rename(boing4, c("T1inT"="36.5A", "T2inT"="36.5B", "T3inT"="38A", "T4inT"="38B"))

boing3$time<-as.POSIXct(boing3$time, tz="UTC")
boing4$time<-as.POSIXct(boing4$time, tz="UTC")

spoing1<-merge(boing3, boing4, all=T)
CBASS_Por_full <- melt(spoing1, id="time")

names(CBASS_Por_full)[names(CBASS_Por_full) == "time"] <- "date"
names(CBASS_Por_full)[names(CBASS_Por_full) == "variable"] <- "Tank"
names(CBASS_Por_full)[names(CBASS_Por_full) == "value"] <- "Temp"

CBASS_Por_full <- na.omit(CBASS_Por_full)

#Porites assay temp data
CBASS_Por1<-selectByDate(CBASS_Por_full, start = "2019-08-01", end = "2019-08-01",hour = 12:23)
CBASS_Por2<-selectByDate(CBASS_Por_full, start = "2019-08-02", end = "2019-08-02",hour = 00:08)

CBASS_Por<-bind_rows(CBASS_Por1, CBASS_Por2)
CBASS_Por$Temp<-as.numeric(CBASS_Por$Temp)
CBASS_Por_av<-timeAverage(CBASS_Por, avg.time = "15 min", type = "Tank")

#calculate mean temp during the hold
CBASS_Por_hold<-selectByDate(CBASS_Por_full, start = "2019-08-01", end = "2019-08-01",hour = 16:18)
CBASS_Por_hold$Temp<-as.numeric(CBASS_Por_hold$Temp)
CBASS_Por_hold_summary <- ddply(CBASS_Por_hold, c("Tank"), summarise, N= length(Temp), mean = mean(Temp),
                                sd   = sd(Temp),
                                se   = sd / sqrt(N))

##########################################################
######## Plotting CBASS Temps for both species ###########
##########################################################

cols <- c("32A" = "navyblue", "32B" = "navyblue", "35A" = "yellow2", "35B" = "yellow2", 
          "36.5A" = "darkorange2", "36.5B" = "darkorange2", "38A" = "red3", "38B" = "red3")
CBASS_Acr_plot<-ggplot(CBASS_Acr_av, aes(x=date, y=Temp, col=Tank)) + scale_colour_manual(values=cols) +
  geom_line(aes(linetype=Tank)) +
  scale_linetype_manual(values=c("solid","dashed","solid", "dashed", "solid", "dashed","solid", "dashed")) +
  scale_y_continuous(limits=c(30,40), breaks = c(30,32,34,36,38,40)) + 
  scale_x_datetime(name = "Date", date_labels = "%b %d %H-h", date_breaks = "3 hour") +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  theme(text = element_text(size=15, face="bold")) + 
  theme(axis.text = element_text(colour = "black")) + theme(panel.background = element_rect(colour = "black", size=1)) + 
  theme(axis.ticks.length=unit(.3,"cm")) +
  geom_hline(yintercept=c(32,35,36.5,38), linetype="dashed", color = c("navyblue","yellow2","darkorange2","red3"), size=0.5)
CBASS_Acr_plot

CBASS_Por_plot<-ggplot(CBASS_Por_av, aes(x=date, y=Temp, col=Tank)) + scale_colour_manual(values=cols) +
  geom_line(aes(linetype=Tank)) +
  scale_linetype_manual(values=c("solid","dashed","solid", "dashed", "solid", "dashed","solid", "dashed")) +
  scale_y_continuous(limits=c(30,40), breaks = c(30,32,34,36,38,40)) + 
  scale_x_datetime(name = "Date", date_labels = "%b %d %H-h", date_breaks = "3 hour") +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  theme(text = element_text(size=15, face="bold")) + 
  theme(axis.text = element_text(colour = "black")) + theme(panel.background = element_rect(colour = "black", size=1)) + 
  theme(axis.ticks.length=unit(.3,"cm")) +
  geom_hline(yintercept=c(32,35,36.5,38), linetype="dashed", color = c("navyblue","yellow2","darkorange2","red3"), size=0.5)
CBASS_Por_plot

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

library(lmerTest)
library(emmeans)
library(sjPlot)

#### Organising, analysing, and plotting Fv/Fm data for panels C and D ####

#read in data
CBASS_PAM_Acr<-read.csv("2019-08-04_ACR_CBASS_PAM.csv")
CBASS_PAM_Acr$Temp<-as.factor(CBASS_PAM_Acr$Temp)
CBASS_PAM_Por<-read.csv("2019-08-01_POR_CBASS_PAM.csv")
CBASS_PAM_Por$Temp<-as.factor(CBASS_PAM_Por$Temp)

#### Analyse Fv/Fm #### 

#Acropora
CBASS_Acr_PAM<-lmer(FvFm ~ Temp*Population + (1|Genotype),data=CBASS_PAM_Acr)
sjPlot::plot_model(CBASS_Acr_PAM, type="diag")
step(CBASS_Acr_PAM, reduce.random = F)
CBASS_Acr_PAM_final<-lmer(FvFm ~ Temp + (1|Genotype),data=CBASS_PAM_Acr)
anova(CBASS_Acr_PAM_final)

sjPlot::plot_model(CBASS_Acr_PAM_final, type="pred")

print(emmeans(CBASS_Acr_PAM_final, list(pairwise ~ Temp)), adjust = c("mvt"))

#Porites
CBASS_Por_PAM<-lmer(FvFm ~ Temp*Population + (1|Genotype),data=CBASS_PAM_Por)
sjPlot::plot_model(CBASS_Por_PAM, type="diag")
step(CBASS_Por_PAM, reduce.random = F)
CBASS_Por_PAM_final<-lmer(FvFm ~ Temp + (1|Genotype),data=CBASS_PAM_Por)
anova(CBASS_Por_PAM_final)

sjPlot::plot_model(CBASS_Por_PAM_final, type="pred")

print(emmeans(CBASS_Por_PAM_final, list(pairwise ~ Temp)), adjust = c("mvt"))

#### Plot Fv/Fm #### 
ACR_PAM_PLOT <- ggplot(data=CBASS_PAM_Acr, 
                    aes(x=Temp, y=FvFm, label=Temp, fill=Population)) +
  scale_fill_manual(values = c("grey60", "grey30"), name = "Site") +
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
        strip.text.x = element_text(color = "black", size = 18, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature") + ylab(label = "Fv/Fm")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom")
ACR_PAM_PLOT


POR_PAM_PLOT <- ggplot(data=CBASS_PAM_Por, 
                       aes(x=Temp, y=FvFm, label=Temp, fill=Population)) +
  scale_fill_manual(values = c("grey60", "grey30"), name = "Site") +
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
        strip.text.x = element_text(color = "black", size = 18, angle = 0, hjust = 0, vjust = 0.5, family = 'ArialMT', face = 'bold.italic'),
        panel.spacing = unit(3, "lines")) + xlab(label = "Temperature") + ylab(label = "Fv/Fm")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, family = 'ArialMT', face = 'bold'),  
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, family = 'ArialMT', face = 'bold'),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5,family = 'ArialMT', face = 'bold'),
        legend.title = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.text = element_text(colour="black", size=15, family = 'ArialMT', face = 'bold'),
        legend.position="bottom")
POR_PAM_PLOT

#END
