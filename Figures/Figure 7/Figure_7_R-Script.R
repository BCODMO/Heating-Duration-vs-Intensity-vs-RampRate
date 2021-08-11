#Code for Figure 7 of "Assessing the roles of heating rate and intensity on the response of corals to thermal stress"

#setwd to source file location

library(car)
library(corrplot)
library(nlme)
library(plyr)
library(MuMIn)
library(sjPlot)
library(tidyr)
library(AICcmodavg)
library(ggplot2)
library(ggpubr)

library(emmeans)
library(drc)
library(ggplot2)
library(Rmisc)

#Read in and structure data
CBASS_Acr<-read.csv("2019-08-04_ACR_CBASS_PAM.csv")
CBASS_Por<-read.csv("2019-08-01_POR_CBASS_PAM.csv")

CBASS_Acr_clean <- subset(CBASS_Acr, select = c(Species, Population, Genotype, FvFm, load34, AbsTemp, Temp))
CBASS_Acr_clean <- CBASS_Acr_clean[complete.cases(CBASS_Acr_clean),]
str(CBASS_Acr_clean)
CBASS_Acr_pro <- subset(CBASS_Acr_clean, Population == "Protected")
CBASS_Acr_exp <- subset(CBASS_Acr_clean, Population == "Exposed")

CBASS_Por_clean <- subset(CBASS_Por, select = c(Species, Population, Genotype, FvFm, load34, AbsTemp, Temp))
CBASS_Por_clean <- CBASS_Por_clean[complete.cases(CBASS_Por_clean),]
str(CBASS_Por_clean)
CBASS_Por_pro <- subset(CBASS_Por_clean, Population == "Protected")
CBASS_Por_exp <- subset(CBASS_Por_clean, Population == "Exposed")

CBASS_Full<-rbind(CBASS_Acr_clean,CBASS_Por_clean)

CBASS_Full[,"species_pop"]<-as.character(NA)
CBASS_Full$species_pop[Name=(CBASS_Full$Species == "Acropora") & (CBASS_Full$Population=="Exposed")]<-"Acr_Exp"
CBASS_Full$species_pop[Name=(CBASS_Full$Species == "Acropora") & (CBASS_Full$Population=="Protected")]<-"Acr_Pro"
CBASS_Full$species_pop[Name=(CBASS_Full$Species == "Porites") & (CBASS_Full$Population=="Exposed")]<-"Por_Exp"
CBASS_Full$species_pop[Name=(CBASS_Full$Species == "Porites") & (CBASS_Full$Population=="Protected")]<-"Por_Pro"
CBASS_Full$species_pop<-as.factor(CBASS_Full$species_pop)

#######################
#######################
#### CBASS AbsTemp ####
#######################
#######################

#### #### #### #### 
#### Acropora  ####
#### #### #### ####

#### Exposed ####

#model
Acr_exp_AbsTemp_CBASS_1 <- drm(FvFm ~ AbsTemp, data = CBASS_Acr_exp, fct = LL.3())
summary(Acr_exp_AbsTemp_CBASS_1)
plot(Acr_exp_AbsTemp_CBASS_1)

#extract coeffs
Acr_exp_AT_CBASS_coeff<-data.frame(ED(Acr_exp_AbsTemp_CBASS_1, c(25)))

Acr_exp_AT_CBASS_coeff_mean<-Acr_exp_AT_CBASS_coeff[,1]
Acr_exp_AT_CBASS_coeff_lower<-Acr_exp_AT_CBASS_coeff[,1] - 1.96*Acr_exp_AT_CBASS_coeff[,2]
Acr_exp_AT_CBASS_coeff_upper<-Acr_exp_AT_CBASS_coeff[,1] + 1.96*Acr_exp_AT_CBASS_coeff[,2]

#### Protected ####

#model
Acr_pro_AbsTemp_CBASS_1 <- drm(FvFm ~ AbsTemp, data = CBASS_Acr_pro, fct = LL.3())
summary(Acr_pro_AbsTemp_CBASS_1)
plot(Acr_pro_AbsTemp_CBASS_1) 

#extract coeffs
Acr_pro_AT_CBASS_coeff<-data.frame(ED(Acr_pro_AbsTemp_CBASS_1, c(25)))

Acr_pro_AT_CBASS_coeff_mean<-Acr_pro_AT_CBASS_coeff[,1]
Acr_pro_AT_CBASS_coeff_lower<-Acr_pro_AT_CBASS_coeff[,1] - 1.96*Acr_pro_AT_CBASS_coeff[,2]
Acr_pro_AT_CBASS_coeff_upper<-Acr_pro_AT_CBASS_coeff[,1] + 1.96*Acr_pro_AT_CBASS_coeff[,2]

###########################################################
#### combine ED25 data plus predict curves from models ####
###########################################################

CBASS_Acr_AT_coeff_means<-data.frame(Acr_exp_AT_CBASS_coeff_mean, Acr_pro_AT_CBASS_coeff_mean)
CBASS_Acr_AT_coeff_lowers<-data.frame(Acr_exp_AT_CBASS_coeff_lower, Acr_pro_AT_CBASS_coeff_lower)
CBASS_Acr_AT_coeff_uppers<-data.frame(Acr_exp_AT_CBASS_coeff_upper, Acr_pro_AT_CBASS_coeff_upper)

CBASS_Acr_exp_AT_preddata = data.frame(temp = seq(31,40, length.out = 100))
CBASS_Acr_exp_AT_pred = as.data.frame(predict(Acr_exp_AbsTemp_CBASS_1, newdata = CBASS_Acr_exp_AT_preddata, interval = 'confidence'))
CBASS_Acr_exp_AT_preddata = data.frame(CBASS_Acr_exp_AT_preddata, fvfm = CBASS_Acr_exp_AT_pred$Prediction, Lower = CBASS_Acr_exp_AT_pred$Lower, Upper = CBASS_Acr_exp_AT_pred$Upper)

CBASS_Acr_pro_AT_preddata = data.frame(temp = seq(31,40, length.out = 100))
CBASS_Acr_pro_AT_pred = as.data.frame(predict(Acr_pro_AbsTemp_CBASS_1, newdata = CBASS_Acr_pro_AT_preddata, interval = 'confidence'))
CBASS_Acr_pro_AT_preddata = data.frame(CBASS_Acr_pro_AT_preddata, fvfm = CBASS_Acr_pro_AT_pred$Prediction, Lower = CBASS_Acr_pro_AT_pred$Lower, Upper = CBASS_Acr_pro_AT_pred$Upper)

#### Plot CBASS Acropora AbsTemp (Fig. 7a) ####

CBASS_AbsTemp_Acr<- ggplot() +
  geom_jitter(data = CBASS_Acr_clean, aes(x = AbsTemp, y = FvFm, color = Population), size = 1) +
  scale_x_continuous(limits=c(31,40), breaks=c(32, 34, 36, 38, 40)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = CBASS_Acr_exp_AT_preddata, aes(x = temp, y = fvfm), color = 'darkorange3', show.legend = FALSE) +
  geom_vline(data = CBASS_Acr_AT_coeff_means, aes(xintercept = Acr_exp_AT_CBASS_coeff_mean), color = 'darkorange3', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Acr_AT_coeff_lowers$Acr_exp_AT_CBASS_coeff_lower, xmax=CBASS_Acr_AT_coeff_uppers$Acr_exp_AT_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange3',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Acr_exp_AT_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange3', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Acr_AT_coeff_means, aes(label=round(Acr_exp_AT_CBASS_coeff_mean, digits = 2)), x = 33, y = 0.15, show.legend = FALSE, color = 'darkorange3') +
  geom_line(data = CBASS_Acr_pro_AT_preddata, aes(x = temp, y = fvfm), color = 'darkorchid4', show.legend = FALSE) +
  geom_vline(data = CBASS_Acr_AT_coeff_means, aes(xintercept = Acr_pro_AT_CBASS_coeff_mean), color = 'darkorchid4', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Acr_AT_coeff_lowers$Acr_pro_AT_CBASS_coeff_lower, xmax=CBASS_Acr_AT_coeff_uppers$Acr_pro_AT_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorchid4',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Acr_pro_AT_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorchid4', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Acr_AT_coeff_means, aes(label=round(Acr_pro_AT_CBASS_coeff_mean, digits = 2)), x = 33, y = 0.1, show.legend = FALSE, color = 'darkorchid4') +
  ggtitle("Acropora - Absolute Temperature") +
  scale_color_manual(values=c("darkorange3", "darkorchid4")) +
  ylab("Fv/Fm") +
  xlab("Temperature (°C)") +
  theme_bw()
CBASS_AbsTemp_Acr

#### #### #### ##
#### Porites ####
#### #### #### ##

#### Exposed ####

#model
Por_exp_AbsTemp_CBASS_1 <- drm(FvFm ~ AbsTemp, data = CBASS_Por_exp, fct = LL.3())
summary(Por_exp_AbsTemp_CBASS_1)
plot(Por_exp_AbsTemp_CBASS_1)

#extract coeffs
Por_exp_AT_CBASS_coeff<-data.frame(ED(Por_exp_AbsTemp_CBASS_1, c(25)))

Por_exp_AT_CBASS_coeff_mean<-Por_exp_AT_CBASS_coeff[,1]
Por_exp_AT_CBASS_coeff_lower<-Por_exp_AT_CBASS_coeff[,1] - 1.96*Por_exp_AT_CBASS_coeff[,2]
Por_exp_AT_CBASS_coeff_upper<-Por_exp_AT_CBASS_coeff[,1] + 1.96*Por_exp_AT_CBASS_coeff[,2]

#### Protected ####

#model
Por_pro_AbsTemp_CBASS_1 <- drm(FvFm ~ AbsTemp, data = CBASS_Por_pro, fct = LL.3())
summary(Por_pro_AbsTemp_CBASS_1)
plot(Por_pro_AbsTemp_CBASS_1)

#extract coeffs
Por_pro_AT_CBASS_coeff<-data.frame(ED(Por_pro_AbsTemp_CBASS_1, c(25)))

Por_pro_AT_CBASS_coeff_mean<-Por_pro_AT_CBASS_coeff[,1]
Por_pro_AT_CBASS_coeff_lower<-Por_pro_AT_CBASS_coeff[,1] - 1.96*Por_pro_AT_CBASS_coeff[,2]
Por_pro_AT_CBASS_coeff_upper<-Por_pro_AT_CBASS_coeff[,1] + 1.96*Por_pro_AT_CBASS_coeff[,2]

###########################################################
#### combine ED25 data plus predict curves from models ####
###########################################################

CBASS_Por_AT_coeff_means<-data.frame(Por_exp_AT_CBASS_coeff_mean, Por_pro_AT_CBASS_coeff_mean)
CBASS_Por_AT_coeff_lowers<-data.frame(Por_exp_AT_CBASS_coeff_lower, Por_pro_AT_CBASS_coeff_lower)
CBASS_Por_AT_coeff_uppers<-data.frame(Por_exp_AT_CBASS_coeff_upper, Por_pro_AT_CBASS_coeff_upper)

CBASS_Por_exp_AT_preddata = data.frame(temp = seq(31,40, length.out = 100))
CBASS_Por_exp_AT_pred = as.data.frame(predict(Por_exp_AbsTemp_CBASS_1, newdata = CBASS_Por_exp_AT_preddata, interval = 'confidence'))
CBASS_Por_exp_AT_preddata = data.frame(CBASS_Por_exp_AT_preddata, fvfm = CBASS_Por_exp_AT_pred$Prediction, Lower = CBASS_Por_exp_AT_pred$Lower, Upper = CBASS_Por_exp_AT_pred$Upper)

CBASS_Por_pro_AT_preddata = data.frame(temp = seq(31,40, length.out = 100))
CBASS_Por_pro_AT_pred = as.data.frame(predict(Por_pro_AbsTemp_CBASS_1, newdata = CBASS_Por_pro_AT_preddata, interval = 'confidence'))
CBASS_Por_pro_AT_preddata = data.frame(CBASS_Por_pro_AT_preddata, fvfm = CBASS_Por_pro_AT_pred$Prediction, Lower = CBASS_Por_pro_AT_pred$Lower, Upper = CBASS_Por_pro_AT_pred$Upper)

#### Plot CBASS Poropora AbsTemp (Fig. 7b) ####

CBASS_AbsTemp_Por<- ggplot() +
  geom_jitter(data = CBASS_Por_clean, aes(x = AbsTemp, y = FvFm, color = Population), size = 1) +
  scale_x_continuous(limits=c(31,40), breaks=c(32, 34, 36, 38, 40)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = CBASS_Por_exp_AT_preddata, aes(x = temp, y = fvfm), color = 'royalblue4', show.legend = FALSE) +
  geom_vline(data = CBASS_Por_AT_coeff_means, aes(xintercept = Por_exp_AT_CBASS_coeff_mean), color = 'royalblue4', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Por_AT_coeff_lowers$Por_exp_AT_CBASS_coeff_lower, xmax=CBASS_Por_AT_coeff_uppers$Por_exp_AT_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue4',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Por_exp_AT_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue4', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Por_AT_coeff_means, aes(label=round(Por_exp_AT_CBASS_coeff_mean, digits = 2)), x = 33, y = 0.15, show.legend = FALSE, color = 'royalblue4') +
  geom_line(data = CBASS_Por_pro_AT_preddata, aes(x = temp, y = fvfm), color = 'springgreen4', show.legend = FALSE) +
  geom_vline(data = CBASS_Por_AT_coeff_means, aes(xintercept = Por_pro_AT_CBASS_coeff_mean), color = 'springgreen4', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Por_AT_coeff_lowers$Por_pro_AT_CBASS_coeff_lower, xmax=CBASS_Por_AT_coeff_uppers$Por_pro_AT_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen4',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Por_pro_AT_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen4', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Por_AT_coeff_means, aes(label=round(Por_pro_AT_CBASS_coeff_mean, digits = 2)), x = 33, y = 0.1, show.legend = FALSE, color = 'springgreen4') +
  ggtitle("Porites - Absolute Temperature") +
  scale_color_manual(values=c("royalblue4", "springgreen4")) +
  ylab("Fv/Fm") +
  xlab("Temperature (°C)") +
  theme_bw()
CBASS_AbsTemp_Por

#### Stats comparing curve fit parameters for CBASS AbsTemp ####

#Acropora
CBASS_Acr_AT_DRC_ID<-drm(FvFm ~ AbsTemp, data = CBASS_Acr_clean, curveid = Population,
                         fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(CBASS_Acr_AT_DRC_ID)
EDcomp(CBASS_Acr_AT_DRC_ID, c(25,25))

#Porites
CBASS_Por_AT_DRC_ID<-drm(FvFm ~ AbsTemp, data = CBASS_Por_clean, curveid = Population,
                         fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(CBASS_Por_AT_DRC_ID)
EDcomp(CBASS_Por_AT_DRC_ID, c(25,25))

####################
####################
#### CBASS HL34 ####
####################
####################

#### #### #### #### #### #### 
#### CBASS Acropora HL34 ####
#### #### #### #### #### #### 

#### Exposed ####

#model
Acr_exp_HL34_CBASS_1 <- drm(FvFm ~ load34, data = CBASS_Acr_exp, fct = LL.3())
mselect(Acr_exp_HL34_CBASS_1, list(LL.2(), LL.4(), LL.5(), LL2.2(), LL2.3(), LL2.4(), LL2.5(), AR.2(), AR.3(), EXD.2(), EXD.3()), icfct = AIC)
summary(Acr_exp_HL34_CBASS_1)
plot(Acr_exp_HL34_CBASS_1)

#extract coeffs
Acr_exp_34_CBASS_coeff<-data.frame(ED(Acr_exp_HL34_CBASS_1, c(25)))

Acr_exp_34_CBASS_coeff_mean<-Acr_exp_34_CBASS_coeff[,1]
Acr_exp_34_CBASS_coeff_lower<-Acr_exp_34_CBASS_coeff[,1] - 1.96*Acr_exp_34_CBASS_coeff[,2]
Acr_exp_34_CBASS_coeff_upper<-Acr_exp_34_CBASS_coeff[,1] + 1.96*Acr_exp_34_CBASS_coeff[,2]

#### Protected ####

#model
Acr_pro_HL34_CBASS_1 <- drm(FvFm ~ load34, data = CBASS_Acr_pro, fct = LL.3())
summary(Acr_pro_HL34_CBASS_1)
plot(Acr_pro_HL34_CBASS_1)

#extract coeffs
Acr_pro_34_CBASS_coeff<-data.frame(ED(Acr_pro_HL34_CBASS_1, c(25)))

Acr_pro_34_CBASS_coeff_mean<-Acr_pro_34_CBASS_coeff[,1]
Acr_pro_34_CBASS_coeff_lower<-Acr_pro_34_CBASS_coeff[,1] - 1.96*Acr_pro_34_CBASS_coeff[,2]
Acr_pro_34_CBASS_coeff_upper<-Acr_pro_34_CBASS_coeff[,1] + 1.96*Acr_pro_34_CBASS_coeff[,2]

###########################################################
#### combine ED25 data plus predict curves from models ####
###########################################################

CBASS_Acr_34_coeff_means<-data.frame(Acr_exp_34_CBASS_coeff_mean, Acr_pro_34_CBASS_coeff_mean)
CBASS_Acr_34_coeff_lowers<-data.frame(Acr_exp_34_CBASS_coeff_lower, Acr_pro_34_CBASS_coeff_lower)
CBASS_Acr_34_coeff_uppers<-data.frame(Acr_exp_34_CBASS_coeff_upper, Acr_pro_34_CBASS_coeff_upper)

CBASS_Acr_exp_34_preddata = data.frame(temp = seq(0,1250, length.out = 100))
CBASS_Acr_exp_34_pred = as.data.frame(predict(Acr_exp_HL34_CBASS_1, newdata = CBASS_Acr_exp_34_preddata, interval = 'confidence'))
CBASS_Acr_exp_34_preddata = data.frame(CBASS_Acr_exp_34_preddata, fvfm = CBASS_Acr_exp_34_pred$Prediction, Lower = CBASS_Acr_exp_34_pred$Lower, Upper = CBASS_Acr_exp_34_pred$Upper)

CBASS_Acr_pro_34_preddata = data.frame(temp = seq(0,1250, length.out = 100))
CBASS_Acr_pro_34_pred = as.data.frame(predict(Acr_pro_HL34_CBASS_1, newdata = CBASS_Acr_pro_34_preddata, interval = 'confidence'))
CBASS_Acr_pro_34_preddata = data.frame(CBASS_Acr_pro_34_preddata, fvfm = CBASS_Acr_pro_34_pred$Prediction, Lower = CBASS_Acr_pro_34_pred$Lower, Upper = CBASS_Acr_pro_34_pred$Upper)

#### Plot CBASS Acropora HL34 (Fig. 7c) ####

CBASS_HL34_Acr<- ggplot() +
  geom_jitter(data = CBASS_Acr_clean, aes(x = load34, y = FvFm, color = Population), size = 1) +
  scale_x_continuous(limits=c(0, 1250), breaks=c(0, 400, 800, 1200)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = CBASS_Acr_exp_34_preddata, aes(x = temp, y = fvfm), color = 'darkorange3', show.legend = FALSE) +
  geom_vline(data = CBASS_Acr_34_coeff_means, aes(xintercept = Acr_exp_34_CBASS_coeff_mean), color = 'darkorange3', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Acr_34_coeff_lowers$Acr_exp_34_CBASS_coeff_lower, xmax=CBASS_Acr_34_coeff_uppers$Acr_exp_34_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange3',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Acr_exp_34_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange3', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Acr_34_coeff_means, aes(label=round(Acr_exp_34_CBASS_coeff_mean, digits = 0)), x = 100, y = 0.15, show.legend = FALSE, color = 'darkorange3') +
  geom_line(data = CBASS_Acr_pro_34_preddata, aes(x = temp, y = fvfm), color = 'darkorchid4', show.legend = FALSE) +
  geom_vline(data = CBASS_Acr_34_coeff_means, aes(xintercept = Acr_pro_34_CBASS_coeff_mean), color = 'darkorchid4', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Acr_34_coeff_lowers$Acr_pro_34_CBASS_coeff_lower, xmax=CBASS_Acr_34_coeff_uppers$Acr_pro_34_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorchid4',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Acr_pro_34_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorchid4', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Acr_34_coeff_means, aes(label=round(Acr_pro_34_CBASS_coeff_mean, digits = 0)), x = 100, y = 0.1, show.legend = FALSE, color = 'darkorchid4') +
  ggtitle("Acropora - Heatload 34°C") +
  scale_color_manual(values=c("darkorange3", "darkorchid4")) +
  ylab("Fv/Fm") +
  xlab("Heatload (minutes) above 34°C") +
  theme_bw()
CBASS_HL34_Acr

#### #### #### #### #### ###
#### CBASS Porites HL34 ####
#### #### #### #### #### ###

#### Exposed ####

#model
Por_exp_HL34_CBASS_1 <- drm(FvFm ~ load34, data = CBASS_Por_exp, fct = LL.3())
mselect(Por_exp_HL34_CBASS_1, list(LL.2(), LL.4(), LL.5(), LL2.2(), LL2.3(), LL2.4(), LL2.5(), AR.2(), AR.3(), EXD.2(), EXD.3()), icfct = AIC)
summary(Por_exp_HL34_CBASS_1)
plot(Por_exp_HL34_CBASS_1)

#extract coeffs
Por_exp_34_CBASS_coeff<-data.frame(ED(Por_exp_HL34_CBASS_1, c(25)))

Por_exp_34_CBASS_coeff_mean<-Por_exp_34_CBASS_coeff[,1]
Por_exp_34_CBASS_coeff_lower<-Por_exp_34_CBASS_coeff[,1] - 1.96*Por_exp_34_CBASS_coeff[,2]
Por_exp_34_CBASS_coeff_upper<-Por_exp_34_CBASS_coeff[,1] + 1.96*Por_exp_34_CBASS_coeff[,2]

#### Protected ####

#model
Por_pro_HL34_CBASS_1 <- drm(FvFm ~ load34, data = CBASS_Por_pro, fct = LL.3())
summary(Por_pro_HL34_CBASS_1)
plot(Por_pro_HL34_CBASS_1)

#extract coeffs
Por_pro_34_CBASS_coeff<-data.frame(ED(Por_pro_HL34_CBASS_1, c(25)))

Por_pro_34_CBASS_coeff_mean<-Por_pro_34_CBASS_coeff[,1]
Por_pro_34_CBASS_coeff_lower<-Por_pro_34_CBASS_coeff[,1] - 1.96*Por_pro_34_CBASS_coeff[,2]
Por_pro_34_CBASS_coeff_upper<-Por_pro_34_CBASS_coeff[,1] + 1.96*Por_pro_34_CBASS_coeff[,2]

###########################################################
#### combine ED25 data plus predict curves from models ####
###########################################################

CBASS_Por_34_coeff_means<-data.frame(Por_exp_34_CBASS_coeff_mean, Por_pro_34_CBASS_coeff_mean)
CBASS_Por_34_coeff_lowers<-data.frame(Por_exp_34_CBASS_coeff_lower, Por_pro_34_CBASS_coeff_lower)
CBASS_Por_34_coeff_uppers<-data.frame(Por_exp_34_CBASS_coeff_upper, Por_pro_34_CBASS_coeff_upper)

CBASS_Por_exp_34_preddata = data.frame(temp = seq(0,1000, length.out = 100))
CBASS_Por_exp_34_pred = as.data.frame(predict(Por_exp_HL34_CBASS_1, newdata = CBASS_Por_exp_34_preddata, interval = 'confidence'))
CBASS_Por_exp_34_preddata = data.frame(CBASS_Por_exp_34_preddata, fvfm = CBASS_Por_exp_34_pred$Prediction, Lower = CBASS_Por_exp_34_pred$Lower, Upper = CBASS_Por_exp_34_pred$Upper)

CBASS_Por_pro_34_preddata = data.frame(temp = seq(0,1000, length.out = 100))
CBASS_Por_pro_34_pred = as.data.frame(predict(Por_pro_HL34_CBASS_1, newdata = CBASS_Por_pro_34_preddata, interval = 'confidence'))
CBASS_Por_pro_34_preddata = data.frame(CBASS_Por_pro_34_preddata, fvfm = CBASS_Por_pro_34_pred$Prediction, Lower = CBASS_Por_pro_34_pred$Lower, Upper = CBASS_Por_pro_34_pred$Upper)

#### Plot CBASS Porites HL34 (Fig. 7d) ####

CBASS_HL34_Por<- ggplot() +
  geom_jitter(data = CBASS_Por_clean, aes(x = load34, y = FvFm, color = Population), size = 1) +
  scale_x_continuous(limits=c(-20, 1000), breaks=c(0, 200, 400, 600, 800, 1000)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = CBASS_Por_exp_34_preddata, aes(x = temp, y = fvfm), color = 'royalblue4', show.legend = FALSE) +
  geom_vline(data = CBASS_Por_34_coeff_means, aes(xintercept = Por_exp_34_CBASS_coeff_mean), color = 'royalblue4', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Por_34_coeff_lowers$Por_exp_34_CBASS_coeff_lower, xmax=CBASS_Por_34_coeff_uppers$Por_exp_34_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue4',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Por_exp_34_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue4', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Por_34_coeff_means, aes(label=round(Por_exp_34_CBASS_coeff_mean, digits = 0)), x = 100, y = 0.15, show.legend = FALSE, color = 'royalblue4') +
  geom_line(data = CBASS_Por_pro_34_preddata, aes(x = temp, y = fvfm), color = 'springgreen4', show.legend = FALSE) +
  geom_vline(data = CBASS_Por_34_coeff_means, aes(xintercept = Por_pro_34_CBASS_coeff_mean), color = 'springgreen4', show.legend = FALSE) +
  annotate("rect", xmin=CBASS_Por_34_coeff_lowers$Por_pro_34_CBASS_coeff_lower, xmax=CBASS_Por_34_coeff_uppers$Por_pro_34_CBASS_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen4',  alpha = 0.2) +
  geom_ribbon(data = CBASS_Por_pro_34_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen4', linetype=2, alpha = 0.2) +
  geom_text(data = CBASS_Por_34_coeff_means, aes(label=round(Por_pro_34_CBASS_coeff_mean, digits = 0)), x = 100, y = 0.1, show.legend = FALSE, color = 'springgreen4') +
  ggtitle("Porites - Heatload 34°C") +
  scale_color_manual(values=c("royalblue4", "springgreen4")) +
  ylab("Fv/Fm") +
  xlab("Heatload (minutes) above 34°C") +
  theme_bw()
CBASS_HL34_Por

#### Stats comparing curve fit parameters for CBASS HL34 ####

#Acropora
CBASS_Acr_DRC_ID<-drm(FvFm ~ load34, data = CBASS_Acr_clean, curveid = Population,
                      fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(CBASS_Acr_DRC_ID)
EDcomp(CBASS_Acr_DRC_ID, c(25,25))

#Porites
CBASS_Por_DRC_ID<-drm(FvFm ~ load34, data = CBASS_Por_clean, curveid = Population,
                      fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(CBASS_Por_DRC_ID)
EDcomp(CBASS_Por_DRC_ID, c(25,25))

#END