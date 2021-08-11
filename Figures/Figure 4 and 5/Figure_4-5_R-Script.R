#Code for Figure 4 and 5 of "Assessing the roles of heating rate and intensity on the response of corals to thermal stress"

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
Acr_data<-read.csv("2019_08_KAUST_PAM_prolonged_acropora.csv")
Por_data<-read.csv("2019_08_KAUST_PAM_prolonged_porites.csv")

Acr_data$Day<-as.factor(Acr_data$Day)
Acr_data$Colony<-as.factor(Acr_data$Colony)
Acr_data$Temp<-as.factor(Acr_data$Temp)
Acr_data$Date<-as.factor(Acr_data$Date)
Acr_data$Tank<-as.factor(Acr_data$Tank)
str(Acr_data)

Acr_clean <- subset(Acr_data, select = c(Day, Site, Colony, Population, Ramp, Tank, FvFm, Heatload32, Heatload33, Heatload34, AbsTemp, ElapsedT))
Acr_clean <- Acr_clean[complete.cases(Acr_clean),]
Acr_pro <- subset(Acr_clean, Site == "Protected")
Acr_exp <- subset(Acr_clean, Site == "Exposed")

Por_data$Day<-as.factor(Por_data$Day)
Por_data$Colony<-as.factor(Por_data$Colony)
Por_data$Temp<-as.factor(Por_data$Temp)
Por_data$Date<-as.factor(Por_data$Date)
Por_data$Tank<-as.factor(Por_data$Tank)
str(Por_data)

Por_clean <- subset(Por_data, select = c(Day, Site, Colony, Population, Ramp, Tank, FvFm, Heatload32, Heatload33, Heatload34, AbsTemp, ElapsedT))
Por_clean <- Por_clean[complete.cases(Por_clean),]
Por_pro <- subset(Por_clean, Site == "Protected")
Por_exp <- subset(Por_clean, Site == "Exposed")


Full_data<-rbind(Acr_clean,Por_clean)

#Model Selection of heating metrics

#Checking for multicollinearity
Predictors <- Full_data[,c(8:12)]
cor_matrix <-cor(Predictors)
corrplot.mixed(cor_matrix, upper = 'pie',lower='number') #remove heaload 33 - near perfect correlation with heatload 32

#Using VIF to check/account for multicollinearity
Main_mod<-nlme::lme(FvFm ~ Heatload32 + Heatload34 + AbsTemp + ElapsedT, random=list(~1|Day,~1|Tank,~1|Colony), data = Full_data, na.action=na.exclude)
summary(Main_mod)

#Use function below to calculate VIF for mixed model such as lme, otherwise can use VIF from car package (see below)
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

vif.lme(Main_mod) #VIFs all below 3 (acceptable after Neter et al. (1996) and Chatterjee et al. (2000))

#model selection
Can.mod<-list()
Can.mod[[1]]<-lme(FvFm ~ 1, random=list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[2]]<-lme(FvFm ~ Heatload32, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[3]]<-lme(FvFm ~ Heatload34, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[4]]<-lme(FvFm ~ AbsTemp, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[5]]<-lme(FvFm ~ ElapsedT, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[6]]<-lme(FvFm ~ Heatload32 * Heatload34, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[7]]<-lme(FvFm ~ Heatload32 * AbsTemp, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[8]]<-lme(FvFm ~ Heatload32 * ElapsedT, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[9]]<-lme(FvFm ~ Heatload34 * AbsTemp, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[10]]<-lme(FvFm ~ Heatload34 * ElapsedT, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[11]]<-lme(FvFm ~ AbsTemp * ElapsedT, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[12]]<-lme(FvFm ~ Heatload32 * Heatload34 * AbsTemp, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[13]]<-lme(FvFm ~ Heatload32 * Heatload34 * ElapsedT, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[14]]<-lme(FvFm ~ Heatload32 * AbsTemp * ElapsedT, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
Can.mod[[15]]<-lme(FvFm ~ Heatload32*Heatload34*AbsTemp*ElapsedT, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)

aic_out<-aictab(cand.set = Can.mod)
aic_out

summary(Can.mod[[9]]) #best model based on AIC

#Run single models and look at the R2m on each

Full.mod1<-lme(FvFm ~ Heatload34, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
r.squaredGLMM(Full.mod1) #R2m = 0.61

Full.mod2<-lme(FvFm ~ AbsTemp, random = list(Day=~1, Tank=~1, Colony=~1), control = lmeControl(opt = "optim"), data = Full_data, na.action=na.exclude)
r.squaredGLMM(Full.mod2) #R2m = 0.39

Full.mod3<-lme(FvFm ~ Heatload34 + AbsTemp, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
r.squaredGLMM(Full.mod3) #R2m = 0.64

Full.mod4<-lme(FvFm ~ Heatload34 * AbsTemp, random = list(Day=~1, Tank=~1, Colony=~1), data = Full_data, na.action=na.exclude)
r.squaredGLMM(Full.mod4) #R2m = 0.65

################################################################################################
################################################################################################
################################################################################################

# Based on model selection - predictors are HL34 and AbsTemp
# Assess affect of ramp rate on thresholds in response to each predictor, for each species

#### DRC Curve Fitting ####

Acr_exp_slow<-subset(Acr_exp, Ramp=='Control_S' | Ramp=='Slow')
Acr_exp_fast<-subset(Acr_exp, Ramp=='Control_F' | Ramp=='Fast')

Acr_pro_slow<-subset(Acr_pro, Ramp=='Control_S' | Ramp=='Slow')
Acr_pro_fast<-subset(Acr_pro, Ramp=='Control_F' | Ramp=='Fast')

Por_exp_slow<-subset(Por_exp, Ramp=='Control_S' | Ramp=='Slow')
Por_exp_fast<-subset(Por_exp, Ramp=='Control_F' | Ramp=='Fast')

Por_pro_slow<-subset(Por_pro, Ramp=='Control_S' | Ramp=='Slow')
Por_pro_fast<-subset(Por_pro, Ramp=='Control_F' | Ramp=='Fast')

#### Absolute Temp DRC analysis and plotting for Figure 4 ####

################################################
#### Acropora Exposed AbsTemp slow vs. fast ####
################################################

#### Acr Exp AbsTemp Slow ####

#model
Acr_exp_AbsTemp_slow_1 <- drm(FvFm ~ AbsTemp, data = Acr_exp_slow, fct = LL.3())
summary(Acr_exp_AbsTemp_slow_1)
plot(Acr_exp_AbsTemp_slow_1)

#extract coeffs
Acr_exp_AbsTemp_slow_coeff<-data.frame(ED(Acr_exp_AbsTemp_slow_1, c(25)))

Acr_exp_AbsTemp_slow_coeff_mean<-Acr_exp_AbsTemp_slow_coeff[,1]
Acr_exp_AbsTemp_slow_coeff_lower<-Acr_exp_AbsTemp_slow_coeff[,1] - 1.96*Acr_exp_AbsTemp_slow_coeff[,2]
Acr_exp_AbsTemp_slow_coeff_upper<-Acr_exp_AbsTemp_slow_coeff[,1] + 1.96*Acr_exp_AbsTemp_slow_coeff[,2]

#### Acr Exp AbsTemp Fast ####

#model
Acr_exp_AbsTemp_fast_1 <- drm(FvFm ~ AbsTemp, data = Acr_exp_fast, fct = LL.3())
summary(Acr_exp_AbsTemp_fast_1)
plot(Acr_exp_AbsTemp_fast_1)

#extract coeffs
Acr_exp_AbsTemp_fast_coeff<-data.frame(ED(Acr_exp_AbsTemp_fast_1, c(25)))

Acr_exp_AbsTemp_fast_coeff_mean<-Acr_exp_AbsTemp_fast_coeff[,1]
Acr_exp_AbsTemp_fast_coeff_lower<-Acr_exp_AbsTemp_fast_coeff[,1] - 1.96*Acr_exp_AbsTemp_fast_coeff[,2]
Acr_exp_AbsTemp_fast_coeff_upper<-Acr_exp_AbsTemp_fast_coeff[,1] + 1.96*Acr_exp_AbsTemp_fast_coeff[,2]

##################################################
#### Acropora Protected AbsTemp slow vs. fast ####
##################################################

#model
Acr_pro_AbsTemp_slow_1 <- drm(FvFm ~ AbsTemp, data = Acr_pro_slow, fct = LL.3())
summary(Acr_pro_AbsTemp_slow_1)
plot(Acr_pro_AbsTemp_slow_1)

#extract coeffs
Acr_pro_AbsTemp_slow_coeff<-data.frame(ED(Acr_pro_AbsTemp_slow_1, c(25)))

Acr_pro_AbsTemp_slow_coeff_mean<-Acr_pro_AbsTemp_slow_coeff[,1]
Acr_pro_AbsTemp_slow_coeff_lower<-Acr_pro_AbsTemp_slow_coeff[,1] - 1.96*Acr_pro_AbsTemp_slow_coeff[,2]
Acr_pro_AbsTemp_slow_coeff_upper<-Acr_pro_AbsTemp_slow_coeff[,1] + 1.96*Acr_pro_AbsTemp_slow_coeff[,2]

#### Acr Pro AbsTemp Fast ####

#model
Acr_pro_AbsTemp_fast_1 <- drm(FvFm ~ AbsTemp, data = Acr_pro_fast, fct = LL.3())
summary(Acr_pro_AbsTemp_fast_1)
plot(Acr_pro_AbsTemp_fast_1)

#extract coeffs
Acr_pro_AbsTemp_fast_coeff<-data.frame(ED(Acr_pro_AbsTemp_fast_1, c(25)))

Acr_pro_AbsTemp_fast_coeff_mean<-Acr_pro_AbsTemp_fast_coeff[,1]
Acr_pro_AbsTemp_fast_coeff_lower<-Acr_pro_AbsTemp_fast_coeff[,1] - 1.96*Acr_pro_AbsTemp_fast_coeff[,2]
Acr_pro_AbsTemp_fast_coeff_upper<-Acr_pro_AbsTemp_fast_coeff[,1] + 1.96*Acr_pro_AbsTemp_fast_coeff[,2]

############################################################################################
#### combine ED25 data plus predict curves from models ####
############################################################################################

Acr_AbsTemp_coeff_means<-data.frame(Acr_exp_AbsTemp_slow_coeff_mean, Acr_exp_AbsTemp_fast_coeff_mean, Acr_pro_AbsTemp_slow_coeff_mean, Acr_pro_AbsTemp_fast_coeff_mean)
Acr_AbsTemp_coeff_lowers<-data.frame(Acr_exp_AbsTemp_slow_coeff_lower, Acr_exp_AbsTemp_fast_coeff_lower, Acr_pro_AbsTemp_slow_coeff_lower, Acr_pro_AbsTemp_fast_coeff_lower)
Acr_AbsTemp_coeff_uppers<-data.frame(Acr_exp_AbsTemp_slow_coeff_upper, Acr_exp_AbsTemp_fast_coeff_upper, Acr_pro_AbsTemp_slow_coeff_upper, Acr_pro_AbsTemp_fast_coeff_upper)

Acr_exp_AbsTemp_slow_preddata = data.frame(temp = seq(31,40, length.out = 100))
Acr_exp_AbsTemp_slow_pred = as.data.frame(predict(Acr_exp_AbsTemp_slow_1, newdata = Acr_exp_AbsTemp_slow_preddata, interval = 'confidence'))
Acr_exp_AbsTemp_slow_preddata = data.frame(Acr_exp_AbsTemp_slow_preddata, fvfm = Acr_exp_AbsTemp_slow_pred$Prediction, Lower = Acr_exp_AbsTemp_slow_pred$Lower, Upper = Acr_exp_AbsTemp_slow_pred$Upper)

Acr_exp_AbsTemp_fast_preddata = data.frame(temp = seq(31,40, length.out = 100))
Acr_exp_AbsTemp_fast_pred = as.data.frame(predict(Acr_exp_AbsTemp_fast_1, newdata = Acr_exp_AbsTemp_fast_preddata, interval = 'confidence'))
Acr_exp_AbsTemp_fast_preddata = data.frame(Acr_exp_AbsTemp_fast_preddata, fvfm = Acr_exp_AbsTemp_fast_pred$Prediction, Lower = Acr_exp_AbsTemp_fast_pred$Lower, Upper = Acr_exp_AbsTemp_fast_pred$Upper)

Acr_pro_AbsTemp_slow_preddata = data.frame(temp = seq(31,40, length.out = 100))
Acr_pro_AbsTemp_slow_pred = as.data.frame(predict(Acr_pro_AbsTemp_slow_1, newdata = Acr_pro_AbsTemp_slow_preddata, interval = 'confidence'))
Acr_pro_AbsTemp_slow_preddata = data.frame(Acr_pro_AbsTemp_slow_preddata, fvfm = Acr_pro_AbsTemp_slow_pred$Prediction, Lower = Acr_pro_AbsTemp_slow_pred$Lower, Upper = Acr_pro_AbsTemp_slow_pred$Upper)

Acr_pro_AbsTemp_fast_preddata = data.frame(temp = seq(31,40, length.out = 100))
Acr_pro_AbsTemp_fast_pred = as.data.frame(predict(Acr_pro_AbsTemp_fast_1, newdata = Acr_pro_AbsTemp_fast_preddata, interval = 'confidence'))
Acr_pro_AbsTemp_fast_preddata = data.frame(Acr_pro_AbsTemp_fast_preddata, fvfm = Acr_pro_AbsTemp_fast_pred$Prediction, Lower = Acr_pro_AbsTemp_fast_pred$Lower, Upper = Acr_pro_AbsTemp_fast_pred$Upper)

#################################################################################
#### Plot Acropora Protected and Exposed AbsTemp - slow vs. fast (Figure 4a) ####
#################################################################################

# Create population/ramp mix column to plot geom_jitters in separate colours
Acr_clean[,"pop_ramp"]<-as.character(NA)
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Slow")]<-"Pro_Slow"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Control_S")]<-"Pro_Slow"

Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Fast")]<-"Pro_Fast"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Control_F")]<-"Pro_Fast"
##
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Slow")]<-"Exp_Slow"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Control_S")]<-"Exp_Slow"

Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Fast")]<-"Exp_Fast"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Control_F")]<-"Exp_Fast"

Acr_clean$pop_ramp<-as.factor(Acr_clean$pop_ramp)

AbsTemp_Acr_slow_v_fast<- ggplot() +
  geom_jitter(data = Acr_clean, aes(x = AbsTemp, y = FvFm, color = pop_ramp), size = 1) + geom_jitter() +
  scale_x_continuous(limits=c(31,40), breaks=c(32,34,36,38,40)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = Acr_exp_AbsTemp_slow_preddata, aes(x = temp, y = fvfm), color = 'darkorange', show.legend = FALSE) +
  geom_ribbon(data = Acr_exp_AbsTemp_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_AbsTemp_coeff_means, aes(xintercept = Acr_exp_AbsTemp_slow_coeff_mean), color = 'darkorange', show.legend = FALSE) +
  annotate("rect", xmin=Acr_AbsTemp_coeff_lowers$Acr_exp_AbsTemp_slow_coeff_lower, xmax=Acr_AbsTemp_coeff_uppers$Acr_exp_AbsTemp_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange',  alpha = 0.2) +
  geom_text(data = Acr_AbsTemp_coeff_means, aes(label=round(Acr_exp_AbsTemp_slow_coeff_mean, digits = 2)), x = 33, y = 0.2, show.legend = FALSE, color = 'darkorange') +
  geom_line(data = Acr_exp_AbsTemp_fast_preddata, aes(x = temp, y = fvfm), color = 'darkorange4', show.legend = FALSE) +
  geom_ribbon(data = Acr_exp_AbsTemp_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange4', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_AbsTemp_coeff_means, aes(xintercept = Acr_exp_AbsTemp_fast_coeff_mean), color = 'darkorange4', show.legend = FALSE) +
  annotate("rect", xmin=Acr_AbsTemp_coeff_lowers$Acr_exp_AbsTemp_fast_coeff_lower, xmax=Acr_AbsTemp_coeff_uppers$Acr_exp_AbsTemp_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange4',  alpha = 0.2) +
  geom_text(data = Acr_AbsTemp_coeff_means, aes(label=round(Acr_exp_AbsTemp_fast_coeff_mean, digits = 2)), x = 33, y = 0.15, show.legend = FALSE, color = 'darkorange4') +
  geom_line(data = Acr_pro_AbsTemp_slow_preddata, aes(x = temp, y = fvfm), color = 'darkorchid1', show.legend = FALSE) +
  geom_ribbon(data = Acr_pro_AbsTemp_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorchid1', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_AbsTemp_coeff_means, aes(xintercept = Acr_pro_AbsTemp_slow_coeff_mean), color = 'darkorchid1', show.legend = FALSE) +
  annotate("rect", xmin=Acr_AbsTemp_coeff_lowers$Acr_pro_AbsTemp_slow_coeff_lower, xmax=Acr_AbsTemp_coeff_uppers$Acr_pro_AbsTemp_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorchid1',  alpha = 0.2) +
  geom_text(data = Acr_AbsTemp_coeff_means, aes(label=round(Acr_pro_AbsTemp_slow_coeff_mean, digits = 2)), x = 33, y = 0.10, show.legend = FALSE, color = 'darkorchid1') +
  geom_line(data = Acr_pro_AbsTemp_fast_preddata, aes(x = temp, y = fvfm), color = 'darkorchid4', show.legend = FALSE) +
  geom_ribbon(data = Acr_pro_AbsTemp_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorchid4', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_AbsTemp_coeff_means, aes(xintercept = Acr_pro_AbsTemp_fast_coeff_mean), color = 'darkorchid4', show.legend = FALSE) +
  annotate("rect", xmin=Acr_AbsTemp_coeff_lowers$Acr_pro_AbsTemp_fast_coeff_lower, xmax=Acr_AbsTemp_coeff_uppers$Acr_pro_AbsTemp_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorchid4',  alpha = 0.2) +
  geom_text(data = Acr_AbsTemp_coeff_means, aes(label=round(Acr_pro_AbsTemp_fast_coeff_mean, digits = 2)), x = 33, y = 0.05, show.legend = FALSE, color = 'darkorchid4') +
  ggtitle("Acropora - Temperature (°C)") +
  scale_color_manual(values=c("darkorange4","darkorange","darkorchid4","darkorchid1")) +
  ylab("Fv/Fm") +
  xlab("Temperature (°C)") +
  theme_bw()
AbsTemp_Acr_slow_v_fast

###############################################
#### Porites Exposed AbsTemp slow vs. fast ####
###############################################

#### Por Exp AbsTemp Slow ####

#model
Por_exp_AbsTemp_slow_1 <- drm(FvFm ~ AbsTemp, data = Por_exp_slow, fct = LL.3())
summary(Por_exp_AbsTemp_slow_1)
plot(Por_exp_AbsTemp_slow_1)

#extract coeffs
Por_exp_AbsTemp_slow_coeff<-data.frame(ED(Por_exp_AbsTemp_slow_1, c(25)))

Por_exp_AbsTemp_slow_coeff_mean<-Por_exp_AbsTemp_slow_coeff[,1]
Por_exp_AbsTemp_slow_coeff_lower<-Por_exp_AbsTemp_slow_coeff[,1] - 1.96*Por_exp_AbsTemp_slow_coeff[,2]
Por_exp_AbsTemp_slow_coeff_upper<-Por_exp_AbsTemp_slow_coeff[,1] + 1.96*Por_exp_AbsTemp_slow_coeff[,2]

#### Por Exp AbsTemp Fast ####

#model
Por_exp_AbsTemp_fast_1 <- drm(FvFm ~ AbsTemp, data = Por_exp_fast, fct = LL.3())
summary(Por_exp_AbsTemp_fast_1)
plot(Por_exp_AbsTemp_fast_1)

#extract coeffs
Por_exp_AbsTemp_fast_coeff<-data.frame(ED(Por_exp_AbsTemp_fast_1, c(25)))

Por_exp_AbsTemp_fast_coeff_mean<-Por_exp_AbsTemp_fast_coeff[,1]
Por_exp_AbsTemp_fast_coeff_lower<-Por_exp_AbsTemp_fast_coeff[,1] - 1.96*Por_exp_AbsTemp_fast_coeff[,2]
Por_exp_AbsTemp_fast_coeff_upper<-Por_exp_AbsTemp_fast_coeff[,1] + 1.96*Por_exp_AbsTemp_fast_coeff[,2]

#################################################
#### Porites Protected AbsTemp slow vs. fast ####
#################################################

#### Por Pro AbsTemp Slow ####

#model
Por_pro_AbsTemp_slow_1 <- drm(FvFm ~ AbsTemp, data = Por_pro_slow, fct = LL.3())
summary(Por_pro_AbsTemp_slow_1)
plot(Por_pro_AbsTemp_slow_1)

#extract coeffs
Por_pro_AbsTemp_slow_coeff<-data.frame(ED(Por_pro_AbsTemp_slow_1, c(25)))

Por_pro_AbsTemp_slow_coeff_mean<-Por_pro_AbsTemp_slow_coeff[,1]
Por_pro_AbsTemp_slow_coeff_lower<-Por_pro_AbsTemp_slow_coeff[,1] - 1.96*Por_pro_AbsTemp_slow_coeff[,2]
Por_pro_AbsTemp_slow_coeff_upper<-Por_pro_AbsTemp_slow_coeff[,1] + 1.96*Por_pro_AbsTemp_slow_coeff[,2]

#### Por Pro AbsTemp Fast ####

#geno
Por_pro_AbsTemp_fast <- drm(FvFm ~ AbsTemp, curveid = Colony, data = Por_pro_fast, fct = LL.3())
summary(Por_pro_AbsTemp_fast)
plot(Por_pro_AbsTemp_fast)

#model
Por_pro_AbsTemp_fast_1 <- drm(FvFm ~ AbsTemp, data = Por_pro_fast, fct = LL.3())
summary(Por_pro_AbsTemp_fast_1)
plot(Por_pro_AbsTemp_fast_1)

#extract coeffs
Por_pro_AbsTemp_fast_coeff<-data.frame(ED(Por_pro_AbsTemp_fast_1, c(25)))

Por_pro_AbsTemp_fast_coeff_mean<-Por_pro_AbsTemp_fast_coeff[,1]
Por_pro_AbsTemp_fast_coeff_lower<-Por_pro_AbsTemp_fast_coeff[,1] - 1.96*Por_pro_AbsTemp_fast_coeff[,2]
Por_pro_AbsTemp_fast_coeff_upper<-Por_pro_AbsTemp_fast_coeff[,1] + 1.96*Por_pro_AbsTemp_fast_coeff[,2]

############################################################################################
#### combine ED25 data plus predict curves from models ####
############################################################################################

Por_AbsTemp_coeff_means<-data.frame(Por_exp_AbsTemp_slow_coeff_mean, Por_exp_AbsTemp_fast_coeff_mean, Por_pro_AbsTemp_slow_coeff_mean, Por_pro_AbsTemp_fast_coeff_mean)
Por_AbsTemp_coeff_lowers<-data.frame(Por_exp_AbsTemp_slow_coeff_lower, Por_exp_AbsTemp_fast_coeff_lower, Por_pro_AbsTemp_slow_coeff_lower, Por_pro_AbsTemp_fast_coeff_lower)
Por_AbsTemp_coeff_uppers<-data.frame(Por_exp_AbsTemp_slow_coeff_upper, Por_exp_AbsTemp_fast_coeff_upper, Por_pro_AbsTemp_slow_coeff_upper, Por_pro_AbsTemp_fast_coeff_upper)

Por_exp_AbsTemp_slow_preddata = data.frame(temp = seq(31,40, length.out = 100))
Por_exp_AbsTemp_slow_pred = as.data.frame(predict(Por_exp_AbsTemp_slow_1, newdata = Por_exp_AbsTemp_slow_preddata, interval = 'confidence'))
Por_exp_AbsTemp_slow_preddata = data.frame(Por_exp_AbsTemp_slow_preddata, fvfm = Por_exp_AbsTemp_slow_pred$Prediction, Lower = Por_exp_AbsTemp_slow_pred$Lower, Upper = Por_exp_AbsTemp_slow_pred$Upper)

Por_exp_AbsTemp_fast_preddata = data.frame(temp = seq(31,40, length.out = 100))
Por_exp_AbsTemp_fast_pred = as.data.frame(predict(Por_exp_AbsTemp_fast_1, newdata = Por_exp_AbsTemp_fast_preddata, interval = 'confidence'))
Por_exp_AbsTemp_fast_preddata = data.frame(Por_exp_AbsTemp_fast_preddata, fvfm = Por_exp_AbsTemp_fast_pred$Prediction, Lower = Por_exp_AbsTemp_fast_pred$Lower, Upper = Por_exp_AbsTemp_fast_pred$Upper)

Por_pro_AbsTemp_slow_preddata = data.frame(temp = seq(31,40, length.out = 100))
Por_pro_AbsTemp_slow_pred = as.data.frame(predict(Por_pro_AbsTemp_slow_1, newdata = Por_pro_AbsTemp_slow_preddata, interval = 'confidence'))
Por_pro_AbsTemp_slow_preddata = data.frame(Por_pro_AbsTemp_slow_preddata, fvfm = Por_pro_AbsTemp_slow_pred$Prediction, Lower = Por_pro_AbsTemp_slow_pred$Lower, Upper = Por_pro_AbsTemp_slow_pred$Upper)

Por_pro_AbsTemp_fast_preddata = data.frame(temp = seq(31,40, length.out = 100))
Por_pro_AbsTemp_fast_pred = as.data.frame(predict(Por_pro_AbsTemp_fast_1, newdata = Por_pro_AbsTemp_fast_preddata, interval = 'confidence'))
Por_pro_AbsTemp_fast_preddata = data.frame(Por_pro_AbsTemp_fast_preddata, fvfm = Por_pro_AbsTemp_fast_pred$Prediction, Lower = Por_pro_AbsTemp_fast_pred$Lower, Upper = Por_pro_AbsTemp_fast_pred$Upper)

################################################################################
#### Plot Porites Protected and Exposed AbsTemp - slow vs. fast (Figure 4b) ####
################################################################################

# Create population/ramp mix column to plot geom_jitters in separate colours
Por_clean[,"pop_ramp"]<-as.character(NA)
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Slow")]<-"Pro_Slow"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Control_S")]<-"Pro_Slow"

Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Fast")]<-"Pro_Fast"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Control_F")]<-"Pro_Fast"
##
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Slow")]<-"Exp_Slow"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Control_S")]<-"Exp_Slow"

Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Fast")]<-"Exp_Fast"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Control_F")]<-"Exp_Fast"

Por_clean$pop_ramp<-as.factor(Por_clean$pop_ramp)

AbsTemp_Por_slow_v_fast<- ggplot() +
  geom_jitter(data = Por_clean, aes(x = AbsTemp, y = FvFm, color = pop_ramp), size = 1) + geom_jitter() +
  scale_x_continuous(limits=c(31,40), breaks=c(32,34,36,38,40)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = Por_exp_AbsTemp_slow_preddata, aes(x = temp, y = fvfm), color = 'royalblue1', show.legend = FALSE) +
  geom_ribbon(data = Por_exp_AbsTemp_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue1', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_AbsTemp_coeff_means, aes(xintercept = Por_exp_AbsTemp_slow_coeff_mean), color = 'royalblue1', show.legend = FALSE) +
  annotate("rect", xmin=Por_AbsTemp_coeff_lowers$Por_exp_AbsTemp_slow_coeff_lower, xmax=Por_AbsTemp_coeff_uppers$Por_exp_AbsTemp_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue1',  alpha = 0.2) +
  geom_text(data = Por_AbsTemp_coeff_means, aes(label=round(Por_exp_AbsTemp_slow_coeff_mean, digits = 2)), x = 33, y = 0.2, show.legend = FALSE, color = 'royalblue1') +
  geom_line(data = Por_exp_AbsTemp_fast_preddata, aes(x = temp, y = fvfm), color = 'royalblue4', show.legend = FALSE) +
  geom_ribbon(data = Por_exp_AbsTemp_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue4', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_AbsTemp_coeff_means, aes(xintercept = Por_exp_AbsTemp_fast_coeff_mean), color = 'royalblue4', show.legend = FALSE) +
  annotate("rect", xmin=Por_AbsTemp_coeff_lowers$Por_exp_AbsTemp_fast_coeff_lower, xmax=Por_AbsTemp_coeff_uppers$Por_exp_AbsTemp_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue4',  alpha = 0.2) +
  geom_text(data = Por_AbsTemp_coeff_means, aes(label=round(Por_exp_AbsTemp_fast_coeff_mean, digits = 2)), x = 33, y = 0.15, show.legend = FALSE, color = 'royalblue4') +
  geom_line(data = Por_pro_AbsTemp_slow_preddata, aes(x = temp, y = fvfm), color = 'springgreen1', show.legend = FALSE) +
  geom_ribbon(data = Por_pro_AbsTemp_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen1', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_AbsTemp_coeff_means, aes(xintercept = Por_pro_AbsTemp_slow_coeff_mean), color = 'springgreen1', show.legend = FALSE) +
  annotate("rect", xmin=Por_AbsTemp_coeff_lowers$Por_pro_AbsTemp_slow_coeff_lower, xmax=Por_AbsTemp_coeff_uppers$Por_pro_AbsTemp_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen1',  alpha = 0.2) +
  geom_text(data = Por_AbsTemp_coeff_means, aes(label=round(Por_pro_AbsTemp_slow_coeff_mean, digits = 2)), x = 33, y = 0.10, show.legend = FALSE, color = 'springgreen1') +
  geom_line(data = Por_pro_AbsTemp_fast_preddata, aes(x = temp, y = fvfm), color = 'springgreen4', show.legend = FALSE) +
  geom_ribbon(data = Por_pro_AbsTemp_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen4', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_AbsTemp_coeff_means, aes(xintercept = Por_pro_AbsTemp_fast_coeff_mean), color = 'springgreen4', show.legend = FALSE) +
  annotate("rect", xmin=Por_AbsTemp_coeff_lowers$Por_pro_AbsTemp_fast_coeff_lower, xmax=Por_AbsTemp_coeff_uppers$Por_pro_AbsTemp_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen4',  alpha = 0.2) +
  geom_text(data = Por_AbsTemp_coeff_means, aes(label=round(Por_pro_AbsTemp_fast_coeff_mean, digits = 2)), x = 33, y = 0.05, show.legend = FALSE, color = 'springgreen4') +
  ggtitle("Porites - Temperature (°C)") +
  scale_color_manual(values=c("royalblue4","royalblue1","springgreen4","springgreen1")) +
  ylab("Fv/Fm") +
  xlab("Temperature (°C)") +
  theme_bw()
AbsTemp_Por_slow_v_fast

#### Stats for AbsTemp data ####

#Acropora
Indoor_Acr_AbsTemp_DRC_ID<-drm(FvFm ~ AbsTemp, data = Acr_clean, curveid = pop_ramp,
                               fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(Indoor_Acr_AbsTemp_DRC_ID)
EDcomp(Indoor_Acr_AbsTemp_DRC_ID, c(25,25)) 


#Porites
Indoor_Por_AbsTemp_DRC_ID<-drm(FvFm ~ AbsTemp, data = Por_clean, curveid = pop_ramp,
                               fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(Indoor_Por_AbsTemp_DRC_ID)
EDcomp(Indoor_Por_AbsTemp_DRC_ID, c(25,25)) 


#### Heatload 34 DRC analysis and plotting for Figure 5 ####

##############################################
#### Acropora Exposed HL34 slow vs. fast ####
#############################################

#### Acr Exp HL 34 Slow ####

#model
Acr_exp_HL34_slow_1 <- drm(FvFm ~ Heatload34, data = Acr_exp_slow, fct = LL.3())
summary(Acr_exp_HL34_slow_1)
plot(Acr_exp_HL34_slow_1)

#extract coeffs
Acr_exp_34_slow_coeff<-data.frame(ED(Acr_exp_HL34_slow_1, c(25)))

Acr_exp_34_slow_coeff_mean<-Acr_exp_34_slow_coeff[,1]
Acr_exp_34_slow_coeff_lower<-Acr_exp_34_slow_coeff[,1] - 1.96*Acr_exp_34_slow_coeff[,2]
Acr_exp_34_slow_coeff_upper<-Acr_exp_34_slow_coeff[,1] + 1.96*Acr_exp_34_slow_coeff[,2]

#### Acr Exp HL 34 Fast ####

#model
Acr_exp_HL34_fast_1 <- drm(FvFm ~ Heatload34, data = Acr_exp_fast, fct = LL.3())
summary(Acr_exp_HL34_fast_1)
plot(Acr_exp_HL34_fast_1)

#extract coeffs
Acr_exp_34_fast_coeff<-data.frame(ED(Acr_exp_HL34_fast_1, c(25)))

Acr_exp_34_fast_coeff_mean<-Acr_exp_34_fast_coeff[,1]
Acr_exp_34_fast_coeff_lower<-Acr_exp_34_fast_coeff[,1] - 1.96*Acr_exp_34_fast_coeff[,2]
Acr_exp_34_fast_coeff_upper<-Acr_exp_34_fast_coeff[,1] + 1.96*Acr_exp_34_fast_coeff[,2]

#################################################
#### Acropora Protected HL34 - slow vs. fast ####
#################################################

#model
Acr_pro_HL34_slow_1 <- drm(FvFm ~ Heatload34, data = Acr_pro_slow, fct = LL.3())
summary(Acr_pro_HL34_slow_1)
plot(Acr_pro_HL34_slow_1)

#extract coeffs
Acr_pro_34_slow_coeff<-data.frame(ED(Acr_pro_HL34_slow_1, c(25)))

Acr_pro_34_slow_coeff_mean<-Acr_pro_34_slow_coeff[,1]
Acr_pro_34_slow_coeff_lower<-Acr_pro_34_slow_coeff[,1] - 1.96*Acr_pro_34_slow_coeff[,2]
Acr_pro_34_slow_coeff_upper<-Acr_pro_34_slow_coeff[,1] + 1.96*Acr_pro_34_slow_coeff[,2]

#### Acr Pro HL 34 Fast ####

#model
Acr_pro_HL34_fast_1 <- drm(FvFm ~ Heatload34, data = Acr_pro_fast, fct = LL.3())
summary(Acr_pro_HL34_fast_1)
plot(Acr_pro_HL34_fast_1)

#extract coeffs
Acr_pro_34_fast_coeff<-data.frame(ED(Acr_pro_HL34_fast_1, c(25)))

Acr_pro_34_fast_coeff_mean<-Acr_pro_34_fast_coeff[,1]
Acr_pro_34_fast_coeff_lower<-Acr_pro_34_fast_coeff[,1] - 1.96*Acr_pro_34_fast_coeff[,2]
Acr_pro_34_fast_coeff_upper<-Acr_pro_34_fast_coeff[,1] + 1.96*Acr_pro_34_fast_coeff[,2]

############################################################
#### combine ED25 data plus predict curves from models ####
###########################################################

Acr_34_coeff_means<-data.frame(Acr_exp_34_slow_coeff_mean, Acr_exp_34_fast_coeff_mean, Acr_pro_34_slow_coeff_mean, Acr_pro_34_fast_coeff_mean)
Acr_34_coeff_lowers<-data.frame(Acr_exp_34_slow_coeff_lower, Acr_exp_34_fast_coeff_lower, Acr_pro_34_slow_coeff_lower, Acr_pro_34_fast_coeff_lower)
Acr_34_coeff_uppers<-data.frame(Acr_exp_34_slow_coeff_upper, Acr_exp_34_fast_coeff_upper, Acr_pro_34_slow_coeff_upper, Acr_pro_34_fast_coeff_upper)

Acr_exp_34_slow_preddata = data.frame(temp = seq(0,120, length.out = 100))
Acr_exp_34_slow_pred = as.data.frame(predict(Acr_exp_HL34_slow_1, newdata = Acr_exp_34_slow_preddata, interval = 'confidence'))
Acr_exp_34_slow_preddata = data.frame(Acr_exp_34_slow_preddata, fvfm = Acr_exp_34_slow_pred$Prediction, Lower = Acr_exp_34_slow_pred$Lower, Upper = Acr_exp_34_slow_pred$Upper)

Acr_exp_34_fast_preddata = data.frame(temp = seq(0,120, length.out = 100))
Acr_exp_34_fast_pred = as.data.frame(predict(Acr_exp_HL34_fast_1, newdata = Acr_exp_34_fast_preddata, interval = 'confidence'))
Acr_exp_34_fast_preddata = data.frame(Acr_exp_34_fast_preddata, fvfm = Acr_exp_34_fast_pred$Prediction, Lower = Acr_exp_34_fast_pred$Lower, Upper = Acr_exp_34_fast_pred$Upper)

Acr_pro_34_slow_preddata = data.frame(temp = seq(0,120, length.out = 100))
Acr_pro_34_slow_pred = as.data.frame(predict(Acr_pro_HL34_slow_1, newdata = Acr_pro_34_slow_preddata, interval = 'confidence'))
Acr_pro_34_slow_preddata = data.frame(Acr_pro_34_slow_preddata, fvfm = Acr_pro_34_slow_pred$Prediction, Lower = Acr_pro_34_slow_pred$Lower, Upper = Acr_pro_34_slow_pred$Upper)

Acr_pro_34_fast_preddata = data.frame(temp = seq(0,120, length.out = 100))
Acr_pro_34_fast_pred = as.data.frame(predict(Acr_pro_HL34_fast_1, newdata = Acr_pro_34_fast_preddata, interval = 'confidence'))
Acr_pro_34_fast_preddata = data.frame(Acr_pro_34_fast_preddata, fvfm = Acr_pro_34_fast_pred$Prediction, Lower = Acr_pro_34_fast_pred$Lower, Upper = Acr_pro_34_fast_pred$Upper)

#############################################################################
#### Plot Acropora Protected and Exposed HL34 - slow vs. fast (Fig. 5a) ####
############################################################################

# Create population/ramp mix column to plot geom_jitters in separate colours
Acr_clean[,"pop_ramp"]<-as.character(NA)
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Slow")]<-"Pro_Slow"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Control_S")]<-"Pro_Slow"

Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Fast")]<-"Pro_Fast"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Pro") & (Acr_clean$Ramp=="Control_F")]<-"Pro_Fast"
##
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Slow")]<-"Exp_Slow"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Control_S")]<-"Exp_Slow"

Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Fast")]<-"Exp_Fast"
Acr_clean$pop_ramp[Name=(Acr_clean$Population == "Acr_Exp") & (Acr_clean$Ramp=="Control_F")]<-"Exp_Fast"

Acr_clean$pop_ramp<-as.factor(Acr_clean$pop_ramp)

HL34_Acr_slow_v_fast<- ggplot() +
  geom_jitter(data = Acr_clean, aes(x = Heatload34, y = FvFm, color = pop_ramp), size = 1) + geom_jitter() +
  scale_x_continuous(limits=c(0, 120), breaks=c(0, 30, 60, 90, 120)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = Acr_exp_34_slow_preddata, aes(x = temp, y = fvfm), color = 'darkorange', show.legend = FALSE) +
  geom_ribbon(data = Acr_exp_34_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_34_coeff_means, aes(xintercept = Acr_exp_34_slow_coeff_mean), color = 'darkorange', show.legend = FALSE) +
  annotate("rect", xmin=Acr_34_coeff_lowers$Acr_exp_34_slow_coeff_lower, xmax=Acr_34_coeff_uppers$Acr_exp_34_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange',  alpha = 0.2) +
  geom_text(data = Acr_34_coeff_means, aes(label=round(Acr_exp_34_slow_coeff_mean, digits = 2)), x = 20, y = 0.2, show.legend = FALSE, color = 'darkorange') +
  geom_line(data = Acr_exp_34_fast_preddata, aes(x = temp, y = fvfm), color = 'darkorange4', show.legend = FALSE) +
  geom_ribbon(data = Acr_exp_34_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorange4', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_34_coeff_means, aes(xintercept = Acr_exp_34_fast_coeff_mean), color = 'darkorange4', show.legend = FALSE) +
  annotate("rect", xmin=Acr_34_coeff_lowers$Acr_exp_34_fast_coeff_lower, xmax=Acr_34_coeff_uppers$Acr_exp_34_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorange4',  alpha = 0.2) +
  geom_text(data = Acr_34_coeff_means, aes(label=round(Acr_exp_34_fast_coeff_mean, digits = 2)), x = 20, y = 0.15, show.legend = FALSE, color = 'darkorange4') +
  geom_line(data = Acr_pro_34_slow_preddata, aes(x = temp, y = fvfm), color = 'darkorchid1', show.legend = FALSE) +
  geom_ribbon(data = Acr_pro_34_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorchid1', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_34_coeff_means, aes(xintercept = Acr_pro_34_slow_coeff_mean), color = 'darkorchid1', show.legend = FALSE) +
  annotate("rect", xmin=Acr_34_coeff_lowers$Acr_pro_34_slow_coeff_lower, xmax=Acr_34_coeff_uppers$Acr_pro_34_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorchid1',  alpha = 0.2) +
  geom_text(data = Acr_34_coeff_means, aes(label=round(Acr_pro_34_slow_coeff_mean, digits = 2)), x = 20, y = 0.10, show.legend = FALSE, color = 'darkorchid1') +
  geom_line(data = Acr_pro_34_fast_preddata, aes(x = temp, y = fvfm), color = 'darkorchid4', show.legend = FALSE) +
  geom_ribbon(data = Acr_pro_34_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkorchid4', linetype=2, alpha = 0.2) +
  geom_vline(data = Acr_34_coeff_means, aes(xintercept = Acr_pro_34_fast_coeff_mean), color = 'darkorchid4', show.legend = FALSE) +
  annotate("rect", xmin=Acr_34_coeff_lowers$Acr_pro_34_fast_coeff_lower, xmax=Acr_34_coeff_uppers$Acr_pro_34_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkorchid4',  alpha = 0.2) +
  geom_text(data = Acr_34_coeff_means, aes(label=round(Acr_pro_34_fast_coeff_mean, digits = 2)), x = 20, y = 0.05, show.legend = FALSE, color = 'darkorchid4') +
  ggtitle("Acropora - Heatload 34°C") +
  scale_color_manual(values=c("darkorange4","darkorange","darkorchid4","darkorchid1")) +
  ylab("Fv/Fm") +
  xlab("Heatload (hours) above 34°C") +
  theme_bw()
HL34_Acr_slow_v_fast

##############################################################################################
##############################################################################################
##############################################################################################

############################################
#### Porites Exposed HL34 slow vs. fast ####
############################################

#### Por Exp HL 34 Slow ####

#model
Por_exp_HL34_slow_1 <- drm(FvFm ~ Heatload34, data = Por_exp_slow, fct = LL.3())
summary(Por_exp_HL34_slow_1)
plot(Por_exp_HL34_slow_1)

#extract coeffs
Por_exp_34_slow_coeff<-data.frame(ED(Por_exp_HL34_slow_1, c(25)))

Por_exp_34_slow_coeff_mean<-Por_exp_34_slow_coeff[,1]
Por_exp_34_slow_coeff_lower<-Por_exp_34_slow_coeff[,1] - 1.96*Por_exp_34_slow_coeff[,2]
Por_exp_34_slow_coeff_upper<-Por_exp_34_slow_coeff[,1] + 1.96*Por_exp_34_slow_coeff[,2]

#### Por Exp HL 34 Fast ####

#model
Por_exp_HL34_fast_1 <- drm(FvFm ~ Heatload34, data = Por_exp_fast, fct = LL.3())
summary(Por_exp_HL34_fast_1)
plot(Por_exp_HL34_fast_1)

#extract coeffs
Por_exp_34_fast_coeff<-data.frame(ED(Por_exp_HL34_fast_1, c(25)))

Por_exp_34_fast_coeff_mean<-Por_exp_34_fast_coeff[,1]
Por_exp_34_fast_coeff_lower<-Por_exp_34_fast_coeff[,1] - 1.96*Por_exp_34_fast_coeff[,2]
Por_exp_34_fast_coeff_upper<-Por_exp_34_fast_coeff[,1] + 1.96*Por_exp_34_fast_coeff[,2]

################################################
#### Porites Protected HL34 - slow vs. fast ####
################################################

#model
Por_pro_HL34_slow_1 <- drm(FvFm ~ Heatload34, data = Por_pro_slow, fct = LL.3())
summary(Por_pro_HL34_slow_1)
plot(Por_pro_HL34_slow_1)

#extract coeffs
Por_pro_34_slow_coeff<-data.frame(ED(Por_pro_HL34_slow_1, c(25)))

Por_pro_34_slow_coeff_mean<-Por_pro_34_slow_coeff[,1]
Por_pro_34_slow_coeff_lower<-Por_pro_34_slow_coeff[,1] - 1.96*Por_pro_34_slow_coeff[,2]
Por_pro_34_slow_coeff_upper<-Por_pro_34_slow_coeff[,1] + 1.96*Por_pro_34_slow_coeff[,2]

#### Por Pro HL 34 Fast ####

#model
Por_pro_HL34_fast_1 <- drm(FvFm ~ Heatload34, data = Por_pro_fast, fct = LL.3())
summary(Por_pro_HL34_fast_1)
plot(Por_pro_HL34_fast_1)

#extract coeffs
Por_pro_34_fast_coeff<-data.frame(ED(Por_pro_HL34_fast_1, c(25)))

Por_pro_34_fast_coeff_mean<-Por_pro_34_fast_coeff[,1]
Por_pro_34_fast_coeff_lower<-Por_pro_34_fast_coeff[,1] - 1.96*Por_pro_34_fast_coeff[,2]
Por_pro_34_fast_coeff_upper<-Por_pro_34_fast_coeff[,1] + 1.96*Por_pro_34_fast_coeff[,2]

############################################################################################
#### combine ED25 data plus predict curves from models ####
############################################################################################

Por_34_coeff_means<-data.frame(Por_exp_34_slow_coeff_mean, Por_exp_34_fast_coeff_mean, Por_pro_34_slow_coeff_mean, Por_pro_34_fast_coeff_mean)
Por_34_coeff_lowers<-data.frame(Por_exp_34_slow_coeff_lower, Por_exp_34_fast_coeff_lower, Por_pro_34_slow_coeff_lower, Por_pro_34_fast_coeff_lower)
Por_34_coeff_uppers<-data.frame(Por_exp_34_slow_coeff_upper, Por_exp_34_fast_coeff_upper, Por_pro_34_slow_coeff_upper, Por_pro_34_fast_coeff_upper)

Por_exp_34_slow_preddata = data.frame(temp = seq(0,220, length.out = 100))
Por_exp_34_slow_pred = as.data.frame(predict(Por_exp_HL34_slow_1, newdata = Por_exp_34_slow_preddata, interval = 'confidence'))
Por_exp_34_slow_preddata = data.frame(Por_exp_34_slow_preddata, fvfm = Por_exp_34_slow_pred$Prediction, Lower = Por_exp_34_slow_pred$Lower, Upper = Por_exp_34_slow_pred$Upper)

Por_exp_34_fast_preddata = data.frame(temp = seq(0,220, length.out = 100))
Por_exp_34_fast_pred = as.data.frame(predict(Por_exp_HL34_fast_1, newdata = Por_exp_34_fast_preddata, interval = 'confidence'))
Por_exp_34_fast_preddata = data.frame(Por_exp_34_fast_preddata, fvfm = Por_exp_34_fast_pred$Prediction, Lower = Por_exp_34_fast_pred$Lower, Upper = Por_exp_34_fast_pred$Upper)

Por_pro_34_slow_preddata = data.frame(temp = seq(0,220, length.out = 100))
Por_pro_34_slow_pred = as.data.frame(predict(Por_pro_HL34_slow_1, newdata = Por_pro_34_slow_preddata, interval = 'confidence'))
Por_pro_34_slow_preddata = data.frame(Por_pro_34_slow_preddata, fvfm = Por_pro_34_slow_pred$Prediction, Lower = Por_pro_34_slow_pred$Lower, Upper = Por_pro_34_slow_pred$Upper)

Por_pro_34_fast_preddata = data.frame(temp = seq(0,220, length.out = 100))
Por_pro_34_fast_pred = as.data.frame(predict(Por_pro_HL34_fast_1, newdata = Por_pro_34_fast_preddata, interval = 'confidence'))
Por_pro_34_fast_preddata = data.frame(Por_pro_34_fast_preddata, fvfm = Por_pro_34_fast_pred$Prediction, Lower = Por_pro_34_fast_pred$Lower, Upper = Por_pro_34_fast_pred$Upper)

###########################################################################
#### Plot Porites Protected and Exposed HL34 - slow vs. fast (Fig. 5b) ####
###########################################################################

# Create population/ramp mix column to plot geom_jitters in separate colours
Por_clean[,"pop_ramp"]<-as.character(NA)
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Slow")]<-"Pro_Slow"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Control_S")]<-"Pro_Slow"

Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Fast")]<-"Pro_Fast"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Pro") & (Por_clean$Ramp=="Control_F")]<-"Pro_Fast"
##
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Slow")]<-"Exp_Slow"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Control_S")]<-"Exp_Slow"

Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Fast")]<-"Exp_Fast"
Por_clean$pop_ramp[Name=(Por_clean$Population == "Por_Exp") & (Por_clean$Ramp=="Control_F")]<-"Exp_Fast"

Por_clean$pop_ramp<-as.factor(Por_clean$pop_ramp)

HL34_Por_slow_v_fast<- ggplot() +
  geom_jitter(data = Por_clean, aes(x = Heatload34, y = FvFm, color = pop_ramp), size = 1) + geom_jitter() +
  scale_x_continuous(limits=c(0, 220), breaks=c(0, 50, 100, 150, 200)) +
  scale_y_continuous(limits=c(-0.02, 0.75), breaks=c(0, 0.2, 0.4, 0.6)) +
  geom_line(data = Por_exp_34_slow_preddata, aes(x = temp, y = fvfm), color = 'royalblue1', show.legend = FALSE) +
  geom_ribbon(data = Por_exp_34_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue1', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_34_coeff_means, aes(xintercept = Por_exp_34_slow_coeff_mean), color = 'royalblue1', show.legend = FALSE) +
  annotate("rect", xmin=Por_34_coeff_lowers$Por_exp_34_slow_coeff_lower, xmax=Por_34_coeff_uppers$Por_exp_34_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue',  alpha = 0.2) +
  geom_text(data = Por_34_coeff_means, aes(label=round(Por_exp_34_slow_coeff_mean, digits = 2)), x = 20, y = 0.2, show.legend = FALSE, color = 'royalblue1') +
  geom_line(data = Por_exp_34_fast_preddata, aes(x = temp, y = fvfm), color = 'royalblue4', show.legend = FALSE) +
  geom_ribbon(data = Por_exp_34_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue4', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_34_coeff_means, aes(xintercept = Por_exp_34_fast_coeff_mean), color = 'royalblue4', show.legend = FALSE) +
  annotate("rect", xmin=Por_34_coeff_lowers$Por_exp_34_fast_coeff_lower, xmax=Por_34_coeff_uppers$Por_exp_34_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue4',  alpha = 0.2) +
  geom_text(data = Por_34_coeff_means, aes(label=round(Por_exp_34_fast_coeff_mean, digits = 2)), x = 20, y = 0.15, show.legend = FALSE, color = 'royalblue4') +
  geom_line(data = Por_pro_34_slow_preddata, aes(x = temp, y = fvfm), color = 'springgreen1', show.legend = FALSE) +
  geom_ribbon(data = Por_pro_34_slow_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen1', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_34_coeff_means, aes(xintercept = Por_pro_34_slow_coeff_mean), color = 'springgreen1', show.legend = FALSE) +
  annotate("rect", xmin=Por_34_coeff_lowers$Por_pro_34_slow_coeff_lower, xmax=Por_34_coeff_uppers$Por_pro_34_slow_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen1',  alpha = 0.2) +
  geom_text(data = Por_34_coeff_means, aes(label=round(Por_pro_34_slow_coeff_mean, digits = 2)), x = 20, y = 0.10, show.legend = FALSE, color = 'springgreen1') +
  geom_line(data = Por_pro_34_fast_preddata, aes(x = temp, y = fvfm), color = 'springgreen4', show.legend = FALSE) +
  geom_ribbon(data = Por_pro_34_fast_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'springgreen4', linetype=2, alpha = 0.2) +
  geom_vline(data = Por_34_coeff_means, aes(xintercept = Por_pro_34_fast_coeff_mean), color = 'springgreen4', show.legend = FALSE) +
  annotate("rect", xmin=Por_34_coeff_lowers$Por_pro_34_fast_coeff_lower, xmax=Por_34_coeff_uppers$Por_pro_34_fast_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'springgreen4',  alpha = 0.2) +
  geom_text(data = Por_34_coeff_means, aes(label=round(Por_pro_34_fast_coeff_mean, digits = 2)), x = 20, y = 0.05, show.legend = FALSE, color = 'springgreen4') +
  ggtitle("Porites - Heatload 34°C") +
  scale_color_manual(values=c("royalblue4","royalblue1","springgreen4","springgreen1")) +
  ylab("Fv/Fm") +
  xlab("Heatload (hours) above 34°C") +
  theme_bw()
HL34_Por_slow_v_fast

#### Stats for indoor HL34 data ####

#Acropora
Indoor_Acr_HL34_DRC_ID<-drm(FvFm ~ Heatload34, data = Acr_clean, curveid = pop_ramp,
                            fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(Indoor_Acr_HL34_DRC_ID)
EDcomp(Indoor_Acr_HL34_DRC_ID, c(25,25))

#Porites
Indoor_Por_HL34_DRC_ID<-drm(FvFm ~ Heatload34, data = Por_clean, curveid = pop_ramp,
                            fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(Indoor_Por_HL34_DRC_ID)
EDcomp(Indoor_Por_HL34_DRC_ID, c(25,25))

#END
