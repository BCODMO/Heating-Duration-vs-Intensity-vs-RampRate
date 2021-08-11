#Code for Figure 1 of "Assessing the roles of heating rate and intensity on the response of corals to thermal stress"

#setwd to source file location

#### Organising and plotting tank temperature profiles for panel A ####

library(reshape2)
library(openair)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(gtable)
library(tidyselect)
library(tidyverse)
library(tidyr)
library(lmerTest)
library(emmeans)
library(multcomp)
library(nlme)
library(car)
library(viridis)
select <- dplyr::select

#Create separate spoing for LT tanks
boing0<-read.delim("KAUST_LT_Tank1-S33.5A_clean.txt")
boing0$DateTime<-strptime(boing0$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing0$DateTime<-as.character(boing0$DateTime)

boing10<-read.delim("KAUST_LT_Tank10-F35B_clean.txt")
boing10$DateTime<-strptime(boing10$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing10<-subset(boing10, DateTime < as.POSIXct('2019-08-6 10:35:00', tz = 'UTC'))
boing10$DateTime<-as.character(boing10$DateTime)
spoing<-merge(boing0[2:3], boing10[2:3], by="DateTime", all=T)

boing11<-read.delim("KAUST_LT_Tank11-F36.5A_clean.txt")
boing11$DateTime<-strptime(boing11$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing11<-subset(boing11, DateTime < as.POSIXct('2019-08-5 18:05:00', tz = 'UTC'))
boing11$DateTime<-as.character(boing11$DateTime)
spoing<-merge(spoing, boing11[2:3], by="DateTime", all=T)

boing12<-read.delim("KAUST_LT_Tank12-F36.5B_clean.txt")
boing12$DateTime<-strptime(boing12$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing12<-subset(boing12, DateTime < as.POSIXct('2019-08-5 18:05:00', tz = 'UTC'))
boing12$DateTime<-as.character(boing12$DateTime)
spoing<-merge(spoing, boing12[2:3], by="DateTime", all=T)

boing13<-read.delim("KAUST_LT_Tank13-Control_32A_clean.txt")
boing13$DateTime<-strptime(boing13$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing13$DateTime<-as.character(boing13$DateTime)
spoing<-merge(spoing, boing13[2:3], by="DateTime", all=T)

boing14<-read.delim("KAUST_LT_Tank14-Control_32B_clean.txt")
boing14$DateTime<-strptime(boing14$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing14$DateTime<-as.character(boing14$DateTime)
spoing<-merge(spoing, boing14[2:3], by="DateTime", all=T)

boing15<-read.delim("KAUST_LT_Tank2-S33.5B_clean.txt")
boing15$DateTime<-strptime(boing15$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing15$DateTime<-as.character(boing15$DateTime)
spoing<-merge(spoing, boing15[2:3], by="DateTime", all=T)

boing16<-read.delim("KAUST_LT_Tank3-S35A_clean.txt")
boing16$DateTime<-strptime(boing16$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing16<-subset(boing16, DateTime < as.POSIXct('2019-08-6 10:35:00', tz = 'UTC'))
boing16$DateTime<-as.character(boing16$DateTime)
spoing<-merge(spoing, boing16[2:3], by="DateTime", all=T)

boing17<-read.delim("KAUST_LT_Tank4-S35B_clean.txt")
boing17$DateTime<-strptime(boing17$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing17<-subset(boing17, DateTime < as.POSIXct('2019-08-6 10:35:00', tz = 'UTC'))
boing17$DateTime<-as.character(boing17$DateTime)
spoing<-merge(spoing, boing17[2:3], by="DateTime", all=T)

boing18<-read.delim("KAUST_LT_Tank5-S36.5A_clean.txt")
boing18$DateTime<-strptime(boing18$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing18<-subset(boing18, DateTime < as.POSIXct('2019-08-6 10:35:00', tz = 'UTC'))
boing18$DateTime<-as.character(boing18$DateTime)
spoing<-merge(spoing, boing18[2:3], by="DateTime", all=T)

boing19<-read.delim("KAUST_LT_Tank6-S36.5B_clean.txt")
boing19$DateTime<-strptime(boing19$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing19<-subset(boing19, DateTime < as.POSIXct('2019-08-6 10:35:00', tz = 'UTC'))
boing19$DateTime<-as.character(boing19$DateTime)
spoing<-merge(spoing, boing19[2:3], by="DateTime", all=T)

boing20<-read.delim("KAUST_LT_Tank7-F33.5A_clean.txt")
boing20$DateTime<-strptime(boing20$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing20$DateTime<-as.character(boing20$DateTime)
spoing<-merge(spoing, boing20[2:3], by="DateTime", all=T)

boing21<-read.delim("KAUST_LT_Tank8-F33.5B_clean.txt")
boing21$DateTime<-strptime(boing21$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing21$DateTime<-as.character(boing21$DateTime)
spoing<-merge(spoing, boing21[2:3], by="DateTime", all=T)

boing22<-read.delim("KAUST_LT_Tank9-F35A_clean.txt")
boing22$DateTime<-strptime(boing22$DateTime, format="%m/%d/%y %I:%M:%S %p")
boing22<-subset(boing22, DateTime < as.POSIXct('2019-08-6 10:35:00', tz = 'UTC'))
boing22$DateTime<-as.character(boing22$DateTime)
spoing<-merge(spoing, boing22[2:3], by="DateTime", all=T)

spoing$DateTime<-as.POSIXct(spoing$DateTime, tz="UTC")
Temp_full <- melt(spoing, id="DateTime")

names(Temp_full)[names(Temp_full) == "DateTime"] <- "date"
names(Temp_full)[names(Temp_full) == "variable"] <- "Tank"
names(Temp_full)[names(Temp_full) == "value"] <- "Temp"

Temp_full$date<-as.POSIXct(Temp_full$date)

levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank1.S33.5A"] <- "S33.5A"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank2.S33.5B"] <- "S33.5B"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank3.S35A"] <- "S35A"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank4.S35B"] <- "S35B"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank5.S36.5A"] <- "S36.5A"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank6.S36.5B"] <- "S36.5B"

levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank7.F33.5A"] <- "F33.5A"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank8.F33.5B"] <- "F33.5B"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank9.F35A"] <- "F35A"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank10.F35B"] <- "F35B"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank11.F36.5A"] <- "F36.5A"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank12.F36.5B"] <- "F36.5B"

levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank13.Control_32A"] <- "32A"
levels(Temp_full$Tank)[levels(Temp_full$Tank)=="KAUST_LT_Tank14.Control_32B"] <- "32B"

Temp_full <- na.omit(Temp_full)
Temp_av<-timeAverage(Temp_full, avg.time = "1 hour", type = "Tank")

Prolonged_cut<-subset(Temp_av, date > as.POSIXct('2019-07-29 16:00:00', tz = 'UTC'))
Prolonged_cut<-subset(Prolonged_cut, date < as.POSIXct('2019-08-12 14:00:00', tz = 'UTC'))

Prolonged_cut<-subset(Prolonged_cut, Tank!="S33.5B")

cols <- c("32A" = "navyblue", "32B" = "navyblue", 
          "S33.5A" = "green3", "S33.5B" = "green3", "F33.5A" = "green4", "F33.5B" = "green4",
          "S35A" = "darkorange", "S35B" = "darkorange", "F35A" = "darkorange3", "F35B" = "darkorange3",
          "S36.5A" = "red", "S36.5B" = "red", "F36.5A" = "red4", "F36.5B" = "red4")

Prolonged_temps<-ggplot(Prolonged_cut, aes(x=date, y=Temp, col=Tank)) + scale_colour_manual(values=cols) +
  geom_line(aes(linetype=Tank)) +
  scale_linetype_manual(values=c("solid","dashed","solid", "dashed", "solid", "dashed","solid", "dashed","solid","dashed","solid", "dashed", "solid")) +
  scale_y_continuous(limits=c(29,39), breaks = c(30,32,34,36,38)) + 
  scale_x_datetime(name = "Date", date_labels = "%b %d", date_breaks = "24 hour") +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  theme(text = element_text(size=15, face="bold")) + 
  theme(axis.text = element_text(colour = "black")) + theme(panel.background = element_rect(colour = "black", size=1)) + 
  theme(axis.ticks.length=unit(.3,"cm"))
Prolonged_temps

#### Organise, plot, and analyse Acropora Fv/Fm data for panel B ####

#Acropora treatment day ranges
#   F36.5 removed after day 4
#   F35 removed after day 5
#   S35 and S36.5 removed after day 8
dataa = read.csv('pam_prolonged_acropora.csv')
dataa = dataa[complete.cases(dataa),]
dataga = dataa %>%
  unite(Treatment, Ramp, Temp, remove = FALSE) %>%
  select(Species, Day, Treatment, Temp, Site, Colony, Rep, FvFm)

dataga$Treatment = as.factor(dataga$Treatment)
dataga$Day = as.factor(dataga$Day)
dataga$Colony = as.factor(dataga$Colony)

#Acropora all
fvfma<-lmer(FvFm ~ Treatment + Day + (1 | Rep) + (1 | Colony) + Treatment:Day,data=dataga)
step(fvfma, reduce.random = F)
print(emmeans(fvfma, list(pairwise ~ Treatment|Day)), adjust = c('mvt'))

emma = emmeans(fvfma, specs = ~ Treatment|Day)
emma

Control_32 = c(1, 0, 0, 0, 0, 0, 0)
Fast_33.5 = c(0, 1, 0, 0, 0, 0, 0)
Slow_33.5 = c(0, 0, 0, 0, 1, 0, 0)
contrast(emma, method = list("Control_32 - Fast_33.5" = Control_32 - Fast_33.5,
                             "Control_32 - Slow_33.5" = Control_32 - Slow_33.5), by=c('Day'), adjust = c("mvt"))

#Acropora comparisons for S35 and S36.5
Control_32 = c(1, 0, 0, 0, 0, 0, 0)
Slow_35 = c(0, 0, 0, 0, 0, 1, 0)
Slow_36.5 = c(0, 0, 0, 0, 0, 0, 1)
contrast(emma, method = list("Control_32 - Slow_35" = Control_32 - Slow_35,
                             "Control_32 - Slow_36.5" = Control_32 - Slow_36.5), by=c('Day'), adjust = c("mvt"))

#Acropora comparisons for F35
Control_32 = c(1, 0, 0, 0, 0, 0, 0)
Fast_35 = c(0, 0, 1, 0, 0, 0, 0)
contrast(emma, method = list("Control_32 - Fast_35" = Control_32 - Fast_35),
         by=c('Day'), adjust = c("mvt"))

#Acropora comparisons for F36
Control_32 = c(1, 0, 0, 0, 0, 0, 0)
Fast_36.5 = c(0, 0, 0, 1, 0, 0, 0)
contrast(emma, method = list("Control_32 - Fast_36.5" = Control_32 - Fast_36.5),
         by=c('Day'), adjust = c("mvt"))

#### Organise, plot, and analyse Porites Fv/Fm data for panel C ####

#Porites
datap = read.csv('pam_prolonged_porites.csv')
datap = datap[complete.cases(datap),]
datagp = datap %>%
  unite(Treatment, Ramp, Temp, remove = FALSE) %>%
  select(Species, Day, Treatment, Temp, Site, Colony, Rep, FvFm)

#Porites treatment day ranges
#   F35 and F36.5 removed after day 6
#   S35 and S36.5 removed after day 8

datagp$Treatment = as.factor(datagp$Treatment)
datagp$Day = as.factor(datagp$Day)
datagp$Colony = as.factor(datagp$Colony)

datagpf35 = subset(datagp, Treatment == 'Fast_35')
datagpf36 = subset(datagp, Treatment == 'Fast_36.5')

#Porites all
fvfmp<-lmer(FvFm ~ Treatment + Site + Day + (1 | Rep) + (1 | Colony) + Treatment:Site + Treatment:Day,data=datagp)
summary(fvfmp)
step(fvfmp, reduce.random = F)
print(emmeans(fvfmp, list(pairwise ~ Treatment|Day)), adjust = c('mvt'))
print(emmeans(fvfmp, list(pairwise ~ Site|Treatment)), adjust = c('mvt'))

emmp = emmeans(fvfmp, specs = ~ Day|Treatment)
emmp

#Porites comparisons for F33.5 and S33.5
Control_32 = c(1, 0, 0, 0, 0, 0, 0)
Fast_33.5 = c(0, 1, 0, 0, 0, 0, 0)
Slow_33.5 = c(0, 0, 0, 0, 1, 0, 0)
contrast(emmp, method = list("Control_32 - Fast_33.5" = Control_32 - Fast_33.5,
                             "Control_32 - Slow_33.5" = Control_32 - Slow_33.5), by=c('Day'), adjust = c("mvt"))

#Porites comparisons for S35 and S36.5
Control_32 = c(1, 0, 0, 0, 0, 0, 0)
Slow_35 = c(0, 0, 0, 0, 0, 1, 0)
Slow_36.5 = c(0, 0, 0, 0, 0, 0, 1)
contrast(emmp, method = list("Control_32 - Slow_35" = Control_32 - Slow_35,
                             "Control_32 - Slow_36.5" = Control_32 - Slow_36.5), by=c('Day'), adjust = c("mvt"))

#Porites comparisons for F35 and F36.5
emmps = emmeans(fvfmp, specs = ~ Site|Day|Treatment)
emmps
Control_32_E = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Control_32_P = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Fast_35_E = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Fast_35_P = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
Fast_36.5_E = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
Fast_36.5_P = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
contrast(emmps, method = list("Control_32_E - Fast_35_E" = Control_32_E - Fast_35_E,
                              "Control_32_P - Fast_35_P" = Control_32_P - Fast_35_P,
                              "Control_32_E - Fast_36.5_E" = Control_32_E - Fast_36.5_E,
                              "Control_32_P - Fast_36.5_P" = Control_32_P - Fast_36.5_P, 
                              "Fast_35_E - Fast_35_P" = Fast_35_E - Fast_35_P,
                              "Fast_36.5_E - Fast_36.5_P" = Fast_36.5_E - Fast_36.5_P), by=c('Day'), adjust = c("mvt"))

fvfmpf35<-lmer(FvFm ~ Site * Day + (1 | Rep) + (1 | Colony),data=datagpf35)
step(fvfmpf35, reduce.random = F)
print(emmeans(fvfmpf35, list(pairwise ~ Site|Day)), adjust = c('mvt'))

#### Plot panels B and C ####

data = rbind(datap, dataa)

data = data %>%
  unite(Treatment, Temp, Ramp, remove = FALSE) %>%
  unite(TreatSite, Treatment, Site, remove = FALSE)

data_group = group_by(data, Species, Day, Temp, Ramp)
dataav = summarize(data_group,
                   fvfm = mean(FvFm, na.rm = TRUE),
                   fvfmsd = sd(FvFm, na.rm = TRUE),
                   heat32 = mean(Heatload32, na.rm = TRUE),
                   heat32sd = sd(Heatload32, na.rm = TRUE),
                   heat33 = mean(Heatload33, na.rm = TRUE),
                   heat33sd = sd(Heatload33, na.rm = TRUE),
                   heat34 = mean(Heatload34, na.rm = TRUE),
                   heat34sd = sd(Heatload34, na.rm = TRUE),
                   abstemp = mean(AbsTemp, na.rm = TRUE))

data_groups = group_by(data, Species, Day, Temp, Ramp, Treatment, TreatSite, Site)
dataavs = summarize(data_groups,
                    fvfm = mean(FvFm, na.rm = TRUE),
                    fvfmsd = sd(FvFm, na.rm = TRUE),
                    heat32 = mean(Heatload32, na.rm = TRUE),
                    heat32sd = sd(Heatload32, na.rm = TRUE),
                    heat33 = mean(Heatload33, na.rm = TRUE),
                    heat33sd = sd(Heatload33, na.rm = TRUE),
                    heat34 = mean(Heatload34, na.rm = TRUE),
                    heat34sd = sd(Heatload34, na.rm = TRUE),
                    abstemp = mean(AbsTemp, na.rm = TRUE))

dataavs = subset(dataavs, Treatment == '36.5_Fast' | Treatment == '35_Fast')
dataavs = subset(dataavs, Species == 'Porites')
dataav = unite(dataav, Treatment, Temp, Ramp, remove = FALSE)

pfv_text = data.frame(Day = c(12.5,12.5,12.5,12.5), fvfm = c(0.5,0.5,0.5,0.5),
                      Temp = factor(c('35','35','36.5','36.5')), Species = factor(c('Porites','Acropora','Porites','Acropora')))

dataavp = subset(dataav, Species == 'Porites')
dataavp = subset(dataavp, Treatment != '36.5_Fast' & Treatment != '35_Fast')
dataava = subset(dataav, Species == 'Acropora')

#Porites
pfvp = ggplot(data = dataavp, aes(x = Day, y = fvfm, color = Treatment)) +
  geom_errorbar(aes(ymin = (fvfm - fvfmsd), ymax = (fvfm + fvfmsd)),
                color = 'gray', size = 0.3, width = 0.2) +
  geom_errorbar(data = dataavs, aes(ymin = (fvfm - fvfmsd), ymax = (fvfm + fvfmsd)),
                color = 'gray', size = 0.3, width = 0.2) +
  geom_point(size = 2, aes(color = Treatment)) +
  geom_line(size = 1, aes(color = Treatment)) +
  geom_point(data = dataavs, aes(x = Day, y = fvfm, color = TreatSite), size = 2) +
  geom_line(data = dataavs, aes(x = Day, y = fvfm, color = TreatSite, linetype = Site), size = 1) +
  ylab("Fv/Fm") +
  ggtitle("Porites") +
  scale_x_continuous(breaks = (by = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))) +
  scale_color_manual(name = "Ramp/Temp", labels = c("Control 32", "Fast 33.5", "Slow 33.5", "Fast 35 Exposed", "Fast 35 Protected", "Slow 35", "Fast 36.5 Exposed","Fast 36.5 Protected", "Slow 36.5"), values=c("#000080", "#00700A", "#00E109", "#CDAD00", "#CDAD00", "#CD6600", "#FF0000", "#FF0000", "#8B0000")) +
  scale_linetype_manual(values = c(3,6)) +
  theme_classic() +
  geom_text(aes(x = 5, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 7, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 8, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 9, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 10, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 11, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 12, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 13, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 14, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 9, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 10, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 11, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 12, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 13, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 14, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 3, y = .74), label = "*", size = 5, color = "#CDAD00") +
  geom_text(aes(x = 4, y = .74), label = "*", size = 5, color = "#CDAD00") +
  geom_text(aes(x = 5, y = .74), label = "*", size = 5, color = "#CDAD00") +
  geom_text(aes(x = 6, y = .74), label = "*", size = 5, color = "#CDAD00") +
  geom_text(aes(x = 5, y = .76), label = "*", size = 5, color = "#CD6600") +
  geom_text(aes(x = 6, y = .76), label = "*", size = 5, color = "#CD6600") +
  geom_text(aes(x = 7, y = .76), label = "*", size = 5, color = "#CD6600") +
  geom_text(aes(x = 8, y = .76), label = "*", size = 5, color = "#CD6600") +
  geom_text(aes(x = 3, y = .78), label = "*", size = 5, color = "#FF0000") +
  geom_text(aes(x = 4, y = .78), label = "*", size = 5, color = "#FF0000") +
  geom_text(aes(x = 5, y = .78), label = "*", size = 5, color = "#FF0000") +
  geom_text(aes(x = 6, y = .78), label = "*", size = 5, color = "#FF0000") +
  geom_text(aes(x = 7, y = .8), label = "*", size = 5, color = "#8B0000") +
  geom_text(aes(x = 8, y = .8), label = "*", size = 5, color = "#8B0000") +
  geom_text(aes(x = 3, y = .05), label = "+", size = 5, color = "#FF0000")
pfvp

#Acropora
pfva = ggplot(data = dataava, aes(x = Day, y = fvfm, color = Treatment)) +
  geom_errorbar(aes(ymin = (fvfm - fvfmsd), ymax = (fvfm + fvfmsd)),
                color = 'gray', size = 0.3, width = 0.2) +
  geom_point(size = 2, aes(color = Treatment)) +
  geom_line(size = 1, aes(color = Treatment)) +
  ylab("Fv/Fm") +
  ggtitle("Acropora") +
  scale_x_continuous(breaks = (by = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))) +
  scale_color_manual(name = "Ramp/Temp", labels = c("Control 32", "Fast 33.5", "Slow 33.5", "Fast 35", "Slow 35", "Fast 36.5", "Slow 36.5"), values=c("#000080", "#00700A", "#00E109", "#CDAD00", "#CD6600", "#FF0000", "#8B0000")) +
  theme_classic() +
  geom_text(aes(x = 9, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 11, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 12, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 13, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 14, y = .7), label = "*", size = 5, color = "#00700A") +
  geom_text(aes(x = 8, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 9, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 10, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 11, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 12, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 13, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 14, y = .72), label = "*", size = 5, color = "#00E109") +
  geom_text(aes(x = 3, y = .74), label = "*", size = 5, color = "#CDAD00") +
  geom_text(aes(x = 4, y = .74), label = "*", size = 5, color = "#CDAD00") +
  geom_text(aes(x = 5, y = .74), label = "*", size = 5, color = "#CDAD00") +
  geom_text(aes(x = 6, y = .76), label = "*", size = 5, color = "#CD6600") +
  geom_text(aes(x = 7, y = .76), label = "*", size = 5, color = "#CD6600") +
  geom_text(aes(x = 8, y = .76), label = "*", size = 5, color = "#CD6600") +
  geom_text(aes(x = 3, y = .78), label = "*", size = 5, color = "#FF0000") +
  geom_text(aes(x = 4, y = .78), label = "*", size = 5, color = "#FF0000") +
  geom_text(aes(x = 7, y = .8), label = "*", size = 5, color = "#8B0000") +
  geom_text(aes(x = 8, y = .8), label = "*", size = 5, color = "#8B0000")
pfva

