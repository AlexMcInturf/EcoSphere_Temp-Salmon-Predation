## Analysis: Year 1, all data ##
## Species: Largemouth bass, LFR Chinook Salmon ##
rm(list=ls())

library(lme4)
library(tidyverse)
library(mgcv)
#setwd("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020")
## Step 1: Bring data in ----
pred <- read.csv("Compiled_Pred_Summary_data.csv")
csas <- read.csv("CS_AS_cleaned_Spr2021.csv")
lmbas <- read.csv("LMB_AS_cleaned_Spr2021.csv")
csburst <- read.csv("CS_burst_cleaned_Sum2021.csv")
lmbburst <- read.csv("LMB_burst_cleaned_Spr2021.csv")

# > integrate controls ----
# these are the mean controls calculated for 2021, since controls during 2020 were not measured
thirtyLmean <- -50.67616
oneLmean <- 59.47707

csas$FIN.MMR <- csas$MMR - oneLmean
csas$FIN.RMR <- csas$RMR - oneLmean
csas$FIN.AS <- csas$FIN.MMR - csas$FIN.RMR



lmbas$FIN.MMR <- lmbas$MMR - thirtyLmean
lmbas$FIN.RMR <- lmbas$RMR - thirtyLmean
lmbas$FIN.AS <- lmbas$FIN.MMR - lmbas$FIN.RMR
  
# how much bacterial respiration was there? 
range(oneLmean/csas$RMR)
range(oneLmean/csas$MMR)
thirtyLmean/lmbas$RMR
thirtyLmean/lmbas$MMR

## Step 2: Visualize ----
## > Aerobic scope ----
## Chinook salmon max out around 22 degrees
ggplot(data=csas, aes(TEST_TEMP, FIN.AS)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,FIN.RMR), color="orange") + 
  geom_point(aes(TEST_TEMP, FIN.MMR), color="blue") + 
  geom_smooth(method = "lm", 
              color="black",  
              formula = y ~ poly(x, 2), size = 1) +
  geom_smooth(aes(x = TEST_TEMP, y = FIN.RMR),
              method = "lm",
              color="orange",
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/hr",
       title = "Late fall-run Chinook Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

# calculate by minute
csas$FIN.MMR.min <- csas$FIN.MMR/60
csas$FIN.RMR.min <- csas$FIN.RMR/60
csas$FIN.AS.min <- csas$FIN.MMR.min - csas$FIN.RMR.min

lmbas$FIN.MMR.min <- lmbas$FIN.MMR/60
lmbas$FIN.RMR.min <- lmbas$FIN.RMR/60
lmbas$FIN.AS.min <- lmbas$FIN.MMR.min - lmbas$FIN.RMR.min

ggplot(data=csas, aes(TEST_TEMP, FIN.AS.min)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,FIN.RMR.min), color="orange") + 
  geom_point(aes(TEST_TEMP, FIN.MMR.min), color="blue") + 
  geom_smooth(method = "lm", 
              color="black",  
              formula = y ~ poly(x, 2), size = 1) +
  geom_smooth(aes(x = TEST_TEMP, y = FIN.RMR.min),
              method = "lm",
              color="orange",
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR.min),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/min",
       title = "Late fall-run Chinook Aerobic Scope") +
  ylim(0,20)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

# calculate mean and range at each temp
csas %>% group_by(TEST_TEMP) %>% 
  dplyr::summarise(Mean_AS = mean(FIN.AS.min, na.rm=T),
                   Mean_RMR = mean(FIN.RMR.min, na.rm=T),
                   Mean_MMR = mean(FIN.MMR.min, na.rm=T))

## Large mouth bass only improve with temperature
ggplot(data=lmbas, aes(TEST_TEMP, FIN.AS)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,FIN.RMR), color="orange") + 
  geom_point(aes(TEST_TEMP, FIN.MMR), color="blue") + 
  geom_smooth(method = "lm", 
              color="black",  
              formula = y ~ poly(x, 2), size = 1) +
  geom_smooth(aes(x = TEST_TEMP, y = FIN.RMR),
              method = "lm",
              color="orange",
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/hr",
       title = "Largemouth bass Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

## Calculate by min
ggplot(data=lmbas, aes(TEST_TEMP, FIN.AS.min)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,FIN.RMR.min), color="orange") + 
  geom_point(aes(TEST_TEMP, FIN.MMR.min), color="blue") + 
  geom_smooth(method = "lm", 
              color="black",  
              formula = y ~ poly(x, 2), size = 1) +
  geom_smooth(aes(x = TEST_TEMP, y = FIN.RMR.min),
              method = "lm",
              color="orange",
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR.min),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  ylim(0,20)+
  labs(x = "Temperature", y = "mgO2/kg/min",
       title = "Largemouth bass Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

# calculate mean and range at each temp
lmbas %>% group_by(TEST_TEMP) %>% 
  dplyr::summarise(Mean_AS = mean(FIN.AS.min, na.rm=T),
                   Mean_RMR = mean(FIN.RMR.min, na.rm=T),
                   Mean_MMR = mean(FIN.MMR.min, na.rm=T))

## > Burst ---- 
table(csburst$USABLE) # Ken already got rid of the mistakes 
csburst.2 <- csburst[-which(csburst$USABLE=="N"),]

csburst.modif.LFR <- csburst.2 %>% group_by(FISH_ID) %>% 
  dplyr::summarise(TEMP = mean(TRIAL_TEMP),
                   Mean_Peak_Speed2 = mean(PEAK_SPEED_2, na.rm=T),
                   Total_Bursts = mean(TOTAL_BURST_EVENTS_new, na.rm=T),
                   Mass = mean(MASS, na.rm=T),
                   SL=mean(SL, na.rm=T))

#cap burst number at 25

csburst.modif.LFR$Total_Bursts <- ifelse(csburst.modif.LFR$Total_Bursts>=25, 25, csburst.modif.LFR$Total_Bursts)

ggplot(csburst.modif.LFR,aes(TEMP, Mean_Peak_Speed2)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "Late fall-run Chinook Burst Speed") +
  ylim(0,180) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black")

ggplot(csburst.modif.LFR,aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Number of burst events", title = "Late fall-run Chinook Number of Burst Events") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  ylim(0,30)+
  geom_smooth(method = "lm", size = 1, color="black")



## LMB burst
table(lmbburst$USABLE) # Ken already got rid of the mistakes 

lmbburst.modif <- lmbburst %>% group_by(PIT) %>% 
  dplyr::summarise(TEMP = mean(Temp),
                   Mean_Peak_Speed2 = mean(PEAK_SPEED_2, na.rm=T),
                   Total_Bursts = mean(TOTAL_EVENTS, na.rm=T),
                   Mass = mean(MASS, na.rm=T),
                   SL = mean(SL, na.rm=T))

# remove outliers above 5000 cm/s
lmbburst.modif.2 <- lmbburst.modif[lmbburst.modif$Mean_Peak_Speed2<5000,]


ggplot(lmbburst.modif.2,aes(TEMP, Mean_Peak_Speed2)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "Largemouth bass Burst Speed") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  ylim(0,160) +
  geom_smooth(method = "lm", size = 1, color="black")


#cap burst number at 25

lmbburst.modif.2$Total_Bursts <- ifelse(lmbburst.modif.2$Total_Bursts>=25, 25, lmbburst.modif.2$Total_Bursts)


ggplot(lmbburst.modif.2,aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Number of burst events", title = "Largemouth bass Number of Burst Events") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + 
  ylim(0,30)+
  geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black")


### > Predation data----
# line plot
pred %>%
  ggplot(aes(x = Temperature,
             y = Number.salmon.eaten)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Largemouth bass Predation") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +scale_y_continuous(breaks=seq(0,12,2))+
  coord_cartesian(ylim=c(0,12))+
  geom_smooth(method = "lm", size = 1, color="black")


# box plot
bp_LMB <- pred %>%
  ggplot(aes(x = as.factor(Temperature),
             y = Number.salmon.eaten)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Largemouth bass Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) 

bp_LMB

# dotbox plot
dp_LMB <- pred %>%
  ggplot(aes(x = as.factor(Temperature),
             y = Number.salmon.eaten)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize = .5)+
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Largemouth bass Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) 

dp_LMB


### > Individual Predation data----

pred.modif <- pred[,c(2,3,5,7,8,11)]
pred.modif$fTrial.Num <- as.factor(pred.modif$Trial.Num)

lmb_modif.df.1 <- merge(pred.modif, lmbburst.modif.2,by="PIT")
lmb_modif.df <- merge(lmb_modif.df.1, lmbas, by="PIT")


as.lmb.all <- lmb_modif.df %>%
  ggplot(aes(x = FIN.AS.min,
             y = Number.salmon.eaten,
             color=TEST_TEMP)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "AS values", y = "Number of Salmon eaten", title = "AS vs. salmon eaten") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(hjust = 0.5, size=10, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "glm", size = 1, color="orange")


numburst.lmb.all <- lmb_modif.df %>%
  ggplot(aes(x = Total_Bursts,
             y = Number.salmon.eaten,
             color=TEST_TEMP)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Burst number", y = "Number of Salmon eaten", title = "Burst number vs. salmon eaten") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(hjust = 0.5, size=10, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "lm", size = 1, color="orange")

speedburst.lmb.all <- lmb_modif.df %>%
  ggplot(aes(x = Mean_Peak_Speed2,
             y = Number.salmon.eaten,
             color=TEST_TEMP)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Burst speed", y = "Number of Salmon eaten", title = "Burst Speed vs. salmon eaten") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(hjust = 0.5, size=10, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "lm", size = 1, color="orange")

library(gridExtra)
grid.arrange(as.lmb.all, numburst.lmb.all, speedburst.lmb.all)


#####Step 3: Actual models (not just for visualisations)----
# Burst Data: CS ----
# linear models 


### Burst No. CS 
mod.burst.no.CS <- glm(Total_Bursts ~ TEMP + Mass, data=csburst.modif.LFR, family="poisson") 
summary(mod.burst.no.CS)

# > model diagnostics

# 1 variance homogeneity 
plot(mod.burst.no.CS)
plot(mod.burst.no.CS, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.burst.no.CS))
'looks ok'

### Burst Speed CS 
mod.burst.speed.CS <- lm(Mean_Peak_Speed2 ~ TEMP + Mass, data=csburst.modif.LFR) 
summary(mod.burst.speed.CS)
# > model diagnostics

# 1 variance homogeneity 
plot(mod.burst.speed.CS)
plot(mod.burst.speed.CS, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.burst.speed.CS))
'looks ok'


# Burst Data: LMB ----
# linear models

### Burst no. LMB
mod.burst.no.LMB <- glm(Total_Bursts ~ TEMP + Mass, data=lmbburst.modif.2, family="poisson") # mass and temp are evenly or normally distributed
summary(mod.burst.no.LMB)
plot(mod.burst.no.LMB)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.burst.no.LMB)
plot(mod.burst.no.LMB, 1)
'looks not great; could consider sqrt transformation of total # of bursts'

#2 normality of errors
hist(resid(mod.burst.no.LMB)) #right skewed
'looks ok'


### Burst speed LMB
mod.burst.speed.LMB <- lm(Mean_Peak_Speed2 ~ TEMP + Mass, data=lmbburst.modif.2) 
summary(mod.burst.speed.LMB)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.burst.speed.LMB)
plot(mod.burst.speed.LMB, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.burst.speed.LMB)) 
'looks ok'


# AS Data: CS ----
mod.as.CS <- lm(FIN.AS.min ~ poly(TEST_TEMP,2) + MASS, data=csas)
summary(mod.as.CS)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.as.CS)
plot(mod.as.CS, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.as.CS)) 
'looks ok'


# AS Data: LMB ----
mod.as.lmbas <- lm(FIN.AS.min ~ poly(TEST_TEMP,2) + MASS, data=lmbas)
summary(mod.as.lmbas)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.as.lmbas)
plot(mod.as.lmbas, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.as.lmbas)) 
'looks ok'

## Differences in LMB and LFRCS ----
lmb.compare.as <- lmbas%>% group_by(TEST_TEMP) %>%
  dplyr::summarise(Mean.AS.LMB = mean(FIN.AS.min, na.rm=T))

cs.compare.as <- csas%>% group_by(TEST_TEMP) %>%
  dplyr::summarise(Mean.AS.CS = mean(FIN.AS.min, na.rm=T))

differences.as <- left_join(lmb.compare.as,cs.compare.as)
differences.as$difference_as <- ((differences.as$Mean.AS.LMB/differences.as$Mean.AS.CS)-1)

lmb.compare.burst <- lmbburst.modif%>% group_by(TEMP) %>%
  dplyr::summarise(Mean.Burst.No.LMB = mean(Total_Bursts, na.rm=T),
                   Mean.Peak.Speed.LMB = mean(Mean_Peak_Speed2, na.rm=T))

cs.compare.burst <- csburst.modif.LFR%>% group_by(TEMP) %>%
  dplyr::summarise(Mean.Burst.No.CS = mean(Total_Bursts, na.rm=T),
                   Mean.Peak.Speed.CS = mean(Mean_Peak_Speed2, na.rm=T))

differences.burst <- left_join(lmb.compare.burst,cs.compare.burst)

differences.burst$difference_burstno <- ((differences.burst$Mean.Burst.No.CS/differences.burst$Mean.Burst.No.LMB)-1)
differences.burst$difference_burstspeed <- ((differences.burst$Mean.Peak.Speed.CS/differences.burst$Mean.Peak.Speed.LMB)-1)



differences.burst.2 <- left_join(differences.burst, pred, by=c("TEMP"="Temperature"))

differences.as.2 <- left_join(differences.as, pred, by=c("TEST_TEMP"="Temperature"))

#### Models
library(MASS)

mod.differences.as <- glm.nb(Number.salmon.eaten~difference_as, data=differences.as.2)
summary(mod.differences.as)

mod.differences.burstno <- glm.nb(Number.salmon.eaten~ difference_burstno, data=differences.burst.2)
summary(mod.differences.burstno)

mod.differences.burstspeed <- glm.nb(Number.salmon.eaten~ difference_burstspeed, data=differences.burst.2)
summary(mod.differences.burstspeed)


# Predation data ----
# linear model
hist(pred$Number.salmon.eaten)
nrow(pred[pred$Number.salmon.eaten > 1,]) # how many at more than one salmon
pred$scaled_LMB_mass <- scale(pred$Bass_Mass)
pred$scaled_Salmon_Ave_Mass <- scale(pred$Salmon_Ave_Mass)
mod.pred.LMB <- glm.nb(Number.salmon.eaten~Temperature + scaled_LMB_mass + scaled_Salmon_Ave_Mass, data=pred)
summary(mod.pred.LMB)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.pred.LMB)
'looks ok'

#2 normality of errors
hist(resid(mod.pred.LMB)) 
'looks ok'

# Individual variation ----
# linear model
mod.pred.LMB.AS <- glm.nb(Number.salmon.eaten~ FIN.AS.min, data=lmb_modif.df)
summary(mod.pred.LMB.AS)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.pred.LMB.AS)
'looks ok'

#2 normality of errors
hist(resid(mod.pred.LMB.AS)) 
'looks ok'

# burst
mod.pred.LMB.bs <- glm.nb(Number.salmon.eaten~ Mean_Peak_Speed2, data=lmb_modif.df)
summary(mod.pred.LMB.bs)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.pred.LMB.bs)
'looks ok'

#2 normality of errors
hist(resid(mod.pred.LMB.bs)) 
'looks ok'

mod.pred.LMB.bn <- glm.nb(Number.salmon.eaten~ Total_Bursts, data=lmb_modif.df)
summary(mod.pred.LMB.bn)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.pred.LMB.bn)
'looks ok'

#2 normality of errors
hist(resid(mod.pred.LMB.bn)) 
'looks ok'

## Model tables ----
library(sjPlot) # table functions
library(sjmisc) # sample data

tab_model(mod.as.CS, show.ci=F, show.se = T, show.obs = F)

tab_model(mod.as.lmbas,show.ci=F, show.se = T, show.obs = F)

tab_model(mod.burst.no.CS, transform=NULL, show.ci=F, show.se = T,  show.obs = F)
tab_model(mod.burst.no.LMB, transform=NULL, show.ci=F, show.se = T, show.obs = F)

tab_model(mod.burst.speed.CS, show.ci=F, show.se = T, show.r2 = T, show.obs = F)

tab_model(mod.burst.speed.LMB, show.ci=F, show.se = T, show.r2 = T, show.obs = F)

tab_model(mod.pred.LMB, transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.obs = F) # this suggests to me that we may not need trial #

# ind variation
tab_model(mod.pred.LMB.AS, transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.icc = F, show.obs = F)
tab_model(mod.pred.LMB.bs,transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.obs = F, show.icc = F)
tab_model(mod.pred.LMB.bn, transform=NULL,show.ci=F, show.se = T, show.icc = F, show.r2 = F, show.obs = F)

# differences in relative performance
tab_model(mod.differences.as, transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.icc = F, show.obs = F)

tab_model(mod.differences.burstno, transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.icc = F, show.obs = F)

tab_model(mod.differences.burstspeed, transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.icc = F, show.obs = F)

