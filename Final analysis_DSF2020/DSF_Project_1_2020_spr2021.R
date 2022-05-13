## Analysis: Year 1, all data ##
## Species: Largemouth bass, LFR Chinook Salmon ##
rm(list=ls())

library(lme4)
library(tidyverse)
library(mgcv)
setwd("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020")
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
              formula = y ~ poly(x, 2), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/hr",
       title = "LFR CS Aerobic Scope") +
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
              formula = y ~ poly(x, 2), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR.min),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/min",
       title = "LFR CS Aerobic Scope") +
  ylim(0,30)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

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
              formula = y ~ poly(x, 2), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/hr",
       title = "LMB Aerobic Scope") +
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
              formula = y ~ poly(x, 2), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR.min),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  ylim(0,20)+
  labs(x = "Temperature", y = "mgO2/kg/min",
       title = "LMB Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))



## > Burst ---- 

csburst.modif.LFR <- csburst %>% group_by(FISH_ID) %>% 
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
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "LFR CS Burst Speed") +
  ylim(0,180) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black")

ggplot(csburst.modif.LFR,aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  labs(x = "Temperature", y = "Burst Number", title = "LFR CS Burst Number") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  ylim(0,30)+
  geom_smooth(method = "lm", size = 1, color="black")



## LMB burst
lmbburst.modif <- lmbburst %>% group_by(PIT) %>% 
  dplyr::summarise(TEMP = mean(Temp),
                   Mean_Peak_Speed2 = mean(PEAK_SPEED_2, na.rm=T),
                   Total_Bursts = mean(TOTAL_EVENTS, na.rm=T),
                   Mass = mean(MASS, na.rm=T))

# remove outliers above 15000 cm/s
lmbburst.modif.2 <- lmbburst.modif[lmbburst.modif$Mean_Peak_Speed2<5000,]


ggplot(lmbburst.modif.2,aes(TEMP, Mean_Peak_Speed2)) +
  geom_point() + 
  theme_classic() + 
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "LMB Burst Speed") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  ylim(0,160) +
  geom_smooth(method = "lm", size = 1, color="black")


#cap burst number at 25

lmbburst.modif.2$Total_Bursts <- ifelse(lmbburst.modif.2$Total_Bursts>=25, 25, lmbburst.modif.2$Total_Bursts)


ggplot(lmbburst.modif.2,aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  labs(x = "Temperature", y = "Burst Number", title = "LMB Burst Number") +
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
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "LMB Predation") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +scale_y_continuous(breaks=seq(0,12,2))+
  coord_cartesian(ylim=c(0,12))+
  geom_smooth(method = "lm", size = 1, color="black")


# box plot
bp_LMB <- pred %>%
  ggplot(aes(x = as.factor(Temperature),
             y = Number.salmon.eaten)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "LMB Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) 

bp_LMB

### > Individual Predation data----

pred.modif <- pred[,c(2,3,5,7,8,11)]
pred.modif$fTrial.Num <- as.factor(pred.modif$Trial.Num)

lmb_modif.df.1 <- merge(pred.modif, lmbburst.modif.2,by="PIT")
lmb_modif.df <- merge(lmb_modif.df.1, lmbas, by="PIT")


as.lmb.all <- lmb_modif.df %>%
  ggplot(aes(x = aerobic_scope,
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
# can read in data from FR-LFR comparison_script to look at both runs
allcs <- read.csv("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020/Combined_Runs_CS_Burst.csv")

# linear models
mod.burst.no.CS <- glm(Total_Bursts ~ TEMP*Run + Mass, data=allcs, family="poisson") 
summary(mod.burst.no.CS)
'Temp sig - , Temp*RunLFR sig +'

mod.burst.speed.CS <- glm(Mean_Peak_Speed2SL ~ TEMP*Run + Mass, data=allcs) 
summary(mod.burst.speed.CS)
'RunLFR sig +, Temp*RunLFR sig -'

# Burst Data: LMB ----
# linear models
mod.burst.no.LMB <- glm(Total_Bursts ~ TEMP + Mass, data=lmbburst.modif, family="poisson") 
summary(mod.burst.no.LMB)
'temp sig +, Mass sig +'
plot(mod.burst.no.LMB)

mod.burst.speed.LMB <- lm(Mean_Peak_Speed2SL ~ TEMP + Mass, data=lmbburst.modif) 
summary(mod.burst.speed.LMB)
'temp sig +'
plot(mod.burst.speed.LMB)

#breakpoint analyses
mod.burst.no.LMB.seg <- glm(Total_Bursts ~ TEMP, data=lmbburst.modif, family="poisson")
LMB.burst.no.seg <- segmented(mod.burst.no.LMB.seg, seg.Z = ~ TEMP) #do not define PSI, allow the model to do that for us
slope(LMB.burst.no.seg)
plot(LMB.burst.no.seg, term="TEMP")
davies.test(mod.burst.no.LMB.seg, seg.Z = ~ TEMP, type="lrt")

mod.burst.speed.LMB.seg <- lm(Mean_Peak_Speed2SL ~ TEMP, data=lmbburst.modif)
LMB.burst.speed.seg <- segmented(mod.burst.speed.LMB.seg, seg.Z = ~ TEMP) #do not define PSI, allow the model to do that for us; no breakpoint estimated
#slope(LMB.burst.no.seg)
#plot(LMB.burst.no.seg, term="TEMP")
#davies.test(mod.burst.no.LMB.seg, seg.Z = ~ TEMP, type="lrt")

#gam
mod.burst.no.LMB.gam <- gam(Total_Bursts~s(TEMP,k=5) + s(Mass,k=5), data = lmbburst.modif, family = 'poisson', method='REML', select=TRUE)
summary(mod.burst.no.LMB.gam)
plot(mod.burst.no.LMB.gam, page=1)

mod.burst.speed.LMB.gam <- gam(Mean_Peak_Speed2SL~s(TEMP,k=5) + s(Mass,k=5), data = lmbburst.modif,  method='REML', select=TRUE)
summary(mod.burst.speed.LMB.gam)
plot(mod.burst.speed.LMB.gam)

# AS Data: CS ----
allcs_as <- read.csv("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020/Combined_Runs_CS_AS.csv")

mod.as.CS <- lm(aerobic_scope ~ poly(TEST_TEMP,2)*Run, data=allcs_as)
summary(mod.as.CS)
'RunLF sig +'


# AS Data: LMB ----
mod.as.lmbas <- lm(aerobic_scope ~ poly(TEST_TEMP,2), data=lmbas)
summary(mod.as.RT)
'temp^2 sig - '

# Predation data ----
# linear model
hist(pred$Number.salmon.eaten)
mod.pred.LMB <- glmer.nb(Number.salmon.eaten~Temperature + Salmon_Ave_Mass + (1|Trial.Num), data=pred)
summary(mod.pred.LMB)
'temp is sig + '
plot(mod.pred.LMB)

# breakpoint analysis
mod.pred.LMB.seg <- glm(Number.salmon.eaten~Temperature, family="poisson", data=pred)
LMB_seg <- segmented(mod.pred.LMB.seg, seg.Z = ~ Temperature) #do not define PSI, allow the model to do that for us
summary(LMB_seg)
slope(LMB_seg)
davies.test(mod.pred.LMB.seg, seg.Z = ~ Temperature, type="lrt")

par(mfrow=c(2,1))
seg_plot <- plot(LMB_seg, term="Temperature")
bp_LMB

#gam
pred$Trial.Num <- as.factor(pred$Trial.Num)
mod.pred.LMB.gam <- gam(Number.salmon.eaten~s(Temperature,k=5) + s(Salmon_Ave_Mass), data = pred, family = 'poisson', method='REML', select=TRUE)
summary(mod.pred.LMB.gam)
plot(mod.pred.LMB.gam, page=1)
