## Analysis: Year 2, all data ##
## Species: Rainbow trout, FR Chinook Salmon, Striped Bass ##
rm(list=ls())
library(lme4)
library(tidyverse)
library(mgcv)
library(ggplot2)

## Step 1: Bring data in ----
setwd("~/Box Sync/Salmon-Pred-Project/Experiment2_2021_CS-SB/DSF_EXP_2021_CODE-FILES/Final analysis_DSF2021")

pred_SB <- read.csv("Compiled SB Pred Data.csv")
pred_RT <- read.csv("Compiled RT Pred Data.csv")
csas <- read.csv("FRCS_AS_cleaned_Spr2021.csv")
rtas <- read.csv("RT_AS_cleaned_Spr2021.csv")
csburst <- read.csv("Cleaned_FRCS_Bursts_Spring2021.csv")
rtburst <- read.csv("Cleaned_RT_Bursts_Spring2021.csv")

#> Calculate mean controls to integrate them into the other figures from Experiment 1, because no controls were used for 2020

# for the 30 L tunnels
thirtyLmean <- mean(rtas$CONTROL_MEAN, na.rm=T)
# for the 1.5 L tunnels
oneLmean <- mean(csas$CONTROL_MEAN, na.rm=T)

## Step 2: Visualize ----
## > Aerobic scope
# CS Aerobic Scope ----
# first need to calculate aerobic scope
csas$FIN.MMR <- csas$MMR - csas$CONTROL_MEAN
csas$FIN.RMR <- csas$RMR - csas$CONTROL_MEAN
csas$AS <- csas$FIN.MMR - csas$FIN.RMR

table(csas$AS)
table(csas$FIN.MMR)
table(csas$FIN.RMR)

# next need to figure out how much of the controls proportionally make up the MMR/RMR
csas$RMR.ctrl.prop <- csas$CONTROL_MEAN/csas$RMR
table(csas$RMR.ctrl.prop)
'controls affect RMR much more than MMR'
csas$MMR.ctrl.prop <- csas$CONTROL_MEAN/csas$MMR
table(csas$MMR.ctrl.prop)
csas$AS.ctrl.prop <- csas$CONTROL_MEAN/csas$AS
table(csas$AS.ctrl.prop)

csas$AS.noctrl <- csas$MMR-csas$RMR

ggplot(data=csas, aes(TEST_TEMP, AS)) +
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
       title = "FR CS Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

ggplot(data=csas, aes(TEST_TEMP, AS.noctrl)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,RMR), color="orange") + 
  geom_point(aes(TEST_TEMP, MMR), color="blue") + 
  geom_smooth(method = "lm", 
              color="black",  
              formula = y ~ poly(x, 2), size = 1) +
  geom_smooth(aes(x = TEST_TEMP, y = RMR),
              method = "lm",
              color="orange",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/hr",
       title = "FR CS Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

# check out by minute to compare values to other papers
csas$FIN.MMR.min <- csas$FIN.MMR/60
csas$FIN.RMR.min <- csas$FIN.RMR/60
csas$AS.min <- csas$FIN.MMR.min - csas$FIN.RMR.min

ggplot(data=csas, aes(TEST_TEMP,AS.min)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,FIN.RMR.min), color="orange") + 
  geom_point(aes(TEST_TEMP,FIN.MMR.min), color="blue") + 
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
       title = "FR CS Aerobic Scope") +
  ylim(0,30) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))


# calculate mean and range at each temp
csas %>% group_by(TEST_TEMP) %>% 
  dplyr::summarise(Mean_AS = mean(AS.min, na.rm=T),
                   Mean_RMR = mean(FIN.RMR.min, na.rm=T),
                   Mean_MMR = mean(FIN.MMR.min, na.rm=T))
# RT Aerobic Scope ----

# next need to figure out how much of the controls proportionally make up the MMR/RMR
rtas$RMR.ctrl.prop <- rtas$CONTROL_MEAN/rtas$RMR
table(rtas$RMR.ctrl.prop)
'controls affect RMR much more than MMR'
rtas$MMR.ctrl.prop <- rtas$CONTROL_MEAN/rtas$MMR
table(rtas$MMR.ctrl.prop)
rtas$AS.ctrl.prop <- rtas$CONTROL_MEAN/rtas$AS
table(rtas$AS.ctrl.prop)



ggplot(data=rtas, aes(TEST_TEMP, AS)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,FIN.RMR), color="orange") + 
  geom_point(aes(TEST_TEMP, FIN.MMR), color="blue") + 
  geom_smooth(aes(TEST_TEMP, AS), method = "lm", 
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
  labs(x = "Temperature", y="mgO2/kg/hr",
       title = "RT Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

# with no controls
ggplot(data=rtas, aes(TEST_TEMP, AS.ctrl)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,RMR), color="orange") + 
  geom_point(aes(TEST_TEMP, MMR), color="blue") + 
  geom_smooth(aes(TEST_TEMP, AS.ctrl), method = "lm", 
              color="black",  
              formula = y ~ poly(x, 2), size = 1) +
  geom_smooth(aes(x = TEST_TEMP, y = RMR),
              method = "lm",
              color="orange",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y="mgO2/kg/hr",
       title = "RT Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

# to get in minutes
rtas$FIN.MMR.min <- rtas$FIN.MMR/60
rtas$FIN.RMR.min <- rtas$FIN.RMR/60
rtas$AS.min <- rtas$FIN.MMR.min - rtas$FIN.RMR.min


# remove any RMR values below 0 
rtas.mod <- rtas[rtas$FIN.RMR.min>0,]
ggplot(data=rtas.mod, aes(TEST_TEMP, AS.min)) +
  theme_classic() +
  geom_point(color='black') +
  geom_point(aes(TEST_TEMP,FIN.RMR.min), color="orange") + 
  geom_point(aes(TEST_TEMP, FIN.MMR.min), color="blue") + 
  geom_smooth(aes(TEST_TEMP, AS.min), method = "lm", 
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
  labs(x = "Temperature", y="mgO2/kg/min",
       title = "RT Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

# calculate mean and range at each temp
rtas %>% group_by(TEST_TEMP) %>% 
  dplyr::summarise(Mean_AS = mean(AS.min, na.rm=T),
                   Mean_RMR = mean(FIN.RMR.min, na.rm=T),
                   Mean_MMR = mean(FIN.MMR.min, na.rm=T))


 ## CS burst----


csburst.modif <- csburst %>% group_by(FISH_ID) %>% 
  dplyr::summarise(TEMP = mean(TRIAL_TEMP),
                    Mean_Peak_Speed_2 = mean(PEAK_SPEED_2, na.rm=T),
                   Total_Bursts = mean(TOTAL_BURST_EVENTS, na.rm=T))

# cap burst # data at 25 
csburst.modif$Total_Bursts <- ifelse(csburst.modif$Total_Bursts>=25, 25, csburst.modif$Total_Bursts)

ggplot(csburst.modif,aes(TEMP, Mean_Peak_Speed_2)) +
  geom_point() + 
  theme_classic() + 
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "FR CS Burst Speed") +
  ylim(0,170)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black")


ggplot(csburst.modif[csburst.modif$Total_Bursts>0,],aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  labs(x = "Temperature", y = "Burst Number", title = "FR CS Burst Number") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  ylim(0,30) +
  geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black")

## RT Bursts ----
# note: had to do some manual cleaning of the doc based on qualitative notes
# need to remove outliers for peak speed
# according to Domenici and Blake (1997), up to 4000 cm/s is reasonable

# also need to get rid of R2Bs (refuse to burst)
rtburst.2 <- rtburst[-which(rtburst$STATUS=="RTB"),]
rtburst.3 <- rtburst.2[-which(rtburst.2$STATUS=="ERROR"),]

hist(rtburst.3$PEAK_SPEED_2)

# remove outliers
rtburst.4 <- rtburst.3[rtburst.3$PEAK_SPEED_2<40000,]

rtburst.modif <- rtburst.4 %>% group_by(PIT) %>% 
  dplyr::summarise(TEMP = mean(TRIAL_TEMP),
            Mean_Peak_Speed2 = mean(PEAK_SPEED_2, na.rm=T),
            Max_Peak_Speed = max(PEAK_SPEED_2, na.rm=T),
            Total_Bursts = mean(as.numeric(TOTAL_BURST_EVENTS), na.rm=T),
            Mass = mean(MASS, na.rm=T),
            SL = mean(SL, na.rm=T))

rtburst.modif$Mean_Peak_Speed2_SL <- rtburst.modif$Mean_Peak_Speed2/rtburst.modif$SL


# there does seem to be one other excpetionally high value
hist(rtburst.modif$Mean_Peak_Speed2_SL)
table(rtburst.modif$Mean_Peak_Speed2_SL)

# need to remove trout that have exceptionally high burst values; likely a tunnel issue
rtburst.modif.2 <- rtburst.modif[rtburst.modif$Mean_Peak_Speed2_SL<100,]
hist(rtburst.modif.2$Mean_Peak_Speed2) # more normally distributed
hist(rtburst.modif.2$Mean_Peak_Speed2_SL) # more normally distributed


ggplot(rtburst.modif.2,aes(TEMP, Mean_Peak_Speed2)) +
  geom_point() + 
  theme_classic() + 
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "RT Burst Speed") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black",fullrange=T)+
  coord_cartesian(ylim=c(0,max(rtburst.modif$Mean_Peak_Speed2)))

# cap this data at 25 
rtburst$TOTAL_BURST_EVENTS <- as.numeric(rtburst$TOTAL_BURST_EVENTS)
rtburst.modif$Total_Bursts <- ifelse(rtburst.modif$Total_Bursts>=25, 25, rtburst.modif$Total_Bursts)

ggplot(rtburst.modif,aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  labs(x = "Temperature", y = "Burst Number", title = "RT Burst Number") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  ylim(0,30) +
  geom_smooth(method = "lm", size = 1, color="black")

### Predation data ----

# considering all data
# Line plot
pred_SB %>%
  ggplot(aes(x = Temperature,
             y = Num.Salmon.Eaten)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "SB Predation") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  geom_smooth(method = "lm", size = 1, color="black")

bp_SB <- pred_SB %>%
  ggplot(aes(x = as.factor(Temperature),
             y = Num.Salmon.Eaten)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "SB Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) 
bp_SB

#line plot
pred_RT %>%
  ggplot(aes(x = Temp,
             y = Num.Salmon.Eaten)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "RT Predation") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  geom_smooth(method = "lm", size = 1, color="black")

bp_RT <- pred_RT %>%
  ggplot(aes(x = as.factor(Temp),
             y = Num.Salmon.Eaten)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "RT Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

bp_RT

# considering instances where at least one fish was eaten; can't really do because not enough data

pred_RT[pred_RT$Num.Salmon.Eaten>0,] %>%
  ggplot(aes(x = Temp,
             y = Num.Salmon.Eaten)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Temperature", y = "# Salmon eaten", title = "RT Predation Data") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "lm", size = 1, color="blue")

###> Individual predation data RT
### > Individual Predation data----

pred.modif <- pred_RT[,c(2,3,5,7,8,9,10)]

# make sure PIT tags match
rtburst.modif$PIT[rtburst.modif$PIT=="0FB3"] <- "OFB3"
rtburst.modif$PIT[rtburst.modif$PIT=="41SD"] <- "415D"
rtburst.modif$PIT[rtburst.modif$PIT=="C0EF"] <- "COEF"
pred.modif$Trout_PIT[pred.modif$Trout_PIT=="3.00E+61"] <- "3E61*"
rtas$PIT[rtas$PIT=="0FB3"] <- "OFB3"
rtas$PIT[rtas$PIT=="3.00E+61"] <- "3E61*"
rtas$PIT[rtas$PIT=="O775"] <- "775"
rtas$PIT[rtas$PIT=="C0EF"] <- "COEF"

# fix column name 
colnames(pred.modif)[3] <- "PIT"

rt_modif.df.1 <- merge(rtas, rtburst.modif,by="PIT")
rt_modif.df <- merge(pred.modif, rt_modif.df.1,by="PIT")



as.rt.all <- rt_modif.df %>%
  ggplot(aes(x = AS,
             y = Num.Salmon.Eaten,
             color=TEST_TEMP)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "AS values", y = "# Salmon eaten", title = "AS vs. Salmon eaten") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(hjust = 0.5, size=10, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "glm", size = 1, color="red")

numburst.rt.all <- rt_modif.df %>%
  ggplot(aes(x = Total_Bursts,
             y = Num.Salmon.Eaten,
             color=TEST_TEMP)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Burst number", y = "# Salmon eaten", title = "Burst # vs. Salmon eaten") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(hjust = 0.5, size=10, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "lm", size = 1, color="red")

speedburst.rt.all <- rt_modif.df %>%
  ggplot(aes(x = Mean_Peak_Speed2SL,
             y = Num.Salmon.Eaten,
             color=TEST_TEMP)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Burst speed", y = "# Salmon eaten", title = "Burst Speed vs. Salmon eaten") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(hjust = 0.5, size=10, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "lm", size = 1, color="red")

library(gridExtra)
grid.arrange(as.rt.all, numburst.rt.all, speedburst.rt.all)

#### Step 3: Actual models (not just for visualizations) ----
'Options for models = GAMs, LMs, threshold temp models'

# Burst Data: CS ----
# can read in data from FR-LFR comparison_script to look at both runs
allcs <- read.csv("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020/Combined_Runs_CS_Burst.csv")
FR <- allcs[allcs$Run=="FR",] #got rid of 2 outliers above 50
nrow(FR)
LFR <- allcs[allcs$Run=="LFR",]
# linear models
mod.burst.no.CS <- glm(Total_Bursts ~ TEMP*Run, data=allcs, family="poisson") 
summary(mod.burst.no.CS)
'Temp sig - , Temp*RunLFR sig +'
# model checks
plot(mod.burst.no.CS)

mod.burst.no.CS.FR <- glm(Total_Bursts ~ TEMP, data=FR, family="poisson") 
summary(mod.burst.no.CS.FR)

mod.burst.no.CS.LFR <- glm(Total_Bursts ~ TEMP, data=LFR, family="poisson") 
summary(mod.burst.no.CS.LFR)

mod.burst.speed.CS <- glm(Mean_Peak_Speed2SL ~ TEMP*Run, data=allcs) 
summary(mod.burst.speed.CS)
'RunLFR sig +, Temp*RunLFR sig -'

mod.burst.speed.CS.FR <- glm(Mean_Peak_Speed2SL ~ TEMP, data=FR) 
summary(mod.burst.speed.CS.FR)

mod.burst.speed.CS.LFR <- glm(Mean_Peak_Speed2SL ~ TEMP, data=LFR) 
summary(mod.burst.speed.CS.LFR)


plot(mod.burst.speed.CS)

# breakpoint analyses ### 


'FR'
mod.burst.speed.CS.seg.FR <- glm(Mean_Peak_Speed2SL ~ TEMP, data=FR)
CS.burst.speed.seg.FR <- segmented(mod.burst.speed.CS.seg.FR, seg.Z = ~ TEMP) #do not define PSI, allow the model to do that for us
slope(CS.burst.speed.seg.FR)
summary(CS.burst.speed.seg.FR)
plot(CS.burst.speed.seg.FR, term="TEMP")
davies.test(mod.burst.speed.CS.seg.FR, seg.Z = ~ TEMP, type="lrt")
'Breakpoint est at 23'

mod.burst.no.CS.seg.FR <- glm(Total_Bursts ~ TEMP, data=FR, family="poisson")
CS.burst.no.seg.FR <- segmented(mod.burst.no.CS.seg.FR, seg.Z = ~ TEMP) #do not define PSI, allow the model to do that for us
slope(CS.burst.no.seg.FR)
plot(CS.burst.no.seg.FR, term="TEMP")
davies.test(mod.burst.no.CS.seg.FR, seg.Z = ~ TEMP, type="lrt")
'No breakpoint'

'LFR'
mod.burst.speed.CS.seg.LFR <- glm(Mean_Peak_Speed2SL ~ TEMP, data=LFR)
CS.burst.speed.seg.LFR <- segmented(mod.burst.speed.CS.seg.LFR, seg.Z = ~ TEMP) #do not define PSI, allow the model to do that for us
summary(CS.burst.speed.seg.LFR)
slope(CS.burst.speed.seg.LFR)
plot(CS.burst.speed.seg.LFR, term="TEMP")
davies.test(mod.burst.speed.CS.seg.LFR, seg.Z = ~ TEMP, type="lrt")
'Breakpoint est at 23'

mod.burst.no.CS.seg.LFR <- glm(Total_Bursts ~ TEMP, data=LFR, family="poisson")
CS.burst.no.seg.LFR <- segmented(mod.burst.no.CS.seg.LFR, seg.Z = ~ TEMP) #do not define PSI, allow the model to do that for us
summary(CS.burst.no.seg.LFR)
slope(CS.burst.no.seg.LFR)
plot(CS.burst.no.seg.LFR, term="TEMP")
davies.test(mod.burst.no.CS.seg.LFR, seg.Z = ~ TEMP, type="lrt")
'No breakpoint estimated'

## gam

mod.burst.speed.CS.seg.FR <- gam(Mean_Peak_Speed2SL ~ s(TEMP,k=5), data=FR, method="REML", select=T)
summary(mod.burst.speed.CS.seg.FR)
plot(mod.burst.speed.CS.seg.FR)

mod.burst.speed.CS.seg.LFR <- gam(Mean_Peak_Speed2SL ~ s(TEMP,k=5), data=LFR, method="REML", select=T)
summary(mod.burst.speed.CS.seg.LFR)
plot(mod.burst.speed.CS.seg.LFR)

mod.burst.no.CS.seg.FR <- gam(Total_Bursts ~ s(TEMP,k=5), data=FR, method="REML", select=T)
plot(mod.burst.no.CS.seg.FR)

mod.burst.no.CS.seg.LFR <- gam(Total_Bursts ~ s(TEMP,k=5), data=LFR, method="REML", select=T)
summary(mod.burst.no.CS.seg.LFR)
plot(mod.burst.no.CS.seg.LFR)

# Burst Data: RT ----
# linear models

mod.burst.no.RT <- glm(Total_Bursts ~ TEMP + Mass, data=rtburst.modif, family="poisson") 
summary(mod.burst.no.RT)
'temp sig +'
plot(mod.burst.no.RT)

mod.burst.speed.RT <- lm(Mean_Peak_Speed2SL ~ TEMP + Mass, data=rtburst.modif) 
summary(mod.burst.speed.RT)
'temp not sig'
plot(mod.burst.speed.RT)

# breakpoint analyses
mod.burst.no.RT.seg <- glm(Total_Bursts ~ TEMP, data=rtburst.modif, family="poisson")
RT.burst.no.seg <- segmented(mod.burst.no.RT.seg, seg.Z = ~ TEMP) #do not define PSI, allow the model to do that for us
summary(RT.burst.no.seg)
slope(RT.burst.no.seg)
plot(RT.burst.no.seg, term="TEMP")
davies.test(mod.burst.no.RT.seg, seg.Z = ~ TEMP, type="lrt")

mod.burst.speed.RT.seg <- lm(Mean_Peak_Speed2SL ~ TEMP, data=rtburst.modif)
RT.burst.speed.seg <- segmented(mod.burst.speed.RT.seg, seg.Z = ~ TEMP) #no breakpoint estimated
#slope(RT.burst.speed.seg)
#plot(RT.burst.speed.seg, term="TEMP")
#davies.test(mod.burst.speed.RT.seg, seg.Z = ~ TEMP, type="lrt")

#gam
mod.burst.no.RT.gam <- gam(Total_Bursts~s(TEMP,k=5), data = rtburst.modif, family = 'poisson', method='REML', select=TRUE)
summary(mod.burst.no.RT.gam)
plot(mod.burst.no.RT.gam)

mod.burst.speed.RT.gam <- gam(Mean_Peak_Speed2SL~s(TEMP,k=5), data = rtburst.modif,  method='REML', select=TRUE)
summary(mod.burst.speed.RT.gam)
plot(mod.burst.speed.RT.gam, page=1)



# AS Data: CS ----
allcs_as <- read.csv("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020/Combined_Runs_CS_AS.csv")

FRAS <- allcs_as[allcs_as$Run=="FR",]
LFRAS <- allcs_as[allcs_as$Run=="LFR",]

mod.as.CS <- lm(aerobic_scope ~ poly(TEST_TEMP,2)*Run, data=allcs_as)
summary(mod.as.CS)
'RunLF sig +'

mod.as.CS.FR <- lm(aerobic_scope ~ poly(TEST_TEMP,2), data=FRAS)
summary(mod.as.CS.FR)
'None sig'

mod.as.CS.LFR <- lm(aerobic_scope ~ poly(TEST_TEMP,2), data=LFRAS)
summary(mod.as.CS.LFR)
'None sig'

# AS Data: RT ----
mod.as.RT <- lm(AS ~ poly(TEST_TEMP,2), data=rtas)
summary(mod.as.RT)
'temp^2 sig - '


# Predation data ----
#linear models
library(lme4)
hist(pred_RT$Num.Salmon.Eaten)
mod.pred.RT <- glmer.nb(Num.Salmon.Eaten~Temp + Salmon_Ave_Mass + (1|Trial.Num), data=pred_RT)
summary(mod.pred.RT) # use negative binomial distribution to account for the overdispersion in data
plot(mod.pred.RT)
'temp not sig'

hist(pred_SB$Num.Salmon.Eaten)
mod.pred.SB <- glmer.nb(Num.Salmon.Eaten~Temperature + + Salmon_Ave_Mass+(1|Trial.Num), data=pred_SB)
summary(mod.pred.SB)
plot(mod.pred.SB)
'temp not sig, but ave salmon mass sig -'

# http://cran.r-project.org/doc/Rnews/Rnews_2008-1.pdf
#https://www.researchgate.net/publication/234092680_Segmented_An_R_Package_to_Fit_Regression_Models_With_Broken-Line_Relationships
library(segmented)
# practice using breakpoint analyses
mod.pred.RT.seg <- glm(Num.Salmon.Eaten~Temp, family="poisson", data=pred_RT)
RT_seg <- segmented(mod.pred.RT.seg, seg.Z = ~ Temp) #do not define PSI, allow the model to do that for us
summary(RT_seg)
slope(RT_seg)
plot(RT_seg, term="Temp")
davies.test(mod.pred.RT.seg, seg.Z = ~ Temp, type="lrt") # tells us if this diff in slope is significant


mod.pred.SB.seg <- glm(Num.Salmon.Eaten~Temperature, family="poisson", data=pred_SB)
SB_seg <- segmented(mod.pred.SB.seg, seg.Z = ~ Temperature) #do not define PSI, allow the model to do that for us
summary(SB_seg)
slope(SB_seg)
plot(SB_seg, term="Temperature")
davies.test(mod.pred.SB.seg, seg.Z = ~ Temperature, type="lrt")

#gam
mod.pred.RT.gam <- gam(Num.Salmon.Eaten~s(Temp,k=5) + s(Salmon_Ave_Mass), data = pred_RT, family = 'poisson', method='REML', select=TRUE)
summary(mod.pred.RT.gam)
plot(mod.pred.RT.gam,pages=1)

mod.pred.SB.gam <- gam(Num.Salmon.Eaten~s(Temperature,k=5), data = pred_SB, family = 'poisson', method='REML', select=TRUE)
summary(mod.pred.SB.gam)
plot(mod.pred.SB.gam, page=1)
plot(pred_SB$Temperature, fitted(mod.pred.SB.gam))
