## Analysis: Year 2, all data ##
## Species: Rainbow trout, FR Chinook Salmon, Striped Bass ##
rm(list=ls())
library(lme4)
library(tidyverse)
library(mgcv)
library(ggplot2)

## Step 1: Bring data in ----
setwd("/Volumes/GoogleDrive/My Drive/Computer back-up, December 2021/Salmon-Pred-Project/Experiment2_2021_CS-SB/DSF_EXP_2021_CODE-FILES/Final analysis_DSF2021")

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
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/hr",
       title = "Fall-run Chinook Aerobic Scope") +
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
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/hr",
       title = "Fall-run Chinook Aerobic Scope") +
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
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR.min),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y = "mgO2/kg/min",
       title = "Fall-run Chinook Aerobic Scope") +
  ylim(0,20) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))



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
               formula = y ~ poly(x, 1), 
              size = 1) + 
   geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR),
               method = "lm",
               color="blue",
               formula = y ~ poly(x, 2), 
               size = 1) + 
  labs(x = "Temperature", y="mgO2/kg/hr",
       title = "Rainbow trout Aerobic Scope") +
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
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = MMR),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  labs(x = "Temperature", y="mgO2/kg/hr",
       title = "Rainbow trout Aerobic Scope") +
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
              formula = y ~ poly(x, 1), 
              size = 1) + 
  geom_smooth(aes(x = TEST_TEMP, y = FIN.MMR.min),
              method = "lm",
              color="blue",
              formula = y ~ poly(x, 2), 
              size = 1) + 
  ylim(0,20)+
  labs(x = "Temperature", y="mgO2/kg/min",
       title = "Rainbow trout Aerobic Scope") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))


 ## CS burst----
table(csburst.3$STATUS)
csburst.2 <- csburst[-which(csburst$STATUS=="ERROR"),]
csburst.3 <- csburst.2[-which(csburst.2$STATUS=="R2B"),]

csburst.modif <- csburst.3 %>% group_by(FISH_ID) %>% 
  dplyr::summarise(TEMP = mean(TRIAL_TEMP),
                    Mean_Peak_Speed_2 = mean(PEAK_SPEED_2, na.rm=T),
                   Total_Bursts = mean(TOTAL_BURST_EVENTS, na.rm=T),
                   Mass = mean(MASS, na.rm=T),
                   SL = mean(SL, na.rm=T))

# cap burst # data at 25 
csburst.modif$Total_Bursts <- ifelse(csburst.modif$Total_Bursts>=25, 25, csburst.modif$Total_Bursts)

# figs
ggplot(csburst.modif,aes(TEMP, Mean_Peak_Speed_2)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "Fall-run Chinook Burst Speed") +
  ylim(0,170)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black")


ggplot(csburst.modif[csburst.modif$Total_Bursts>0,],aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Number of burst events", title = "Fall-run Chinook Number of Burst Events") +
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
# cap this data at 25 
rtburst.modif.2$Total_Bursts <- as.numeric(rtburst.modif.2$Total_Bursts)
rtburst.modif.2$Total_Bursts <- ifelse(rtburst.modif.2$Total_Bursts>=25, 25, rtburst.modif.2$Total_Bursts)

ggplot(rtburst.modif.2,aes(TEMP, Mean_Peak_Speed2)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Peak Speed (cm/s)", title = "Rainbow trout Burst Speed") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  geom_smooth(method = "lm", size = 1, color="black",fullrange=T)+
  coord_cartesian(ylim=c(0,max(rtburst.modif$Mean_Peak_Speed2)))



ggplot(rtburst.modif.2,aes(TEMP, Total_Bursts)) +
  geom_point() + 
  theme_classic() + 
  geom_jitter(width = 0.5, height = 0.7)+
  labs(x = "Temperature", y = "Number of burst events", title = "Rainbow trout Number of Burst Events") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) + geom_smooth(method = "lm", se = T) + 
  ylim(0,30) +
  geom_smooth(method = "lm", size = 1, color="black")

### Predation data ----

# considering all data
# SB plots
pred_SB %>%
  ggplot(aes(x = Temperature,
             y = Num.Salmon.Eaten)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Striped bass Predation") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  geom_smooth(method = "lm", size = 1, color="black")

bp_SB <- pred_SB %>%
  ggplot(aes(x = as.factor(Temperature),
             y = Num.Salmon.Eaten)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Striped bass Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) 
bp_SB

dp_SB <- pred_SB %>%
  ggplot(aes(x = as.factor(Temperature),
             y = Num.Salmon.Eaten)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize = .5)+
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Striped bass Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) 
dp_SB



#RT plots
pred_RT <- pred_RT[which(pred_RT$Num.Salmon.Eaten!="NA"),]
pred_RT %>%
  ggplot(aes(x = Temp,
             y = Num.Salmon.Eaten)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Rainbow trout Predation") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  geom_smooth(method = "lm", size = 1, color="black")

bp_RT <- pred_RT %>%
  ggplot(aes(x = as.factor(Temp),
             y = Num.Salmon.Eaten)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Rainbow trout Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

bp_RT

dp_RT <- pred_RT %>%
  ggplot(aes(x = as.factor(Temp),
             y = Num.Salmon.Eaten)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_classic() + 
  labs(x = "Temperature", y = "Number of Salmon eaten", title = "Rainbow trout Predation") +
  coord_cartesian(ylim=c(0,12))+
  scale_y_continuous(breaks=seq(0,12,2))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

dp_RT



# considering instances where at least one fish was eaten; can't really do because not enough data

pred_RT[pred_RT$Num.Salmon.Eaten>0,] %>%
  ggplot(aes(x = Temp,
             y = Num.Salmon.Eaten)) +
  geom_point(size = 2) +
  theme_classic() + 
  labs(x = "Temperature", y = "# Salmon eaten", title = "Rainbow trout Predation Data") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold")) + scale_x_continuous(breaks=seq(10,26,2)) +
  geom_smooth(method = "lm", size = 1, color="blue")

###> Individual predation data RT
### > Individual Predation data----

pred.modif <- pred_RT[,c(2,3,5,6,7,8,9,10,11,15,16)]

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

# Burst Data: CS ----
# can read in data from FR-LFR comparison_script to look at both runs


## Burst speed
mod.burst.speed.CS <- lm(Mean_Peak_Speed_2 ~ TEMP + Mass, data=csburst.modif) 
summary(mod.burst.speed.CS)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.burst.speed.CS)
plot(mod.burst.speed.CS, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.burst.speed.CS)) 
'looks ok'

## Burst number
mod.burst.no.CS <- glm(Total_Bursts ~ TEMP + Mass, data=csburst.modif) 
summary(mod.burst.no.CS)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.burst.no.CS)
plot(mod.burst.no.CS, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.burst.no.CS)) 
'looks ok'



# Burst Data: RT ----
# linear models

mod.burst.no.RT <- glm(Total_Bursts ~ TEMP + Mass, data=rtburst.modif.2, family="poisson") 
summary(mod.burst.no.RT)
plot(mod.burst.no.RT)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.burst.no.RT)
plot(mod.burst.no.RT, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.burst.no.RT)) 
'looks ok - maybe slightly skewed'


mod.burst.speed.RT <- lm(Mean_Peak_Speed2 ~ TEMP + Mass, data=rtburst.modif.2) 
summary(mod.burst.speed.RT)
plot(mod.burst.speed.RT)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.burst.speed.RT)
plot(mod.burst.speed.RT, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.burst.speed.RT)) 
'looks ok'



# AS Data: CS ----

mod.as.CS <- lm(AS.min ~ poly(TEST_TEMP,2) + MASS..g., data=csas)
summary(mod.as.CS)
# > model diagnostics
# 1 variance homogeneity 
plot(mod.as.CS)
plot(mod.as.CS, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.as.CS)) 
'looks ok'


# AS Data: RT ----
mod.as.RT <- lm(AS.min ~ poly(TEST_TEMP,2) + MASS, data=rtas)
summary(mod.as.RT)

# check with mass correction
rtas$AS.min.2 <- rtas$AS.min / rtas$MASS * rtas$MASS^.95 # according to values from Verhille et al. 2017
mod.as.RT.2 <- lm(AS.min.2 ~ poly(TEST_TEMP,2), data=rtas)
summary(mod.as.RT.2)


# > model diagnostics
# 1 variance homogeneity 
plot(mod.as.RT)
plot(mod.as.RT, 1)
'looks ok'

plot(mod.as.RT.2)
plot(mod.as.RT.2, 1)
'looks ok'

#2 normality of errors
hist(resid(mod.as.RT)) 
'looks ok'

hist(resid(mod.as.RT.2)) 
'looks ok'

## Differences in RT and FRCS ----
rt.compare.as <- rtas%>% group_by(TEST_TEMP) %>%
              dplyr::summarise(Mean.AS.RT = mean(AS.min, na.rm=T))

cs.compare.as <- csas%>% group_by(TEST_TEMP) %>%
  dplyr::summarise(Mean.AS.CS = mean(AS.min, na.rm=T))

differences.as <- left_join(rt.compare.as,cs.compare.as)
differences.as$difference_as <- ((differences.as$Mean.AS.CS/differences.as$Mean.AS.RT)-1)



rt.compare.burst <- rtburst.modif%>% group_by(TEMP) %>%
  dplyr::summarise(Mean.Burst.No.RT = mean(Total_Bursts, na.rm=T),
                   Mean.Peak.Speed.RT = mean(Mean_Peak_Speed2, na.rm=T))

cs.compare.burst <- csburst.modif%>% group_by(TEMP) %>%
  dplyr::summarise(Mean.Burst.No.CS = mean(Total_Bursts, na.rm=T),
                   Mean.Peak.Speed.CS = mean(Mean_Peak_Speed_2, na.rm=T))

differences.burst <- left_join(rt.compare.burst,cs.compare.burst)
differences.burst <- differences.burst[(complete.cases(differences.burst)),]
differences.burst$difference_burstno <- ((differences.burst$Mean.Burst.No.CS/differences.burst$Mean.Burst.No.RT)-1)
differences.burst$difference_burstspeed <- ((differences.burst$Mean.Peak.Speed.CS/differences.burst$Mean.Peak.Speed.RT)-1)

differences.burst.2 <- left_join(differences.burst, pred_RT, by=c("TEMP"="Temp"))

differences.as.2 <- left_join(differences.as, pred_RT, by=c("TEST_TEMP"="Temp"))

#### Models
library(MASS)

mod.differences.as <- glm.nb(Num.Salmon.Eaten~difference_as, data=differences.as.2)
summary(mod.differences.as)

mod.differences.burstno <- glm.nb(Num.Salmon.Eaten~ difference_burstno, data=differences.burst.2)
summary(mod.differences.burstno)

mod.differences.burstspeed <- glm.nb(Num.Salmon.Eaten~ difference_burstspeed, data=differences.burst.2)
summary(mod.differences.burstspeed)


# Predation data ----
#linear models
library(lme4)
library(MASS)
hist(pred_RT$Num.Salmon.Eaten)

pred_RT$scaled_RT_mass <- scale(pred_RT$Mass_Trout)
pred_RT$scaled_Salmon_Ave_Mass <- scale(pred_RT$Salmon_Ave_Mass)
mod.pred.RT <- glm.nb(Num.Salmon.Eaten~Temp + scaled_Salmon_Ave_Mass + scaled_RT_mass, data=pred_RT)
summary(mod.pred.RT) # use negative binomial distribution to account for the overdispersion in data
plot(mod.pred.RT)
'temp not sig'
# > model diagnostics
# 1 variance homogeneity 
plot(mod.pred.RT)
'looks not great'

#2 normality of errors
hist(resid(mod.pred.RT)) 
'looks ok'


pred_SB$scaled_SB_mass <- scale(pred_SB$Mass_Bass)
pred_SB$scaled_Salmon_Ave_Mass <- scale(pred_SB$Salmon_Ave_Mass)

hist(pred_SB$Num.Salmon.Eaten)
mod.pred.SB <- glm.nb(Num.Salmon.Eaten~Temperature + scaled_SB_mass+scaled_Salmon_Ave_Mass, data=pred_SB)
summary(mod.pred.SB)
plot(mod.pred.SB)
'temp not sig, but ave salmon mass sig -'
# > model diagnostics
# 1 variance homogeneity 
plot(mod.pred.SB)
'looks not great'

#2 normality of errors
hist(resid(mod.pred.SB)) 
'looks ok'


## Model tables ----
library(sjPlot) # table functions
library(sjmisc) # sample data

tab_model(mod.as.CS, transform=NULL, show.ci=F, show.se = T, show.obs = F)

tab_model(mod.as.RT,show.ci=F, transform=NULL, show.se = T, show.obs = F)

tab_model(mod.burst.no.CS, transform=NULL, show.ci=F, show.se = T,  show.obs = F)
tab_model(mod.burst.no.RT, transform=NULL, show.ci=F, show.se = T, show.obs = F)

tab_model(mod.burst.speed.CS, show.ci=F, show.se = T, show.r2 = T, show.obs = F)

tab_model(mod.burst.speed.RT, transform=NULL, show.ci=F, show.se = T, show.r2 = T, show.obs = F)

tab_model(mod.pred.RT, transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.obs = F) # this suggests to me that we may not need trial #

tab_model(mod.pred.SB, transform=NULL, show.ci=F, show.se = T, show.r2 = F, show.obs = F) # this suggests to me that we may not need trial #

