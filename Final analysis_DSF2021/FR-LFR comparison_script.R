### Comparing between FR and LFR 
rm(list=ls())
library(ggplot2)
# Burst comparison ----
setwd("~/Box Sync/Salmon-Pred-Project/Experiment2_2021_CS-SB/DSF_EXP_2021_CODE-FILES/Final analysis_DSF2021")
csburst_FR <- read.csv("Cleaned_FRCS_Bursts_Spring2021.csv")
setwd("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020")
csburst_LFR <-  read.csv("CS_burst_cleaned_Sum2021.csv")

# add column to compare salmon run

csburst.modif.LFR <- csburst_LFR %>% group_by(FISH_ID) %>% 
  dplyr::summarise(TEMP = mean(TRIAL_TEMP),
                   Mean_Peak_Speed2SL = mean(PEAK_SPEED_2_SL, na.rm=T),
                   Total_Bursts = mean(TOTAL_BURST_EVENTS_new, na.rm=T),
                   Mass = mean(MASS, na.rm=T),
                   SL=mean(SL, na.rm=T))

csburst.modif_FR <- csburst_FR[c(2,4,3,7,5,6)]

csburst.modif.LFR$Run <- rep("LFR", nrow(csburst.modif.LFR))
csburst.modif_FR$Run <- rep("FR", nrow(csburst.modif_FR))
csburst.modif_FR <- csburst.modif_FR[csburst.modif_FR$Peak_Speed_SL<80,]

## combine all data

colnames(csburst.modif_FR) <- colnames(csburst.modif.LFR)
allcs <- rbind(csburst.modif_FR, csburst.modif.LFR)
table(allcs$Run)
# now to see if there is an effect of temperature during our trials BASED on run

mod.burst.no <- glm(Total_Bursts ~ TEMP*Run, data=allcs, family="poisson") 
summary(mod.burst.no)


qplot(x = TEMP, y = Total_Bursts, facets = ~Run, data = allcs) +
  geom_smooth(method = "lm") +
  labs(x = "Temperature", 
       title = "CS Total Bursts") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

mod.burst.speed <- glm(Mean_Peak_Speed2SL ~ TEMP*Run, data=allcs) 
summary(mod.burst.speed)

qplot(x = TEMP, y = Mean_Peak_Speed2SL, facets = ~Run, data = allcs) +
  geom_smooth(method = "lm") +
  labs(x = "Temperature", 
       title = "CS Peak Speed") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"), plot.title = element_text(hjust = 0.5, size=20, face="bold"))

'run does significantly affect response to temp'

# AS comparison ----
setwd("~/Box Sync/Salmon-Pred-Project/Experiment2_2021_CS-SB/DSF_EXP_2021_CODE-FILES/Final analysis_DSF2021")
csas_FR <- read.csv("FRCS_AS_cleaned_Spr2021.csv")
setwd("~/Box Sync/Salmon-Pred-Project/Experiment1_2020_CS-LMB/DSF_EXP_2020_CODE-FILES/Final analysis_DSF2020")
csas_LFR <- read.csv("CS_AS_cleaned_Spr2021.csv")

csas_FR$Run <- rep("FR", nrow(csas_FR))
csas_LFR$Run <- rep("LFR", nrow(csas_LFR))



colnames(csas_FR) # need to divide by 60 to make them to per min vs. per hour
colnames(csas_LFR)

csas_FR.2 <- csas_FR[,c(1,4,18,19,20,21,22, 35)]
csas_LFR.2 <- csas_LFR[,c(2,3,4,5,8,6,7,9)]
colnames(csas_FR.2) <- colnames(csas_LFR.2)

allcs_as <- rbind(csas_FR.2, csas_LFR.2)
allcs_as.2 <- allcs_as[allcs_as$aerobic_scope>0,]

mod.as <- lm(aerobic_scope ~ TEST_TEMP*Run, data=allcs_as.2) 
summary(mod.as)


# to see if mass has anything to do with it
par(mfrow=c(2,1))
hist(csas_FR$MASS..g.)
hist(csas_LFR$MASS)

median(csas_FR$MASS..g.)
median(csas_LFR$MASS)


# modify models to account for mass ----
allcs.mass <- allcs[allcs$Mass<=3,]
table(allcs.mass$Mass)

mod.burst.no.mass <- glm(Total_Bursts ~ TEMP*Run, data=allcs.mass, family="poisson") 
summary(mod.burst.no)

mod.burst.speed.mass <- glm(Mean_Peak_Speed2SL ~ TEMP*Run, data=allcs.mass) 
summary(mod.burst.speed)

'# run still matters for burst response to temp, as well as intercepts'

allcs_as.mass <- allcs_as[allcs_as$MASS<=3,]
table(allcs_as.mass$MASS)

mod.as_mass <- lm(aerobic_scope ~ TEST_TEMP*Run, data=allcs_as.mass) 
'difference in run intercept, not in response to temp'
summary(mod.as_mass)

'run still just matters for intercepts, not for slope'


write.csv(allcs, "Combined_Runs_CS_Burst.csv")
write.csv(allcs_as.2, "Combined_Runs_CS_AS.csv")
