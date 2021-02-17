#ECON 8010 - Empirical Project
#Grant Davis and Quinn Lemarr
#11/25/20

rm(list=ls())
setwd("~/R/R Data/")
options("scipen"=99999, digits=6)

#Load in the data
library(readxl)
data = read_xlsx("Master Dallas Force 2017 to 2019.xlsx")
View(data)

##Creating dummies & relevant variables
  #Creating an experience column
  data = transform(data, exper = as.numeric(data$OCCURRED_D - data$HIRE_DT))
  
  #Creating a dummy for officer sex -- Male = 1
  data$OffSex =  tolower(data$OffSex)
  data$OffsexB = ifelse(data$OffSex=="male", 1,0) 
  #data$OffsexB
  
  #Creating a dummy variable for the citizens' sex -- Male = 1
  data$CitSex =  tolower(data$CitSex)
  data$CitSexB = ifelse(data$CitSex=="male", 1,0) 
  #data$CitSexB
  
  #Creating a factor for officer race
  data = transform(data, Orace.f = as.factor(OffRace))
  #data$Orace.f
  
  #Creating a factor for citizen race
  data = transform(data, Crace.f = as.factor(CitRace))
  #data$Crace.f
  
  #Creating a dummy for race match -- 1 = match
  data$raceMatch = ifelse(data$Orace.f == data$Crace.f, 1, 0) 
  #data$raceMatch
  
  #DO I HAVE TO DO THIS???? Turning CIT_INJURE from T/F to 0/1 -- 1 = Injured
  data$citInj = ifelse(data$CIT_INJURE == "TRUE", 1, 0) 
  #data$citInj
  
  #Agressive binary -- 1 = aggressive
  data$aggressive = ifelse(data$UOF_REASON == "Active Aggression" | data$UOF_REASON == "Danger to self or others" 
                            | data$UOF_REASON == "Assault to Other Person", 1, 0) 
  #data$aggressive
  
  #Armed Binary -- 1 = armed
  data$armed = ifelse(data$UOF_REASON == "Weapon Display", 1, 0) 
  #data$armed
  
  # cleaning NA from data
  data1 <- na.omit(data)
  
#creating a base LPM model
LPM = lm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + 
           raceMatch + aggressive + armed, data = data)
summary(LPM)

  # Testing % correctly predicted
  data$LPM.fitted <- LPM$fitted.values

  data$fit.CIT_INJURE[data$LPM.fitted>=0.5]<-1
  data$fit.CIT_INJURE[data$LPM.fitted<0.5]<-0

  sum(with(data, fit.CIT_INJURE==CIT_INJURE))
  sum(with(data, fit.CIT_INJURE==CIT_INJURE))/nrow(data)
  # 73.31% Correct
  
#creating Probit base model
Pro <- glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
             aggressive + armed, family = binomial(link = "probit"), data=data)
summary(Pro)

  # Testing % correctly predicted
  library("pscl")
  hitmiss(Pro) # 73.3%
  
#Creating Logit base model
Lo <-  glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
             aggressive + armed, family = binomial(link = "logit"), data=data)
summary(Lo)  

  # Testing % correctly predicted
  hitmiss(Lo) # 73.3% 
  
##### Making new vars to include in model #####
# CIT_INFL_A to factor or dummy 
library("dplyr")
distinct(data, data$CIT_INFL_A)  
  # Agitated, Unknown Drugs, Alchohol, Alchohol and unknown drugs
  data$c.state = ifelse(data$CIT_INFL_A == "Agitated" | data$CIT_INFL_A == "Unknown Drugs" |
                          data$CIT_INFL_A == "Alchohol" | 
                          data$CIT_INFL_A == "Alchohol and unknown drugs", 1, 0)  
distinct(data, data$OFF_HOSPIT)
  # Dummy for officer hospitalized 
  data$o.hospital = ifelse(data$OFF_HOSPIT == "TRUE", 1, 0)
  
  # Dummy for cit arrested
  data$c.arrest = ifelse(data$CIT_ARREST == "TRUE" , 1 , 0)
  
# Making exper go from days to years 
  # exper 
  data$expy = (data$exper/365)
  # View(data)
##### LPM/Probit/Logit w/ citizen state var #####
# LPM 
LPM1 = lm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + 
            raceMatch + aggressive + armed + c.state, data = data)
summary(LPM1)

  # Testing % correctly predicted
  data$LPM1.fitted <- LPM1$fitted.values
  
  data$fit.CIT_INJURE1[data$LPM1.fitted>=0.5]<-1
  data$fit.CIT_INJURE1[data$LPM1.fitted<0.5]<-0
  
  sum(with(data, fit.CIT_INJURE1==CIT_INJURE))
  sum(with(data, fit.CIT_INJURE1==CIT_INJURE))/nrow(data)
  # 73.37%
  
#Probit  
Pro1 <- glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
             aggressive + armed + c.state, family = binomial(link = "probit"), data=data)
summary(Pro1)

  # Testing % correctly predicted
  hitmiss(Pro1) # 73.3%
  
#Logit
Lo1 <-  glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
             aggressive + armed + c.state, family = binomial(link = "logit"), data=data)
summary(Lo1)  

  # Testing % correctly predicted
  hitmiss(Lo1) # 73.4% 
  
##### LPM/Probit/Logit w/ officer hospitalized var and c.state #####
# LPM 
LPM2 = lm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + 
            raceMatch + aggressive + armed + c.state + o.hospital, data = data)
summary(LPM2)
  
  # Testing % correctly predicted
  data$LPM2.fitted <- LPM2$fitted.values
  
  data$fit.CIT_INJURE2[data$LPM2.fitted>=0.5]<-1
  data$fit.CIT_INJURE2[data$LPM2.fitted<0.5]<-0
  
  sum(with(data, fit.CIT_INJURE2==CIT_INJURE))
  sum(with(data, fit.CIT_INJURE2==CIT_INJURE))/nrow(data)
  # 73.56%
  
#Probit  
Pro2 <- glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
              aggressive + armed + c.state + o.hospital, family = binomial(link = "probit"), data=data)
summary(Pro2)
  
  # Testing % correctly predicted
  hitmiss(Pro2) # 73.6%
  
#Logit
Lo2 <-  glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
              aggressive + armed + c.state + o.hospital, family = binomial(link = "logit"), data=data)
summary(Lo2)  
  
  # Testing % correctly predicted
  hitmiss(Lo2) # 73.6%
  
##### LPM/Probit/Logit w/ o.hospital, c.state and c.arrest #####
# LPM 
LPM3 = lm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + 
            raceMatch + aggressive + armed + c.state + o.hospital + c.arrest, data = data)
summary(LPM3)

  # Testing % correctly predicted
  data$LPM3.fitted <- LPM3$fitted.values
  
  data$fit.CIT_INJURE3[data$LPM3.fitted>=0.5]<-1
  data$fit.CIT_INJURE3[data$LPM3.fitted<0.5]<-0
  
  sum(with(data, fit.CIT_INJURE3==CIT_INJURE))
  sum(with(data, fit.CIT_INJURE3==CIT_INJURE))/nrow(data)
  # 73.75%
  
#Probit  
Pro3 <- glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
              aggressive + armed + c.state + o.hospital +
              c.arrest, family = binomial(link = "probit"), data=data)
summary(Pro3)
  
  # Testing % correctly predicted
  hitmiss(Pro3) # 73.8%
  
#Logit
Lo3 <-  glm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace.f + raceMatch +
              aggressive + armed + c.state + o.hospital +
              c.arrest, family = binomial(link = "logit"), data=data)
summary(Lo3)  
  
  # Testing % correctly predicted
  hitmiss(Lo3) # 73.7 %
  
##### Screen print out #####
library("texreg")
screenreg(l = list(LPM3, Pro3, Lo3), stars = c(0.001, 0.01, 0.05, 0.10),
          custom.model.names = c("LPM", "Probit", "Logit"))

##### Setting white as base race factor using relevel() function #####
distinct(data, Crace.f)
  data <- within(data, Crace1.f <- relevel(Crace.f, ref = "White" ))
  # View(data)
  # run model with Crace1.f to see if white is base
     LPMtrial = lm(CIT_INJURE ~ OffsexB + Orace.f + exper + CitSexB + Crace1.f + 
              raceMatch + aggressive + armed + c.state + o.hospital + c.arrest, data = data)
     summary(LPMtrial)
     # changes happen
distinct(data, Orace.f)
  data <- within(data, Orace1.f <- relevel(Orace.f, ref = "White" ))
  # running model with Crace and Crace as white
    LPMtrial1 = lm(CIT_INJURE ~ OffsexB + Orace1.f + exper + CitSexB + Crace1.f + 
                    raceMatch + aggressive + armed + c.state + o.hospital + c.arrest, data = data)
    summary(LPMtrial1)

##### Redoing LMP3/Probit3/Logit3 with white as Race factor base group + AME/PEA #####
# LPM 
LPM4 <- lm(CIT_INJURE ~ OffsexB + Orace1.f + exper + CitSexB + Crace1.f + 
            raceMatch + aggressive + armed + c.state + o.hospital + c.arrest, data = data)
summary(LPM4)
    
  # Testing % correctly predicted
  data$LPM4.fitted <- LPM4$fitted.values
  
  data$fit.CIT_INJURE4[data$LPM4.fitted>=0.5]<-1
  data$fit.CIT_INJURE4[data$LPM4.fitted<0.5]<-0
  
  sum(with(data, fit.CIT_INJURE4==CIT_INJURE))
  sum(with(data, fit.CIT_INJURE4==CIT_INJURE))/nrow(data)
  # 73.75%
    
#Probit  
Pro4 <- glm(CIT_INJURE ~ OffsexB + Orace1.f + exper + CitSexB + Crace1.f + raceMatch +
              aggressive + armed + c.state + o.hospital +
              c.arrest, family = binomial(link = "probit"), data=data)
summary(Pro4)

  # Testing % correctly predicted
  hitmiss(Pro4) # 73.8%

#Logit
Lo4 <-  glm(CIT_INJURE ~ OffsexB + Orace1.f + exper + CitSexB + Crace1.f + raceMatch +
              aggressive + armed + c.state + o.hospital +
              c.arrest, family = binomial(link = "logit"), data=data)
summary(Lo4)  

  # Testing % correctly predicted
  hitmiss(Lo4) # 73.7%
  
## doing PEA / MEA for Logit/Probit
  library("mfx")
  # Logit model AME & PEA
  Lo4AME <-  logitmfx(Lo4, data = data, atmean = T) #logit AME
  Lo4PEA <- logitmfx(Lo4, data = data, atmean = F) #logit PEA
  Lo4AME
  Lo4PEA
  # Probit model AME & PEA
  Pro4AME <- probitmfx(Pro4, data = data, atmean =T) # probit AME
  Pro4PEA <- probitmfx(Pro4, data = data, atmean = F) # probit PEA
  Pro4AME
  Pro4PEA
  
  screenreg( l = list(Lo4AME, Pro4AME, Lo4PEA, Pro4PEA), stars = c(0.001, 0.01, 0.05, 0.10), 
             custom.model.names = c("Logit AME", "Probit AME", "Logit PEA", "Probit PEA") )

##### Comparing American Indian base group to the current white base group race factor #####
  P1 <- screenreg(l = list(LPM3, Pro3, Lo3), stars = c(0.001, 0.01, 0.05, 0.10),
            custom.model.names = c("LPM", "Probit", "Logit"))
  P2 <- screenreg(l = list(LPM4, Pro4, Lo4), stars = c(0.001, 0.01, 0.05, 0.10),
                  custom.model.names = c("LPM", "Probit", "Logit"))
P1
P2
  # combined print out
  screenreg(l = list(LPM3, LPM4, Pro3, Pro4, Lo3, Lo4), stars = c(0.001, 0.01, 0.05, 0.10),
          custom.model.names = c("LPM-Indian", "LPM-White", "Probit-Indian", "Probit-White",
                                 "Logit-Indian", "Logit-White"))
  
##### Making exports for word doc of the models we've built #####
## Screen print of LPM/PRO/LOG comparison
  wordreg(l = list(LPM4, Pro4, Lo4), stars = c(0.001, 0.01, 0.05, 0.10),
                  custom.model.names = c("LPM", "Probit", "Logit"),
          custom.gof.rows = list("Percent Correctly Predicted" = 
                                   c("73.75%", "73.76%", "73.73%" )), file = "testfile.doc")
  
  unlink("testfile.doc")
## Add means and St Dev for all vars as well
  
## Individual Model Exports as well
  LPMsummary <- summary(LPM4)
  screenreg(l =list(LPM4), stars = c(0.001, 0.01, 0.05, 0.10), 
            custom.model.names = ("LPM") )  
  
##### Made models with experience as years/ checked to make sure it didnt have a effect #####
  
# Doing LPM with expy as years not days to see if there is an effect     
LPM5 <- lm(CIT_INJURE ~ OffsexB + Orace1.f + expy + CitSexB + Crace1.f + 
               raceMatch + aggressive + armed + c.state + o.hospital + c.arrest, data = data)
  summary(LPM5)  
screenreg(l = list(LPM5, Pro5, Lo5), stars = c(0.001, 0.01, 0.05, 0.10)  )

#Probit  
Pro5 <- glm(CIT_INJURE ~ OffsexB + Orace1.f + expy + CitSexB + Crace1.f + raceMatch +
              aggressive + armed + c.state + o.hospital +
              c.arrest, family = binomial(link = "probit"), data=data)
  summary(Pro5)

  # Testing % correctly predicted
  hitmiss(Pro4) # 73.8%

#Logit
Lo5 <-  glm(CIT_INJURE ~ OffsexB + Orace1.f + expy + CitSexB + Crace1.f + raceMatch +
              aggressive + armed + c.state + o.hospital +
              c.arrest, family = binomial(link = "logit"), data=data)
  summary(Lo5)  

  # Testing % correctly predicted
  hitmiss(Lo3) # 73.7%
  
## doing PEA / MEA for Logit/Probit
library("mfx")
# Logit model AME & PEA
Lo5AME <- logitmfx(Lo5, data = data, atmean = T) #logit AME
Lo5PEA <- logitmfx(Lo5, data = data, atmean = F) #logit PEA/ APE
Lo5AME
Lo5PEA
# Probit model AME & PEA
Pro5AME <- probitmfx(Pro5, data = data, atmean = T) # probit AME
Pro5PEA <- probitmfx(Pro5, data = data, atmean = F) # probit PEA
Pro5AME
Pro5PEA
##### Look at Expy SD and Mean #####
# expy SD
sd(data$expy)
mean(data$expy)

##### Check VIF on models #####
library("car")
vif(LPM5)
vif(Pro5)
vif(Lo5)
# were good!
##### Making table w/  Mean/ SD and N# for quinn #####
# mean and stdev chart for the control variables

our_summary1 <-
  list("Officer Gender" =
         list("Mean"               = ~ mean(OffsexB),
              "Standard Deviation" = ~ sd(OffsexB)),
       "Officer Experience" =
         list("Mean"               = ~ mean(expy),
              "Standard Deviation" = ~ sd(expy)),
       "Citizen Gender" =
         list("Mean"               = ~ mean(CitSexB),
              "Standard Deviation" = ~ sd(CitSexB)),
       "Citizen Aggressive" =
         list("Mean"               = ~ mean(aggressive),
              "Standard Deviation" = ~ sd(aggressive)),
       "Citizen Armed" =
         list("Mean"               = ~ mean(armed),
              "Standard Deviation" = ~ sd(armed)),
       "Citizen Impaired State" =
         list("Mean"               = ~ mean(c.state),
              "Standard Deviation" = ~ sd(c.state)),
       "Officer Hospitalized" =
         list("Mean"               = ~ mean(o.hospital),
              "Standard Deviation" = ~ sd(o.hospital)),
       "Citizen Arrested" =
         list("Mean"               = ~ mean(c.arrest),
              "Standard Deviation" = ~ sd(c.arrest))
)
library("dplyr")
# install.packages("data.frame")
library("data.table")
library("tidyverse")
# install.packages("qwraps2")
library("qwraps2")
whole <- summary_table(data, our_summary1)

whole

