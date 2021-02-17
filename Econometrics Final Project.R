# Grant Davis - Econometric Project - Data

rm(list=ls())
setwd("~/R/R Data")
options("scipen"=99999, digits=3)


# refer to word doc to see the variables names and what they stand for
# wow this is huge
library("readxl")

data <- read.csv("allegations_202007271729.csv")

# View(data)
## Need to make ethnicity dummies for "White, Asian, Hispanic, American Indian, Black"
# install.packages("dplyr")
library("dplyr")

##### Look at distinct types of data #####
distinct(data, data$mos_ethnicity, .keep_all = F)
distinct(data, data$mos_gender, .keep_all = F)
distinct(data, data$complainant_ethnicity, .keep_all = F)
distinct(data, data$complainant_gender, .keep_all = F)
distinct(data, data$fado_type, .keep_all = F)
distinct(data, data$allegation, .keep_all = F)
distinct(data, data$board_disposition, .keep_all = F)

## Need to convert Mos_ethnicity to dummies
## Need to convert mos_gender to M=1 and F=2
## Need to convert complaint_ethnicity to dummies
## Need to convert complaint_gender to dummies
## Need to convert fado_type to dummies
## Need to convert allegation to dummies
## Need to convert board_disposition to dummies

distinct(data, data$precinct, .keep_all = F)
distinct(data, data$contact_reason, .keep_all = F)

##### Histogram #####

### Look at histogram of data$year received to see where most data is present
# install.packages("ZOO")
library("zoo") # zoo for dates cleaning
hist(data$year_received)

##### Editing Variables - dummies - cleaning - NA #####
data <- transform(data, race.f = as.factor(complainant_ethnicity))
data$race.f

data$fado_type <-  tolower(data$fado_type) # turns to lowercase

data <-  transform(data, force = as.numeric(data$fado_type == "force"))
# View(data) # new col with force as binary

# Transforming the separate year date columns into one usable column
data$date_received <- as.yearmon(paste(data$year_received, data$month_received), "%Y %m")
  # date received became year and month in Y, M format

data$date_closed <- as.yearmon(paste(data$year_closed, data$month_closed), "%Y %m")
  # date closed became year and month in Y, M format

data <- transform(data, duration = as.numeric(data$date_closed - data$date_received))
  #  created duration with date closed and date received subtracted
hist(data$duration) # Crated duration of time from received to close 



### Create dummy for unsubstantiated vs substantiated claims
data$board_disposition <- tolower(data$board_disposition)
   # make all lowercase 
data$substantiated <- ifelse(substring(data$board_disposition, 1,4) == "subs", 1, 0) 
  # new col with substantiated, if the first 4 letters had "subs" in them it counted as 1 if not then 0
data$substantiated
# View(data)

### removing the observations with NA because we want full information
goodData <- na.omit(data)
# View(goodData)

# make complaint gender binary
goodData$complainant_gender <- tolower(goodData$complainant_gender)
goodData$comp_gender <- as.numeric ( with ( 
  goodData, ifelse ( ( goodData$complainant_gender == "male" ), 1 , 0 ) ) )

# make officer gender binary
goodData$mos_gender <- tolower(goodData$mos_gender)
goodData$officer_gender <- as.numeric ( with ( 
  goodData, ifelse ( ( goodData$mos_gender == "m" ), 1 , 0 ) ) )

# officer race as dummy

goodData <- transform(goodData, o.race.f = as.factor(mos_ethnicity))
goodData$o.race.f

##### Base Models #####

#creating a base model
base <- lm(substantiated ~ complainant_age_incident + year_closed 
           + race.f + force, data = goodData)
summary(base) # nope


#base model with duration
based <- lm(substantiated ~ complainant_age_incident + duration
            + year_closed + race.f + force, data = goodData)
summary(based) # nope 


#duration model
dModel <- lm(duration ~ substantiated + complainant_age_incident
             + year_closed + race.f + force, data = goodData)
summary(dModel) # this one shows promise

##### Lm and Glm models - nothing crazy helpful besides R1 - Ignore #####
# sub model with gender
R0 <- lm(substantiated ~ complainant_age_incident + year_closed + race.f +
           officer_gender+ force, data = goodData)
summary(R0)
# take out complainant age
R1 <- lm(substantiated ~ year_closed + race.f +  comp_gender +
            officer_gender + force, data = goodData)
summary(R1) # shows gender is important, year closed, force and some race dummies


# add officer gender, ethnicity, same for complaint, force 
R2 <- lm(substantiated ~ year_closed + race.f + comp_gender + o.race.f
         + officer_gender
         + force, data = goodData)
summary(R2) # officer race has no sig. 


library("texreg")
screenreg(l = list(R1, R2) , stars = c(0.001, 0.01, 0.05, 0.1)) 

# R1 is better right now, try a glm 
library("pscl")
Logit1 <- glm( substantiated ~ year_closed + race.f +  comp_gender +
                 officer_gender + force, data = goodData, 
               family = binomial(link = "logit") )
summary(Logit1)
hitmiss(Logit1)

Probit1 <- glm( substantiated ~ year_closed + race.f +  comp_gender +
                  officer_gender + force, data = goodData, 
                family = binomial(link = "probit") )
summary(Probit1)
hitmiss(Probit1)

screenreg(l = list(R1, Logit1, Probit1), stars = c(0.001, 0.01, 0.05, 0.1))

##### duration model w/ no race binary - Ignore ##### 

dModel <- lm(duration ~ substantiated + complainant_age_incident
             + year_closed + race.f + force, data = goodData)
summary(dModel) # this one shows promise

D2 <- lm(duration ~ substantiated + complainant_age_incident
         + year_closed + race.f + comp_gender + force, data = goodData)
summary(D2) # added complainant gender

D3 <- lm(duration ~ complainant_age_incident
         + year_closed + race.f + comp_gender + force, data = goodData)
summary(D3) # took out substantiated

# look at the 3 models
screenreg(l = list(dModel, D2, D3), stars = c(0.001, 0.01, 0.05, 0.1))

# look at officer gender and ethnicity as well
D4 <- lm(duration ~ complainant_age_incident
         + year_closed + race.f + comp_gender 
         + officer_gender + o.race.f + force, data = goodData)
summary(D4) # officer race and gender has no effect- don't use
 
##### Making complainant race dummies #####

#### Realized we need to avoid co linearity so lets make dummies on race
goodData$complainant_ethnicity <- tolower(goodData$complainant_ethnicity)
distinct(goodData, goodData$complainant_ethnicity, .keep_all = F)

goodData$comp_white <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "white" ), 1 , 0 ) ) ) # white race binary
goodData$comp_black <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "black" ), 1 , 0 ) ) ) # black race binary
goodData$comp_asian <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "asian" ), 1 , 0 ) ) ) # asian race binary
goodData$comp_refused <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "refused" ), 1 , 0 ) ) ) # refused race binary
goodData$comp_hispanic <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "hispanic" ), 1 , 0 ) ) ) # hispanic race binary
goodData$comp_unknown <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "unknown" ), 1 , 0 ) ) ) # unknown race binary
goodData$comp_other <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "other race" ), 1 , 0 ) ) ) # other race binary
goodData$comp_amer_indian <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == "american indian" ), 1 , 0 ) ) ) # american indian race binary


# View(goodData)

##### Making officer race dummies #####
goodData$mos_ethnicity <- tolower(goodData$mos_ethnicity)
distinct(goodData, goodData$mos_ethnicity, .keep_all = F)
# Hispanic, White, Black, Asian, and American Indian

goodData$officer_white <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$mos_ethnicity == "white" ), 1 , 0 ) ) ) # white race binary

goodData$officer_black <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$mos_ethnicity == "black" ), 1 , 0 ) ) ) # black race binary

goodData$officer_asian <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$mos_ethnicity == "asian" ), 1 , 0 ) ) ) # asian race binary

goodData$officer_hispanic <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$mos_ethnicity == "hispanic" ), 1 , 0 ) ) ) # hispanic race binary

goodData$officer_amer_indian <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$mos_ethnicity == "american indian" ), 1 , 0 ) ) ) # american indian race binary

# View(goodData)

##### Recreate models with dummies/ Base group is white, so leave it out - Ignore #####


D5 <- lm(duration ~ complainant_age_incident
         + year_closed + comp_black + comp_asian + comp_refused + comp_hispanic + comp_unknown
         + comp_other + comp_amer_indian + comp_gender + force, data = goodData)
summary(D5) # White race is not included
summary(D3)

##### Substantiated Model - Ignore #####

R3 <- lm(substantiated ~ year_closed + comp_black + comp_asian 
        + comp_refused + comp_hispanic + comp_unknown
        + comp_other + comp_amer_indian +  comp_gender +
          officer_gender + force, data = goodData)
summary(R3)

##### Look at officer rank at time of report for substantiated - Ignore #####
# Create binary for officer rank

#View(goodData$rank_abbrev_incident)
distinct(goodData, goodData$rank_abbrev_incident, .keep_all = F)
  # idk about ranks, theres 18 duummies id have to make
##### Create Dummies based on if officer and comp are same race #####

# We will be making dummies that = 1 if the officer and comp are same race, 0 otherwise
goodData$mos_ethnicity <- tolower(goodData$mos_ethnicity)

goodData$same_race <- as.numeric ( 
  with ( goodData, ifelse ( 
    ( goodData$complainant_ethnicity == goodData$mos_ethnicity ), 1 , 0 ) ) )

# View(goodData)
##### Use the New Dummy to run some regressions - Ignore #####

# Duration ref. D5
D6 <- lm(duration ~ complainant_age_incident
         + year_closed + same_race + comp_gender + force, data = goodData )
summary(D6)
D7 <- lm(duration ~ complainant_age_incident
         + year_closed + same_race + comp_gender 
         + substantiated + force, data = goodData )
summary(D7) # with substantiated included
D8 <- lm(duration ~ complainant_age_incident
         + year_closed + same_race + comp_gender + officer_gender
         + substantiated + force, data = goodData )
summary(D8) # added officer gender 1 = male, 0 = female

## make a table to show these and pick
screenreg(l = list(D5, D6, D7, D8), stars = c(0.001, 0.01, 0.05, 0.1))
screenreg(l = list( D6, D7, D8), stars = c(0.001, 0.01, 0.05, 0.1))

# Substantiated ref. R3
R4 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + force, data = goodData)
summary(R4)
R5 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + duration + force, data = goodData)
summary(R5) # with duration included 
R6 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + mos_age_incident + force, data = goodData)
summary(R6) # age is significant, same race isnt
R7 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + mos_age_incident + complainant_age_incident + 
           force, data = goodData) # comp age and same race noi sig
summary(R7)

## Make table to show substantiated models
screenreg(l = list(R3, R4, R5, R6, R7), stars = c(0.001, 0.01, 0.05, 0.1 ))
screenreg(l = list(R4, R5, R6, R7), stars = c(0.001, 0.01, 0.05, 0.1 ))

##### Make var log of duration #####
goodData <- transform(goodData, durationl = log(goodData$duration))
# View(goodData)

##### Redo Regs with durationl = log - Ignore #####
### Duration model
D9 <- lm(durationl ~ complainant_age_incident
         + year_closed + same_race + comp_gender + force, data = goodData )
summary(D9)
D10 <- lm(durationl ~ complainant_age_incident
         + year_closed + same_race + comp_gender 
         + substantiated + force, data = goodData )
summary(D10) # with substantiated included
D11 <- lm(durationl ~ complainant_age_incident
         + year_closed + same_race + comp_gender + officer_gender
         + substantiated + force, data = goodData )
summary(D11) # added officer gender 1 = male, 0 = female
screenreg(l = list( D9, D10, D11), stars = c(0.001, 0.01, 0.05, 0.1))

### Substantiated Model
R4 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + force, data = goodData)
summary(R4)
R8 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + durationl + force, data = goodData)
summary(R5) # with duration included 
R6 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + mos_age_incident + force, data = goodData)
summary(R6) # age is significant, same race isnt
R7 <- lm(substantiated ~ year_closed + same_race +  comp_gender +
           officer_gender + mos_age_incident + complainant_age_incident + 
           force, data = goodData) # comp age and same race noi sig
summary(R7)
screenreg(l = list(R4, R8, R6, R7), stars = c(0.001, 0.01, 0.05, 0.1 ))
##### Note - Clearly see omitted var. bias must explore more, IV potential. #####
# need to figure out how to see larger effects in coefficents

##### Split to 2005 - 2009 & 2011 - 2015 | exclude 2010 to look for differences with new quota law - ignore #####

# View(goodData) # sub by year_recieved
time0509 <- subset(goodData, 
                   year_received == 2005 |
                     year_received == 2006 |
                     year_received == 2007 |
                     year_received == 2008 |
                     year_received == 2009 
                     )
# View(time0509)  # looks good
  # FYI this is 2005 to 2009 subset
  
time1115 <- subset(goodData, 
                     year_received == 2011 |
                     year_received == 2012 |
                     year_received == 2013 |
                     year_received == 2014 |
                     year_received == 2015)
# View(time1115) # looks good
  # FYI this is 2011 to 2015 subset


##### Duration models based on pre 2010 and post 2010 - Ignore #####

#### use time0509, Regs with only White =  Base, Black, Hispanic, Asian & split by time

### Duration 

## Using time0509 & white as base dummy 

## comp gender male = 1, female = 0

DD1 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time0509)
summary(DD1) # asian isnt sig

# adding officer gender
DD2 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time0509)
summary(DD2) # oficer gender isnt sig

# see if same race has effect
DD3 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time0509)
summary(DD3) # same race has no effect

# add substantiated=1 or not in 
DD4 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time0509)
summary(DD4) # substantiated has no effect

### make a print out to show for 2005 to 2009
screenreg(l = list(DD1, DD2, DD3, DD4), stars = c(0.001, 0.01, 0.05, 0.1 ))




#### Duration for 2011 to 2015, time1115

DD5 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time1115)
summary(DD5) # all sig

# adding officer gender
DD6 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time1115)
summary(DD6) # officer gender not sig

# see if same race has effect
DD7 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time1115)
summary(DD7) # race is not sig

# add substantiated=1 or not in 
DD8 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time1115)
summary(DD8) # sub has no sig

### Make a print out 2011 to 2015
screenreg(l = list(DD5, DD6, DD7, DD8), stars = c(0.001, 0.01, 0.05, 0.1 ))

# Compare same models to there counterpart time0509 to time1115
screenreg(l = list(DD1, DD5), stars = c(0.001, 0.01, 0.05, 0.1 ), 
          custom.model.names = c("Pre 2010", "Post 2010") )
screenreg(l = list(DD2, DD6), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010"))
screenreg(l = list(DD3, DD7), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 
screenreg(l = list(DD4, DD8), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 

##### Substantiated models based on pre 2010 and post 2010 - Ignore #####

#### Regs with only White =  Base, Black, Hispanic, Asian & split by time

### Substantiated

## Using time0509 & white as base dummy 

## comp gender male = 1, female = 0

RR1 <- lm(substantiated ~ year_closed + same_race + comp_gender +
        officer_gender + comp_asian + comp_black + comp_hispanic +
        force, data = time0509)
summary(RR1) # same race/ hispanic and asian are not sig, keep dummies though

# take out same race/ Add duration
RR2 <- lm(substantiated ~ year_closed +  comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            duration + force, data = time0509)
summary(RR2) # duration, asian, hispanic not sig

# add officer age to RR1
RR3 <- lm(substantiated ~ year_closed + same_race + comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            mos_age_incident + force, data = time0509)
summary(RR3) # officer age is sig at 5%


#### Substantiated with time1115

RR4 <- lm(substantiated ~ year_closed + same_race + comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            force, data = time1115)
summary(RR4) 

# take out same race/ Add duration
RR5 <- lm(substantiated ~ year_closed +  comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            duration + force, data = time1115)
summary(RR5) 

# add officer age to RR3
RR6 <- lm(substantiated ~ year_closed + same_race + comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            mos_age_incident + force, data = time1115)
summary(RR6) 

### non of these are good models, we should use duration models - Grant

##### Updated Duration Models | Control officer demographics - Ignore #####

### Quinn wants to add officer age, officer race, gender and if they are same race 
# make time0509 & time1115 versions
# use black, hispanic and asian dummies, base group = white


# time0509
DD9 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
            + officer_black + officer_gender + mos_age_incident + 
            same_race  + force , data = time0509)
summary(DD9)

# time1115
DD10 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
            + officer_black + officer_gender + mos_age_incident + 
            same_race  + force , data = time1115)
summary(DD10)

# make a print out
screenreg(l = list(DD9, DD10), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 
##### Updated Duration Model | log of duration as explanatory - Ignore #####

#### use durationl to in original duration models and compare dif.
### FYI 
## log - linear = the change in y given a one unit increase in x

# View(time0509)
# View(time1115)

### time0509 Durationl 

DL1 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time0509)
summary(DL1)  # asian not sig

# adding officer gender
DL2 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time0509)
summary(DL2) # asian and officer gender not sig

# see if same race has effect
DL3 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time0509)
summary(DL3) # asian and same race not sig

# add substantiated=1 or not in 
DL4 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time0509)
summary(DL4) # asian and substantiated not sig

### make a print out to show for 2005 to 2009 models comparison
screenreg(l = list(DL1, DL2, DL3, DL4), stars = c(0.001, 0.01, 0.05, 0.1 ))


### time1115 Durationl

DL5 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time1115)
summary(DL5) # all sig

# adding officer gender
DL6 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time1115)
summary(DL6) # officer gender not sig

# see if same race has effect
DL7 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time1115)
summary(DL7) # same race not sig

# add substantiated=1 or not in 
DL8 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time1115)
summary(DL8) # substantiated not sig

# Comparison of time1115 models
screenreg(l = list(DL5, DL6, DL7, DL8), stars = c(0.001, 0.01, 0.05, 0.1 ))

# Compare same models to there counterpart time0509 to time1115
screenreg(l = list(DL1, DL5), stars = c(0.001, 0.01, 0.05, 0.1 ), 
          custom.model.names = c("Pre 2010", "Post 2010") )
screenreg(l = list(DL2, DL6), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010"))
screenreg(l = list(DL3, DL7), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 
screenreg(l = list(DL4, DL8), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 

#### adding all groups we think we should control for with durationl

# time0509
DL9 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
            + officer_black + officer_gender + mos_age_incident + 
            same_race  + force , data = time0509)
summary(DL9)

# time1115
DL10 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
           + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
             + officer_black + officer_gender + mos_age_incident + 
             same_race  + force , data = time1115)
summary(DL10)

# make a print out
screenreg(l = list(DL9, DL10), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 
##### Need to subset data so only dummies are white, black, hispanic & asian #####


# trial subsetting only by race dummies in dpylr
trial <- goodData %>% 
  filter(comp_white == 1 | comp_black == 1 | comp_asian == 1 | comp_hispanic == 1)
trial2 <- trial %>% 
  filter(officer_white == 1 | officer_black == 1 | officer_asian == 1 | 
           officer_hispanic == 1 )
# the trial works          
# View(trial2)
distinct(trial2, trial2$mos_ethnicity, .keep_all = F)
distinct(trial2, trial2$complainant_ethnicity, .keep_all = F)


# View(goodData) # sub by year_recieved
time0509s <- subset(trial2, 
                   year_received == 2005 |
                     year_received == 2006 |
                     year_received == 2007 |
                     year_received == 2008 |
                     year_received == 2009 
)
# View(time0509)  # looks good
# FYI this is 2005 to 2009 subset

time1115s <- subset(trial2, 
                   year_received == 2011 |
                     year_received == 2012 |
                     year_received == 2013 |
                     year_received == 2014 |
                     year_received == 2015
)
# View(time1115) # looks good
# FYI this is 2011 to 2015 subset

##### Redo Log Duration Model with new subset time0509s & time1115s #####

### time0509 Duration with white as only base b/c im dumb and didnt do that originally

DLS1 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time0509s)
summary(DLS1)  # asian not sig

# adding officer gender
DLS2 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time0509s)
summary(DLS2) # asian and officer gender not sig

# see if same race has effect
DLS3 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time0509s)
summary(DLS3) # asian and same race not sig

# add substantiated=1 or not in 
DLS4 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time0509s)
summary(DLS4) # asian and substantiated not sig

### make a print out to show for 2005 to 2009 models comparison
screenreg(l = list(DLS1, DLS2, DLS3, DLS4), stars = c(0.001, 0.01, 0.05, 0.1 ))


### time1115s Durationl

DLS5 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time1115s)
summary(DLS5) # all sig

# adding officer gender
DLS6 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time1115s)
summary(DLS6) # officer gender not sig

# see if same race has effect
DLS7 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time1115s)
summary(DLS7) # same race not sig

# add substantiated=1 or not in 
DLS8 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time1115s)
summary(DLS8) # substantiated not sig

# Comparison of time1115 models
screenreg(l = list(DLS5, DLS6, DLS7, DLS8), stars = c(0.001, 0.01, 0.05, 0.1 ))

# Compare same models to there counterpart time0509 to time1115
screenreg(l = list(DLS1, DLS5), stars = c(0.001, 0.01, 0.05, 0.1 ), 
          custom.model.names = c("Pre 2010", "Post 2010") )
screenreg(l = list(DLS2, DLS6), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010"))
screenreg(l = list(DLS3, DLS7), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 
screenreg(l = list(DLS4, DLS8), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 

#### adding all groups we think we should control for with durationl

# time0509s
DLS9 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
            + officer_black + officer_gender + mos_age_incident + 
            same_race  + force , data = time0509s)
summary(DLS9)  # asian, officer gender not sig

# time1115s
DLS10 <- lm(durationl ~ comp_hispanic + comp_asian + comp_black 
           + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
             + officer_black + officer_gender + mos_age_incident + 
             same_race  + force , data = time1115s)
summary(DLS10) # officer hispanic, officer black, officer gender, same race not sig

# make a print out
screenreg(l = list(DLS9, DLS10), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 

##### Redo Duration Models with new subset time0509s & time1115s #####

#### use time0509s, Regs with only White =  Base, Black, Hispanic, Asian & split by time

### Duration 

## Using time0509s & white as base dummy 

## comp gender male = 1, female = 0

DDS1 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time0509s)
summary(DDS1) # asian isnt sig

# adding officer gender
DDS2 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time0509s)
summary(DDS2) # Asian, officer gender isnt sig

# see if same race has effect
DDS3 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time0509s)
summary(DDS3) # asian, same race has no effect

# add substantiated=1 or not in 
DDS4 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time0509s)
summary(DDS4) # asian, substantiated has no effect

### make a print out to show for 2005 to 2009
screenreg(l = list(DDS1, DDS2, DDS3, DDS4), stars = c(0.001, 0.01, 0.05, 0.1 ))



#### Duration for 2011 to 2015, time1115s

DDS5 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + force , data = time1115s)
summary(DDS5) # all sig

# adding officer gender
DDS6 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + officer_gender + force , data = time1115s)
summary(DDS6) # officer gender not sig

# see if same race has effect
DDS7 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + same_race + force , data = time1115s)
summary(DDS7) # race is not sig

# add substantiated=1 or not in 
DDS8 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
          + comp_gender + substantiated + force , data = time1115s)
summary(DDS8) # sub has no sig

### Make a print out 2011 to 2015
screenreg(l = list(DDS5, DDS6, DDS7, DDS8), stars = c(0.001, 0.01, 0.05, 0.1 ))

# Compare same models to there counterpart time0509s to time1115s
screenreg(l = list(DDS1, DDS5), stars = c(0.001, 0.01, 0.05, 0.1 ), 
          custom.model.names = c("Pre 2010", "Post 2010") )
screenreg(l = list(DDS2, DDS6), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010"))
screenreg(l = list(DDS3, DDS7), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 
screenreg(l = list(DDS4, DDS8), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 

# time0509s
DDS9 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
           + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
             + officer_black + officer_gender + mos_age_incident + 
             same_race  + force , data = time0509s)
summary(DDS9)  # asian, officer gender not sig

# time1115s
DDS10 <- lm(duration ~ comp_hispanic + comp_asian + comp_black 
            + comp_gender + complainant_age_incident + officer_hispanic + officer_asian + 
              + officer_black + officer_gender + mos_age_incident + 
              same_race  + force , data = time1115s)
summary(DDS10) # officer hispanic, officer black, officer gender, same race not sig

# make a print out
screenreg(l = list(DDS9, DDS10), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010"))

##### Redo Substantiated Models with new subset time0509 & time 1115s - Ignore #####

#### Regs with only White =  Base, Black, Hispanic, Asian & split by time

### Substantiated with time0509s

## Using time0509s & white as base dummy 

## comp gender male = 1, female = 0

RRS1 <- lm(substantiated ~ year_closed + same_race + comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            force, data = time0509s)
summary(RRS1) # same race/ hispanic and asian are not sig, keep dummies though

# take out same race/ Add duration
RRS2 <- lm(substantiated ~ year_closed +  comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            duration + force, data = time0509s)
summary(RRS2) # duration, asian, hispanic not sig

# add officer age to RR1
RRS3 <- lm(substantiated ~ year_closed + same_race + comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            mos_age_incident + force, data = time0509s)
summary(RRS3) # same race, asian, and hispanic not sig, black sig at 10%

### make a print out to show for 2005 to 2009 models comparison
screenreg(l = list(RRS1, RRS2, RRS3), stars = c(0.001, 0.01, 0.05, 0.1 ))


#### Substantiated with time1115s

RRS4 <- lm(substantiated ~ year_closed + same_race + comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            force, data = time1115s)
summary(RR4) 

# take out same race/ Add duration
RRS5 <- lm(substantiated ~ year_closed +  comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            duration + force, data = time1115s)
summary(RRS5) 

# add officer age
RRS6 <- lm(substantiated ~ year_closed + same_race + comp_gender +
            officer_gender + comp_asian + comp_black + comp_hispanic +
            mos_age_incident + force, data = time1115s)
summary(RRS6) 

### make a print out to show for 2005 to 2009 models comparison
screenreg(l = list(RRS4, RRS5, RRS6), stars = c(0.001, 0.01, 0.05, 0.1 ))

# Compare same models to there counterpart time0509 to time1115
screenreg(l = list(RRS1, RRS4), stars = c(0.001, 0.01, 0.05, 0.1 ), 
          custom.model.names = c("Pre 2010", "Post 2010") )
screenreg(l = list(RRS2, RRS5), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010"))
screenreg(l = list(RRS3, RRS6), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 

### Control for all relevant Vars. at time0509s

RRS7 <-  lm(substantiated ~ comp_asian + comp_black + comp_hispanic +
              comp_gender + officer_asian + officer_black + officer_hispanic +
               officer_gender + same_race + duration + force , data = time0509s)
summary(RRS7) # comp asian, hispanic and officer black and same race, duration not sig

# now time1115s 

RRS8 <-  lm(substantiated ~ comp_asian + comp_black + comp_hispanic +
              comp_gender + officer_asian + officer_black + officer_hispanic +
              officer_gender + same_race + duration + force , data = time1115s)
summary(RRS8) # all comp race and officer race + same race and duration not sig

# make a print out
screenreg(l = list(RRS7, RRS8), stars = c(0.001, 0.01, 0.05, 0.1 ),
          custom.model.names = c("Pre 2010", "Post 2010")) 

##### nothing worthwhile in the sub models - still report fail to accept the null for sub models #####
##### Running VIF on Models for Colinearity #####

# Car package has VIF for models 

library("car")

### Check log duration models

vif(DLS1)
vif(DLS2)
vif(DLS3)
vif(DLS4)
vif(DLS5)
vif(DLS6)
vif(DLS7)
vif(DLS8)
vif(DLS9)
vif(DLS10)

### Check for duration models

vif(DDS1)
vif(DDS2)
vif(DDS3)
vif(DDS4)
vif(DDS5)
vif(DDS6)
vif(DDS7)
vif(DDS8)

### Check for Substantiated models

vif(RRS1)
vif(RRS2)
vif(RRS3)
vif(RRS4)
vif(RRS5)
vif(RRS6)
vif(RRS7)
vif(RRS8)

##### Run ANOVA test on duation log, duration and sub. #####

Anova(DLS9, DLS10) # duration log
Anova(DDS9, DDS10 ) # duration
Anova(RRS8, RRS7) # sub 
