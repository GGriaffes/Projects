# Grant Davis - Non Profit - Data Analysis - New one

rm(list=ls())
setwd("~/R/R Data")
options("scipen"=99999, digits=3)

# install.packages("dplyr")
# install.packages("readxl")

##### Part 1 Outside #####
library("readxl")
library("ggplot2")
library("dplyr")

npt <- read_excel("OhioNonprofitCompensation-grant.xlsx", sheet = 1)

# new charity, 4 Paws dogs 
# Start Filtering using the stringr - rebus packages
# Rebus - START , END , ANY_CHAR , %R% , use OR for | instead

# install.packages('stringr')
library("stringr")
# install.packages("rebus")
library("rebus")

# Filter by nteedescription first
# Code = P80
# Services to Promote the Independence of Specific Populations

nd <- subset(npt, nteecode == "P80")
View(nd)

nd1 <- subset(nd, title == "Executive Director" | 
                title == "EXECUTIVE DIRECTOR" | 
                title == "CEO" |
                title == "CHIEF EXECUTIVE OFFICER" |
                title == "Co-executive director" |
                title == "CO-EXECUTIVE DIRECTOR" |
                title == "EXEC DIRECTO" |
                title == "EXEC DIRECTOR" |
                title == "EXECUTIVE DI" |
                title == "Executive Dir." |
                title == "EXECUTIVE DIR." |
                title == "EXECUTIVE DIRECTOR & CEO" |
                title == "Chief Esecutive Officer" |
                title == "Chief executive officer" |
                title == "DIRECTOR, PRESIDENT, CEO" |
                title == "PRESIDENT & CEO MERCY HEALTH" |
                title == "President & CEO" |
                title == "President/CEO" |
                title == "PRESIDENT/CEO" |
                title == "President & CEO; Trustee" |
                title == "President/CEO/Trustee" |
                title == "PRESIDENT AND CEO" |
                title == "PRESIDENT/ CEO" |
                title == "PRESIDENT CEO" ) 
 # View(nd1)

# 4 PAWS comp - $113,941 - so lets look at the data now

ggplot(nd1, aes(revenue, totalcompensation)) +
  geom_point()
# 4 PAWS revenue = $4,776,398

nd2 <- subset(nd1, totalcompensation <= 270000 & revenue < 7000000 & 
                personid != "IqIe_dBApkSZySnAweq9cA" & 
                totalcompensation > 20000 &
                revenue > 350000)

ggplot(nd2, aes(revenue, totalcompensation)) +
  geom_point()

 # View(nd2)
# nd2 is 23 obs. 

# find percentile 
percentiles <- quantile(nd2$totalcompensation,
                        c(.10, .25, .50, .75, .90))
percentiles

# find more stats
# find age of foundations
  nd2 <- transform(nd2, age = (2020-formationyr)) 
  #View(nd2)
  
# Mean, median, max, min 
# total compensation, revenue, assets, age 
  # arent right b/c changed one point for ref

  mean(nd2$totalcompensation) # $83,523
  mean(nd2$revenue) # $1,717,178
  mean(nd2$assets) # $3,320,355
  mean(nd2$age) # 35.2
  
  median(nd2$totalcompensation) # $75,426
  median(nd2$revenue) # $763,539
  median(nd2$assets) # $1,361,738
  median(nd2$age) # 36
  
  max(nd2$totalcompensation) # $296,700
  max(nd2$revenue) # $64,77,972
  max(nd2$assets) # $22,058,551
  max(nd2$age) # 70
  
  min(nd2$totalcompensation) # $32,830
  min(nd2$revenue) # $388,389
  min(nd2$assets) # $95,455
  min(nd2$age) # 8
  
# find the specific  total compensation percentile of 4 PAWS 
# $113,941
percentiles
paws <- quantile(nd2$totalcompensation, c(.77, .78, .79,
                                          .80, .81, .82, 
                                          .83, .84, .85,
                                          .86, .87, .88))
  paws # 80th percentile 
  
# make some graphs for interpretation, ablines, dots too
  #install.packages("gghighlight")
  library("gghighlight")
  
plot1 <- ggplot(nd2, aes(x = revenue, y = totalcompensation)) +
    geom_point(position = "jitter", alpha = .5, size = 5) +
    geom_smooth(method = 'lm', se = T, col = "black") +
    gghighlight(revenue == 4776398,
                unhighlighted_colour = "blue"
                ) +
    ylab("Total Compensation") +
    xlab("Revenue") +
    ggtitle("Total Compensation VS Revenue Best Fit Line")
  
  plot1
  
  
plot2 <- ggplot(nd2, aes(x = revenue, y = totalcompensation, col = name)) +
    geom_jitter(alpha = .5) +
    ylab("Total Compensation") +
    xlab("Revenue") +
    ggtitle("Total Compensation VS Revenue")
  
  plot2
  # plot2 is meh dont use it, plot1 is nice

print(nd2$name)
print(nd2$totalcompensation)
  

paste <- data.frame(nd2$name, nd2$totalcompensation,
                    nd2$revenue, nd2$assets, nd2$age)
paste
# View(paste)

#look at some regs 
  # all char. first
R1 <- lm(totalcompensation ~ revenue + assets + age, data = nd2)
library("jtools")
summ(R1)

  # reg no age now
R2 <- lm(totalcompensation ~ revenue + assets, data = nd2)
summ(R2)
  
  # maybe try and use all data points even those we filtered out, no age
R3 <- lm( totalcompensation ~ revenue + assets, data = nd1)
summ(R3)

  # just comp vs revenue now nd2 data
R4 <- lm(totalcompensation ~ revenue, data = nd2)
summ(R4)

# Nothing is productive, regs arent good. sad days

# plot the full nd1 dataset

t1 <- ggplot(nd1, aes(revenue, totalcompensation)) +
  geom_point(position = "jitter", alpha = .6) +
  geom_smooth(method = 'lm', se = T, col = "blue")
  t1

trial <- subset(nd1, revenue <= 20000000)

t2 <- ggplot(trial, aes(revenue, totalcompensation)) +
  geom_point(position = "jitter", alpha = .45, size = 4, colour = "black") +
  geom_smooth(method = 'lm', se = T, col = "blue")
t2 # 4 paws looks comparable to these, i think its about right 
t2 + theme(plot.title = element_text(hjust=0.5))
t2 + theme_classic()

plot1 + theme(plot.title = element_text(hjust=0.5))

# this is the age vs comp
plot3 <- ggplot(nd2, aes(x = age, y = totalcompensation)) +
  geom_point(position = "jitter", alpha = .6) +
  geom_smooth(method = 'lm', se = T, col = "blue")
plot3

# do the total dataset w/ revenue vs total compensation

ld <- ggplot(trial, aes(x = revenue, y = totalcompensation)) +
  geom_point(position = "jitter", alpha = .5, size = 5) +
  geom_smooth(method = 'lm', se = T, col = "black") +
  gghighlight(revenue == 4776398,
              unhighlighted_colour = "blue") +
  ylab("Total Compensation") +
  xlab("Revenue") +
  ggtitle("Total Compensation VS Revenue Best Fit Line, Full Dataset")
ld
ld + theme(plot.title = element_text(hjust=0.5))

##### Part 2 Inside #####
### Use nd2 for regs
## Total Revenue (totrevenue) and Fundraising Efficiency (defined as grsincfndrsng / lessdirfndrsng) 
## or the amount of money raised per dollar spent on fundraising

dta <- read_excel("Copy of Ohio Nonprofit Data 9-21-2020.xlsx",
                  sheet = "Outside 2")

dta2 <-  merge(nd2, dta)
View(dta2) # sweet it works

T1 <- ggplot(dta2, aes(y = percentchange, x = totalcompensation)) +
  geom_point(position = "jitter")
T1

nd3 <- cbind(nd2$orgname, nd2$totalcompensation, nd2$revenue, nd2$assets, nd2$age)
View(dta4)
dta4 <- cbind(nd2$orgname, nd2$totalcompensation, nd2$revenue, nd2$assets, nd2$age, dta2$percentchange * 100)
