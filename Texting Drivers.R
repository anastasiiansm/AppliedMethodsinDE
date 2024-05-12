#Texting Drivers

library(data.table)
library(ggplot2)
library(stargazer)
library(plm)
setwd("D:/Nova Sbe/PHD course/Applied Methods/week 9")
load("txtbans.RData")

#Number of states
length(unique(dt.txtbans$state))

#number of treated and control states
dt.txtbans[, list(n_states=length(unique(state))), by=treated]

#number of month
length(unique(dt.txtbans$time))

#number of month in the before and after period
dt.txtbans[, list(n_months=length(unique(time))), by=after]

#is panel balanced?
dt.txtbans[, list(.N), by=list(after, time)]

#My regression model
#B0+B1*treated+B2*after+B2*treated*after

#Table with the means
dt.means <- data.table(dt.txtbans)
dt.means <- dt.means[, list( mean_laccsvobyinhab = mean(laccsvobyinhab)
                             , mean_accsvobyinhab = mean(accsvobyinhab)
), by=list(treated, after)]
dt.means

#Dif-in-Dif estimator

dif_in_dif <- (0.002446106 - 0.003082875) - (0.003321791 - 0.004350064)

#simple dif in dif regression model
ddl <- lm( laccsvobyinhab ~ after*treated, data = dt.txtbans)
coefficients(ddl)[4]

#interpret coefficients
coefficients(ddl)[1]
coefficients(ddl)[2]
coefficients(ddl)[3]
coefficients(ddl)[4]

#will suggest common trend assumption

#assumptions
qplot( data = dt.txtbans[ after==0
                          , list(mean_laccsvobyinhab = mean(laccsvobyinhab)
                          ), by=list(treated, time)]
       , x = time
       , y = mean_laccsvobyinhab
       , geom = "line"
       , col=factor(treated)) +
  theme_bw()

#The plot shows that the changes in the outcome variable exhibited a similar trend in both groups during the 12 months preceding treatment

#Examples of violations of the assumtions
#No effect of the text ban
#Common trend

#Percentage of the states
#States with text ban
length(unique(dt.txtbans[treated==1, state]))

#States with call ban
length(unique(dt.txtbans[callban==1, state]))

#States with text and call ban
length(unique(dt.txtbans[txmsban==1 & callban==1, state]))

#In percentage with both bans
length(unique(dt.txtbans[txmsban==1 & callban==1, state]))/length(unique(dt.txtbans[txmsban==1, state]))

#Hard part
names(dt.txtbans)
summary( out <- plm( laccsvobyinhab ~ treated * factor(time), data = dt.txtbans[ after == 0], model = 'pooling'))


require(car)

#looked at solution
linearHypothesis(
  out
  ,c(
    'treated:factor(time)2 = 0',
    'treated:factor(time)3 = 0',
    'treated:factor(time)4 = 0',
    'treated:factor(time)5 = 0',
    'treated:factor(time)6 = 0',
    'treated:factor(time)7 = 0',
    'treated:factor(time)8 = 0',
    'treated:factor(time)9 = 0',
    'treated:factor(time)10 = 0',
    'treated:factor(time)11 = 0',
    'treated:factor(time)12 = 0')
)
         



