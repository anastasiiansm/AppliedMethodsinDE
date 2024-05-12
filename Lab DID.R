#Lab on DID

setwd("D:/Nova Sbe/PHD course/Applied Methods/week 7")
require(data.table) # you can use library or require
require(ggplot2) # you can use library or require
require(stargazer) # you can use library or require
load("fastfood3.RData")
load("fastfood4.RData")
load("fastfood.RData")

#explore the data
head(dt.fastfood)

#plot it
plot1 <- ggplot( data = dt.fastfood, aes(x = wage))
plot1 + geom_density() + facet_wrap( ~ state + time) + theme_bw()

#change in employment
qplot( data = dt.fastfood, x = factor(time), y = emptot
       , fill = factor(state)
       , geom = 'boxplot') + theme_bw() + xlab("time") + ylab("FTE Employment")

#means of key variables
dt.bf.aft <- data.table(dt.fastfood) # Create a new table called dt.bf.aft
dt.bf.aft <- dt.bf.aft[, list( # Create a list of the columns of your new table
  mean_emptot = mean(emptot , na.rm=TRUE)
  , mean_wage = mean(wage , na.rm=TRUE)
  , mean_pmeal = mean(pmeal , na.rm=TRUE)
  , mean_hrsopen = mean(hrsopen , na.rm=TRUE)
), by=list(state, time)] # Specifiy the list of grouping variables
dt.bf.aft

#by using a clean data
dt.bf.aft.clean <- dt.fastfood[!is.na(wage),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(pmeal),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(emptot),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(hrsopen),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(emptot),]
dt.bf.aft.clean <- data.table(dt.fastfood.clean)
dt.bf.aft.clean <- dt.bf.aft.clean[, list(
  mean_emptot = mean(emptot , na.rm=TRUE)
  , mean_wage = mean(wage , na.rm=TRUE)
  , mean_pmeal = mean(pmeal , na.rm=TRUE)
  , mean_hrsopen = mean(hrsopen , na.rm=TRUE)
), by=list(state, time)]
dt.bf.aft.clean

#Difference in FTE employment between NJ and PA at T0.
t.test( dt.fastfood.clean[state==0 & time==0, emptot]
        , dt.fastfood.clean[state==1 & time==0, emptot])

#Difference in FTE employment between NJ and PA at T1.
t.test( dt.fastfood.clean[state==0 & time==1, emptot]
        , dt.fastfood.clean[state==1 & time==1, emptot])


#Differences in Differences
Treatment_effect <- (21.02743 - 20.44557) - (21.16558 - 23.33117)
Treatment_effect

Cleaned_data_TE <- (20.71293-20.51397) - (21.50000-23.62687)
Cleaned_data_TE

#Effect on employment though regression
lm1 <- lm( emptot ~ time + state + time*state, data = dt.fastfood.clean)
stargazer(lm1, type = "text")

coeffs <- coefficients(lm1)
coeffs

#The coefficient for state (-3.113) is statistically significat
#The coefficient for time (-2.127) and time state (2.326) are not statistically significant

#emptot00: The baseline level of average FTE employment at T1 in PA.
#emptot01: The average FTE employment at T1 in NJ, accounting for the baseline difference between NJ and PA.
#emptot10: The average FTE employment at T2 in PA, accounting for the change over time within PA.
#emptot11: The average FTE employment at T2 in NJ, accounting for both the change over time within PA and the differential change between NJ and PA over time.

#correspondence between the betas and the values from the table
dt.bf.aft.clean

B0 <- 21.50000 - 23.62687
B0

#(β2) = emptot01 - emptot00  
B2 <- 21.50000 - 23.62687
B2

#(β3) = (emptot11 - emptot10)-(emptot01 - emptot00) =
B3 <- (20.71293 - 20.51397) - (21.50000 - 23.62687)
B3

#Add controls for chain and ownership
lm <- lm( emptot ~ time + state + time*state + factor(chain) + co_owned
          , data = dt.fastfood.clean)
stargazer(lm, type = "text")

#Change in meal prices
qplot( data = dt.fastfood, x = factor(time), y = pmeal,
       fill = factor(state),
       geom = 'boxplot') + theme_bw() + xlab("time") + ylab("Meal Price")

#effect on meal prices
lm <- lm( pmeal ~ time + state + time*state
          , data = dt.fastfood.clean)
stargazer(lm, type = "text")

#Change in number of hours of operation
qplot( data = dt.fastfood, x = factor(time), y = hrsopen
       , fill = factor(state)
       , geom = 'boxplot') + theme_bw() + xlab("time") + ylab("Hours of Operation")

#Effect on hours open
lm <- lm( hrsopen ~ time + state + time*state
          , data = dt.fastfood.clean)
stargazer(lm, type = "text")

#Effect on the fraction of full-time employees
lm <- lm( fracft ~ time + state + time*state, data = dt.fastfood.clean)
stargazer(lm, type = "text")


summary(dt.fastfood.clean$gap)
lm <- lm( emptot ~ gap * time , data = dt.fastfood.clean)
stargazer(lm, type = "text")
