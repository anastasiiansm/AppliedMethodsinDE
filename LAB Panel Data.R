library(data.table)
library(ggplot2)
library(stargazer)
library(plm)
library(lmtest)
setwd("D:/Nova Sbe/PHD course/Applied Methods/week 7")

load("rental.RData")
head(dt.rental)

#What is the unit of analysis? City.
#each row represent the data for a specific city

#What are the time periods?
# 1980 is 80 and 1990 is 90

#How many periods?
#there are 2 periods 1980 and 1990

#Is there a treatment? No.

#What are the key variables of interest?
#rental rates (lrent) and percentage of students (pctstu)

#Dependent variable (y)? Rent or log(rent). Which one will we use? lrent for % effects.
#natural logorithm of rental rates

#What is the independent variable of interest? pctstu.
#proportion of students in each city

#What factors should we control for?
#pop is population, average income (avginc)

#Estimate the equation by pooled OLS
out.ols <- plm( lrent ~ pctstu + lpop + lavginc + y90
                , c = index("city", "year")
                , model = "pooling"
                , data=dt.rental)
stargazer(out.ols, type = "text")

#serial correlation
u <- residuals(out.ols)
length(u)
nrow(dt.rental)

dt.rental[,'u'] <- u
head(dt.rental)

dt.rental$L1_u <- c(NA, dt.rental$u[-nrow(dt.rental)])
summary(dt.rental$L1_u)

out.u <- plm( u ~ L1_u, data=dt.rental, model='pooling')
stargazer(out.u, type="text")

#Can these ai also bias our beta estimates?
#Yes, unobserved time-invariant factors (ai) can bias the beta estimates if they are correlated with both the independent variable of interest (percentage of students) and the dependent variable (rental rates)

#What are the consequences of serial correlation to our estimate?
#Serial correlation can lead to biased and inefficient estimates of the regression coefficients

#What model should we use?
#Panel data models or fixed effects models


#estimate the model by first-differences
pdt.rental <- pdata.frame( dt.rental, index = c("city", "year"))
pdt.rental$d2 <- as.double(pdt.rental$year==90)
out.fd <- plm( lrent ~ pctstu + lpop + lavginc,
               , model = "fd"
               , data=pdt.rental)
stargazer(out.fd, type = "text")

#We can test for the presence of heteroskedasticity using the bptest:
bptest(out.fd)

stargazer(coeftest(out.fd, vcov. = vcovHC(out.fd, method = c("arellano"))), type = "text")

#Estimate the model by fixed effect
out.fe <- plm( lrent ~ 0 + pctstu + lpop + lavginc + y90
               , c = index("city", "year")
               , model = "within"
               , data=dt.rental)
stargazer(out.fe, type = "text")
