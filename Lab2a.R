#Lab 2a

#Example 1
#BankX plans to launch a new financial product. A sample of 25 potential investors, collected the following
#information on the amount P they wish to invest in the new product (normally distributed, with variance 500):
#  i xi = 1000 and P i(xi − ¯x)2 = 9600 use α = 0.05

alpha = .05 # significance level
z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha) # critical values

xbar = 1000/25 # sample mean
mu0 = 45 # hypothesized value
sigma = sqrt(500) # population standard deviation
n = 25 # sample size
z = (xbar-mu0)/(sigma/sqrt(n)) # test statistic
z

pnorm(z, lower.tail=FALSE) # upper tail
pnorm(z, lower.tail=TRUE) # lower tail

pval = 2 * pnorm(z) # lower tail
pval # two-tailed p-value

#Hypothesis Testing for a Population Mean with unknown variance
t.alpha = .05 # significance level
t.half.alpha = qt(1-alpha/2, 25-1)
c(-t.half.alpha, t.half.alpha) # critical values

xbar = 1000/25 # sample mean
mu0 = 45 # hypothesized value
s = sqrt(400) # sample standard deviation
n = 25 # sample size
t = (xbar-mu0)/(s/sqrt(n)) # test statistic
t

pt(t, df=25-1, lower.tail=FALSE) # upper tail
pt(t, df=25-1, lower.tail=TRUE) # lower tail
pval = 2 * pt(t, df=25-1) # lower tail
pval # two-tailed p-value

#Hypothesis Testing for a Population Variance
q.alpha = .05 # significance level
q.half.alpha.up = qchisq(1-alpha/2, 25-1) # critical values
q.half.alpha.up
q.half.alpha.low = qchisq(alpha/2, 25-1) # critical values
q.half.alpha.low
sigma_sqr_0 = 500 # hypothesized value
s_sqr = 400 # sample standard deviation
n = 25 # sample size
Q = ((n-1)*s_sqr)/sigma_sqr_0 # test statistic
Q

#Example 2
#A particular type of cancer therapy has a 60% success rate. A group of researchers developed a new type of
#treatment and its effectiveness is to be tested. In 61 cases, 47 were successfully treated.

#Hypothesis Testing for a Proportion
alpha = .05 # significance level
z.alpha = qnorm(1-alpha)
z.alpha # critical value
p0 = 0.60
fn = 47/61 # sample proportion
n = 61 # sample size
z = (fn-p0)/sqrt((0.6*(1-0.6))/n) # test statistic
z

#Example 3
#Hypothesis Testing for a Difference in Population Means - Independent Samples and Variance
#Unknown

#Is there a difference between average dividends of the stocks in Dow Jones and the ones in Eurostoxx, knowing
#they have equal variances and normally distributed? The data of two independent samples is the following,
#use α = 0.05.

nX = 21
nY = 25
sX = 1.30
sY = 1.16
Sp_sqr = ((nX - 1)*(sX^2) + (nY-1)*(sY^2))/(nX + nY - 2)
Sp_sqr
df = nX + nY - 2
df
alpha = .05 # significance level
t.half.alpha = qt(1-alpha/2, 44)
c(-t.half.alpha, t.half.alpha) # critical values
xbar = 3.27
ybar = 2.53
t = ((xbar-ybar)-0)/sqrt((Sp_sqr/nX)+(Sp_sqr/nY))
t
pt(t, df=44, lower.tail=FALSE) # upper tail
pt(t, df=44, lower.tail=TRUE) # lower tail
pval = 2 * pt(t, df=44, lower.tail=FALSE)
pval

#Example 4
library(data.table)
setwd("D:/Nova Sbe/PHD course/Applied Methods/Week 2")
dt.stocks <- data.table(read.csv("data_r.csv"))
dt.stocks <- setnames(dt.stocks, tolower(names(dt.stocks)))
head(dt.stocks)                        

#Calculate the 95% confidence interval for the stocks’ means
xbar <- dt.stocks[, mean(idjcomp, na.rm=TRUE)]
s <- dt.stocks[, sd(idjcomp, na.rm=TRUE)]
n <- dt.stocks[, length(which(!is.na(idjcomp)))]
error <- qnorm(0.975)*s/sqrt(n)
left <- xbar-error
right <- xbar+error
left
right

#Lets look at some examples using our stock data. For instance, if we wanted to check whether the mean of
#S&P500 is equal to zero we would write
dt.stocks[, t.test(isp500)]
dt.stocks[, t.test(isp500, alternative = c("greater"), mu=0.5, conf.level = 0.99)]


#Inference for difference of population means - paired samples
#Paired t-test: t.test(y1,y2,paired=TRUE) where y1 & y2 are numeric.
dt.stocks[, t.test(idjcomp, inasdaq, paired=TRUE)]

#Inference for difference of population means - independent samples
#Impact of crisis on stock indices
#Independent 2-group t-test: t.test(y~x) where y is numeric and x is a binary factor.
#Create an indicator variable that takes the value of 1 if it is post-2008 (year of the economic crisis).
dt.stocks[, postcrisis:=ifelse(year>2008,1,0)]
dt.stocks[postcrisis==0, mean(idjcomp, na.rm=TRUE)]
dt.stocks[postcrisis==1, mean(idjcomp, na.rm=TRUE)]
dt.stocks[, t.test(idjcomp ~ postcrisis)]

#Independent 2-group t-test: t.test(y1,y2) where y1 and y2 are numeric
dt.stocks[, t.test(idjcomp, inasdaq, var.equal=TRUE)]
dt.stocks[, t.test(idjcomp, inasdaq, var.equal=FALSE)]
