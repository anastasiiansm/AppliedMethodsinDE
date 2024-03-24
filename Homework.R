# simulate x variable

ssize <- 1000
x1 <- rnorm (n = ssize, sd =3)
x2 <- rnorm (n = ssize, sd =5)
y <- 2 + 3*x1 + 5 * x2 +rnorm(n = ssize, sd=5)

out.y.full <- lm(y ~ x1 + x2)
out.y.x1.om <-lm (y ~ x1)
out.y.x2.om <- lm (y ~ x2)

cor.test(x=x1, y = x2)

#output
library(stargazer)
stargazer(out.y.full, out.y.x1.om, out.y.x2.om, type = 'text', omit.stat = c('f', 'ser'), no.space = T )

#we can check that B1 not equal B1

#-- Simulate x variables
ssize <- 1000
x1 <- rnorm( n = ssize , sd = 3 )
x2 <- rnorm( n = ssize , mean = x1, sd = 5 )
y <- 2 + 3*x1 + 5 * x2 + rnorm(n = ssize, sd = 5)
out.y.full <- lm( y ~ x1 + x2)
out.y.x1.om <- lm( y ~ x1)
out.y.x2.om <- lm( y ~ x2 )
cor.test(x = x1, y = x2)

#-- Output
stargazer(out.y.full, out.y.x1.om, out.y.x2.om,
          type = 'text', omit.stat = c('f','ser'), no.space =T)

#We can check that β1 = +~ β1 ^ β2 ^ δ̃

y <- 2 + 3*x1 + 5 * x2 + rnorm(n = 1000, sd = 5)
out.y.full <- lm( y ~ x1 + x2)
out.y.incomp.x1 <- lm( y ~ x1 )
out.y.incomp.x2 <- lm( y ~ x2 )
out.x1.parti <- lm( x1 ~ x2 )
out.x2.parti <- lm( x2 ~ x1 )
stargazer(
  out.x1.parti,
  out.x2.parti,
  out.y.full,
  out.y.incomp.x1,
  out.y.incomp.x2,
  type = 'text', omit.stat = c('f','ser'))

coef(out.y.full)['x1'] + coef(out.y.full) ['x2']*coef(out.x2.parti)['x1']

coef(out.y.full)['x2'] + coef(out.y.full) ['x1']*coef(out.x1.parti)['x2']

#As a side note, lets think about the partialling out interpretation of regression

#-- Simulate x variables
ssize <- 1000
x1 <- rnorm( n = ssize , sd = 3 )
x2 <- rnorm( n = ssize , mean = x1 ,sd = 3 )
y <- 2 + 3*x1 + 5 * x2 + rnorm(n = ssize, sd = 5)
out.y.full <- lm( y ~ x1 + x2)
out.parti.x2 <- lm( x1 ~ x2 )
out.y.x1 <- lm( y ~ residuals(out.parti.x2))
out.y.x1.om <- lm( y ~ x1)
#-----------------
#-- Unify Output
stargazer(
  out.y.full,
  out.parti.x2,
  out.y.x1.om,
  out.y.x1,
  type = 'text',
  omit.stat = c('f','ser'), no.space =T)
##


#the task 3

ssize <- 1000
x1 <- rnorm(n = ssize, sd = 3)
x2 <- rnorm(n = ssize, mean = 1.33 * x1, sd = 5)
y <- 2 + 3 * x1 + 5 * x2 + rnorm(n = ssize, sd = 5)


out.y.full <- lm(y ~ x1 + x2)
out.y.x1.om <- lm(y ~ x1)
out.y.x2.om <- lm(y ~ x2)

cor.test(x = x1, y = x2)

# Output
library(stargazer)
stargazer(out.y.full, out.y.x1.om, out.y.x2.om, type = 'text', omit.stat = c('f', 'ser'), no.space = TRUE)

##c. Instead of omitting x2, try omitting x1. Can you estimate x2 without bias?
out.y.x2.om <- lm(y ~ x2)
summary(out.y.x2.om)
