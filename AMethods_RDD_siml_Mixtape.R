library(dplyr)
library(ggplot2)

# simulate the data
dat <- tibble(
  x = rnorm(1000, 50, 25)
) %>%
  mutate(
    x = if_else(x < 0, 0, x)
  ) %>%
  filter(x < 100)

# cutoff at x = 50
dat <- dat %>% 
  mutate(
    D  = if_else(x > 50, 1, 0),
    y1 = 25 + 0 * D + 1.5 * x + rnorm(n(), 0, 20)
  )

ggplot(aes(x, y1, colour = factor(D)), data = dat) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 50, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y1)")

Sys.sleep(2)

## Now lets simulate an Effect on y2:

Sys.sleep(2)

# cutoff at x = 50
dat2 <- dat %>% 
  mutate(
    D  = if_else(x > 50, 1, 0),
    y2 = 25 + 45 * D + 1.5 * x + rnorm(n(), 0, 20)
  )

ggplot(aes(x, y2, colour = factor(D)), data = dat2) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 50, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y2 = "Potential Outcome (Y1)")

# now you can proceed to estimate the effect.


model_y1 <- lm(y1 ~ x + D, data = dat)
summary(model_y1)

model_y2 <- lm(y2 ~ x + D, data = dat2)
summary(model_y2)

#For y1, there is no significant treatment effect observed at the cutoff point.
#For y2, there is a significant treatment effect observed at the cutoff point, with an estimated increase of approximately  46.3 for individuals just above the cutoff compared to individuals just below the cutoff.
