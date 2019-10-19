#### 4. Diagnostics (Demonstration)  ####

## make a dataset of the residuals and expected values
## to do this, we use the
## fitted() function for expected values
## resid() function for model residuals
## NOTE: these two functions work on linear regression models too
d.residuals <- data.table(
  Yhat = fitted(m2lmm),
  Residuals = resid(m2lmm))

## check for normality of the outcome
## by examining residuals
ggplot(d.residuals, aes(Residuals)) +
  geom_histogram()

## check for normality of the outcome
## using QQ plot
ggplot(d.residuals, aes(sample = Residuals)) +
  stat_qq() + stat_qq_line()

## check for homogeneity of variance assumption
ggplot(d.residuals, aes(Yhat, Residuals)) +
  geom_point(alpha = .2)


## make a dataset of the random effects by UserID
d.random <- as.data.table(coef(m1lmm)$UserID)

## check whether the random effects are normally distributed
## note that these have mean 0
ggplot(d.random, aes(`(Intercept)`)) +
  geom_histogram()

## normality via QQ plot
ggplot(d.random, aes(sample = `(Intercept)`)) +
  stat_qq() + stat_qq_line()


#### 5. Inference (Demonstration)  ####

## fixed effects table
fetable <- coef(summary(m2lmm))
## view the table
print(fetable)

## extract the t values (b / se)
## take their absolute value (as we typically do 2 sided hypothesis tests
## calculate the p-value using the normal distribution function, pnorm()
## and multiply by 2 so its a two-tailed test
pnorm(abs(fetable[, "t value"]), lower.tail = FALSE) * 2

## confidence intervals using the Wald method are based
## on assuming a normal distribution and are very fast and easy
## but do not give any confidence intervals for the random effects
confint(m1lmm, method = "Wald")

## confidence intervals using the profile method are based
## on the change in model performance (the log likelihood)
## and are much slower, but generally a bit more precise and
## are appropriate for random effects
confint(m1lmm, method = "profile")


## we can compare models using likelihood ratio tests
## which are OK (not dissimilar from assuming normal) for fixed effects
## and are about the best that can be done for random effects
## note that you always need at least one random effect in LMMs
## so you cannot simply remove the random intercept; generally this is
## used for testing more complex random effects we will learn in future weeks
## or testing several differences at once

## setup a new model with an additional predictor, SurveyInteger
lmmalt <- lmer(PosAff ~ 1 + SurveyInteger + (1 | UserID), d)

## conduct a likelihood ratio test of these two, nested models
anova(lmmalt, m1lmm, test = "LRT")

## assuming normal distribution, test the fixed effects in
## our alternate model, lmmalt
fetablealt <- coef(summary(lmmalt))
pnorm(abs(fetablealt[, "t value"]), lower.tail = FALSE) * 2
