library(wooldridge)

# Q1
# level-log: test score = 12 + 8*log(income). What does the 8 mean?
# a 1% rise in x changes y by b1/100 units
b1 <- 8
b1 / 100
# Answer: a 1% rise in income -> rise of about 0.08 points in the predicted test score

# Q2
# log-level: log(y) = 1.4 + 0.04x. % change in y when x FALLS by 3 (a fall = negative dx)
b1 <- 0.04
dx <- -3
round(100 * b1 * dx, 2)

# Q3
# bwght log-log: % change in predicted birth weight when family income rises 20% (slope = elasticity)
reg <- lm(log(bwght) ~ log(faminc), data = bwght)
b <- reg$coefficients
pct_x <- 20
round(b[2] * pct_x, 2)

# Q4
# 1,000 samples, OLS unbiased (SLR1-SLR4): which statement is correct?
# Answer: the average of the 1,000 estimates is close to beta1   (unbiased: E[b1-hat] = beta1)

# Q5
# why prefer an unbiased estimator with a SMALLER Var(b1-hat)?
# Answer: its estimates land closer to beta1 from sample to sample, so any one estimate is more reliable

# Q6
# Var(b1-hat) = sigma^2 / SST_X. Which changes make Var(b1-hat) LARGER? (select all)
# QUIZ VERSION: flipped to which changes make Var(b1-hat) SMALLER -> pick the opposites
# Answer (LARGER): larger error variance sigma^2 / smaller sample (smaller SST_X) / X values bunched tightly (smaller SST_X)
# Answer (SMALLER): smaller error variance sigma^2 / X values more spread out (larger SST_X) / larger sample, if listed (larger SST_X)
# NEVER: anything about the true slope beta1 (it's not in the formula)

# Q7
# se(b1-hat) from n, sigma-hat, and the SD of X: SST_X = (n - 1) * Sx^2, se = sigma-hat / sqrt(SST_X)
n <- 8
sigmahat <- 5.9
Sx <- 2.1
SSTx <- (n - 1) * Sx^2          # if they give the VARIANCE of X instead, don't square it
round(sigmahat / sqrt(SSTx), 2)

# Q8
# se(b1-hat) from the X values and SSR: sigma-hat = sqrt(SSR / (n - 2)), SST_X = sum((x - mean(x))^2)
x <- c(2, 4, 5, 6, 12)
SSR <- 61
n <- length(x)
sigmahat <- sqrt(SSR / (n - 2))
SSTx <- sum((x - mean(x))^2)
round(sigmahat / sqrt(SSTx), 2)

# Q9
# sd(b1-hat) = 1.64. Sample 25 times as large (SST_X x 25) -> new sd = old sd / sqrt(25)
sd_old <- 1.64
k <- 25
round(sd_old / sqrt(k), 2)

# Q10
# ceosal1 salary ~ roe: se(b1-hat) by hand from sigma-hat and SST_X, then check against summary
reg <- lm(salary ~ roe, data = ceosal1)
n <- nobs(reg)
SSR <- sum(reg$residuals^2)
sigmahat <- sqrt(SSR / (n - 2))
SSTx <- sum((ceosal1$roe - mean(ceosal1$roe))^2)
round(sigmahat / sqrt(SSTx), 2)
summary(reg)$coefficients        # roe row, "Std. Error" column should match

# Q11
# wage = b0 + b1*educ + b2*exper. What does b1 measure?
# Answer: the predicted change in wage from one more year of education, HOLDING EXPERIENCE FIXED

# Q12
# predicted y-hat from y = 10 + 3.6*x1 + 0.9*x2 at x1 = 5, x2 = 1
b0 <- 10
b1 <- 3.6
b2 <- 0.9
x1 <- 5
x2 <- 1
round(b0 + b1 * x1 + b2 * x2, 2)

# Q13
# change in y-hat from y = 7 + 3.8*x1 - 0.7*x2 when x1 rises 6 and x2 rises 4 (intercept drops out; a fall = negative)
b1 <- 3.8
b2 <- -0.7
dx1 <- 6
dx2 <- 4
round(b1 * dx1 + b2 * dx2, 2)

# Q14
# wage1 wage ~ educ + exper + tenure: predicted wage at educ = 14, exper = 10, tenure = 5
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients            # order: b[1] intercept, b[2] educ, b[3] exper, b[4] tenure
round(b[1] + b[2] * 14 + b[3] * 10 + b[4] * 5, 2)

# Q15
# same regression: change in predicted wage when tenure rises 3 years, holding educ and exper fixed
# QUIZ VERSION: asks about EXPERIENCE (exper) rising, not tenure -> use b[3], not b[4]. Read which variable moves!
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients            # order: b[1] intercept, b[2] educ, b[3] exper, b[4] tenure
dtenure <- 3
round(b[4] * dtenure, 2)
dexper <- 3                      # quiz version: put the quiz's number of years here
round(b[3] * dexper, 2)

# Q16
# ceosal1 log(salary) ~ log(sales) + roe: % change in salary when roe rises 10 points (roe term is log-level)
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b <- reg$coefficients            # order: b[1] intercept, b[2] log(sales), b[3] roe
droe <- 10
round(100 * b[3] * droe, 2)

# Q17
# same regression: predicted salary (thousands) at sales = 5000, roe = 15 -> predict log(salary), then exp()
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b <- reg$coefficients            # order: b[1] intercept, b[2] log(sales), b[3] roe
round(exp(b[1] + b[2] * log(5000) + b[3] * 15), 2)

# Q18
# formula (3.23): simple slope = multiple slope + b2-hat * delta1 -> 1.28 + 1.47 * 0.56
b1_hat <- 1.28
b2_hat <- 1.47
delta1 <- 0.56
round(b1_hat + b2_hat * delta1, 2)

# Q19
# omitted-variable bias of the simple slope = beta2 * delta1 (negative = biased downward)
beta2 <- 1.97
delta1 <- -0.29
round(beta2 * delta1, 2)

# Q20
# omitted X2 lowers Y (beta2 < 0) and X2 is positively correlated with X1. Direction of bias?
# bias = beta2 * delta1 = (negative) * (positive) = negative
# Answer: biased downward

# Q21
# wage1: b2-hat (exper, from wage ~ educ + exper) times delta1 (educ slope from exper ~ educ)
reg_long <- lm(wage ~ educ + exper, data = wage1)
reg_aux <- lm(exper ~ educ, data = wage1)
b2_hat <- reg_long$coefficients[3]
delta1 <- reg_aux$coefficients[2]
round(b2_hat * delta1, 2)
reg_short <- lm(wage ~ educ, data = wage1)
round(reg_short$coefficients[2] - reg_long$coefficients[2], 2)   # check: should match the line above
