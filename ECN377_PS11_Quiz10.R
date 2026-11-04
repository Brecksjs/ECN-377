if (!require(wooldridge)) install.packages("wooldridge", repos = "https://cloud.r-project.org")
library(wooldridge)

# sign of omitted-variable bias = sign(b2) x sign(cor(X1, X2)), used in Q16-Q19
ovb <- function(b2, cor) {
  s <- sign(b2) * sign(cor)
  if (s > 0) "biased upward (overstates)" else if (s < 0) "biased downward (understates)" else "unbiased"
}


# Q1 (quiz)
# log-level MLR: % change in predicted y when x1 and x2 both change = 100 * (b1*dx1 + b2*dx2)
b1 <- 0.033; b2 <- -0.051
dx1 <- 4; dx2 <- 4
round(100 * (b1*dx1 + b2*dx2), 2)   # -7.2


# Q2 (quiz)
# wage1, log(wage) on educ + exper + tenure: % change in wage when exper +2 and tenure +1
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients   # b[1] intercept, b[2] educ, b[3] exper, b[4] tenure
round(100 * (b[3]*2 + b[4]*1), 2)   # 3.03


# Q3 (quiz)
# same log(wage) regression: predicted wage in DOLLARS at educ 12, exper 10, tenure 3 -> plug in, then exp()
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
round(exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3), 2)   # 4.46 (R prints "(Intercept)" above it, ignore that label)
round(exp(predict(reg, data.frame(educ = 12, exper = 10, tenure = 3))), 2)   # same answer, other way


# Quiz 9 Q10 (the one you missed, answer 7.16)
# plain wage (no log) on educ + exper + tenure: predicted wage at educ 16, exper 5, tenure 2
# start with b[1] (intercept); no exp() because wage isn't logged
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
round(b[1] + b[2]*16 + b[3]*5 + b[4]*2, 2)   # 7.16 (ignore the "(Intercept)" label)


# Q4 (quiz)
# select all: which residual facts ALWAYS hold after OLS with an intercept
reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
u <- reg$residuals
round(c(mean_u = mean(u), cov_ACT_u = cov(gpa1$ACT, u), cov_hsGPA_u = cov(gpa1$hsGPA, u), cov_Y_u = cov(gpa1$colGPA, u)), 4)
# -> 0, 0, 0, but cov(Y, u) is 0.11, not 0
# answer: avg residual is 0; passes through the point of means; cov(each Xj, residuals) is 0
# not: cov(Y, residuals) is 0 (that's the FITTED values); every residual is 0


# Q5
# gpa1: predicted colGPA at mean ACT and mean hsGPA
reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b <- reg$coefficients
round(b[1] + b[2]*mean(gpa1$ACT) + b[3]*mean(gpa1$hsGPA), 2)   # 3.06
round(mean(gpa1$colGPA), 2)   # also 3.06, because OLS passes through the point of means


# Q6 (quiz)
# ybar from the fitted equation: ybar = b0 + b1*x1bar + b2*x2bar (b2 is negative)
b0 <- 7.7; b1 <- 1.9; b2 <- -0.5
x1bar <- 11.3; x2bar <- 12.7
round(b0 + b1*x1bar + b2*x2bar, 2)   # 22.82


# Q7 (quiz)
# select all: which regressions break MLR3 (perfect collinearity)? R shows NA when it can't separate a variable
vote1$totexpend <- vote1$expendA + vote1$expendB
lm(voteA ~ expendA + expendB + totexpend, data = vote1)$coefficients              # totexpend NA
lm(wage ~ educ + I(educ / 1000), data = wage1)$coefficients                       # same variable, new units -> NA
lm(wage ~ educ + exper + tenure + female + married, data = wage1[1:4, ])$coefficients   # 5 x's, 4 rows -> NAs
# answer: totexpend = expendA + expendB; income in dollars AND thousands; 5 x's on 4 observations
# fine: educ and exper that both vary; ACT and hsGPA correlated but not perfectly


# Q8
# vote1: what R reports for totexpend when totexpend = expendA + expendB
vote1$totexpend <- vote1$expendA + vote1$expendB
lm(voteA ~ expendA + expendB + totexpend, data = vote1)
# answer: NA, R drops totexpend because it's an exact linear function of expendA and expendB


# Q9 (quiz)
# which situation breaks MLR4, E[U | X's] = 0 (endogeneity)
# answer: a variable that affects Y and is correlated with X1 is left out of the regression
# not: n = 50, units of Y, or X1 and X2 highly but not perfectly correlated


# Q10
# unbiasedness only needs MLR1-MLR4, so why not add every variable?
# answer: adding variables can increase the variance of the OLS estimates and can even violate MLR3


# Q11 (quiz)
# formula (3.23): b1_tilde = b1_hat + b2_hat * d1_tilde -> solve for b1_hat = b1_tilde - b2_hat * d1_tilde
b1_tilde <- -0.25   # simple regression of Y on X1
b2_hat   <- 0.72    # X2 coefficient in the multiple regression
d1_tilde <- -0.53   # slope from regressing X2 on X1
round(b1_tilde - b2_hat * d1_tilde, 2)   # 0.13


# Q12
# bwght: simple regression slope on cigs
round(lm(bwght ~ cigs, data = bwght)$coefficients["cigs"], 2)   # -0.51


# Q13
# bwght: add faminc, cigs slope holding faminc fixed
round(lm(bwght ~ cigs + faminc, data = bwght)$coefficients["cigs"], 2)   # -0.46


# Q14 (quiz)
# b2_hat * d1_tilde: faminc coef from bwght ~ cigs + faminc, times the cigs slope from faminc ~ cigs
b2_hat   <- lm(bwght ~ cigs + faminc, data = bwght)$coefficients["faminc"]
d1_tilde <- lm(faminc ~ cigs, data = bwght)$coefficients["cigs"]
round(b2_hat * d1_tilde, 2)   # -0.05
# check with (3.23): simple slope minus multiple slope gives the same number
round(lm(bwght ~ cigs, data = bwght)$coefficients["cigs"] - lm(bwght ~ cigs + faminc, data = bwght)$coefficients["cigs"], 2)   # -0.05


# Q15
# simple cigs slope -0.51 vs -0.46 holding faminc fixed: which reading is right
# answer: -0.46 is smoking's effect holding income fixed; the other -0.05 is income's effect riding along
#         (heavier smokers tend to have lower incomes)


# Q16 (quiz)
# omitted X2 lowers Y (b2 < 0) and is negatively correlated with X1: direction of the bias
ovb(b2 = -1, cor = -1)   # biased upward


# Q17 (quiz)
# yield on fertilizer, land quality left out: better land raises yield (b2 > 0), more fertilizer on better land (cor > 0)
ovb(b2 = 1, cor = 1)   # biased upward -> overstates the effect of fertilizer


# Q18
# exam score on tutoring, ability left out: ability raises scores (b2 > 0), lower ability gets more tutoring (cor < 0)
ovb(b2 = 1, cor = -1)   # biased downward -> understates the effect of tutoring


# Q19
# select all: when is there NO omitted-variable bias? when either leg is 0
ovb(b2 = 1, cor = 0)   # X2 uncorrelated with X1 (d1_tilde = 0) -> unbiased
ovb(b2 = 0, cor = 1)   # X2 has no ceteris-paribus effect on Y (b2 = 0) -> unbiased
# answer: those two. A high R^2, a big sample, or a big X2 effect don't remove the bias
