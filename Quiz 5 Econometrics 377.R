# ==================================================================
# ECON 377 - Problem Set 5 Complete Key & R Script (v2)
# Variable names match problem notation with question suffixes (_qN)
# ==================================================================

rm(list = ls()) # Clear workspace environment

# ==================================================================
# PART 1: Theoretical Concepts (Questions 1 - 8)
# ==================================================================

# Question 1:
# In the simple linear regression model Y = β0 + β1 X + U, the DEPENDENT variable is:
# Answer: Y
# Reason: Y is the outcome/dependent variable; X is the independent variable; U is the error term.

# Question 2:
# In Y = β0 + β1 X + U, the error term U represents:
# Answer: Factors OTHER than X that affect Y
# Reason: The error term U captures all unobserved variables other than X that influence Y.

# Question 3:
# In Y = β0 + β1 X + U, the slope β1 measures:
# Answer: The ceteris paribus effect of X on Y (the change in Y per one-unit rise in X, holding other factors fixed)
# Reason: β1 represents the marginal effect of a 1-unit increase in X on Y, holding all else in U constant.

# Question 4:
# The zero conditional mean assumption E[U | X] = 0 says:
# Answer: The average of U does not depend on X
# Reason: Unobserved factors in U are uncorrelated on average with the independent variable X.

# Question 5:
# Under the SLR model with E[U | X] = 0, the conditional mean E[Y | X = x] equals:
# Answer: β0 + β1 x
# Reason: E[Y | X=x] = E[β0 + β1 x + U | X=x] = β0 + β1 x + E[U | X=x] = β0 + β1 x.

# Question 6:
# The OLS slope estimate β̂1 can be written as:
# Answer: cov(x,y)/var(x)
# Reason: In simple linear regression, β̂1 = Cov(x, y) / Var(x) = ∑((x_i - x̄)(y_i - ȳ)) / ∑((x_i - x̄)^2).

# Question 7:
# We must ESTIMATE β0, β1 (rather than compute them exactly) because:
# Answer: They are population quantities; we observe only a sample
# Reason: True parameters β0 and β1 belong to the full population; sample data yields estimates β̂0 and β̂1.

# Question 8:
# In Y = (β0 + β1 X) + U, the term β0 + β1 X is:
# Answer: The part of Y EXPLAINED by X (its conditional mean)
# Reason: β0 + β1 X is the deterministic component explained by X; U is the unexplained residual.


# ==================================================================
# PART 2: OLS Formulas & Manual Estimation (Questions 9 - 14)
# ==================================================================

# Question 9:
# Given cov(x,y) = 12 and var(x) = 2, find β̂1 = cov(x,y)/var(x).
cov_xy_q9    <- -5
var_x_q9     <- 9
beta1_hat_q9 <- cov_xy_q9 / var_x_q9
round(beta1_hat_q9, 2)  # Output: 6.00


# Question 10:
# Given ȳ = 3, x̄ = 7, and slope β̂1 = 9/10, find β̂0 = ȳ - β̂1 x̄.
ybar_q10      <- 9
xbar_q10      <- 9
beta1_hat_q10 <- 5/10
beta0_hat_q10 <- ybar_q10 - beta1_hat_q10 * xbar_q10
round(beta0_hat_q10, 2) 


# Question 11:
# For x = (2,2,1), y = (3,8,2), find the OLS slope β̂1 = ∑((x_i - x̄)(y_i - ȳ)) / ∑((x_i - x̄)^2).
x_q11         <- c(8, 6, 7)
y_q11         <- c(6, 10, 0)
beta1_hat_q11 <- cov(x_q11, y_q11) / var(x_q11)
round(beta1_hat_q11, 2) 


# Question 12:
# For x = (3,4,5), y = (8,7,1), find the OLS intercept β̂0 = ȳ - β̂1 x̄.
x_q12         <- c(1, 8, 2)
y_q12         <- c(7, 0, 8)
beta1_hat_q12 <- cov(x_q12, y_q12) / var(x_q12)
beta0_hat_q12 <- mean(y_q12) - beta1_hat_q12 * mean(x_q12)
round(beta0_hat_q12, 2) # Output: 19.33


# Question 13:
# A fitted regression line is ŷ = β̂0 + β̂1 x. With β̂0 = 1 and β̂1 = 9/10, find ŷ at x = 10.
beta0_hat_q13 <- 1
beta1_hat_q13 <- 9/10
x_val_q13     <- 10
y_hat_q13     <- beta0_hat_q13 + beta1_hat_q13 * x_val_q13
round(y_hat_q13, 2)     # Output: 10.00


# Question 14:
# For x = (5,6,3), y = (10,3,3), fit OLS line and predict ŷ at x = 8.
x_q14     <- c(4, 4, 2)
y_q14     <- c(6, 11, 7)
fit_q14   <- lm(y_q14 ~ x_q14)
y_hat_q14 <- predict(fit_q14, newdata = data.frame(x_q14 = 4))
round(y_hat_q14, 2)     # Output: 7.00


# ==================================================================
# PART 3: Econometric Applications & Predictions (Questions 15 - 16)
# ==================================================================

# Question 15:
# E[colGPA | hsGPA] = 15/10 + (3/10) * hsGPA. Expected colGPA at hsGPA = 36/10:
beta0_q15    <- 10/10
beta1_q15    <- 7/10
hsGPA_q15    <- 20/10
E_colGPA_q15 <- beta0_q15 + beta1_q15 * hsGPA_q15
round(E_colGPA_q15, 2)  # Output: 2.58


# Question 16:
# In wage = β0 + β1 educ + U, estimated slope β̂1 = 3/10. If educ rises by 9 years, change in predicted wage:
beta1_hat_q16  <- 8/10
delta_educ_q16 <- 10
delta_wage_q16 <- beta1_hat_q16 * delta_educ_q16
round(delta_wage_q16, 2) # Output: 2.70


# ==================================================================
# PART 4: Joint Probability Distributions (Questions 17 - 20)
# ==================================================================

# Question 17:
# Joint distribution: (x=2, y=3, P=0.2), (x=0, y=0, P=0.3), (x=5, y=1, P=0.5). 
#Find E[X]. *****Don't mistake E[X] for E[Y]*****
x_q17    <- c(4, 5, 4)            ### All the X Values
prob_q17 <- c(0.2, 0.3, 0.5)      ### All the P Values
EX_q17   <- sum(x_q17 * prob_q17)
round(EX_q17, 2)        # Output: 2.90


# Question 18:
# Joint distribution: (x=3, y=3, P=0.2), (x=4, y=8, P=0.3), (x=3, y=8, P=0.5). Find Cov(X,Y) = E[XY] - E[X]E[Y].
x_q18    <- c(6, 6, 3)
y_q18    <- c(6, 6, 7)
prob_q18 <- c(0.2, 0.3, 0.5)
EX_q18   <- sum(x_q18 * prob_q18)
EY_q18   <- sum(y_q18 * prob_q18)
EXY_q18  <- sum(x_q18 * y_q18 * prob_q18)
Cov_q18  <- EXY_q18 - (EX_q18 * EY_q18)
round(Cov_q18, 2)       # Output: 0.30


# Question 19:
# Joint distribution: (x=4, y=8, P=0.2), (x=3, y=5, P=0.3), (x=5, y=4, P=0.5). Find population slope β1 = Cov(X,Y)/Var(X).
x_q19     <- c(2, 6, 0)
y_q19     <- c(1, 2, 0)
prob_q19  <- c(0.2, 0.3, 0.5)
EX_q19    <- sum(x_q19 * prob_q19)
EY_q19    <- sum(y_q19 * prob_q19)
EX2_q19   <- sum(x_q19^2 * prob_q19)
EXY_q19   <- sum(x_q19 * y_q19 * prob_q19)
VarX_q19  <- EX2_q19 - (EX_q19^2)
CovXY_q19 <- EXY_q19 - (EX_q19 * EY_q19)
beta1_q19 <- CovXY_q19 / VarX_q19
round(beta1_q19, 2)    # Output: -0.68


# Question 20:
# For x = (1,6,6), y = (0,4,1), find sample covariance S_xy = cov(x,y).
x_q20    <- c(6, 3, 6)
y_q20    <- c(0, 4, 1)
S_xy_q20 <- cov(x_q20, y_q20)
round(S_xy_q20, 2)     # Output: 4.17
