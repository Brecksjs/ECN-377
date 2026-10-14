library("wooldridge")

# Q1 - Zero conditional mean: family income is in U and is related to class size
# If something in U moves with x, E[U|x] is not 0, so the assumption fails
# Answer: zero conditional mean fails (avg of U changes with class size)

# Q2 - Which candidate line is OLS?
# OLS residuals sum to 0 AND it has the smallest sum of squared residuals
# A and C both sum to 0, C has the smaller squared sum
# Answer: Line C

# Q3 - SD of 2X - 1, given E[X] and E[X^2]
# Var(X) = E[X^2] - E[X]^2, sd(aX+b) = |a|*sd(X), the -1 doesn't matter
# Change EX, EX2, and a (the number in front of X). b is the +/- number, it doesn't affect sd
EX = 3
EX2 = 45
a = 2
b = -1
varX = EX2 - EX^2
abs(a)*sqrt(varX)

# Q4 - Variance of a combination 3X - 2Y
# Var(aX+bY) = a^2*Var(X) + b^2*Var(Y) + 2ab*Cov(X,Y), b is negative here
varX = 5
varY = 8
covXY = -1.2
a = 3
b = -2
a^2*varX + b^2*varY + 2*a*b*covXY

# Q5 - Correlation from a joint distribution table
# Weight everything by the probabilities (don't use cor())
# Cov = E[XY] - E[X]E[Y], Cor = Cov/(sdX*sdY)
x = c(3,4,5,8)
y = c(6,8,8,3)
p = c(0.2,0.3,0.4,0.1)
EX = sum(p*x)
EY = sum(p*y)
covXY = sum(p*x*y) - EX*EY
sdX = sqrt(sum(p*x^2) - EX^2)
sdY = sqrt(sum(p*y^2) - EY^2)
covXY/(sdX*sdY)

# Q6 - Residual of the 3rd observation
# Residual = actual y - fitted y
x = c(1,2,5,7,8)
y = c(7,5,6,11,6)
resid(lm(y~x))[3]

# Q7 - Find the missing 6th residual
# OLS residuals always sum to 0, so it's the negative of the sum of the others
-(1.2 + 0.6 + 1.1 - 2.2 + 0.2)

# Q8 - Sum of x times the residuals
# OLS property: sum(x*u) = 0 always (if R prints something like -8.7e-15, enter 0)
x = c(2,4,7,9,10,13)
y = c(8,6,6,10,8,1)
reg = lm(y~x)
sum(x*reg$residuals)

# Q9 - Find SSR given SSE and R^2
# R^2 = SSE/SST -> SST = SSE/R^2, then SSR = SST - SSE
SSE = 186
R2 = 0.79
SST = SSE/R2
SST - SSE

# Q10 - Find SST from the sample variance
# Sy^2 = SST/(n-1) -> SST = Sy^2*(n-1)
n = 51
Sy2 = 18
Sy2*(n-1)

# Q11 - Residual for row 12 in wage1 (wage on tenure)
# Run the regression, pull the 12th residual
data("wage1")
reg = lm(wage~tenure,data=wage1)
reg$residuals[12]

# Q12 - Mean of y given the OLS line and mean of x
# (xbar, ybar) is always on the OLS line, so ybar = b0 + b1*xbar
b0 = 1
b1 = 2.4
xbar = 4
b0 + b1*xbar
