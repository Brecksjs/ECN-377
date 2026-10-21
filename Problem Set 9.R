library("wooldridge")

# ---------- From Problem Set 8 ----------

# PS8 Q14 - Every x is divided by 10, what's the new slope?
# x multiplied by c -> slope multiplied by 1/c. Dividing by k = multiplying by 1/k, so slope gets multiplied by k
# Change b1 and k (what x is divided by)
b1 = 0.43
k = 10
b1*k

# PS8 Q17 - Log-level: % change in y when x rises by some amount
# %change y = 100 * b1 * change in x
b1 = 0.06
dx = 3
100*b1*dx

# PS8 Q19 - Level-log: units change in y when x rises by some %
# change y = (b1/100) * %change x
b1 = 11.3
pctx = 13
(b1/100)*pctx

# ---------- From Problem Set 9 ----------

# PS9 Q1 - Log-log, salary changed from thousands to dollars (times 1000)
# log(1000*y) = log(1000) + log(y), so only the intercept moves
# Answer: b1 does not change; only b0 changes

# PS9 Q5 - Log-log: % change in y when x FALLS by 10%
# %change y = b1 * %change x, a fall means %change x is negative
b1 = 0.88
pctx = -10
b1*pctx

# PS9 Q6 - ceosal1, log-level regression, % change in salary when roe rises 5 points
# %change y = 100 * b1 * change in x (use the unrounded slope)
data("ceosal1")
reg = lm(log(salary)~roe,data=ceosal1)
b1 = reg$coefficients[2]
droe = 5
100*b1*droe

# PS9 Q9 - Omitted variable bias in wage on educ
# Bias comes from something in U that affects wage AND is related to educ
# Answer: the worker's ability (raises wages and higher for more educated people)

# PS9 Q13 - What makes Var(b1) smaller?
# Var(b1) = sigma^2/SSTx -> smaller sigma^2 or bigger SSTx (more spread in X, bigger sample)
# Answer: more spread-out values of X (a larger SSTx)

# PS9 Q14 - sd(b1) from sigma and a list of x values
# SSTx = sum((x - mean(x))^2), then sd(b1) = sigma/sqrt(SSTx)
sigma = 2
x = c(2,3,4,7,9)
SSTx = sum((x - mean(x))^2)
sigma/sqrt(SSTx)

# PS9 Q15 - Estimate of the error variance (sigma hat squared)
# sigma^2 hat = SSR/(n-2), divide by n-2 (not n or n-1)
n = 42
SSR = 810
SSR/(n-2)

# PS9 Q16 - Standard error of b1
# sigma hat = sqrt(SSR/(n-2)), then se(b1) = sigma hat/sqrt(SSTx)
n = 13
SSR = 437
SSTx = 12
sigmahat = sqrt(SSR/(n-2))
sigmahat/sqrt(SSTx)
