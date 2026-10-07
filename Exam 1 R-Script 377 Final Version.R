# ============================================================
#  ECN 377 -- EXAM 1 SUPER COOL R SCRIPT BY BRECKEN
#  1. Click Source once. 
#  Name inputs like the question writes them (no brackets):
#    E[X] -> EX   E[X^2] -> EX2   Var(X) -> VarX   sd(Y) -> sdY   Cov(X,Y) -> Cov
# ============================================================


# ---------- MENU  (example -> what it prints) ----------
#
# BASIC MATH
#   % change (old to new) ........ pct_change(old = 41, new = 78)          -> 90.24
#   percentage POINT change ...... pp_change(old = 1, new = 12)            -> 11
#   change in y (or y-hat) ....... delta_y(b = 3/10, dx = 10)              -> 3
#     two or more x's ............ delta_y(b = c(4, 3), dx = c(3, 3))      -> 21
#   predict y from b0, b1, x ..... yhat_at(b0 = 1, b1 = 7, x = 8)          -> 57
#   mean from a total ............ r2d(192 / 9)                            -> 21.33
#   slope from two changes ....... r2d(39 / 7)                             -> 5.57
#   undo a log (log() is ln) ..... r2d(exp(2.62))                          -> 13.74
#
# DATA LIST  (sample stats, divide by n - 1)
#   sum, mean, var, sd ........... desc(x = c(2, 9, 2))                   -> var 16.33
#     + cov and cor .............. desc(x = c(5, 0, 6), y = c(3, 0, 1))   -> cov 3.17
#   cor from Cov and sd's ........ cor_from(Cov = -2, sdX = 3, sdY = 4)   -> -0.17
#     given variances ............ cor_from(Cov = 1, VarX = 6.9, VarY = 5.4)  -> 0.16
#
# PROBABILITIES  (weight by p)  *** NEVER var() or cov() on a probability table ***
#   E[X], E[X^2], Var(X), sd(X) .. rv(x = c(2, 9, 6), p = c(.2, .5, .3))  -> EX2 52.1
#     equally likely ............. rv(x = c(0, 8, 3))                     -> EX 3.67
#   joint table: E[XY], Cov, Cor . joint(x = c(6,4,6), y = c(0,1,1), p = c(.2,.3,.5))  -> Cov -0.12
#     pairs, equally likely ...... joint(x = c(5, 1, 5), y = c(4, 5, 0))  -> Cov -2.67
#     (table? type each COLUMN: x column -> x, y column -> y, P column -> p)
#     (joint() also prints the population slope b1 = Cov/VarX and E[Y | X = each x])
#   E[Y | X = x] with probs ...... cond_E(y = c(1, 4, 0), p = c(.2, .3, .5))  -> 1.4
#   E[Y | X = x] from a list ..... cond_E(y = c(1, 8, 8, 9))              -> 6.5
#   Var(Y | X = x) ............... cond_Var(y = c(1, 4, 0), p = c(.2, .3, .5))  -> 3.04
#   any other E[ ], like E[XY^2] . x <- c(9, 9, 4); y <- c(10, 9, 9); p <- c(1/2, 1/4, 1/4)
#     then ....................... r2d(sum(x * y^2 * p))                   -> 713.25
#
# RULES  (numbers given, no table)
#   E[aX + b] .................... E_aX_b(a = 5, EX = 1, b = 8)           -> 13
#   E[aY + b | X = x] ............ E_aX_b(a = 2, EX = 7, b = 5)           -> 19
#   E[aX + bY + c] ............... E_aX_bY(a = 3, EX = 5, b = 4, EY = 3, c = -9)  -> 18
#     ("- 9" in the question -> c = -9)
#   Var from E[X^2] and E[X] ..... Var_from_E(EX2 = 78, EX = 5)           -> 53
#     (squaring a negative by hand: write (-1.9)^2, not -1.9^2)
#   Var(aX + b)  *square a* ...... Var_aX_b(a = 3, VarX = 6)              -> 54
#   Var(aX + bY) ................. Var_aX_bY(a = 2, VarX = 3, b = 1, VarY = 3, Cov = -1)  -> 11
#     (X - Y: b = -1. Independent: Cov = 0)
#   Cov from E[XY], E[X], E[Y] ... Cov_from_E(EXY = 23, EX = 4, EY = 1)   -> 19
#   Cov(a1 X + b1, a2 Y + b2) .... Cov_a1X_a2Y(a1 = 5, a2 = 4, Cov = -1)  -> -20
#   Cor from Cov and sd's ........ cor_from(Cov = -4, sdX = 5, sdY = 3)   -> -0.27
#   Cov from Cor: Cor*sdX*sdY .... r2d(0.5 * 2 * 3)                        -> 3
#   sd from Var .................. r2d(sqrt(23))                          -> 4.8
#
# REGRESSION BY HAND
#   slope, intercept, R^2, predict ols(x = c(4, 4, 2), y = c(10, 12, 6), at = 7)  -> yhat 18.5
#   slope from cov(x,y), var(x) .. b1_from(cov = 2, var = 5)              -> 0.4
#   intercept from means, slope .. b0_from(ybar = 5, xbar = 11, b1 = 3/10)  -> 1.7
#   fitted value at x ............ yhat_at(b0 = 1, b1 = 4/10, x = 18)     -> 8.2
#   residual (actual - fitted) ... resid_at(b0 = -1, b1 = 9/10, x = 8, y = 15)  -> 8.8
#     (+ = under-predicted, - = over-predicted)
#   fitted / residuals, sample ... x <- c(4, 4, 2); y <- c(10, 12, 6)
#     then ....................... r2d(lm(y ~ x)$residuals)                -> -1 1 0
#     (fitted values instead: r2d(lm(y ~ x)$fitted.values))
#   predicted change in y ........ delta_y(b = 3/10, dx = 10)             -> 3
#   SSR for a line + points ...... ssr_line(b0 = 0, b1 = 0.8, x = c(7,9,5), y = c(1,15,10))  -> 118
#   SST, SSE, SSR, R^2 (any 2) ... ss(SST = 343, SSR = 172)               -> SSE 171
#   R^2 from a correlation ....... r2d(0.6^2)                              -> 0.36
#
# DATASETS  (wooldridge -- shows 4 decimals, round when you type it in)
#   regression ................... reg_report(wage ~ educ, data = wage1)
#     ("regress Y on X" -> Y ~ X. Names must match exactly; names(vote1) lists them)
#     + person i's fit / resid ... reg_report(bwght ~ cigs, data = bwght, i = 200)
#     + predict at x ............. reg_report(salary ~ roe, data = ceosal1, at = 30)
#     + change when x rises by dx  reg_report(colgpa ~ hsrank, data = gpa2, dx = 10)
#   E[Y | X = x] in the data ..... r2d(mean(wage1$wage[wage1$educ == 12]))  -> 5.37
#   correlation in the data ...... r2d(cor(sleep75$educ, sleep75$exper))   -> -0.47
#   share of rows meeting a rule . r2d(mean(ceosal2$ceoten == 0))          -> 0.03
#   count rows ................... sum(ceosal2$ceoten == 0 & ceosal2$salary >= 600)
#   column has NA's .............. r2d(mean(bwght$motheduc, na.rm = TRUE))
#   n / what a variable is ....... nrow(wage1)   ?wage1
#   mean / sd of a variable ...... r2d(mean(wage1$wage))   r2d(sd(wage1$educ))   -> 5.9, 2.77


# ---------- TOOLS   ----------
require(wooldridge)                      # if wooldridge is missing, the rest still loads
options(scipen = 999)                    # no scientific notation
r2d <- function(v) round(v, 2)           # round to the hundredths

check_p <- function(p, x) {              # catches typos in probability lists
  if (length(p) != length(x)) stop("need one probability for each value", call. = FALSE)
  if (abs(sum(p) - 1) > 0.000001) warning("probabilities add to ", sum(p), ", not 1", call. = FALSE)
}

# basic math
pct_change <- function(old, new) r2d(100 * (new - old) / old)
pp_change  <- function(old, new) r2d(new - old)
delta_y    <- function(b, dx) r2d(sum(b * dx))            # b1*dx1 + b2*dx2 + ... (no intercept)
yhat_at    <- function(b0, b1, x) r2d(if (length(b1) > 1) b0 + sum(b1 * x) else b0 + b1 * x)

# data lists (sample stats, n - 1)
desc <- function(x, y = NULL) {
  out <- c(sum = sum(x), mean = mean(x), var = var(x), sd = sd(x))
  if (!is.null(y)) out <- c(out, mean_y = mean(y), var_y = var(y), sd_y = sd(y),
                            cov = cov(x, y), cor = suppressWarnings(cor(x, y)))
  r2d(out)
}
cor_from <- function(Cov, sdX = sqrt(VarX), sdY = sqrt(VarY), VarX, VarY) r2d(Cov / (sdX * sdY))

# probabilities (weight by p)
rv <- function(x, p = rep(1 / length(x), length(x))) {
  check_p(p, x)
  EX <- sum(x * p); EX2 <- sum(x^2 * p); VarX <- max(EX2 - EX^2, 0)
  r2d(c(EX = EX, EX2 = EX2, VarX = VarX, sdX = sqrt(VarX)))
}
joint <- function(x, y, p = rep(1 / length(x), length(x))) {
  if (length(x) != length(y)) stop("x and y need one value per row", call. = FALSE)
  check_p(p, x)
  EX <- sum(x * p); EY <- sum(y * p); EXY <- sum(x * y * p)
  EX2 <- sum(x^2 * p); EY2 <- sum(y^2 * p)
  VarX <- max(EX2 - EX^2, 0); VarY <- max(EY2 - EY^2, 0)
  Cov <- EXY - EX * EY                                    # E[XY] - E[X]E[Y]
  out <- c(EX = EX, EY = EY, EXY = EXY, EX2 = EX2, EY2 = EY2, VarX = VarX, VarY = VarY,
           sdX = sqrt(VarX), sdY = sqrt(VarY), Cov = Cov, Cor = Cov / sqrt(VarX * VarY),
           b1 = Cov / VarX, b0 = EY - Cov / VarX * EX)
  for (v in sort(unique(x)))                              # E[Y | X = each x value]
    out[paste0("EY|X=", v)] <- sum(y[x == v] * p[x == v]) / sum(p[x == v])
  r2d(out)
}
cond_E <- function(y, p = rep(1 / length(y), length(y))) { check_p(p, y); r2d(sum(y * p)) }
cond_Var <- function(y, p = NULL) {
  if (is.null(p)) return(r2d(var(y)))                     # just a list of Y's -> var(), like Day 7
  check_p(p, y)
  r2d(sum(y^2 * p) - sum(y * p)^2)
}

# rules
E_aX_b      <- function(a, EX, b = 0) r2d(a * EX + b)
E_aX_bY     <- function(a, EX, b, EY, c = 0) r2d(a * EX + b * EY + c)
Var_from_E  <- function(EX2, EX) r2d(EX2 - EX^2)
Var_aX_b    <- function(a, VarX) r2d(a^2 * VarX)          # a gets SQUARED, b drops out
Var_aX_bY   <- function(a, VarX, b, VarY, Cov = 0) r2d(a^2 * VarX + b^2 * VarY + 2 * a * b * Cov)
Cov_from_E  <- function(EXY, EX, EY) r2d(EXY - EX * EY)
Cov_a1X_a2Y <- function(a1, a2, Cov) r2d(a1 * a2 * Cov)   # b1, b2 drop out

# regression by hand
ols <- function(x, y, at = NULL) {
  if (length(x) != length(y)) stop("x and y need the same number of values", call. = FALSE)
  if (var(x) == 0) stop("all the x's are the same, so there's no slope", call. = FALSE)
  b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)   # = cov(x, y) / var(x)
  b0 <- mean(y) - b1 * mean(x)
  SST <- sum((y - mean(y))^2); SSR <- sum((y - (b0 + b1 * x))^2); SSE <- SST - SSR
  out <- c(b1 = b1, b0 = b0)
  if (!is.null(at)) out <- c(out, yhat = b0 + b1 * at)
  r2d(c(out, R2 = SSE / SST, SST = SST, SSE = SSE, SSR = SSR))
}
b1_from  <- function(cov, var) r2d(cov / var)
b0_from  <- function(ybar, xbar, b1) r2d(ybar - b1 * xbar)
resid_at <- function(b0, b1, x, y) r2d(y - (b0 + b1 * x))           # actual - fitted
ssr_line <- function(b0, b1, x, y) r2d(sum((y - (b0 + b1 * x))^2))
ss <- function(SST = NA, SSE = NA, SSR = NA, R2 = NA) {             # give any two
  for (k in 1:3) {
    if (is.na(SST) && !is.na(SSE) && !is.na(SSR)) SST <- SSE + SSR
    if (is.na(SSE) && !is.na(SST) && !is.na(SSR)) SSE <- SST - SSR
    if (is.na(SSR) && !is.na(SST) && !is.na(SSE)) SSR <- SST - SSE
    if (is.na(R2)  && !is.na(SST) && !is.na(SSE)) R2  <- SSE / SST
    if (is.na(SSE) && !is.na(SST) && !is.na(R2))  SSE <- R2 * SST
    if (is.na(SST) && !is.na(SSE) && !is.na(R2))  SST <- SSE / R2
    if (is.na(SST) && !is.na(SSR) && !is.na(R2))  SST <- SSR / (1 - R2)
  }
  r2d(c(SST = SST, SSE = SSE, SSR = SSR, R2 = R2, unexplained = 1 - R2))
}

# datasets
reg_report <- function(formula, data, i = NULL, at = NULL, dx = NULL) {
  reg <- lm(formula, data = data)
  b0 <- coef(reg)[[1]]; b1 <- coef(reg)[[2]]
  y <- reg$model[[1]]
  SST <- sum((y - mean(y))^2); SSR <- sum(reg$residuals^2)
  out <- c(b1 = b1, b0 = b0, R2 = summary(reg)$r.squared, SST = SST, SSE = SST - SSR, SSR = SSR)
  if (!is.null(i))  out <- c(out, fitted = reg$fitted.values[[i]], resid = reg$residuals[[i]])
  if (!is.null(at)) out <- c(out, yhat = b0 + b1 * at)
  if (!is.null(dx)) out <- c(out, change = b1 * dx)
  round(out, 4)
}

cat("Toolkit loaded. Quick test (should say 80):", Var_aX_b(a = 4, VarX = 5), "\n")

# Add in from Problem set 7
#Questions 1-3
# How many workers are in the wage1 sample? (Hint: nrow(wage1))
#What is the sample mean of hourly wage, wage1$wage? (Hint: mean()) (round to the hundredths)
#What is the sample standard deviation of years of education, wage1$educ? (Hint: sd()) (round to the hundredths)
# Print this:# Q4-9
reg_report(wage ~ educ, data = wage1, at = 14, dx = 2)
#Question	Label to read	Answer to type
# Q4 slope  =b1
# Q5 intercept  =b0
# Q6 prediction at educ = 14  = yhat
# Q7 change for +2 years  =change
# Q8 SSR  = SSR
# Q9 R^2  = R2
# If a question doesn't ask for a prediction or a change, leave off at or dx
# ---------- ANSWERS  (type below) ----------
# example:
# Q1
# Var_aX_b(a = 4, VarX = 5)

