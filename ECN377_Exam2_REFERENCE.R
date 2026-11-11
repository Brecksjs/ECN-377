# =====================================================================================
#  ECN 377 -- EXAM 2 REFERENCE (Brecken)   search here; type answers in ECN377_Exam2_ANSWERS.R
#  Covers Days 13-21 + a few questions from before Exam I. Chapter 4 t-tests are NOT on it.
#  ~5 questions follow Problem Set 12 #2, 4, 6, 9, 12 (standard errors) -> @PS12
#
#  HOW TO USE:  1) Source ECN377_Exam2_ANSWERS.R first (the other tab). Then every line here runs.
#               2) Ctrl/Cmd + F a TAG from the INDEX below (or a word from the question)
#               3) find the label that reads like your question. Triple-click the code line under it,
#                  Ctrl/Cmd + C, paste it under its # Q# in ANSWERS, swap in your numbers, Ctrl/Cmd + Enter.
#                  The comment after each code line says which number from the question goes where.
#  Outline button (top right of this pane, or Ctrl/Cmd + Shift + O) lists every section.
# =====================================================================================


# @GAMEPLAN  (read before the exam starts) ---------------------------------------------
#  Exam I: 19.8 / 38. 13 of the lost points were BLANKS on questions this script does
#  in one line. The fix is order + never leaving a blank:
#   PASS 1 (first ~45 min): every concept question (@FLIP answers them in seconds) and every
#          one-liner. If a question takes more than ~2 minutes, write its number down and move on.
#   PASS 2: the skipped ones (data questions, se pieces).
#   LAST 5 MINUTES: NOTHING BLANK. A numeric guess can get credit; a blank can't.
#          Multiple choice: cross out what @FLIP rules out, then pick.
#   LAST 2 MINUTES: File > Save, then upload ECN377_Exam2_ANSWERS.R in the last question
#          (partial credit needs the script you submitted, with the code under each # Q#).
#  ~2 minutes per question. Round to the hundredths, but keep unrounded numbers in R until the end.
#  Before it starts: Source ECN377_Exam2_ANSWERS.R. If it says there's no package called 'wooldridge',
#  run install.packages("wooldridge") once, then Source again.


# @INDEX  (words in the question -> tag to search) -------------------------------------
#  "every value of y/x multiplied/divided", "measured in dollars/thousands", "percent -> decimal"  @UNITS
#  "log(", "by about what percent", "elasticity", "What does the 0.45 mean"                       @LOGS
#  "predicted value", "predicted wage at", "change in y-hat", "sample mean of y"                    @PREDICT
#  "Data: wage1 / ceosal1 / bwght / gpa1 / hprice1 / vote1 / meap93", "Run lm(...)"                @DATA
#  "Var(b1-hat)", "sd(b1-hat)", "standard error", "sigma-hat", "SST_x", "times as large"           @VAR
#  "R_j^2", "which regression gives", "SST_j", "by hand", "summary(reg)$coefficients"             @PS12
#  "omitted / left out", "biased upward/downward", "overstates", "formula (3.23)", "delta1"        @OVB
#  "MLR1-MLR5", "perfect collinearity", "NA", "endogenous", "unbiased means", "Gauss-Markov"       @ASSUME
#  "always true after OLS", "residuals", "point of means", "R^2"                                    @MLR
#  "E[X]", "Var(aX + b)", "probability table", "joint distribution", "Cor(X, Y)", "sample sd of"   @BEFORE1
#  "fifth residual", "sum of x times residual", "SST from S_y^2", "SSR from SSE and R^2"           @BEFORE1
#  "the question looks like a PS question but reversed (larger/smaller, up/down, fails/holds)"     @FLIP
#  Worked versions of every PS8-12 / Quiz 7-10 question (+ FLIP versions)                          @BANK
#  Your Exam I, all 38 questions with the right answers                                            @EXAM1
#  Where to type answers: the other tab, ECN377_Exam2_ANSWERS.R (Ctrl+End jumps to # Q1-Q38)


# @TRAPS  (points you have actually lost) ------------------------------------------------
#  Exam I (19.8 / 38):
#   - 13 blanks: SST = (n-1)S_y^2, SSR from SSE and R^2, y-bar = b0 + b1 x-bar, the missing residual,
#     sum(x * u-hat) = 0, sd(4X - 4), Var(2X - 2Y), mean of a vector, corr after a units change,
#     n / mean / correlation / residual from data. Every one is a one-liner in @BEFORE1.
#   - Q19: E[3Y + 6 | X] -- typed E[Y | X] = 17.1. Apply the a and b: 3 * 17.1 + 6 = 57.3.
#   - Q35: "% of variation explained" -- typed SSE = 861.63. It's 100 * R^2 = 12.03.
#   - Q7 / Q8: E[price | Rolls-Royce] > E[price | Toyota]; Var(income | age 40) > Var(income | age 10).
#   - Q11: OLS line = residuals sum to 0, THEN smallest sum of SQUARED residuals (not absolute values).
#   - Q4: beta0 = intercept parameter (not "fitted value").
#  Quizzes 8-10:
#   - OVB direction (Quiz 10 Q10 + Q11, both missed): multiply the two signs. Run ovb(). Don't guess.
#   - MLR3 select-all (Quiz 10 Q6): "total = online + store" IS perfect collinearity;
#     "study and sleep hours, negatively but not perfectly correlated" is NOT.
#   - Quiz 10 Q9: the quiz used hprice1, not the PS's bwght -> you typed the PS answer. Rerun with the new data.
#   - Quiz 8 Q3: log-level needs x 100 (typed 0.138, answer 13.8).
#   - Quiz 9 Q10: plain wage on the left = NO exp(). log(wage) on the left = exp() for dollars.
#   - Read WHICH variable moves (the quiz moved exper, the PS moved tenure). "Falls" = negative.
#   - Given the SD of X -> square it (Sx). Given the VARIANCE -> don't (Sx2). 4x the data -> sd / 2.
#   - (3.23) signs: b1-hat = b1-tilde - b2-hat * d1-tilde. Minus a (negative x negative) -> let f323() do it.
#   - Probability tables: weight by p (rv(), joint()). Never var() / sd() / cor() on a table's values.


# @FLIP  (concept questions both ways -- the exam flips the PS wording) -----------------
#  VARIANCE / sd / se OF A SLOPE
#    LARGER  <- bigger sigma^2 (noisier U) | smaller n | x bunched together (smaller SST_x)
#               | x_j more correlated with the other x's (bigger R_j^2, multicollinearity)
#    SMALLER <- smaller sigma^2 | bigger n | x more spread out (bigger SST_x) | smaller R_j^2
#    NO EFFECT: the true slope beta1, the intercept beta0
#    n x k -> sd / sqrt(k)   |  n / k -> sd x sqrt(k)  |  sigma x k -> sd x k  |  R_j^2 -> 1: se -> infinity
#  BIAS DIRECTION (Table 3.2: sign(beta2) x sign(corr(x1, x2)))
#    (+)(+) UP  |  (-)(-) UP  |  (+)(-) DOWN  |  (-)(+) DOWN
#    UP = overstates, E[b1-tilde] > beta1   |   DOWN = understates / more negative, E[b1-tilde] < beta1
#    NO bias <- beta2 = 0 (x2 doesn't affect y) OR corr(x1, x2) = 0     (needs BOTH legs to be biased)
#    STILL biased even with: a huge sample, a high R^2, a big effect of x2
#    simple slope = multiple slope <- b2-hat = 0 OR delta1-tilde = 0 (same two legs, in the sample)
#  MLR3 (no PERFECT collinearity)
#    BREAKS: a total next to its parts (totexpend = expendA + expendB; total sales = online + store)
#            | one variable in two units (sq ft & sq m; $ & $1000s) | log(x) and log(x^2)
#            | n < k + 1 (5 x's on 4 obs; 3 x's on 3 obs) | an x that never varies
#    FINE:   highly or negatively (but not perfectly) correlated x's | x and x^2 | x's that vary | one x alone
#    Breaks -> R prints NA, can't estimate.   Highly-but-not-perfectly correlated -> estimates exist, big se's.
#  MLR4 / SLR4 (zero conditional mean, E[U | x] = 0)
#    FAILS (x endogenous) <- something in U affects y AND moves with x: ability & educ, motivation & training,
#            land quality & fertilizer, family income & class size; or the wrong functional form
#    HOLDS / not about MLR4: small n, units of y, correlated (not perfect) x's, an omitted factor UNRELATED
#            to x (weather on test day, survey ID, day of the week)
#    Fails -> OLS BIASED.  MLR1-4 hold -> OLS UNBIASED.
#  SLR5 / MLR5 (homoskedasticity, Var(U | x) = sigma^2)
#    FAILS <- spread of y around the line changes with x (food spending vs income; wage spread vs educ)
#    Fails -> OLS STILL UNBIASED, but Var / se formulas are wrong and OLS isn't BLUE.
#    MLR1-5 hold -> Gauss-Markov: OLS is BLUE (Best = smallest variance, Linear, Unbiased);
#                   E[sigma-hat^2] = sigma^2 (Thm 3.3)
#  UNBIASED MEANS: averaged over MANY samples, b-hat = beta. Each sample's estimate still differs.
#    DOES NOT MEAN: every sample hits beta | no spread | R^2 = 1 | residuals are 0 | most researchers overestimate
#  OLS FACTS (with an intercept)
#    ALWAYS: residuals sum/average to 0 | cov(each x_j, residuals) = 0 (sum x * u-hat = 0)
#            | cov(fitted values, residuals) = 0 | (x-bar's, y-bar) is on the line | SST = SSE + SSR
#    NOT ALWAYS: cov(y, residuals) = 0 | every residual = 0 | residuals add up to SSR
#    OLS line = residuals sum to 0 AND smallest sum of SQUARED residuals (ignore sum of |u-hat|)
#  R^2: adding an x never LOWERS it | dropping an x never RAISES it | units never change it
#    | all points on the line -> SSR = 0, R^2 = 1 | low R^2 is common in economics, says nothing about causation
#  UNITS:  y x c -> b0 and b1 x c  |  y / c -> both / c  |  x x c -> b1 / c, b0 same  |  x / c -> b1 x c, b0 same
#    log(y) rescaled -> only b0 moves (+ log c) | log(x) rescaled -> only b0 moves (- b1 log c) | slope of a logged
#    variable never changes | correlation never changes | covariance x (c x d)
#  LOGS (which reading goes with which model)
#    %dy = 100 b dx -> LOG-LEVEL (semi-elasticity) | %dy = b %dx -> LOG-LOG (elasticity)
#    dy = (b/100) %dx -> LEVEL-LOG | dy = b dx -> LEVEL-LEVEL.  Rises vs falls: same formula, negative change.
#  sd vs se: sd uses the TRUE sigma (given) | se uses sigma-hat = sqrt(SSR / (n - k - 1)) (estimated)
#  POPULATION (mu, beta, sigma) = fixed numbers | SAMPLE (y-bar, b-hat, sigma-hat) = random variables
#  PERCENT vs PERCENTAGE POINTS: 5% -> 4% = -1 point = -20 percent | 4% -> 6% = +2 points = +50 percent
#  CONDITIONAL GROUPS: bigger mean -> E[price | Rolls-Royce] | smaller -> Toyota.
#    bigger spread -> Var(income | age 40) | smaller -> age 10 (kids have ~no income)
#  RESIDUAL SIGN: u-hat > 0 -> actual above the line -> UNDER-predicted | u-hat < 0 -> OVER-predicted
#  y-hat = 12, u-hat = -3 -> y = 9, over-predicted.   Correlation alone never proves causation.
#  DATA TYPES: cross-section (many units, one time) | time series (one unit over time: a stock's daily price)
#    | pooled cross section (new samples each year) | panel (the SAME units over time)


if (FALSE) {   # ONE-LINER MENUS: Source skips this block. Each label reads like the question (-> the example's
               # answer). Triple-click the code line under it, copy it into ANSWERS, swap in your numbers.
# @UNITS  units of measurement (Day 13) ---------------------------------------------------
#   HOW TO READ THIS: the label is how the QUESTION reads -> the example's answer.
#   The comment after the code says which number from your question goes where.
#   "Every y (the dependent variable) is MULTIPLIED by 10. New slope?"  -> b1 20.3
rescale(b1 = 2.03, y_times = 10)                    # b1 = old slope | y_times = what every y is multiplied by
#   "Every x is DIVIDED by 10. New slope?"  -> b1 4.3
rescale(b1 = 0.43, x_times = 1/10)                  # x_times = what every x is multiplied by (divided by 10 = 1/10)
#   "Every x is MULTIPLIED by 10. New slope?"  -> b1 0.04
rescale(b1 = 0.43, x_times = 10)                    # x_times = 10
#   "y measured in DOLLARS instead of $1000s. New intercept and slope?" (y x 1000)  -> 963190, 18500
rescale(b0 = 963.19, b1 = 18.5, y_times = 1000)     # b0 = old intercept | b1 = old slope | y_times = 1000
#   "y measured in $1000s instead of dollars" (y divided by 1000)  -> 963.19, 18.5
rescale(b0 = 963190, b1 = 18500, y_times = 1/1000)  # y_times = 1/1000
#   "x entered as a DECIMAL instead of a percent (15% -> 0.15). New slope?"  -> b1 1850
rescale(b1 = 18.5, x_times = 1/100)                 # x_times = 1/100 (a decimal is the percent / 100)
#   "A LOG model is rescaled" (say which model)  -> b0 11.73, b1 0.26
rescale(b0 = 4.82, b1 = 0.26, y_times = 1000, model = "log-log")   # model = "log-level", "level-log" or "log-log"
#     (in a log model only the intercept moves; the slope on a logged variable never changes)
#   "Using ceosal1 ... salary in dollars instead of thousands. New slope?" (slope taken from data)  -> 15.47
rescale(b1 = lm(salary ~ sales, data = ceosal1)$coefficients[[2]], y_times = 1000)   # [[2]] = the slope
#   "Slope is per month, question asks per year" -> use dx = 12 in loglevel() or delta_y()
#   "New covariance after rescaling" (the correlation never changes)  -> 8.5
cov_rescale(Sxy = 8500, c = 1/1000)                 # Sxy = old covariance | c = what x is multiplied by | d = what y is


# @LOGS  Table 2.3 (Day 13) -------------------------------------------------------------
#   Which model? log(y) on plain x = log-level | log(y) on log(x) = log-log | plain y on log(x) = level-log
#   log(y) on PLAIN x: "By approximately what PERCENT does y change if x rises by 3?"  -> 18
loglevel(b = 0.06, dx = 3)                          # b = slope on that x | dx = change in x. Output is already in %
#     ...two x's change at once  -> -7.2
loglevel(b = c(0.033, -0.051), dx = c(4, 4))        # slopes and changes in the same order
#     ..."if x FALLS by 3"  -> -12
loglevel(b = 0.04, dx = -3)                         # falls -> dx is NEGATIVE
#   log(y) on log(x): "By approximately what PERCENT does y change if x rises 10%?"  -> 1.3
loglog(b = 0.13, pctx = 10)                         # b = slope on log(x) | pctx = % change in x
#     ..."if x FALLS 10%"  -> -8.8
loglog(b = 0.88, pctx = -10)                        # falls -> pctx is NEGATIVE
#   PLAIN y on log(x): "By approximately how many UNITS does y change if x rises 13%?"  -> 1.47
levellog(b = 11.3, pctx = 13)                       # b = slope on log(x) | pctx = % change in x
#   PLAIN y on PLAIN x: "By how much does y change?"  -> 20
delta_y(b = c(3.8, -0.7), dx = c(6, 4))             # b = slopes | dx = change in each x (same order)
#   mixed model, log(sal) ~ log(sales) + roe: a % change in sales -> loglog();  a change in roe -> loglevel()
#   REVERSE, log(y) on x: "How much must x rise for y to rise 10%?"  -> 1.67
r2d(10 / (100 * 0.06))                              # target % change / (100 x slope)
#   REVERSE, log-log: "By what % must x rise for y to rise 5%?"  -> 38.46
r2d(5 / 0.13)                                       # target % change / slope
#   REVERSE, y on log(x): "By what % must x rise for y to rise 2 units?"  -> 17.7
r2d(100 * 2 / 11.3)                                 # 100 x target change in y / slope
#   "What does the 20 mean?" / "Interpret the slope"  -> 0.2 units
means("level-log", b = 20, x = "advertising", y = "sales")   # model: "level-level", "log-level", "level-log", "log-log"
#   "EXACT percent change" (only if the question says exactly)  -> 19.72
r2d(100 * (exp(0.06 * 3) - 1))                      # 100 x (exp(slope x change) - 1)
#   class examples: wage1 log(wage) = 0.584 + 0.083 educ -> about 8.3% per year of educ;
#     ceosal1 log(salary) on log(sales) -> elasticity 0.257 (1% more sales -> 0.257% more salary)


# @PREDICT  predictions, changes, y-bar ----------------------------------------------------
#   "Find the PREDICTED y" from an equation in the question (plain y on the left)  -> 39.8
yhat_at(b0 = 7, b1 = c(2.7, 1.6), x = c(8, 7))     # b0 = intercept | b1 = slopes | x = the x values (same order)
#   "By how much does PREDICTED y change?" from an equation in the question  -> -6.7
delta_y(b = c(0.9, -1.9), dx = c(1, 4))             # b = slopes | dx = change in each x (0 if it doesn't move)
#   "Find y-bar" given the intercept, slopes and x-bars (the line goes through the means)  -> 22.82
yhat_at(b0 = 7.7, b1 = c(1.9, -0.5), x = c(11.3, 12.7))   # x = the x-bars
#   "Predicted y in DOLLARS" when the equation has log(y) on the left  -> 6.13
yhat_log(b0 = 0.28, b = c(0.092, 0.0041, 0.022), x = c(16, 4, 2))   # same as yhat_at, then exp()
#     ...and a log(x) on the right: type log(5000), not 5000  -> 1065
yhat_log(b0 = 4.36, b = c(0.275, 0.0179), x = c(log(5000), 15))
#   don't round before exp(): keep the full coefficients (mreg does this for you)
#   Quadratic y = b0 + b1 x + b2 x^2: "the effect of x at x = 10"  -> 0.18
quad_slope(b1 = 0.3, b2 = -0.006, x = 10)           # b1 = slope on x | b2 = slope on x^2 | x = where
#   y-hat = 12 and u-hat = -3 -> y = 9 (over-predicted).   u-hat = y - y-hat.


# @DATA  any wooldridge regression (prints 4 decimals: round when you type it in) -------------
#   "Using wage1, regress wage on educ, exper and tenure" -> coefficients, se, n, df, R2, SSR, SST, SSE, sigma-hat
mreg(wage ~ educ + exper + tenure, data = wage1)    # y ~ x1 + x2 + ... | data = the dataset. Any NA = perfect collinearity
#   "...What is the PREDICTED wage for educ = 16, exper = 5, tenure = 2?"  -> pred 7.1609
mreg(wage ~ educ + exper + tenure, data = wage1, at = c(educ = 16, exper = 5, tenure = 2))   # at = the x values
#     log(y) on the left -> also prints pred_y = the answer in dollars.  log(x) on the right -> give the plain number
#   "...By about what % does predicted wage change if exper rises 2 and tenure rises 1?"  -> pct_change 3.0309
mreg(log(wage) ~ educ + exper + tenure, data = wage1, dx = c(exper = 2, tenure = 1))   # dx = only the x's that move
#     plain y on the left -> prints change (in y's units) instead of pct_change
#   "...What is the fitted value / residual for observation 12?"
mreg(wage ~ tenure, data = wage1, i = 12)           # i = the row number
#   "What percent of the variation in y is explained?" = 100 x R2 from mreg()   (NOT the SSE)
#   save the coefficients: reg <- mreg(...);  b <- reg$coefficients;  then b[2], b[3] ... (unrounded)
#   "How many observations are in ceosal1?"  -> 209
nrow(ceosal1)
#   "What is the mean of roe?"  -> 17.18
r2d(mean(ceosal1$roe))                              # dataset$variable
#   "What is the correlation between salary and roe?"  -> 0.11
r2d(cor(ceosal1$salary, ceosal1$roe))
#   "What is the mean wage for women?" (mean within a group)  -> mean 4.59, sd 2.53
gmean(y = wage1$wage, group = wage1$female, value = 1)   # y = variable | group = the 0/1 variable | value = which group
#   "Which variables are in wage1?" (what one means: ?wage1)
names(wage1)


# @VAR  variance, sd and se of the slope (Days 16, 20, 21) --------------------------------
#   TRUE sigma given -> sigma = ... (that's the sd).   sigma-hat or SSR given -> sigmahat = ... or SSR = ... (that's the se)
#   "Find sd(b1-hat) / se(b1-hat)" given sigma-hat, n and the SD of x  -> 1.79
se_b(sigmahat = 8.7, n = 15, Sx = 1.3)              # Sx = the SD of x (it gets squared for you)
#   ...given the VARIANCE of x instead of its SD  -> 0.4
se_b(sigma = 4, n = 26, Sx2 = 4)                    # Sx2 = the VARIANCE of x (not squared again)
#   ...given the x values and SSR  -> 0.6
se_b(x = c(2, 4, 5, 6, 12), SSR = 61)               # x = the x values | SSR = sum of squared residuals
#   ...given sigma and the x values  -> 0.34
se_b(sigma = 2, x = c(2, 3, 4, 7, 9))
#   ...given SSR, n and SST_x  -> 1.82
se_b(SSR = 437, n = 13, SSTx = 12)                  # SSTx = sum of squares of x (NOT of y)
#   "Find Var(b1-hat)" (the VARIANCE, not the sd)  -> Var_b 1.17
se_b(sigma = sqrt(28), SSTx = 24)                   # read Var_b. If sigma^2 = 28 is given, sigma = sqrt(28)
#   "Find sigma-hat^2 / sigma-hat" in a SIMPLE regression, given SSR and n  -> sigma2hat 20.25, sigmahat 4.5
sigma_hat(SSR = 810, n = 42)                        # simple regression: df = n - 2 automatically
#   "The sample is 4 times as large. New sd?"  -> 0.98
sd_scale(sd = 1.95, n_times = 4)                    # n_times = new n / old n
#   "The sample is 1/4 as large. New sd?"  -> 1.96
sd_scale(sd = 0.98, n_times = 1/4)
#   "sigma doubles. New sd?"  -> 0.8
sd_scale(sd = 0.4, sigma_times = 2)                 # sigma_times = new sigma / old sigma
#   "Find the se of the INTERCEPT" given the x values and SSR  -> 4.01
se_b0(x = c(2, 4, 5, 6, 12), SSR = 61)
#     ...given sigma-hat, n, the sum of x^2 and SST_x  -> 11.98
se_b0(sigmahat = 10.02, n = 10, sum_x2 = 400, SSTx = 28)   # sum_x2 = sum of x^2 (not squared deviations)
#   REVERSE: "The se is 1.47. Find SST_j"  -> 64.91
solve_SST(se = 1.47, sigma = 9.4, Rj2 = 0.37)       # Rj2 = 0 in a simple regression
#   REVERSE: "The se is 1.89. Find sigma-hat"  -> 10
solve_sigma(se = 1.89, SSTx = 28)
#   REVERSE: "The se is 1.47. Find R_j^2"  -> 0.37
solve_Rj2(se = 1.47, sigma = 9.4, SSTx = 65)
#   Gus's sheet: "n = 10, SSR = 804, SST_x = 28. Find sigma-hat and se"  -> sigma 10.02, se 1.89
se_b(SSR = 804, n = 10, SSTx = 28)
#   Why prefer the smaller variance? Its estimates land closer to beta from sample to sample (more reliable).


# @PS12  standard errors in multiple regression (exam follows PS12 #2, 4, 6, 9, 12) -----------
#   WHAT TO KNOW
#   X_j = the x whose coefficient's se you want.  SST_j = sum((x_j - mean(x_j))^2) = (n - 1) * S_xj^2
#   R_j^2 = R^2 of x_j regressed on the OTHER x's. y is NOT in that regression.
#   df = n - k - 1 (k = number of x's). sigma-hat = sqrt(SSR / df) ("Residual standard error" in summary()).
#   se(bj) = sigma-hat / sqrt(SST_j * (1 - R_j^2)).  VIF = 1 / (1 - R_j^2).  R_j^2 -> 1: se -> infinity.
#   NAs: lm() drops those rows, so compute every piece on the rows the regression used (se_parts() does).
#   MATCH THE QUESTION:
#   "Which regression gives R_j^2 for EXPER?" / "Find R_j^2 for exper" (in wage ~ educ + exper + tenure)
summary(lm(exper ~ educ + tenure, data = wage1))$r.squared   # left of ~ = the x asked about | right = the OTHER x's
#   "Find sigma-hat" / "the standard error of the regression", given SSR, n and how many x's  -> sigmahat 8.56
sigma_hat(SSR = 2857, n = 42, k = 2)                # SSR = sum of squared residuals | n = observations | k = number of x's
#   "Find se(b_j-hat)" given sigma-hat, SST_j and R_j^2  -> 1.47
se_b(sigmahat = 9.4, SSTx = 65, Rj2 = 0.37)         # sigmahat = sigma-hat | SSTx = SST_j | Rj2 = R_j^2
#   "Using hprice1, regress price on sqrft, bdrms, lotsize. Find df / sigma-hat / SST_j / R_j^2 / se for bdrms"  -> 84, 59.83, 61.59, 0.28, 9.01
se_parts(price ~ sqrft + bdrms + lotsize, data = hprice1, x = "bdrms")   # the whole regression | x = the variable asked about
#   "...Check it against summary()": the Std. Error of bdrms  -> 9.01
summary(lm(price ~ sqrft + bdrms + lotsize, data = hprice1))$coefficients[3, 2]   # [row, 2]: row 1 = intercept, then x's in order
#   "Find SST_j" given n = 20 and the SD of x_j = 1.3  -> 32.11
r2d((20 - 1) * 1.3^2)                               # (n - 1) x SD^2.  Given the VARIANCE: (n - 1) x variance
#   "Find the VIF" given R_j^2  -> 1.59
vif(Rj2 = 0.37)
#   class example (Day 21): "se of educ in wage ~ educ + exper"  -> se 0.0538
se_parts(wage ~ educ + exper, data = wage1, x = "educ")


# @OVB  omitted-variable bias (Days 15, 19, 20) -------------------------------------------
#   WHAT TO KNOW
#   Squiggly (tilde, ~) = SIMPLE regression that leaves x2 out.  Hatty (hat, ^) = MULTIPLE regression with x2 in.
#   (3.23)  b1-tilde = b1-hat + b2-hat * d1-tilde      d1-tilde = slope from regressing x2 (omitted) on x1 (included)
#   bias = E[b1-tilde] - beta1 = beta2 * delta1. Two-legged table: leg 1 = beta2 (x2's effect on y),
#   leg 2 = delta1 (how x2 moves with x1). Multiply the signs. Story in words: what x2 does to y + how it moves with x1.
#   Examples: wage ~ educ, ability omitted (+)(+) UP | yield ~ fertilizer, land quality (+)(+) UP
#     scores ~ tutoring, ability, weaker students tutor more (+)(-) DOWN | scores ~ class size, income (+)(-) DOWN
#     bwght ~ cigs: simple -0.51, with faminc -0.46, the other -0.05 is income riding along
#     wage1 omit exper: beta2 = +0.07, corr(educ, exper) = -0.30 -> 0.54 < 0.64 DOWN
#     gpa1: colGPA ~ ACT 0.027 alone; with hsGPA 0.0094; hsGPA = 2.46 + 0.039 ACT; 0.0094 + 0.453 x 0.039 = 0.027
#     meap93 math10 ~ lnchprg = -0.319: "wrong sign" from a confounder (poverty)
#   MATCH THE QUESTION:
#   "x2 is left out. Is the slope on x1 biased upward or downward?" (a story)  -> "biased downward"
ovb(b2 = 1, cor = -1)       # b2 = +1 if the LEFT-OUT variable raises y, -1 if it lowers y | cor = +1 moves WITH x1, -1 AGAINST
#   all four: ovb(1, 1) UP | ovb(-1, -1) UP | ovb(1, -1) DOWN | ovb(-1, 1) DOWN | ovb(0, 1) unbiased
#   Table 3.2 (the 2 x 2 sign table)
table32()
#   (3.23) "Find the SIMPLE slope b1-tilde" given b1-hat, b2-hat and d1-tilde  -> 2.1
f323(b1_hat = 1.28, b2_hat = 1.47, d1_tilde = 0.56)        # give the 3 you know; leave out the one asked for
#   (3.23) "Find the MULTIPLE slope b1-hat" given b1-tilde, b2-hat and d1-tilde  -> 0.13
f323(b1_tilde = -0.25, b2_hat = 0.72, d1_tilde = -0.53)    # b1_tilde = SIMPLE slope | b2_hat = multiple slope on x2
#   (3.23) "Find b2-hat" (the multiple slope on x2)  -> 0.46
f323(b1_tilde = 0.84, b1_hat = 0.61, d1_tilde = 0.5)       # d1_tilde = slope from regressing x2 on x1
#   (3.23) "Find d1-tilde" (the slope from regressing x2 on x1)  -> 0.5
f323(b1_tilde = 0.84, b1_hat = 0.61, b2_hat = 0.46)
#   "Find the bias" given beta2 and delta1 (negative = biased DOWN)  -> -0.57
r2d(1.97 * -0.29)                                          # beta2 x delta1
#   "Using bwght, compare bwght on cigs with bwght on cigs and faminc" / "how much does faminc ride along?"  -> -0.5138, -0.4634, -0.0504
ovb_parts(y = "bwght", x1 = "cigs", x2 = "faminc", data = bwght)   # x1 = the variable KEPT | x2 = the one LEFT OUT (in quotes)
#     prints the simple slope, the multiple slope, and b2-hat x d1-tilde (= simple minus multiple)


# @MLR  multiple regression + OLS facts (Days 17-19) -----------------------------------------
#   bj = change in y-hat from +1 in xj HOLDING THE OTHER x's FIXED (ceteris paribus). Not "letting exper change too".
#   prediction: plug in every x.  change: sum of bj * dxj (intercept drops out; a fixed x has dx = 0)
#   ALWAYS (with an intercept): residuals average 0 | cov(each xj, residuals) = 0 | (x-bar's, y-bar) on the line
#   NOT always: cov(y, residuals) = 0 | every residual 0 | residuals add up to SSR
#   R^2 never goes down when you add an x. Example 3.1 (gpa1): 1.29 + 0.0094 ACT + 0.453 hsGPA (+10 ACT -> 0.09)
#   Example 3.2 (wage1): log(wage) = 0.284 + 0.092 educ + 0.0041 exper + 0.022 tenure (+1 educ -> 9.2%)
#   simple vs multiple: they differ unless b2-hat = 0 or the x's are uncorrelated in the sample  -> @OVB


# @ASSUME  assumptions, unbiasedness, Gauss-Markov (Days 15, 18, 21) ---------------------------
#   SLR1 linear in parameters | SLR2 random sample | SLR3 x varies (not all the same) | SLR4 E[U | X] = 0
#   SLR5 Var(U | X) = sigma^2.     MLR1-MLR5 same idea, MLR3 = no PERFECT collinearity (and n >= k + 1).
#   MLR6 normality (Chapter 4, t-tests: NOT on this exam).
#   Thm 2.1 / 3.1 (1-4): unbiased.  Thm 2.2 / 3.2 (1-5): the variance formulas.
#   Thm 2.3 / 3.3 (1-5): E[sigma-hat^2] = sigma^2 (sigma-hat^2 is unbiased).
#   Thm 3.4 Gauss-Markov (1-5): OLS is BLUE.
#   Simulation demos (class): estimates scatter AROUND the truth (unbiased); something in U that moves with x
#     shifts the whole cloud (bias); bigger sigma -> wider spread; bigger n -> narrower spread, same center.
#   Every worker has 12 years of educ -> SLR3 fails: SST_x = 0, slope can't be computed.
#   "Linear" regression = linear in the PARAMETERS, so log(x), x^2 are allowed.
#   Why not add every variable? It can raise the variance (and can even break MLR3). An irrelevant x causes no bias.
#   Names: y = dependent / explained; x = independent / explanatory / regressor; u = error / disturbance;
#   beta0 = intercept parameter; beta1 = slope parameter. OLS is a METHOD, not a model.
#   Errors u are never observed; residuals u-hat are computed from the data.


# @BEFORE1  Exam I material (a few questions) -- also every Exam I miss --------------------
#   data LIST -> mean(), sd(), var() (divide by n - 1) | probability TABLE -> weight by p: rv(), joint(), cond_E()
#   "Find the mean / variance / sd of 12, 1, 12, 3" (a data list)  -> sd 5.83
desc(x = c(12, 1, 12, 3))                           # type the numbers inside c( )
#     ...plus the covariance and correlation of two lists
desc(x = c(5, 0, 6), y = c(3, 0, 1))
#   "Find the mean of 18 numbers: the first is 43 and the rest are 2"  -> 4.28
mean_ab(a = 43, b = 2, n = 18)                      # a = the odd one | b = all the others | n = how many numbers
#   "Find the correlation" given Cov, an SD and a variance  -> 0.46
cor_from(Cov = 11, sdX = 3, VarY = 64)              # sdX / sdY = SDs, VarX / VarY = variances: use what you're given
#   "Find the percent change from 3.4 to 4.5"  -> 32.35
pct_change(old = 3.4, new = 4.5)
#   "Find the percentage-POINT change from 5% to 4%"  -> -1
pp_change(old = 5, new = 4)
#   "Find E[X] / Var(X) / sd(X)" from a probability TABLE  -> EX 1.6, VarX 3.64
rv(x = c(0, 2, 5), p = c(0.5, 0.3, 0.2))            # x = the values | p = their probabilities
#   "Find Cov(X, Y) / Cor(X, Y)" from a JOINT table (also prints the population slope b1)  -> Cor 0.23
joint(x = c(2, 4, 5, 8), y = c(11, 2, 6, 8), p = c(.1, .4, .3, .2))   # one entry per ROW: its x, its y, its probability
#   "Find E[Y | X = x]" (the Y values and probabilities GIVEN X = x)  -> 17.1
cond_E(y = c(9, 22, 30), p = c(.5, .3, .2))
#   "Find Var(Y | X = x)"  -> 18
cond_Var(y = c(4, 10, 16), p = c(.25, .5, .25))
#   "Find E[3Y + 6 | X = x]" (apply the 3 and the 6)  -> 57.3
E_aX_b(a = 3, EX = cond_E(y = c(9, 22, 30), p = c(.5, .3, .2)), b = 6)
#   "Find sd(4X - 4)" given E[X] and E[X^2]  -> 13.86
sd_aX_b(a = 4, EX = 5, EX2 = 37)                    # a = the number times X (b never matters for sd or Var)
#   "Find Var(3X + b)" given Var(X)  -> 54
Var_aX_b(a = 3, VarX = 6)
#   "Find Var(2X - 2Y)" given Var(X), Var(Y) and Cov(X, Y)  -> 83.2
Var_aX_bY(a = 2, VarX = 10, b = -2, VarY = 4, Cov = -3.4)   # a = number on X | b = number on Y (minus -> negative)
#   "Find Cov(5X + 1, 4Y - 2)" given Cov(X, Y)  -> -20
Cov_a1X_a2Y(a1 = 5, a2 = 4, Cov = -1)               # a1 = number on X | a2 = number on Y
#   "Fit the OLS line to x = 2, 4, 5, 8, 10 and y = 8, 9, 3, 4, 5" (slope, intercept)  -> b1 -0.47
ols(x = c(2, 4, 5, 8, 10), y = c(8, 9, 3, 4, 5))
#   "Find the residual for observation 3"  -> -5.57
x <- c(2, 3, 4, 5, 8); y <- c(12, 11, 3, 7, 9); r2d(lm(y ~ x)$residuals[3])   # [3] = which observation
#   "Find SST" given n = 25 and the sample variance of y = 22  -> 528
r2d((25 - 1) * 22)                                  # (n - 1) x sample variance of y
#   "Find SSR" given SSE = 122 and R^2 = 0.22  -> SSR 432.55
ss(SSE = 122, R2 = 0.22)                            # give any two of SST, SSE, SSR, R2
#   "Find y-bar" given b0 = 4, b1 = 1.2 and x-bar = 10  -> 16
yhat_at(b0 = 4, b1 = 1.2, x = 10)
#   "Four residuals are given. Find the fifth" (they sum to 0)  -> 7
r2d(-sum(c(-0.3, -2.4, -1.8, -2.5)))
#   "Find the sum of x times the residuals" -> 0, always (R may print -8.7e-15: that's 0)
#   "Which line is the OLS line?" (sum of residuals and sum of squared residuals for lines A, B, C, D)  -> "B"
pick_ols(sum_u = c(0, 0, -0.5, 1), sum_u2 = c(3.30, 2.40, 2.55, 3.00))   # sum_u = sum of residuals | sum_u2 = sum of SQUARED
#   "Correlation after changing units" -> unchanged (Exam I Q12: 0.13)
#   "Find the slope" given the correlation and the two SDs  -> 1.5
r2d(0.6 * 5 / 2)                                    # cor x sd_y / sd_x
#   population slope from a joint table = joint(...)[["b1"]] (= Cov / VarX)
#   "Using wage1, regress wage on tenure. Residual for row 2?"  -> resid -2.11
mreg(wage ~ tenure, data = wage1, i = 2)
#   "Using bwght, regress bwght on faminc. Find the SSE"  -> SSE 6819.05
mreg(bwght ~ faminc, data = bwght)
#   "...How much does predicted bwght change if faminc rises by 22?"  -> 2.6
delta_y(b = 0.1183, dx = 22)                        # b = the slope | dx = the change in x
#   population quantity (mu_Y) = a fixed number;  sample statistic (Y-bar) = a random variable


# @TTEST  t-tests -- NOT ON EXAM II (study guide). Kept for the final. -------------------------
#   t, critical value, decision  -> t 10.8, REJECT
ttest(bhat = 0.54, se = 0.05, df = 524)
#   given t and c  -> FAIL TO REJECT
ttest(t = -3, c = 1.68)                             # h1 = "<" or h1 = "two" for the other alternatives
#   critical value  -> 1.65
crit(df = 522, alpha = 0.05, sides = 1)
#   t = b-hat / se; H0: beta = a -> t = (b-hat - a) / se; one-sided reject if t > c; two-sided if |t| > c;
#   p-value < alpha -> reject; summary column 3 = t, column 4 = two-sided p.
#   large df: 5% one-sided c = 1.645, two-sided c = 1.96. Big NEGATIVE t with H1: beta > 0 -> fail to reject.
}   # end of the one-liner menus


# @BANK  every PS 8-12 / Quiz 7-10 / Exam I question, worked (Source skips this block) -------------
# Click a line, Cmd/Ctrl + Enter. 'FLIP:' lines = the same question reversed (the exam likes doing that).
if (FALSE) {

# @BANK-PS8  Problem Set 8, Q13-21 (Day 13: units + logs) ------------------------------------

# Q13 (PS8)
# slope 2.03, every y multiplied by 10 -> both b0 and b1 x 10
rescale(b1 = 2.03, y_times = 10)                    # -> 20.3
# FLIP: every y DIVIDED by 10
rescale(b1 = 2.03, y_times = 1/10)                  # -> 0.2

# Q14 (PS8)
# slope 0.43, every x DIVIDED by 10 -> x_times = 1/10 -> slope x 10
rescale(b1 = 0.43, x_times = 1/10)                  # -> 4.3
# FLIP: every x MULTIPLIED by 10
rescale(b1 = 0.43, x_times = 10)                    # -> 0.04

# Q15 (PS8)
# every x multiplied by 10: which is right?
# answer: the slope is divided by 10, the intercept does not change, and R^2 does not change
# FLIP: every x DIVIDED by 10 -> slope x 10, intercept and R^2 the same. Every y x 10 -> BOTH b0 and b1 x 10

# Q16 (PS8)
# ceosal1 salary on sales, salary in DOLLARS instead of thousands -> slope x 1000
reg <- mreg(salary ~ sales, data = ceosal1)
rescale(b1 = reg$coefficients[["sales"]], y_times = 1000)   # -> 15.47
# FLIP: sales in BILLIONS instead of millions (every x divided by 1000) -> slope x 1000
rescale(b1 = reg$coefficients[["sales"]], x_times = 1/1000)   # -> 15.47

# Q17 (PS8)
# log-level 1.6 + 0.06x, x rises by 3: %dy = 100 * b * dx
loglevel(b = 0.06, dx = 3)                          # -> 18
# FLIP: x FALLS by 3
loglevel(b = 0.06, dx = -3)                         # -> -18
# FLIP: how much must x rise for y to rise 12%?  dx = %dy / (100 b)
r2d(12 / (100 * 0.06))                              # -> 2

# Q18 (PS8)
# log-log 4.4 + 0.13 log(x), x rises 10%: %dy = b * %dx
loglog(b = 0.13, pctx = 10)                         # -> 1.3
# FLIP: x FALLS 10%
loglog(b = 0.13, pctx = -10)                        # -> -1.3

# Q19 (PS8)
# level-log 87 + 11.3 log(x), x rises 13%: dy = b/100 * %dx
levellog(b = 11.3, pctx = 13)                       # -> 1.47
# FLIP: x FALLS 13%
levellog(b = 11.3, pctx = -13)                      # -> -1.47

# Q20 (PS8)
# log(rent) = 2.1 + 0.45 log(income): what does 0.45 mean?
means("log-log", b = 0.45, x = "income", y = "rent")
# answer: a 1% rise in income is associated with about a 0.45% rise in rent
# FLIP: rent = 2.1 + 0.45 log(income) (rent NOT logged) -> level-log: 1% rise -> 0.0045 units
means("level-log", b = 0.45, x = "income", y = "rent")

# Q21 (PS8)
# wage1 log(wage) on exper: % change in wage from 10 more years of exper
mreg(log(wage) ~ exper, data = wage1, dx = c(exper = 10))   # -> pct_change 4.36
# FLIP: 10 FEWER years
mreg(log(wage) ~ exper, data = wage1, dx = c(exper = -10))  # -> pct_change -4.36


# @BANK-PS9  Problem Set 9, all 19 (Quiz 8 drew PS8 #14, 17, 19 and PS9 #1, 5, 6, 9, 13-16) -----

# Q1 (PS9)
# log-log salary on log(sales), salary changed from thousands to dollars (x 1000): what happens?
rescale(b0 = 4.82, b1 = 0.26, y_times = 1000, model = "log-log")
# answer: b1 does not change; only b0 changes
# FLIP: SALES rescaled instead (x in a log): still only b0 changes (b0 - b1 log c). Slopes of logged variables never move.

# Q2 (PS9)
# log(wage) on tenure in MONTHS, slope 0.0032: % change from one more YEAR -> dx = 12
loglevel(b = 0.0032, dx = 12)                       # -> 3.84
# FLIP: slope is per YEAR (0.04), asked per MONTH -> dx = 1/12
loglevel(b = 0.04, dx = 1/12)                       # -> 0.33

# Q3 (PS9)
# log(salary) = 5.02 + 0.25 log(sales), salary now in dollars: new intercept = 5.02 + log(1000)
rescale(b0 = 5.02, b1 = 0.25, y_times = 1000, model = "log-log")   # -> new_b0 11.93
# FLIP: salary from DOLLARS back to thousands -> b0 - log(1000)
rescale(b0 = 11.93, b1 = 0.25, y_times = 1/1000, model = "log-log")   # -> new_b0 5.02

# Q4 (PS9)
# which model reads as %dy = 100 * b1 * dx?
# answer: log-level, log(y) = b0 + b1 x + u
# FLIP: %dy = b1 %dx -> log-log | dy = (b1/100) %dx -> level-log | dy = b1 dx -> level-level

# Q5 (PS9)
# log-log 2.7 + 0.88 log(x), x FALLS by 10%
loglog(b = 0.88, pctx = -10)                        # -> -8.8
# FLIP: x RISES 10%
loglog(b = 0.88, pctx = 10)                         # -> 8.8

# Q6 (PS9)
# ceosal1 log(salary) on roe: % change in salary when roe rises 5 points
mreg(log(salary) ~ roe, data = ceosal1, dx = c(roe = 5))    # -> pct_change 6.93
# FLIP: roe FALLS 5 points
mreg(log(salary) ~ roe, data = ceosal1, dx = c(roe = -5))   # -> pct_change -6.93

# Q7 (PS9)
# ceosal1 salary on log(sales) (level-log): change in salary ($1000s) when sales rise 10%
reg <- mreg(salary ~ log(sales), data = ceosal1)
levellog(b = reg$coefficients[2], pctx = 10)        # -> 26.29

# Q8 (PS9)
# Theorem 2.1, E[b1-hat] = beta1, means?
# answer: averaged over many random samples, b1-hat equals beta1 (NOT: every sample gives beta1 exactly)
# FLIP: 'which is NOT what unbiased means?' -> every sample gives beta1 exactly / no spread / R^2 = 1

# Q9 (PS9)
# omitted-variable bias in wage on educ: what's hiding in U?
# answer: the worker's ability (raises wages AND is higher for more-educated people)
# FLIP: something in U that does NOT cause bias -> unrelated to educ (survey ID, day of the week)

# Q10 (PS9)
# every worker has exactly 12 years of educ, regress wage on educ: what goes wrong?
# answer: SLR3 fails -- the X values are all the same, so the OLS slope can't be computed
# FLIP: x varies but ability is in U and goes with educ -> SLR4 fails (estimate exists but is biased)

# Q11 (PS9)
# sigma^2 = 28, SST_x = 24: Var(b1-hat) = sigma^2 / SST_x
r2d(28 / 24)                                        # -> 1.17
# FLIP: the SD of b1-hat instead of the variance -> square root
r2d(sqrt(28 / 24))                                  # -> 1.08

# Q12 (PS9)
# sigma = 14, n = 9, sample VARIANCE of X = 6: sd(b1-hat)
se_b(sigma = 14, n = 9, Sx2 = 6)                    # -> sd_or_se 2.02
# FLIP: given the SD of X = 6 (not the variance) -> Sx, it gets squared
se_b(sigma = 14, n = 9, Sx = 6)                     # -> sd_or_se 0.82

# Q13 (PS9)
# what makes Var(b1-hat) smaller?
# answer: more spread-out values of X (a larger SST_x)
# FLIP: LARGER -> x bunched together (smaller SST_x) | bigger sigma^2 | smaller n. Never: the true beta1

# Q14 (PS9)
# sd(b1-hat) from sigma = 2 and the x values
se_b(sigma = 2, x = c(2, 3, 4, 7, 9))              # -> sd_or_se 0.34

# Q15 (PS9)
# estimate of the error variance: sigma-hat^2 = SSR / (n - 2)
sigma_hat(SSR = 810, n = 42)                        # -> sigma2hat 20.25
# FLIP: sigma-hat itself (not squared) -> same output, sigmahat 4.5

# Q16 (PS9)
# se(b1-hat) from SSR, n and SST_x
se_b(SSR = 437, n = 13, SSTx = 12)                  # -> sd_or_se 1.82

# Q17 (PS9)
# food spending on income, spread around the line grows with income: which is right?
# answer: SLR5 (homoskedasticity) fails, so Var(b1) = sigma^2 / SST_x no longer applies (OLS is still unbiased)
# FLIP: same spread at every income -> SLR5 holds. SLR5 failing never causes BIAS

# Q18 (PS9)
# wage1 wage on educ: sigma-hat, the standard error of the regression
mreg(wage ~ educ, data = wage1)                     # -> sigmahat 3.38

# Q19 (PS9)
# wage1: SST_x for educ
r2d(sum((wage1$educ - mean(wage1$educ))^2))         # -> 4025.43


# @BANK-QUIZ8  Quiz 8 (10/21, you got 10/11) ---------------------------------------------------

# Q1 (Quiz 8)
# log(salary) on log(sales), SALES now in thousands (x 1000): what happens?
rescale(b0 = 4.82, b1 = 0.26, x_times = 1000, model = "log-log")
# FLIP: SALARY rescaled -> also only b0 changes (b0 + log c)
# answer: b1 does not change; only b0 changes

# Q2 (Quiz 8)
# slope 0.62, every x divided by 100
rescale(b1 = 0.62, x_times = 1/100)                 # -> 62

# Q3 (Quiz 8) -- the one you missed (typed 0.138: forgot the x 100)
# log-level 2.2 + 0.046x, x rises by 3
loglevel(b = 0.046, dx = 3)                         # -> 13.8
# FLIP: x FALLS by 3
loglevel(b = 0.046, dx = -3)                        # -> -13.8

# Q4 (Quiz 8)
# log-log 3.8 + 0.33 log(x), x FALLS by 10%
loglog(b = 0.33, pctx = -10)                        # -> -3.3

# Q5 (Quiz 8)
# level-log 148 + 27.5 log(x), x rises 20%
levellog(b = 27.5, pctx = 20)                       # -> 5.5

# Q6 (Quiz 8)
# ceosal1 log(salary) on roe, roe rises 6 points
mreg(log(salary) ~ roe, data = ceosal1, dx = c(roe = 6))    # -> pct_change 8.32

# Q7 (Quiz 8)
# test score on class size: most likely source of omitted-variable bias?
# answer: family income (raises scores AND related to class size). NOT: weather (unrelated to class size), wall color, IDs
# FLIP: which would NOT cause bias? -> weather on test day, wall color, student ID (unrelated to class size)

# Q8 (Quiz 8)
# which change makes Var(b1-hat) LARGER?
# answer: a larger error variance sigma^2
# FLIP: SMALLER -> smaller sigma^2 | bigger n | more spread-out x (bigger SST_x)

# Q9 (Quiz 8)
# sigma = 8, x = (1, 2, 5, 7, 8): sd(b1-hat)
se_b(sigma = 8, x = c(1, 2, 5, 7, 8))              # -> sd_or_se 1.31

# Q10 (Quiz 8)
# n = 60, SSR = 639: sigma-hat^2
sigma_hat(SSR = 639, n = 60)                        # -> sigma2hat 11.02
# FLIP: sigma-hat (not squared) -> same output, sigmahat 3.32

# Q11 (Quiz 8)
# n = 9, SSR = 391, SST_x = 22: se(b1-hat)
se_b(SSR = 391, n = 9, SSTx = 22)                   # -> sd_or_se 1.59


# @BANK-PS10  Problem Set 10 (Quiz 9 drew #1, 2, 3, 4, 6, 7, 9, 12, 13, 14, 15) ----------------

# Q1 (PS10)
# test score = 12 + 8 log(income): what does the 8 mean? (level-log)
means("level-log", b = 8, x = "income", y = "test score")
# answer: a 1% rise in income -> rise of about 0.08 points in the predicted test score

# Q2 (PS10)
# log(y) = 1.4 + 0.04x, x FALLS by 3
loglevel(b = 0.04, dx = -3)                         # -> -12
# FLIP: x RISES by 3
loglevel(b = 0.04, dx = 3)                          # -> 12

# Q3 (PS10)
# bwght log-log on faminc: % change in birth weight when family income rises 20%
reg <- mreg(log(bwght) ~ log(faminc), data = bwght)
loglog(b = reg$coefficients[2], pctx = 20)          # -> 0.41

# Q4 (PS10)
# 1,000 samples, OLS unbiased (SLR1-SLR4): which is correct?
# answer: the average of the 1,000 estimates is close to beta1 (they are NOT all equal to it)

# Q5 (PS10)
# why prefer an unbiased estimator with a smaller variance?
# answer: its estimates land closer to beta1 from sample to sample, so any one estimate is more reliable

# Q6 (PS10)
# which changes make Var(b1-hat) LARGER? (select all)
# answer: a larger error variance | a smaller sample (smaller SST_x) | x values bunched together (smaller SST_x)
# NOT: a larger sample, a larger true slope (beta1 isn't in the formula)
# FLIP (Quiz 9 Q5 asked it this way): SMALLER -> smaller sigma^2 | bigger n | more spread-out x (bigger SST_x)

# Q7 (PS10)
# n = 8, sigma-hat = 5.9, sample SD of X = 2.1: se(b1-hat)
se_b(sigmahat = 5.9, n = 8, Sx = 2.1)               # -> sd_or_se 1.06

# Q8 (PS10)
# five x values and SSR = 61: se(b1-hat)
se_b(x = c(2, 4, 5, 6, 12), SSR = 61)               # -> sd_or_se 0.6

# Q9 (PS10)
# sd(b1-hat) = 1.64, sample 25 times as large -> sd / sqrt(25)
sd_scale(sd = 1.64, n_times = 25)                   # -> 0.33
# FLIP: sample 1/25 as large -> sd x 5
sd_scale(sd = 1.64, n_times = 1/25)                 # -> 8.2
# FLIP: 4x the sample AND sigma doubles -> cancels
sd_scale(sd = 1.64, n_times = 4, sigma_times = 2)  # -> 1.64

# Q10 (PS10)
# ceosal1 salary on roe: se(b1-hat) by hand, check against summary
se_parts(salary ~ roe, data = ceosal1, x = "roe")   # -> se_by_hand 11.1233 = se_summary

# Q11 (PS10)
# wage = b0 + b1 educ + b2 exper: what does b1 measure?
# answer: the predicted change in wage from one more year of education, HOLDING EXPERIENCE FIXED
# FLIP: what does b2 measure? -> change in wage from +1 year of exper, holding EDUC fixed

# Q12 (PS10)
# y-hat = 10 + 3.6 x1 + 0.9 x2 at x1 = 5, x2 = 1
yhat_at(b0 = 10, b1 = c(3.6, 0.9), x = c(5, 1))     # -> 28.9

# Q13 (PS10)
# y-hat = 7 + 3.8 x1 - 0.7 x2, x1 rises 6 and x2 rises 4 (intercept drops out)
delta_y(b = c(3.8, -0.7), dx = c(6, 4))             # -> 20

# Q14 (PS10)
# wage1 wage on educ, exper, tenure: predicted wage at educ 14, exper 10, tenure 5
mreg(wage ~ educ + exper + tenure, data = wage1, at = c(educ = 14, exper = 10, tenure = 5))   # -> pred 6.58

# Q15 (PS10)
# same regression: change in wage when tenure rises 3 (QUIZ VERSION asked about exper -> read which variable moves!)
mreg(wage ~ educ + exper + tenure, data = wage1, dx = c(tenure = 3))   # -> change 0.51
mreg(wage ~ educ + exper + tenure, data = wage1, dx = c(exper = 3))    # -> change 0.07

# Q16 (PS10)
# ceosal1 log(salary) on log(sales) + roe: % change in salary when roe rises 10 (roe term is log-level)
mreg(log(salary) ~ log(sales) + roe, data = ceosal1, dx = c(roe = 10))   # -> pct_change 17.87

# Q17 (PS10)
# same regression: predicted salary ($1000s) at sales = 5000, roe = 15 -> exp() is automatic
mreg(log(salary) ~ log(sales) + roe, data = ceosal1, at = c(sales = 5000, roe = 15))   # -> pred_y 1067.66

# Q18 (PS10)
# (3.23): b1_hat = 1.28, b2_hat = 1.47, delta1 = 0.56 -> the SIMPLE slope
f323(b1_hat = 1.28, b2_hat = 1.47, d1_tilde = 0.56) # -> b1_tilde 2.1
# FLIP: given the SIMPLE slope, find the MULTIPLE slope
f323(b1_tilde = 2.1032, b2_hat = 1.47, d1_tilde = 0.56)   # -> b1_hat 1.28

# Q19 (PS10)
# bias of the simple slope = beta2 * delta1 (negative = biased downward)
r2d(1.97 * -0.29)                                   # -> -0.57
# FLIP: beta2 = -1.97, delta1 = -0.29 -> positive bias (UP)
r2d(-1.97 * -0.29)                                  # -> 0.57

# Q20 (PS10)
# omitted x2 LOWERS y (beta2 < 0), x2 POSITIVELY correlated with x1
ovb(b2 = -1, cor = 1)                               # -> biased downward
# FLIP: x2 RAISES y, positively correlated -> (+)(+)
ovb(b2 = 1, cor = 1)                                # -> biased upward

# Q21 (PS10)
# wage1: b2-hat (exper) x delta1 (exper on educ)
ovb_parts(y = "wage", x1 = "educ", x2 = "exper", data = wage1)   # -> b2hat_x_d1tilde -0.10


# @BANK-QUIZ9  Quiz 9 (10/28, you got 10/11) ---------------------------------------------------

# Q1 (Quiz 9)
# sales = 50 + 20 log(advertising): what does the 20 mean?
means("level-log", b = 20, x = "advertising", y = "sales")
# answer: a 1% rise in advertising -> rise of about 0.20 units in predicted sales

# Q2 (Quiz 9)
# log(y) = 2.5 + 0.028x, x FALLS by 1
loglevel(b = 0.028, dx = -1)                        # -> -2.8

# Q3 (Quiz 9)
# bwght log-log on faminc, family income rises 15%
reg <- mreg(log(bwght) ~ log(faminc), data = bwght)
loglog(b = reg$coefficients[2], pctx = 15)          # -> 0.31

# Q4 (Quiz 9)
# 1,000 researchers, SLR1-SLR4: which is correct?
# answer: they get different estimates, but their average is close to beta1

# Q5 (Quiz 9)
# which changes make Var(b1-hat) SMALLER? (select all)
# answer: more spread-out X values (a larger SST_x) | a smaller error variance
# FLIP: LARGER -> bigger error variance | x bunched together | smaller n

# Q6 (Quiz 9)
# n = 15, sigma-hat = 8.7, sample SD of X = 1.3: se(b1-hat)
se_b(sigmahat = 8.7, n = 15, Sx = 1.3)              # -> sd_or_se 1.79

# Q7 (Quiz 9)
# sd(b1-hat) = 1.95, sample 4 times as large
sd_scale(sd = 1.95, n_times = 4)                    # -> 0.98
# FLIP: sample 1/4 as large -> sd x 2
sd_scale(sd = 1.95, n_times = 1/4)                  # -> 3.9

# Q8 (Quiz 9)
# y-hat = 7 + 2.7 x1 + 1.6 x2 at x1 = 8, x2 = 7
yhat_at(b0 = 7, b1 = c(2.7, 1.6), x = c(8, 7))      # -> 39.8

# Q9 (Quiz 9)
# y-hat = 4 + 0.9 x1 - 1.9 x2, x1 rises 1 and x2 rises 4
delta_y(b = c(0.9, -1.9), dx = c(1, 4))             # -> -6.7

# Q10 (Quiz 9) -- the one you missed
# wage1 wage (NOT logged) on educ, exper, tenure: predicted wage at educ 16, exper 5, tenure 2
mreg(wage ~ educ + exper + tenure, data = wage1, at = c(educ = 16, exper = 5, tenure = 2))   # -> pred 7.16

# Q11 (Quiz 9)
# same regression: change in wage when exper rises 10
mreg(wage ~ educ + exper + tenure, data = wage1, dx = c(exper = 10))   # -> change 0.22


# @BANK-PS11  Problem Set 11 (Quiz 10 drew #1, 2, 3, 4, 6, 7, 9, 11, 14, 16, 17) ---------------

# Q1 (PS11)
# log-level MLR 0.033 x1 - 0.051 x2, both rise by 4: %dy = 100 * (b1 dx1 + b2 dx2)
loglevel(b = c(0.033, -0.051), dx = c(4, 4))        # -> -7.2

# Q2 (PS11)
# wage1 log(wage) on educ, exper, tenure: % change when exper +2 and tenure +1
mreg(log(wage) ~ educ + exper + tenure, data = wage1, dx = c(exper = 2, tenure = 1))   # -> pct_change 3.03

# Q3 (PS11)
# same regression: predicted wage in DOLLARS at educ 12, exper 10, tenure 3 (exp() automatic)
mreg(log(wage) ~ educ + exper + tenure, data = wage1, at = c(educ = 12, exper = 10, tenure = 3))   # -> pred_y 4.46

# Q4 (PS11)
# select all: which residual facts ALWAYS hold after OLS with an intercept?
# answer: average residual is 0 | passes through the point of means | cov(each Xj, residuals) = 0
# NOT: cov(Y, residuals) = 0 (that's the FITTED values) | every residual is 0 in a large sample
# FLIP: 'which is NOT always true?' -> cov(Y, residuals) = 0 | every residual = 0 | residuals add up to SSR

# Q5 (PS11)
# gpa1: predicted colGPA at mean ACT and mean hsGPA = mean colGPA (point of means)
mreg(colGPA ~ ACT + hsGPA, data = gpa1, at = c(ACT = mean(gpa1$ACT), hsGPA = mean(gpa1$hsGPA)))   # -> pred 3.06
r2d(mean(gpa1$colGPA))                              # -> 3.06, same

# Q6 (PS11)
# y-hat = 7.7 + 1.9 x1 - 0.5 x2, x1-bar = 11.3, x2-bar = 12.7: y-bar
yhat_at(b0 = 7.7, b1 = c(1.9, -0.5), x = c(11.3, 12.7))   # -> 22.82

# Q7 (PS11)
# select all: which regressions break MLR3?
# answer: voteA on expendA, expendB, totexpend | income in dollars AND thousands | 5 x's on 4 observations
# NOT: educ and exper that vary | ACT and hsGPA correlated but not perfectly
# FLIP: 'which do NOT break MLR3?' -> highly (not perfectly) correlated x's | x and x^2 | one x alone

# Q8 (PS11)
# vote1 with totexpend = expendA + expendB: what does R report for totexpend?
vote1$totexpend <- vote1$expendA + vote1$expendB
mreg(voteA ~ expendA + expendB + totexpend, data = vote1)
# answer: NA, R drops totexpend because it's an exact linear function of expendA and expendB

# Q9 (PS11)
# which situation makes MLR4 fail?
# answer: a variable that affects y and is correlated with x1 is left out of the regression
# FLIP: 'which does NOT make MLR4 fail?' -> left-out factor UNRELATED to x1 | small n | y's units | x's correlated (not perfectly)

# Q10 (PS11)
# unbiasedness only needs MLR1-MLR4, so why not add every variable?
# answer: adding variables can increase the variance of the OLS estimates and can even violate MLR3

# Q11 (PS11)
# (3.23): simple slope -0.25, b2_hat 0.72, delta1 -0.53 -> the MULTIPLE slope b1_hat
f323(b1_tilde = -0.25, b2_hat = 0.72, d1_tilde = -0.53)   # -> b1_hat 0.13

# Q12 (PS11)
# bwght on cigs, simple slope
mreg(bwght ~ cigs, data = bwght)                    # -> cigs -0.5138 -> -0.51

# Q13 (PS11)
# add faminc: cigs slope holding faminc fixed
mreg(bwght ~ cigs + faminc, data = bwght)           # -> cigs -0.4634 -> -0.46

# Q14 (PS11)
# b2_hat (faminc) x delta1 (faminc on cigs)
ovb_parts(y = "bwght", x1 = "cigs", x2 = "faminc", data = bwght)   # -> b2hat_x_d1tilde -0.05

# Q15 (PS11)
# -0.51 simple vs -0.46 holding income fixed: which reading is right?
# answer: -0.46 is smoking's effect holding income fixed; the other -0.05 is income riding along
#         (heavier smokers tend to have lower incomes)

# Q16 (PS11)
# omitted x2 LOWERS y and is NEGATIVELY correlated with x1
ovb(b2 = -1, cor = -1)                              # -> biased upward
# FLIP: x2 LOWERS y, POSITIVELY correlated -> (-)(+)
ovb(b2 = -1, cor = 1)                               # -> biased downward

# Q17 (PS11)
# yield on fertilizer, land quality left out: better land raises yield (+), more fertilizer on better land (+)
ovb(b2 = 1, cor = 1)                                # -> upward, overstates fertilizer
# FLIP: farmers put MORE fertilizer on WORSE land -> (+)(-)
ovb(b2 = 1, cor = -1)                               # -> downward, understates

# Q18 (PS11)
# exam score on tutoring, ability left out: ability raises scores (+), weaker students tutor more (-)
ovb(b2 = 1, cor = -1)                               # -> downward, understates tutoring
# FLIP: STRONGER students tutor more -> (+)(+)
ovb(b2 = 1, cor = 1)                                # -> upward, overstates

# Q19 (PS11)
# select all: when is there NO omitted-variable bias?
ovb(b2 = 1, cor = 0); ovb(b2 = 0, cor = 1)          # -> unbiased, unbiased
# answer: x2 uncorrelated with x1 | x2 has no ceteris-paribus effect on y. NOT: high R^2, big sample, big x2 effect
# FLIP: when IS there bias? -> x2 affects y AND x2 is correlated with x1 (both legs non-zero)


# @BANK-QUIZ10  Quiz 10 (11/4, you got 7.33/11 -- all 11, misses marked) ------------------------

# Q1 (Quiz 10)
# log(y) = 1.8 - 0.052 x1 + 0.037 x2, both rise by 2
loglevel(b = c(-0.052, 0.037), dx = c(2, 2))        # -> -3

# Q2 (Quiz 10)
# wage1 log(wage): % change when educ +1 AND tenure +2
mreg(log(wage) ~ educ + exper + tenure, data = wage1, dx = c(educ = 1, tenure = 2))   # -> pct_change 13.62

# Q3 (Quiz 10)
# same regression: predicted wage in dollars at educ 16, exper 5, tenure 1
mreg(log(wage) ~ educ + exper + tenure, data = wage1, at = c(educ = 16, exper = 5, tenure = 1))   # -> pred_y 6.05

# Q4 (Quiz 10)
# select all: always true after OLS with an intercept
# answer: residuals average 0 | passes through the point of means | cov(each Xj, residuals) = 0
# NOT: residuals add up to SSR | cov(Y, residuals) = 0

# Q5 (Quiz 10)
# y-hat = 4.4 - 2.2 x1 + 2.5 x2, x1-bar = 4.4, x2-bar = 5.9: y-bar
yhat_at(b0 = 4.4, b1 = c(-2.2, 2.5), x = c(4.4, 5.9))   # -> 9.47

# Q6 (Quiz 10) -- PARTIAL (0.33): you missed the first one and picked the last one
# select all MLR3 violations
# answer: profit on online, store AND total sales (total = online + store) | 3 x's on 3 observations
#         | price on square feet AND square meters
# NOT: wage on educ alone | study hours and sleep hours (negatively, but NOT perfectly, correlated)

# Q7 (Quiz 10)
# wage on educ, ability in U, ability goes with educ: which assumption fails?
# answer: MLR4, E[U | educ] = 0 -> educ is endogenous
# FLIP: ability in U but UNRELATED to educ -> MLR4 holds for educ, no bias

# Q8 (Quiz 10)
# (3.23): simple -0.92, b2_hat -1.32, delta1 -0.34 -> b1_hat (minus a negative x negative)
f323(b1_tilde = -0.92, b2_hat = -1.32, d1_tilde = -0.34)  # -> b1_hat -1.37

# Q9 (Quiz 10) -- MISSED (typed -0.05, the PS11 number)
# hprice1 price on bdrms, sqrft left out: b2_hat x delta1
ovb_parts(y = "price", x1 = "bdrms", x2 = "sqrft", data = hprice1)   # -> 46.8264 -> 46.83

# Q10 (Quiz 10) -- MISSED (picked "downward")
# x2 RAISES y (beta2 > 0), POSITIVELY correlated with x1: (+)(+)
ovb(b2 = 1, cor = 1)                                # -> biased upward
# FLIP: x2 LOWERS y, positively correlated -> (-)(+)
ovb(b2 = -1, cor = 1)                               # -> biased downward

# Q11 (Quiz 10) -- MISSED (picked "upward")
# test score on class size, family income left out: income raises scores (+), richer = SMALLER classes (-)
ovb(b2 = 1, cor = -1)                               # -> biased downward (more negative than the true class-size effect)
# FLIP: if richer districts had BIGGER classes -> (+)(+)
ovb(b2 = 1, cor = 1)                                # -> biased upward


# @BANK-PS12  Problem Set 12 -- Exam II has ~5 questions like #2, 4, 6, 9, 12 --------------------

# Q1 (PS12)
# in se(bj) = sigma-hat / sqrt(SST_j (1 - R_j^2)), what is X_j?
# answer: the independent variable whose coefficient bj we want the standard error of

# Q2 (PS12) *** exam ***
# wage on educ, exper, tenure; you want se(b_exper). Which regression gives R_j^2?
# answer: exper on the OTHER x's: lm(exper ~ educ + tenure). y (wage) is NOT in it.
summary(lm(exper ~ educ + tenure, data = wage1))$r.squared
# FLIP: se of TENURE -> lm(tenure ~ educ + exper) | se of EDUC -> lm(educ ~ exper + tenure). Never y on the left.
se_parts(wage ~ educ + exper + tenure, data = wage1, x = "exper")   # every piece, any x

# Q3 (PS12)
# what does sigma-hat = sqrt(SSR / (n - K - 1)) estimate?
# answer: sigma, the standard deviation of the error U

# Q4 (PS12) *** exam ***
# K = 2, n = 42, SSR = 2,857: sigma-hat
sigma_hat(SSR = 2857, n = 42, k = 2)                # -> df 39, sigmahat 8.56
# FLIP: K = 3 x's -> df = 38
sigma_hat(SSR = 2857, n = 42, k = 3)                # -> df 38, sigmahat 8.67

# Q5 (PS12)
# n = 20, SD of X_j = 1.3: SST_j = (n - 1) * S^2
r2d((20 - 1) * 1.3^2)                               # -> 32.11

# Q6 (PS12) *** exam ***
# sigma-hat = 9.4, SST_j = 65, R_j^2 = 0.37: se(bj)
se_b(sigmahat = 9.4, SSTx = 65, Rj2 = 0.37)         # -> sd_or_se 1.47
# FLIP: bigger R_j^2 (0.6) -> se LARGER
se_b(sigmahat = 9.4, SSTx = 65, Rj2 = 0.6)          # -> sd_or_se 1.84
# FLIP: R_j^2 = 0 (x_j unrelated to the others) -> se smaller
se_b(sigmahat = 9.4, SSTx = 65, Rj2 = 0)            # -> sd_or_se 1.17
# FLIP: given the se, find a missing piece
solve_Rj2(se = 1.47, sigma = 9.4, SSTx = 65)        # -> 0.37
solve_SST(se = 1.47, sigma = 9.4, Rj2 = 0.37)       # -> 64.91
solve_sigma(se = 1.47, SSTx = 65, Rj2 = 0.37)       # -> 9.41 (not exactly 9.4 because 1.47 was rounded)

# Q7 (PS12)
# R_j^2 close to 1 (x_j nearly a linear function of the others): what happens to se(bj)?
# answer: it grows without bound (multicollinearity)
# FLIP: R_j^2 near 0 -> no multicollinearity, se as small as it gets for that sigma-hat and SST_j
vif(Rj2 = 0.9)                                      # -> 10

# Q8-Q13 (PS12) *** #9 and #12 are exam ***
# hprice1 price on sqrft, bdrms, lotsize: se of the bdrms coefficient, piece by piece
se_parts(price ~ sqrft + bdrms + lotsize, data = hprice1, x = "bdrms")
# -> Q8 df 84 | Q9 sigmahat 59.83 | Q10 SST_j 61.59 | Q11 R_j^2 0.28 | Q12 se 9.01 | Q13 summary 9.01
# by hand, the way the PS hints write it:
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
SSR <- sum(reg$residuals^2); sigma_hat0 <- sqrt(SSR / (nrow(hprice1) - 3 - 1))
SST_j <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)
R2_j <- summary(lm(bdrms ~ sqrft + lotsize, data = hprice1))$r.squared
r2d(sigma_hat0 / sqrt(SST_j * (1 - R2_j)))          # -> 9.01
r2d(summary(reg)$coefficients[3, 2])                # Q13: row 3 = bdrms, column 2 = Std. Error -> 9.01
# FLIP: se of sqrft or lotsize instead -> change x =
se_parts(price ~ sqrft + bdrms + lotsize, data = hprice1, x = "sqrft")    # -> se 0.01

# Q14 (PS12)
# summary(reg)'s "Residual standard error: ... on 84 degrees of freedom" matches?
# answer: Problem 9 (sigma-hat), and its df match problem 8

# Q15 (PS12)
# missing data: y has NAs, x_j doesn't, you compute SST_j from ALL rows of x_j: what happens?
# answer: your hand se can differ from summary(); compute every piece on the rows the regression used


# @BANK-BEFORE1  Quiz 7 + Exam I group review (a few exam questions) -----------------------------

# Q4 (Quiz 7)
# E[X] = 5, E[X^2] = 50: sd of 2X - 8
sd_aX_b(a = 2, EX = 5, EX2 = 50)                    # -> 10
# FLIP: Var(2X - 8) instead of the sd -> a squared
Var_aX_b(a = 2, VarX = Var_from_E(EX2 = 50, EX = 5))   # -> 100

# Q5 (Quiz 7)
# Var(X) = 10, Var(Y) = 11, Cov = -2.3: Var(3X - 2Y)
Var_aX_bY(a = 3, VarX = 10, b = -2, VarY = 11, Cov = -2.3)   # -> 161.6

# Q6 (Quiz 7)
# joint table x = 1, 4, 7, 8; y = 3, 9, 2, 9; p = 0.3, 0.2, 0.1, 0.4: Cor(X, Y)
joint(x = c(1, 4, 7, 8), y = c(3, 9, 2, 9), p = c(0.3, 0.2, 0.1, 0.4))   # -> Cor 0.63

# Q7 (Quiz 7)
# x = (3, 4, 5, 6, 8), y = (5, 2, 8, 1, 4): residual of observation 4
x <- c(3, 4, 5, 6, 8); y <- c(5, 2, 8, 1, 4)
r2d(lm(y ~ x)$residuals[4])                         # -> -2.84

# Q8 (Quiz 7)
# residuals -0.4, 1.5, 0.8, 1.9 and one more: the fifth (residuals sum to 0)
r2d(-sum(c(-0.4, 1.5, 0.8, 1.9)))                   # -> -3.8

# Q9 (Quiz 7)
# SSE = 103, R^2 = 0.39: SSR
ss(SSE = 103, R2 = 0.39)                            # -> SSR 161.1
# FLIP: SSE from SSR and R^2
ss(SSR = 161.1, R2 = 0.39)                          # -> SSE 103

# Q10 (Quiz 7)
# n = 34, S_y^2 = 8: SST
r2d((34 - 1) * 8)                                   # -> 264
# FLIP: S_y^2 from SST
r2d(264 / (34 - 1))                                 # -> 8

# Q11 (Quiz 7)
# wage1 wage on tenure: residual of row 9
mreg(wage ~ tenure, data = wage1, i = 9)            # -> resid -2.1

# Group review A1, B2, E2 (Exam I review sheet)
sd_aX_b(a = 3, EX = 2, EX2 = 20)                    # A1: sd(3X - 7) -> 12
E_aX_b(a = 3, EX = cond_E(y = c(4, 10, 16), p = c(0.25, 0.5, 0.25)), b = -2)   # B2a: E[3Y - 2 | X = x] -> 28
cond_Var(y = c(4, 10, 16), p = c(0.25, 0.5, 0.25))  # B2b: Var(Y | X = x) -> 18
r2d(100 * summary(lm(salary ~ roe, data = ceosal1))$r.squared)   # E2b: % of salary variation explained -> 1.32
# G1: income in $ -> $1000s: correlation unchanged (0.35)

# study guide: mean within a group
gmean(y = wage1$wage, group = wage1$female, value = 1)   # -> mean 4.59 (women), sd 2.53
r2d(mean(wage1$wage[wage1$female == 1]))            # same mean, the way the study guide writes it


# @EXAM1  your Exam I, all 38 (answers checked in R) -- 19.8/38: 13 blanks, 5 wrong -----------------

# Q1-Q3, Q5, Q6, Q9, Q10: concept questions you got right.
# Q4 (partial): beta0 is the INTERCEPT PARAMETER (beta1 = slope parameter, u = error, y-hat = fitted value)

# Q7 (wrong): which is larger? E[price | Rolls-Royce] > E[price | Toyota]
# FLIP: smaller -> Toyota
# Q8 (wrong): which is larger? Var(income | age 40) > Var(income | age 10) (10-year-olds have ~no income)
# FLIP: smaller -> age 10

# Q11 (wrong): which line is OLS? Keep lines whose residuals sum to 0, then the smallest sum of SQUARED residuals
pick_ols(sum_u = c(0, 0, -0.5, 1), sum_u2 = c(3.30, 2.40, 2.55, 3.00))   # -> "B"

# Q12 (blank): correlation after changing units -> UNCHANGED (0.13)
# FLIP: the covariance DOES change: x c x d
cov_rescale(Sxy = 0.13, c = 1000)                   # -> 130

# Q13 (blank): mean of 18 values, the first is 43 and the other 17 are 2
mean_ab(a = 43, b = 2, n = 18)                      # -> 4.28

# Q14: change in y on a line (slopes 3.5 and -0.9, x1 +6, x2 -3)
delta_y(b = c(3.5, -0.9), dx = c(6, -3))            # -> 23.7

# Q15: percent change 3.4 -> 4.5
pct_change(old = 3.4, new = 4.5)                    # -> 32.35
# FLIP: 4.5 -> 3.4 (a fall, and NOT -32.35: the base changes)
pct_change(old = 4.5, new = 3.4)                    # -> -24.44

# Q16: sample sd of 12, 1, 12, 3
desc(x = c(12, 1, 12, 3))                           # -> sd 5.83

# Q17: Cov = 11, sd(X) = 3, Var(Y) = 64 -> correlation (Var given -> VarY =)
cor_from(Cov = 11, sdX = 3, VarY = 64)              # -> 0.46
# FLIP: covariance from the correlation: Cov = cor x sdX x sdY
r2d(0.46 * 3 * 8)                                   # -> 11.04

# Q18 (blank): sd(4X - 4) with E[X] = 5, E[X^2] = 37 (b never matters)
sd_aX_b(a = 4, EX = 5, EX2 = 37)                    # -> 13.86
# FLIP: Var(4X - 4) -> a SQUARED
Var_aX_b(a = 4, VarX = Var_from_E(EX2 = 37, EX = 5))   # -> 192

# Q19 (wrong, typed 17.1): E[3Y + 6 | X = x], Y = 9, 22, 30 with P = 0.5, 0.3, 0.2
E_aX_b(a = 3, EX = cond_E(y = c(9, 22, 30), p = c(0.5, 0.3, 0.2)), b = 6)   # -> 57.3
# FLIP: Var(3Y + 6 | X = x) -> 9 x Var(Y | X = x)
Var_aX_b(a = 3, VarX = cond_Var(y = c(9, 22, 30), p = c(0.5, 0.3, 0.2)))   # -> 659.61

# Q20 (blank): Var(2X - 2Y), Var(X) = 10, Var(Y) = 4, Cov = -3.4
Var_aX_bY(a = 2, VarX = 10, b = -2, VarY = 4, Cov = -3.4)   # -> 83.2
# FLIP: Var(2X + 2Y) -> the Cov term flips sign
Var_aX_bY(a = 2, VarX = 10, b = 2, VarY = 4, Cov = -3.4)    # -> 28.8

# Q21: joint table -> Cor(X, Y) (also prints the population slope b1 = Cov / VarX)
joint(x = c(2, 4, 5, 8), y = c(11, 2, 6, 8), p = c(0.1, 0.4, 0.3, 0.2))   # -> Cor 0.23

# Q22: OLS slope from x = 2, 4, 5, 8, 10 and y = 8, 9, 3, 4, 5
ols(x = c(2, 4, 5, 8, 10), y = c(8, 9, 3, 4, 5))   # -> b1 -0.47

# Q23: residual of observation 3, x = 2, 3, 4, 5, 8 and y = 12, 11, 3, 7, 9
x <- c(2, 3, 4, 5, 8); y <- c(12, 11, 3, 7, 9)
r2d(lm(y ~ x)$residuals[3])                         # -> -5.57
# FLIP: the FITTED value of observation 3
r2d(lm(y ~ x)$fitted.values[3])                     # -> 8.57

# Q24 (blank): n = 25, S_y^2 = 22 -> SST = (n - 1) S_y^2
r2d((25 - 1) * 22)                                  # -> 528

# Q25 (blank): SSE = 122, R^2 = 0.22 -> SSR
ss(SSE = 122, R2 = 0.22)                            # -> SSR 432.55
# FLIP: SSE from SSR and R^2
ss(SSR = 432.55, R2 = 0.22)                         # -> SSE 122

# Q26 (blank): b0 = 4, b1 = 1.2, x-bar = 10 -> y-bar (the line goes through the means)
yhat_at(b0 = 4, b1 = 1.2, x = 10)                   # -> 16
# FLIP: x-bar from y-bar: (y-bar - b0) / b1
r2d((16 - 4) / 1.2)                                 # -> 10

# Q27 (blank): four residuals -0.3, -2.4, -1.8, -2.5 -> the fifth (they sum to 0)
r2d(-sum(c(-0.3, -2.4, -1.8, -2.5)))                # -> 7

# Q28 (blank): sum of x times the residuals -> 0, always (R may print -8.7e-15: type 0)
x <- c(4, 6, 7, 9, 14, 15); y <- c(8, 15, 13, 15, 2, 8)
r2d(sum(x * lm(y ~ x)$residuals))                   # -> 0

# Q29-Q31 (blank): ceosal1 -- n, mean roe, correlation of salary and roe
nrow(ceosal1)                                       # -> 209
r2d(mean(ceosal1$roe))                              # -> 17.18
r2d(cor(ceosal1$salary, ceosal1$roe))               # -> 0.11

# Q32-Q36: wage1, wage on tenure -- slope, prediction at tenure 5, change for +13, % explained, row 2's residual
mreg(wage ~ tenure, data = wage1, at = c(tenure = 5), dx = c(tenure = 13), i = 2)
# -> tenure 0.18 (Q32) | pred 5.88 (Q33) | change 2.31 (Q34) | 100 x R2 = 12.03 (Q35, NOT the SSE) | resid -2.11 (Q36)
r2d(100 * summary(lm(wage ~ tenure, data = wage1))$r.squared)   # -> 12.03
# FLIP: % NOT explained = 100 x (1 - R^2)
r2d(100 * (1 - summary(lm(wage ~ tenure, data = wage1))$r.squared))   # -> 87.97

# Q37-Q38: bwght on faminc -- SSE (explained), change for faminc +22
mreg(bwght ~ faminc, data = bwght, dx = c(faminc = 22))   # -> SSE 6819.05 (Q37) | change 2.6 (Q38)


# @BANK-DAYS  Class days 13-23 (instructor's starter problems) -----------------------------------

# Day 13
# Example 2.3 ceosal1 salary on roe (963.19, 18.50); salary in dollars; roe as a decimal
mreg(salary ~ roe, data = ceosal1)
rescale(b0 = 963.1913, b1 = 18.5012, y_times = 1000)      # -> 963191.3, 18501.2
rescale(b0 = 963.1913, b1 = 18.5012, x_times = 1/100)     # -> 963.19, 1850.12   (R^2 = 0.0132 in all three)
# log(wage) = 0.58 + 0.08 educ: % change from +1 year, +4 years
loglevel(b = 0.08, dx = 1); loglevel(b = 0.08, dx = 4)    # -> 8, 32
# in a log-log model the slope is an ELASTICITY

# Day 15
# meap93 math10 on lnchprg = 32.14 - 0.319 lnchprg: negative slope = poverty confounds, not a real effect
mreg(math10 ~ lnchprg, data = meap93)
# wage on educ, ability in U: ability (1) affects wage and (2) goes with educ -> biased UP -> SLR4 E[U|X] = 0 fails

# Day 16
# sigma = 4, n = 26, sample VARIANCE of X = 4: SST_x = 100, sd(b1-hat) = 0.4; sigma doubles -> 0.8
se_b(sigma = 4, n = 26, Sx2 = 4)                    # -> SSTx 100, sd_or_se 0.4
se_b(sigma = 8, n = 26, Sx2 = 4)                    # -> 0.8

# Day 17
# wage-hat = b0 + 0.6 educ + 0.07 exper: +1 educ (exper fixed), +1 exper (educ fixed)
delta_y(b = c(0.6, 0.07), dx = c(1, 0)); delta_y(b = c(0.6, 0.07), dx = c(0, 1))   # -> 0.6, 0.07

# Day 18
# Example 3.2: log(wage) on educ, exper, tenure; predict at 16, 4, 2; % change +1 educ; +1 exper and +1 tenure
mreg(log(wage) ~ educ + exper + tenure, data = wage1, at = c(educ = 16, exper = 4, tenure = 2))   # -> pred_y 6.16
mreg(log(wage) ~ educ + exper + tenure, data = wage1, dx = c(educ = 1))               # -> 9.2
mreg(log(wage) ~ educ + exper + tenure, data = wage1, dx = c(exper = 1, tenure = 1))  # -> 2.62
loglevel(b = 0.092, dx = 4)                         # problem (a): +4 years educ -> 36.8

# Day 19
# Example 3.1 gpa1: ACT alone 0.0271; with hsGPA 1.29 + 0.0094 ACT + 0.453 hsGPA; +10 ACT -> 0.09
mreg(colGPA ~ ACT, data = gpa1)
mreg(colGPA ~ ACT + hsGPA, data = gpa1, dx = c(ACT = 10))
ovb_parts(y = "colGPA", x1 = "ACT", x2 = "hsGPA", data = gpa1)   # check (3.23): 0.0094 + 0.4535 x 0.0389 = 0.0271
yhat_at(b0 = 1.29, b1 = c(0.0094, 0.453), x = c(30, 3.0))       # problem (a) -> 2.93

# Day 20
# Table 3.2 in wage1, exper left out: exper slope +0.07, cor(educ, exper) -0.30 -> simple educ slope smaller: DOWN
ovb_parts(y = "wage", x1 = "educ", x2 = "exper", data = wage1)
r2d(cor(wage1$educ, wage1$exper))                   # -> -0.3
# (a) b2 > 0, delta1 > 0: UP   (b) b2 > 0, delta1 < 0: DOWN   (c) no bias: beta2 = 0 or delta1 = 0
# (d) sigma^2 rises: Var UP   (e) n grows: Var DOWN   (f) X barely varies: Var UP

# Day 21
# se(educ) in wage ~ educ + exper, every piece by hand: df 523, sigma-hat 3.257, SST_educ 4025.43, R_j^2 0.0897
se_parts(wage ~ educ + exper, data = wage1, x = "educ")   # -> se 0.0538, matches summary
# Gauss-Markov: under MLR1-MLR5, OLS is BLUE

# Day 22 -- NOT on Exam II (study guide), kept for the final
# t = estimate / se (summary column 3)
mreg(wage ~ educ, data = wage1)                     # educ t value 10.17
# Example 4.1: is the return to experience positive? t_exper = 2.39 > c = 1.65 -> reject
mreg(log(wage) ~ educ + exper + tenure, data = wage1)
crit(df = 522)                                      # -> 1.65
# problems: beta-hat 0.54, se 0.05 -> t = 10.8, far from 0, tests H0: beta = 0
ttest(bhat = 0.54, se = 0.05)
ttest(t = 2.4, c = 1.68)                            # (d) reject
ttest(t = 0.9, c = 1.68)                            # (e) fail to reject
ttest(t = -3.0, c = 1.68)                           # (f) fail to reject (wrong direction for H1: beta > 0)

# Day 23 (two-sided + p-values: after this exam)
ttest(t = -3.2, c = 1.98, h1 = "two")               # reject
mreg(colGPA ~ hsGPA + ACT + skipped, data = gpa1)   # Pr(>|t|) column = p-values

}   # end of PRACTICE BANK


# @TOOLS  backup copy of the tools in ECN377_Exam2_ANSWERS.R ---------------------------------
# Same functions. If you Source this file by mistake, everything still loads.
if (!suppressWarnings(require(wooldridge, quietly = TRUE)))
  message("No package called 'wooldridge': run install.packages(\"wooldridge\"), then Source again. The rest still works.")
options(scipen = 999)                    # no scientific notation
r2d <- function(v) round(v + sign(v) * 1e-9, 2)   # round to the hundredths (0.975 -> 0.98)

fix_model <- function(model) {
  m <- gsub("[^a-z]", "", tolower(model))
  ok <- c(levellevel = "level-level", loglevel = "log-level", levellog = "level-log", loglog = "log-log")
  if (!m %in% names(ok)) stop('model must be "level-level", "log-level", "level-log" or "log-log"', call. = FALSE)
  ok[[m]]
}

# units of measurement: y x y_times, x x x_times (divide by 10 -> x_times = 1/10)
rescale <- function(b0 = NA, b1 = NA, y_times = 1, x_times = 1, model = "level-level") {
  model <- fix_model(model); cy <- y_times; cx <- x_times
  if (model == "level-level") { nb0 <- cy * b0;                           nb1 <- cy * b1 / cx }
  if (model == "log-level")   { nb0 <- b0 + log(cy);                      nb1 <- b1 / cx }
  if (model == "level-log")   { nb0 <- cy * (b0 - b1 * log(cx));          nb1 <- cy * b1 }
  if (model == "log-log")     { nb0 <- b0 + log(cy) - b1 * log(cx);       nb1 <- b1 }
  cat("(", model, ": R-squared does not change)\n", sep = "")
  r2d(c(new_b0 = nb0, new_b1 = nb1))
}

# logs (Table 2.3)
loglevel <- function(b, dx)   r2d(100 * sum(b * dx))     # % change in y
loglog   <- function(b, pctx) r2d(sum(b * pctx))         # % change in y (b = elasticity)
levellog <- function(b, pctx) r2d(sum(b / 100 * pctx))   # change in y, in y's own units
delta_y  <- function(b, dx)   r2d(sum(b * dx))           # level-level, any number of x's (no intercept)
means <- function(model, b, x = "x", y = "y") {
  model <- fix_model(model); f <- function(v) format(signif(v, 4))
  txt <- switch(model,
    "level-level" = paste0("A 1-unit rise in ", x, " -> ", y, " changes by ", f(b), " units"),
    "log-level"   = paste0("A 1-unit rise in ", x, " -> ", y, " changes by about ", f(100 * b), "%  (100 x b)"),
    "level-log"   = paste0("A 1% rise in ", x, " -> ", y, " changes by about ", f(b / 100), " units  (b / 100)"),
    "log-log"     = paste0("A 1% rise in ", x, " -> ", y, " changes by about ", f(b), "%  (b = elasticity)"))
  cat(txt, "\n"); invisible(txt)
}

# predictions
yhat_at  <- function(b0, b1, x) r2d(b0 + sum(b1 * x))      # one or many x's; also y-bar from the x-bar's
yhat_log <- function(b0, b, x)  r2d(exp(b0 + sum(b * x)))  # log(y) model: predict log(y), then exp()

# any regression on a dataset: everything at once
mreg <- function(formula, data, at = NULL, dx = NULL, i = NULL) {
  reg <- lm(formula, data = data)
  s <- summary(reg); b <- coef(reg)
  ylog <- grepl("^log\\(", deparse(formula[[2]]))
  y <- reg$model[[1]]; SSR <- sum(reg$residuals^2); SST <- sum((y - mean(y))^2)
  out <- c(R2 = s$r.squared, SSR = SSR, SST = SST, SSE = SST - SSR, sigmahat = s$sigma, sigma2hat = s$sigma^2)
  if (!is.null(at)) {                                       # prediction (exp() for a log(y) model)
    xvars <- all.vars(formula[[3]])
    if (is.null(names(at)) && length(at) == length(xvars)) names(at) <- xvars
    if (!all(xvars %in% names(at)))
      stop("at = needs a value for every x: ", paste(xvars, collapse = ", "), call. = FALSE)
    p <- predict(reg, newdata = as.data.frame(as.list(at)))[[1]]
    out <- if (ylog) c(out, pred_log = p, pred_y = exp(p)) else c(out, pred = p)
  }
  if (!is.null(dx)) {                                       # change (x 100 = % for a log(y) model)
    if (is.null(names(dx)) && length(dx) == length(b) - 1) names(dx) <- names(b)[-1]
    if (!all(names(dx) %in% names(b))) stop("dx names must be among: ", paste(names(b)[-1], collapse = ", "),
                                            "  (for a log(x) term, a 10% rise = 0.10)", call. = FALSE)
    ch <- sum(b[names(dx)] * dx)
    out <- if (ylog) c(out, pct_change = 100 * ch) else c(out, change = ch)
  }
  if (!is.null(i)) {                                        # row i's fitted value and residual
    k <- as.character(i)
    if (!k %in% names(reg$residuals)) k <- names(reg$residuals)[i]
    out <- c(out, fitted = reg$fitted.values[[k]], resid = reg$residuals[[k]])
  }
  cat("\n", deparse(formula), "    n =", nobs(reg), "   df = n - k - 1 =", reg$df.residual, "\n")
  print(round(s$coefficients, 4))
  if (any(is.na(b))) cat("NA for:", names(which(is.na(b))), "-> perfect collinearity (MLR3 fails), R dropped it\n")
  print(round(out, 4))
  invisible(reg)
}

# variance of OLS and standard errors
sigma_hat <- function(SSR, n, k = 1) {                   # k = number of x's (SLR: k = 1, so n - 2)
  df <- n - k - 1
  r2d(c(df = df, sigma2hat = SSR / df, sigmahat = sqrt(SSR / df)))
}
se_b <- function(sigma = NA, sigmahat = NA, SSR = NA, n = NA, k = 1,
                 SSTx = NA, Sx = NA, Sx2 = NA, x = NULL, Rj2 = 0) {
  if (!is.null(x)) { SSTx <- sum((x - mean(x))^2); if (is.na(n)) n <- length(x) }
  if (is.na(SSTx) && !is.na(Sx))  SSTx <- (n - 1) * Sx^2      # sd of X given -> square it
  if (is.na(SSTx) && !is.na(Sx2)) SSTx <- (n - 1) * Sx2       # variance of X given -> don't square
  if (is.na(sigma)) sigma <- sigmahat
  if (is.na(sigma) && !is.na(SSR)) sigma <- sqrt(SSR / (n - k - 1))
  if (is.na(sigma) || is.na(SSTx))
    stop("need sigma (or sigmahat, or SSR + n) AND SSTx (or the x values, or Sx / Sx2 + n)", call. = FALSE)
  r2d(c(sigma = sigma, SSTx = SSTx, Rj2 = Rj2,
        Var_b = sigma^2 / (SSTx * (1 - Rj2)), sd_or_se = sigma / sqrt(SSTx * (1 - Rj2))))
}
sd_b1 <- se_b                                              # same formula: sigma -> sd, sigma-hat -> se
sd_scale <- function(sd, n_times = 1, sigma_times = 1) r2d(sd * sigma_times / sqrt(n_times))
se_parts <- function(formula, data, x) {                   # Day 21: se of one coefficient, every piece by hand
  reg <- lm(formula, data = data)
  X <- model.matrix(reg)
  if (!x %in% colnames(X)) stop("x must be one of: ", paste(colnames(X)[-1], collapse = ", "), call. = FALSE)
  n <- nobs(reg); k <- ncol(X) - 1; df <- n - k - 1
  SSR <- sum(reg$residuals^2); sigmahat <- sqrt(SSR / df)
  xj <- X[, x]; SSTj <- sum((xj - mean(xj))^2)
  others <- X[, setdiff(colnames(X), c("(Intercept)", x)), drop = FALSE]
  Rj2 <- if (ncol(others) == 0) 0 else summary(lm(xj ~ others))$r.squared
  round(c(n = n, k = k, df = df, SSR = SSR, sigmahat = sigmahat, SSTj = SSTj, Rj2 = Rj2,
          se_by_hand = sigmahat / sqrt(SSTj * (1 - Rj2)),
          se_summary = summary(reg)$coefficients[x, 2]), 4)
}

# omitted-variable bias
# sign of the bias = sign(b2) x sign(cor)
#   b2:  does the LEFT-OUT variable raise y (+1) or lower it (-1)?
#   cor: does the left-out variable move with (+1) or against (-1) the INCLUDED x?
ovb <- function(b2, cor) {
  s <- sign(b2) * sign(cor)
  if (s > 0) "biased upward (overstates)" else if (s < 0) "biased downward (understates)" else "unbiased"
}
table32 <- function() cat(
"                      corr(x1, x2) > 0       corr(x1, x2) < 0
  beta2 > 0           biased UP              biased DOWN
  beta2 < 0           biased DOWN            biased UP
  no bias if beta2 = 0 or corr(x1, x2) = 0 (delta1 = 0)\n")
# formula (3.23): b1_tilde = b1_hat + b2_hat * d1_tilde. Give any 3, it finds the 4th
f323 <- function(b1_tilde = NA, b1_hat = NA, b2_hat = NA, d1_tilde = NA) {
  if (sum(is.na(c(b1_tilde, b1_hat, b2_hat, d1_tilde))) != 1) stop("give exactly 3 of the 4", call. = FALSE)
  if (is.na(b1_hat))   b1_hat   <- b1_tilde - b2_hat * d1_tilde
  if (is.na(b1_tilde)) b1_tilde <- b1_hat + b2_hat * d1_tilde
  if (is.na(b2_hat))   b2_hat   <- (b1_tilde - b1_hat) / d1_tilde
  if (is.na(d1_tilde)) d1_tilde <- (b1_tilde - b1_hat) / b2_hat
  r2d(c(b1_tilde = b1_tilde, b1_hat = b1_hat, b2_hat = b2_hat, d1_tilde = d1_tilde, ride_along = b2_hat * d1_tilde))
}
# runs all 3 regressions on ANY dataset and prints every piece: x1 = included variable, x2 = left out
# shows 4 decimals: round the one the question asks for when you type it in
ovb_parts <- function(y, x1, x2, data) {
  d <- na.omit(data[, all.vars(reformulate(c(x1, x2), y))])
  simple   <- coef(lm(reformulate(x1, y), data = d))[[x1]]
  multiple <- coef(lm(reformulate(c(x1, x2), y), data = d))
  d1       <- coef(lm(reformulate(x1, x2), data = d))[[x1]]
  round(c(simple_slope = simple, multiple_slope = multiple[[x1]], b2_hat = multiple[[x2]],
          d1_tilde = d1, b2hat_x_d1tilde = multiple[[x2]] * d1), 4)
}

# t-tests: h1 = ">" (beta > null), "<" (beta < null), "two" (beta != null)
crit <- function(df = Inf, alpha = 0.05, sides = 1) r2d(qt(1 - alpha / sides, df))
ttest <- function(bhat = NA, se = NA, t = NA, null = 0, df = Inf, alpha = 0.05, h1 = ">", c = NA) {
  if (!h1 %in% c(">", "<", "two", "!=")) stop('h1 must be ">", "<" or "two"', call. = FALSE)
  if (is.na(t)) t <- (bhat - null) / se
  two <- h1 %in% c("two", "!=")
  if (is.na(c)) c <- qt(1 - alpha / ifelse(two, 2, 1), df)
  reject <- if (two) abs(t) > c else if (h1 == ">") t > c else t < -c
  p <- if (two) 2 * (1 - pt(abs(t), df)) else if (h1 == ">") 1 - pt(t, df) else pt(t, df)
  cat("t =", r2d(t), "  c =", r2d(c), "  ->", ifelse(reject, "REJECT H0", "FAIL TO REJECT H0"),
      "  p-value =", round(p, 4), ifelse(is.infinite(df), "(df not given: normal approx)", ""), "\n")
  invisible(c(t = t, c = c, p = p))
}

# ----- Exam 1 carry-over (review questions) -----
pct_change <- function(old, new) r2d(100 * (new - old) / old)
pp_change  <- function(old, new) r2d(new - old)
check_p <- function(p, x) {
  if (length(p) != length(x)) stop("need one probability for each value", call. = FALSE)
  if (abs(sum(p) - 1) > 0.000001) warning("probabilities add to ", sum(p), ", not 1", call. = FALSE)
}
desc <- function(x, y = NULL) {
  out <- c(sum = sum(x), mean = mean(x), var = var(x), sd = sd(x))
  if (!is.null(y)) out <- c(out, mean_y = mean(y), var_y = var(y), sd_y = sd(y),
                            cov = cov(x, y), cor = suppressWarnings(cor(x, y)))
  r2d(out)
}
cor_from <- function(Cov, sdX = sqrt(VarX), sdY = sqrt(VarY), VarX, VarY) r2d(Cov / (sdX * sdY))
rv <- function(x, p = rep(1 / length(x), length(x))) {
  check_p(p, x)
  EX <- sum(x * p); EX2 <- sum(x^2 * p); VarX <- max(EX2 - EX^2, 0)
  r2d(c(EX = EX, EX2 = EX2, VarX = VarX, sdX = sqrt(VarX)))
}
joint <- function(x, y, p = rep(1 / length(x), length(x))) {   # one row per (x, y, probability)
  if (length(x) != length(y)) stop("x and y need one value per row", call. = FALSE)
  check_p(p, x)
  EX <- sum(x * p); EY <- sum(y * p); EXY <- sum(x * y * p)
  EX2 <- sum(x^2 * p); EY2 <- sum(y^2 * p)
  VarX <- max(EX2 - EX^2, 0); VarY <- max(EY2 - EY^2, 0); Cov <- EXY - EX * EY
  b1 <- Cov / VarX                                           # population slope = Cov(X, Y) / Var(X)
  if (any(duplicated(x))) {                                  # an x value repeats -> E[Y | X = x] for each x
    ux <- sort(unique(x))
    cE <- sapply(ux, function(v) sum(y[x == v] * p[x == v]) / sum(p[x == v]))
    cat("E[Y | X = x]:", paste0("x = ", ux, ": ", r2d(cE), collapse = "  |  "), "\n")
  }
  r2d(c(EX = EX, EY = EY, EXY = EXY, VarX = VarX, VarY = VarY, sdX = sqrt(VarX), sdY = sqrt(VarY),
        Cov = Cov, Cor = Cov / sqrt(VarX * VarY), b1 = b1, b0 = EY - b1 * EX))
}
cond_E <- function(y, p = rep(1 / length(y), length(y))) { check_p(p, y); r2d(sum(y * p)) }
cond_Var <- function(y, p = NULL) {
  if (is.null(p)) return(r2d(var(y)))
  check_p(p, y); r2d(sum(y^2 * p) - sum(y * p)^2)
}
gmean <- function(y, group, value) {                     # mean / sd of y among rows where group == value
  v <- y[group == value & !is.na(group)]; v <- v[!is.na(v)]
  r2d(c(n = length(v), mean = mean(v), sd = sd(v)))
}
sd_aX_b     <- function(a, VarX = EX2 - EX^2, EX, EX2) r2d(abs(a) * sqrt(VarX))   # b never matters
E_aX_b      <- function(a, EX, b = 0) r2d(a * EX + b)
E_aX_bY     <- function(a, EX, b, EY, c = 0) r2d(a * EX + b * EY + c)
Var_from_E  <- function(EX2, EX) r2d(EX2 - EX^2)
Var_aX_b    <- function(a, VarX) r2d(a^2 * VarX)          # a gets SQUARED, b drops out
Var_aX_bY   <- function(a, VarX, b, VarY, Cov = 0) r2d(a^2 * VarX + b^2 * VarY + 2 * a * b * Cov)
Cov_from_E  <- function(EXY, EX, EY) r2d(EXY - EX * EY)
Cov_a1X_a2Y <- function(a1, a2, Cov) r2d(a1 * a2 * Cov)
ols <- function(x, y, at = NULL) {                         # simple regression by hand from two lists
  if (length(x) != length(y)) stop("x and y need the same number of values", call. = FALSE)
  if (var(x) == 0) stop("all the x's are the same, so there's no slope", call. = FALSE)
  n <- length(x); SSTx <- sum((x - mean(x))^2)
  b1 <- sum((x - mean(x)) * (y - mean(y))) / SSTx; b0 <- mean(y) - b1 * mean(x)
  SST <- sum((y - mean(y))^2); SSR <- sum((y - (b0 + b1 * x))^2); SSE <- SST - SSR
  out <- c(b1 = b1, b0 = b0)
  if (!is.null(at)) out <- c(out, yhat = b0 + b1 * at)
  out <- c(out, R2 = SSE / SST, SST = SST, SSE = SSE, SSR = SSR, SSTx = SSTx)
  if (n > 2) out <- c(out, sigma2hat = SSR / (n - 2), se_b1 = sqrt(SSR / (n - 2)) / sqrt(SSTx))
  r2d(out)
}
b1_from  <- function(cov, var) r2d(cov / var)
b0_from  <- function(ybar, xbar, b1) r2d(ybar - b1 * xbar)
resid_at <- function(b0, b1, x, y) r2d(y - (b0 + b1 * x))           # actual - fitted
ss <- function(SST = NA, SSE = NA, SSR = NA, R2 = NA) {             # give any two
  for (j in 1:3) {
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

# ----- added for Exam II (Gus's sheet, reverse-solves, Exam I one-liners) -----
mean_ab     <- function(a, b, n) r2d((a + (n - 1) * b) / n)     # mean when the 1st value is a, the other n - 1 are b
cov_rescale <- function(Sxy, c = 1, d = 1) r2d(c * d * Sxy)     # Cov(cX, dY) = c d Cov(X, Y). Correlation never changes
quad_slope  <- function(b1, b2, x) r2d(b1 + 2 * b2 * x)         # y = b0 + b1 x + b2 x^2: slope at x
vif         <- function(Rj2) r2d(1 / (1 - Rj2))                 # variance inflation factor
se_b0 <- function(sigmahat = NA, n = NA, sum_x2 = NA, SSTx = NA, x = NULL, SSR = NA, k = 1) {   # se of the INTERCEPT (SLR)
  if (!is.null(x)) { n <- length(x); sum_x2 <- sum(x^2); SSTx <- sum((x - mean(x))^2) }
  if (is.na(sigmahat) && !is.na(SSR)) sigmahat <- sqrt(SSR / (n - k - 1))
  if (any(is.na(c(sigmahat, n, sum_x2, SSTx))))
    stop("need sigmahat (or SSR), n, sum_x2 and SSTx -- or the x values plus sigmahat / SSR", call. = FALSE)
  r2d(sigmahat * sqrt(sum_x2 / n) / sqrt(SSTx))
}
# reverse-solve se = sigma / sqrt(SST * (1 - Rj2)) for the missing piece
solve_SST   <- function(se, sigma, Rj2 = 0) r2d((sigma / se)^2 / (1 - Rj2))
solve_sigma <- function(se, SSTx, Rj2 = 0) r2d(se * sqrt(SSTx * (1 - Rj2)))
solve_Rj2   <- function(se, sigma, SSTx) r2d(1 - (sigma / se)^2 / SSTx)
# which line is the OLS line? residuals must sum to 0, THEN the smallest sum of SQUARED residuals
pick_ols <- function(sum_u, sum_u2, names = LETTERS[seq_along(sum_u)]) {
  ok <- abs(sum_u) < 1e-9
  if (!any(ok)) return("none of them: OLS residuals always sum to 0")
  names[ok][which.min(sum_u2[ok])]
}

cat("Exam 2 tools loaded. Quick test (should say 1.79):",
    se_b(sigmahat = 8.7, n = 15, Sx = 1.3)[["sd_or_se"]], "\n")
