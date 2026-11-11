# =====================================================================================
#  ECN 377 -- EXAM 2 ANSWERS (Brecken)   <- SUBMIT THIS FILE          38 questions * 75 minutes
#  1) Click Source (top right of this pane). Wait for "Exam 2 tools loaded".
#  2) Ctrl+End (Mac: Cmd+Down) jumps to # Q1-Q38 at the bottom. Work there the whole exam.
#  3) Find the line you need in the other tab, ECN377_Exam2_REFERENCE.R (Ctrl/Cmd + F a @TAG),
#     paste it under its # Q#, change the numbers, Ctrl/Cmd + Enter.
#  Last 2 minutes: File > Save, then upload THIS file. Nothing blank.
# =====================================================================================


# @TOOLS  (Source runs this part; you never need to read it) ----------------------------------
# Every function's example is in ECN377_Exam2_REFERENCE.R.
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


# @ANSWERS  type your code under each # Q#, then Cmd/Ctrl + Enter ------------------------------------
# Partial credit needs this code. Last 2 minutes: File > Save, then upload this file in the last question.
# example:
# Q1
# loglevel(b = 0.04, dx = -3)

# Q1

# Q2

# Q3

# Q4

# Q5

# Q6

# Q7

# Q8

# Q9

# Q10

# Q11

# Q12

# Q13

# Q14

# Q15

# Q16

# Q17

# Q18

# Q19

# Q20

# Q21

# Q22

# Q23

# Q24

# Q25

# Q26

# Q27

# Q28

# Q29

# Q30

# Q31

# Q32

# Q33

# Q34

# Q35

# Q36

# Q37

# Q38

