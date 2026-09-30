# ============================================================
#  ECN 377 — PS6 / Quiz: Simple OLS  (simple version)
#  1. Run the whole file once.
#  2. Find the TYPE that matches the question.
#  3. Change the numbers, run those lines, copy the answer.
# ============================================================

# ---------- QUICK CONCEPTS (Q1–8) ----------
# Fitted value  = the model's PREDICTION of y
# Residual      = actual - fitted
#   positive -> under-predicted, negative -> over-predicted
# Residuals always add up to 0
# (x_bar, y_bar) is always on the OLS line
# SST = SSE + SSR   (total = explained + residual)
# R^2 = SSE/SST = share of y's variation explained by x
# R^2 near 0 = explains little; common, not automatically bad
# Unexplained share = SSR/SST = 1 - R^2


# ---------- TYPE 1: SLOPE, INTERCEPT, PREDICTION from data (Q9–11) ----------
x  <- c(5, 7, 8)     # <- put the x values here
y  <- c(9, 6, 3)     # <- put the y values here
x0 <- 1              # <- x to predict at (Q11 only)

b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)
round(b1, 2)             # SLOPE
round(b0, 2)             # INTERCEPT
round(b0 + b1 * x0, 2)   # PREDICTION


# ---------- TYPE 2: FITTED VALUE from a given line (Q12) ----------
b0 <- 7; b1 <- 2/10; x0 <- 6          # <- change these
round(b0 + b1 * x0, 2)


# ---------- TYPE 3: RESIDUAL from a given line (Q13) ----------
b0 <- 3; b1 <- 0.5; x0 <- 6; y_actual <- 7   # <- change these
round(y_actual - (b0 + b1 * x0), 2)


# ---------- TYPE 4: PREDICTED CHANGE in y (Q14) ----------
b1 <- 0.5; dx <- 10                  # <- change these
round(b1 * dx, 2)


# ---------- TYPE 5: SSR from a given line + points (Q15) ----------
b0 <- 0.5; b1 <- 1.5                 # <- the given line
x <- c(1, 2, 3); y <- c(2, 4, 5)     # <- the points
round(sum((y - (b0 + b1 * x))^2), 2)


# ---------- TYPE 6: SUMS OF SQUARES & R^2 (Q16–20) ----------
# Just use the formula that matches what you're given:
SST <- 100; SSR <- 40; SSE <- 60; R2 <- 0.35   # <- change what you have

round(SST - SSR, 2)   # find SSE (explained)      given SST, SSR
round(SST - SSE, 2)   # find SSR (residual)       given SST, SSE
round(SSE / SST, 2)   # find R^2                  given SSE, SST
round(1 - SSR / SST, 2)  # find R^2               given SSR, SST
round(SSR / SST, 2)   # UNEXPLAINED share         given SSR, SST
round(R2 * SST, 2)    # find SSE                  given R^2, SST
round((1 - R2) * SST, 2)  # find SSR              given R^2, SST
