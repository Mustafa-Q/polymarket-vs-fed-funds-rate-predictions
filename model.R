library(tidyverse)

d <- read.csv("final_analysis_dataset.csv")

## Main model: signed disagreement
m_signed <- lm(
  error ~ days_to_meeting +
    log_volume_1wk +
    log_liquidity +
    spread +
    competitive +
    option_type,
  data = d
)

summary(m_signed)

op <- par(mfrow = c(2,2))

# Standard diagnostics (no Cook's-distance curve)
plot(m_signed, which = 1)
plot(m_signed, which = 2)
plot(m_signed, which = 3)

# Manual leverage plot (no Cook's-distance curve, no NaN warning)
h <- hatvalues(m_signed)
r <- rstandard(m_signed)
plot(h, r,
     xlab = "Leverage",
     ylab = "Standardized residuals",
     main = "Residuals vs Leverage")
abline(h = 0, lty = 2)
text(h, r, labels = seq_along(h), pos = 4, cex = 0.8)

par(op)

## Robustness check: magnitude only
m_abs <- lm(
  abs_error ~ days_to_meeting +
    log_volume_1wk +
    log_liquidity +
    spread +
    competitive +
    option_type,
  data = d
)

summary(m_abs)

