# This code shows how multiple testing affects the interpretation of statistical significance 

library(pwr)

# ------------------------
# chance of false positive
# ------------------------

# where number of true effects is zero

tests <- 1:20
fpr <- 1 - 0.95 ^ tests

# --------------------
# ppv = TP / (TP + FP)
# --------------------

# assuming no risk of false negative

ppv <- function (nvar = 10, neffect = 1, obs = 50, d = 0.1) {
  prob_FN <- 1 - pwr.norm.test(d = d, n = obs)$power
  FN <- neffect * prob_FN
  TP <- neffect - FN
  FP <- 0.05 * (nvar - neffect)
  TN <- nvar - (neffect + FP)
  c(ppv = TP / (TP + FP))
}


samples <- c(50, 100, 250, 500, 1000, 10000)
inputs_ppv <- expand.grid(nvar = 1:20, neffect = 1:5, obs = samples)
inputs_ppv$ppv <- ppv(nvar = inputs_ppv$nvar, 
                      neffect = inputs_ppv$neffect, 
                      obs = inputs_ppv$obs)
inputs_ppv <- inputs_ppv[inputs_ppv$neffect <= inputs_ppv$nvar,]

# -----
# plots
# -----

png('Fig4.png', height = 7, width = 12, units = 'in', res = 300)

par(mar = c(5, 7, 3, 1), oma = c(1, 0, 1, 5), mfrow = c(1, 2), xpd = NA)

# Panel A

plot(1, type = 'n', xlim = c(1, 20), ylim = c(0, 1), axes = F, xlab = NA, ylab = NA)

points(tests, fpr, pch = 20, cex = 0.7)
lines(tests, fpr)
axis(1, 1:10 * 2, pos = 0)
axis(2, 0:10/10, paste0(0:10*10, '%'), las = 2, pos = 1)
rect(1, 0, 20, 1)
title(xlab = 'Number of variables being studied')
title(ylab = 'Probability of at least one false positive')
text(10.5, 1.1, 'A: How likely are you to get a false positive,\ngiven no real risk factors?')

# panel B

ys <- inputs_ppv[inputs_ppv$neffect == 1 & inputs_ppv$nvar == 20, 'ppv']

plot(1, type = 'n', xlim = c(1, 20), ylim = c(0, 1), axes = F, xlab = NA, ylab = NA)

lapply(seq_along(samples), function (x) {
  with(inputs_ppv[inputs_ppv$neffect == 1 & inputs_ppv$obs == samples[x],], {
    points(nvar, ppv, pch = 20, cex = 0.7)
    lines(nvar, ppv)
  })
})
axis(1, 1:10 * 2, pos = 0)
axis(2, 0:10/10, paste0(0:10*10, '%'), las = 2, pos = 1)
rect(1, 0, 20, 1)
title(xlab = 'Number of variables being studied')
title(ylab = 'Positive predictive value')
text(10.5, 1.1, "B: How likely is an observed risk factor to be real,\ngiven one real risk factor?")
text(21.5, ys, prettyNum(samples, big.mark = ','), adj = 0)
text(21.5, max(ys) + 0.1, 'Sample\nsize', adj = 0)

dev.off()
