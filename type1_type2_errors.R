library(MASS)

f <- function(nvar = 3, obs = 100, effect = 0.1) {
  cov <- matrix(rep(0, nvar^2), nrow = nvar)
  cov[1,] <- effect
  cov[,1] <- effect
  diag(cov) <- 1
  d <- mvrnorm(obs, mu = rep(0, nvar), Sigma = cov)
  d <- as.data.frame.matrix(d)
  varnames <- LETTERS[1:nvar]
  names(d) <- varnames
  form <- as.formula(paste0('A~', paste0(varnames[-1], collapse = '+')))
  m <- lm(form, data = d)
  coef(summary(m))[-1,4]
}

f2 <- function(..., B = 100) {
  sapply(1:B, function (x) {
    if (x %% 100 == 0) print(x)
    pvals <- f(...)
    sum(pvals < 0.05)
  })
}

inputs <- expand.grid(n = c(50, 500, 5000, 50000), effect = c(0, 0.01, 0.1, 0.3))

set.seed(22)
n_sig <- mapply(f2,
                nvar = 6, # number of variables is arbitrary in this simulation
                obs = inputs$n,
                effect = inputs$effect,
                B = 1e4, # this takes a while - set B to a lower value to run the code quickly
                SIMPLIFY = F)

inputs$n_sig <- sapply(n_sig, mean)
inputs$prop_sig <- inputs$n_sig / 5
round(matrix(inputs$prop_sig, ncol = 4) * 100, 0)
