# In a 'Factors Associated With' study, the researcher includes a number of independent variables in a regression model, then reports those "significantly" associated with the outcome as risk factors
# There are various approaches and degrees of complexity in choosing which variables are significant
# To illustrate this, we have done our own 'Factors Associated With' study using publicly available data

# -----------------------
# functions and libraries
# =======================

# summarise coefficients from a glm object

modsum <- function (m, label = 'OR') {
  coef <- cbind(coef(m), confint(m))
  coef <- exp(coef)
  coef <- format(round(coef, 2), digits = 2, nsmall = 2)
  coef <- paste0(coef[,1], ' (', coef[,2], '-', coef[,3], ')')
  coef <- gsub('\\(', ' (', gsub(' ', '', coef))
  p <- summary(m)$coef[,4]
  p <- format(round(p, 3), digits = 3, nsmall = 3)
  p <- ifelse(p == "0.000", 'p<0.001', paste0('p=', p))
  r <- data.table(var = names(coef(m)), OR = paste0(coef, ', ', p))
  colnames(r) <- c('var', label)
  r[r$var != '(Intercept)',]
}

# vectorised binomial confidence interval

pt <- function (X, N) {
  f <- function (x, n) c(x/n, prop.test(x, n)$conf.int[1:2])
  `colnames<-`(t(mapply(f, x = X, n = N)), c('prop', 'lower', 'upper'))
}

library(data.table)
library(MASS)
library(RColorBrewer)

# ---------------
# load NSDUH 2019
# ===============

# https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2019-nsduh-2019-ds0001
# MJREC - cannabis use in the past 30 days
# AGE2 - age
# IRSEX - sex
# NEWRACE2 - race
# HEALTH - Would you say your health in general is excellent, very good, good, fair, or poor? 
# MILTFAMILY - ANY IMMEDIATE FAMILY CURRENTLY IN US MILITARY?
# YUHOSPYR - During the past 12 months, have you stayed overnight or longer in any type of hospital to receive treatment or counseling for emotional or behavioral problems that were not caused by alcohol or drugs? 
# ASTHMAEVR - Please read the list and type in the numbers of all of the conditions that a doctor or other health care professional has ever told you that you had. EVER TOLD HAD ASTHMA
# IRPRVHLT - PRIVATE HEALTH INSURANCE
# CELLNOTCL - Is there at least one telephone at this address that is not a cell phone? 
# COUTYP4 - COUNTY METRO/NONMETRO STATUS (2013 3-LEVEL) 
# ALCEVER - Have you ever, even once, had a drink of any type of alcoholic beverage? Please do not include times when you only had a sip or two from a drink.
# CIGEVER - Have you ever smoked part or all of a cigarette?

d <- fread("NSDUH_2019_Tab.txt", sep = '\t', header = T, select = c('AGE2', 'IRSEX', 'NEWRACE2', 'ASTHMAEVR', 'HEALTH2', 'ANYNSMH', 'MILTFAMLY', 'IRPRVHLT', 'CELLNOTCL', 'INCOME', 'COUTYP4', 'ALCEVER', 'CIGEVER', 'MJREC'))

# ----------------
# format variables
# ================

d[, age := factor(AGE2, 1:10, 12:21)]
d[, age := as.integer(as.character(age))]
d[, sex := factor(IRSEX, 1:2, c('male', 'female'))]
d[, race := factor(NEWRACE2, 1:7, c('white', 'black', 'other', 'other', 'other', 'other', 'hispanic'))]
d[, race2 := factor(race, c('white', 'black', 'other', 'hispanic'), c('white', 'other', 'other', 'other'))]
d[, health := factor(HEALTH2, 1:4, c('excellent', 'very good', 'good', 'fair/poor'))]
d[, health2 := factor(health, c('excellent', 'very good', 'good', 'fair/poor'), c('ex_vg', 'ex_vg', 'gd_fr_pr', 'gd_fr_pr'))]
d[, asthma := ASTHMAEVR == 1]
d[, mh_treatment := ANYNSMH %in% 1]
d[, military_family := MILTFAMLY == 1]
d[, private_health := IRPRVHLT == 1]
d[, landline := CELLNOTCL == 1]
d[, income := factor(INCOME, 1:4, c('<20k', '20k-49k', '50k-74k', '>75k'))]
d[, income2 := factor(income, c('<20k', '20k-49k', '50k-74k', '>75k'), c('<50k', '<50k', '50k+', '50k+'))]
d[, urban := factor(COUTYP4, 1:3, c('city', 'town', 'rurual'))]
d[, urban2 := factor(urban, c('city', 'town', 'rurual'), c('city', 'town_rural', 'town_rural'))]
d[, alcohol := ALCEVER == 1]
d[, smoking := CIGEVER == 1]
d[, can := MJREC == 1]

# limit to age 12-21 and drop rows with missing health

d <- d[AGE2 %in% 1:10] # age 12-21
d <- d[!is.na(health)]

# ----------------- 
# prevalence by age
# =================

tab <- with(d, table(age, can))
tab <- cbind(tab, total = rowSums(tab))
tab <- cbind(tab, pt(X = tab[,2], N = tab[,3]))

png('prev.png', height = 4, width = 5, units = 'in', res = 300)

par(mar = c(4, 4, 0, 0))
cols <- brewer.pal(3, 'Set3')
off <- 0.1
plot(1, type = 'n', xlim = c(0, 10), ylim = c(0, 0.35), axes = F, xlab = NA, ylab = NA)
rect(0:9 + off, 0, 1:10 - off, tab[,4], col = cols[1])
arrows(0:9 + 0.5, y0 = tab[,5], y1 = tab[,6], angle = 90, length = 0.05, code = 3)
axis(2, 0:7 * 5 / 100, paste0(0:7 * 5, '%'), las = 2, pos = 0)
axis(1, 0:9 + 0.5, 12:21, tick = F, pos = 0)
axis(1, c(0, 10), labels = F, pos = 0)
title(xlab = 'Age (years)', line = 2)
title(ylab = 'Prevalence')

dev.off()

# --------------------------------------------------
# set analysis variables and select a sample of 2500
# ==================================================

vars <- c('age', 'sex', 'race2', 'health2', 'asthma', 'mh_treatment', 'military_family', 'private_health', 'landline', 'income2', 'urban2', 'alcohol', 'smoking', 'can')
set.seed(734)
d2 <- d[sample(.N, 2500, replace = F), vars, with = F]

# -----------------
# regression models
# =================

# model with all variables
  
m1 <- glm(can ~ ., data = d2, family = 'binomial')
m1_sum <- modsum(m1, label = 'model1')

# stepwise model
  
intercept_only <- glm(can ~ 1, data = d2, family = 'binomial')
m2 <- step(intercept_only, direction = 'forward', scope = formula(m1), trace = 0)
m2_sum <- modsum(m2, label = 'model2')

# join results table

results_table <- merge(m1_sum, m2_sum, all.x = T, sort = F)
results_table$model2[is.na(results_table$model2)] <- 'NS'
results_table

write.csv(results_table, 'nsduh_regression_results.csv')
