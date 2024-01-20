# This code draws a plot showing the growth of 'Factors Associated With' studies over time

library(RColorBrewer)
cols <- brewer.pal(4, 'Set1')

d <- read.csv("https://raw.githubusercontent.com/danlewer/faw/main/pubmed_data.csv")
tt <- function (x, tt = 5) x[x %% tt == 0]
baseYear <- 1990
d <- d[d$Year >= baseYear,]
ind <- mapply(`/`, x = d, y = d[1,])
labs <- c('All PubMed\ncitations', 'Factors\nassociated with', 'Predictors of', 'Risk\nfactors for')

png('Fig1.png', height = 5, width = 5, units = 'in', res = 300)

par(mar = c(5, 5, 1, 7), xpd = NA)
plot(1, type = 'n', xlim = c(baseYear, 2022), ylim = c(0, 4), axes = F, xlab = NA, ylab = NA)
rect(baseYear, 0, 2022, 4)
mapply(lines,
       x = list(baseYear:2022),
       y = as.data.frame(log(ind[,-1])),
       col = cols,
       lwd = 1)
mapply(points,
       x = list(baseYear:2022),
       y = as.data.frame(log(ind[,-1])),
       col = cols,
       pch = 19)
axis(1, tt(baseYear:2022), pos = 0, labels = F)
text(tt(baseYear:2022), -0.25, labels = tt(baseYear:2022), srt = 45, adj = 1)
ys <- c(1:5, 10, 15, 20, 25, 40, 50)
axis(2, log(ys), labels = ys, pos = baseYear, las = 2)
mapply(text,
       x = 2023,
       y = log(ind[nrow(ind),-1]),
       col = cols,
       labels = labs,
       adj = 0)
title(ylab = 'Ratio of PubMed citations vs. 1990')
title(xlab = 'Year of publication')

dev.off()
