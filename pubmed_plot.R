library(RColorBrewer)
cols <- brewer.pal(4, 'Set1')

d <- read.csv("https://raw.githubusercontent.com/danlewer/faw/main/pubmed_data.csv")
tt <- function (x, tt = 5) x[x %% tt == 0]
baseYear <- 1990
d <- d[d$Year >= baseYear,]
ind <- mapply(`/`, x = d, y = d[1,])

m <- matrix(c(1:4, rep(5, 4)), ncol = 4)

fp1 <- function(y, mainlab = NA, col = 1) {
  plot(1, type = 'n', xlim = c(baseYear, 2022), ylim = c(0, max(y) * 1.1), axes = F, xlab = NA, ylab = NA)
  lines(x = d$Year, y = y, lwd = 2, col = col)
  axis(1, tt(baseYear:2022), pos = 0)
  axis(2, pos = baseYear, las = 2)
  rect(baseYear, 0, 2022, max(y) * 1.1)
  text(mean(c(baseYear, 2022)), max(y), mainlab, col = col)
}

labs <- c('All PubMed\ncitations', 'Factors\nassociated with', 'Predictors\nof', 'Risk\nfactors for')

png('pubmedplots.png', height = 6, width = 11.5, units = 'in', res = 300)

  layout(m)
  par(xpd = NA, mar = c(3, 3, 3, 3), oma = c(0, 5, 0, 0), cex = 0.8)
  mapply(fp1, 
         y = d[,-1],
         mainlab = labs,
         col = cols)
  mtext('Number of citations', side = 2, outer = T, line = 3)
  par(mar = c(1.5, 4, 1.5, 7))
  plot(1, type = 'n', xlim = c(baseYear, 2022), ylim = c(0, 4), axes = F, xlab = NA, ylab = NA)
  mapply(lines,
         x = list(baseYear:2022),
         y = as.data.frame(log(ind[,-1])),
         col = cols,
         lwd = 2)
  axis(1, tt(baseYear:2022), pos = 0)
  ys <- c(1:5, 10, 15, 20, 25, 40, 50)
  axis(2, log(ys), labels = ys, pos = baseYear, las = 2)
  rect(baseYear, 0, 2022, 4)
  mapply(text,
         x = 2023,
         y = log(ind[nrow(ind),-1]),
         col = cols,
         labels = labs,
         adj = 0)
  title(ylab = 'Ratio vs. 1990')

dev.off()
