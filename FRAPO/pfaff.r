library(FRAPO)
library(fPortfolio)
library(lattice)

#### LOAD DATA, CALC RETURNS ###########

data(SPISECTOR)
Idx  = interpNA(SPISECTOR[, -1], method = "before")
R    = returnseries(Idx, method = "discrete", trim = TRUE)
V    = cov(R)

##### PORTFOLIO OPTIMIZATION ############

GMVw = Weights(PGMV(R))
MDPw = Weights(PMD(R))
MTDw = Weights(PMTD(R)) 
ERCw = Weights(PERC(V))

######## GRAPH ALLOCATIONS  ##################
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))

dotchart(GMVw, xlim = c(0, 40), main = "GMV Allocation", pch=19)

dotchart(MDPw - GMVw, xlim = c(-20, 20), main = "MDP vs. GMV", pch=19)
abline(v = 0, col = "gray")

dotchart(MTDw - GMVw, xlim = c(-20, 20), main = "MTD vs. GMV", pch=19)
abline(v = 0, col = "gray")

dotchart(ERCw - GMVw, xlim = c(-20, 20), main = "ERC vs. GMV", pch=19)
abline(v = 0, col = "gray")

par(oldpar)
