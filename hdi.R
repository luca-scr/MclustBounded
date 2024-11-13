# install.packages(c("mclust", "mclustAddons"))
library(mclustAddons)

data  = read.csv("hdi.csv")
hdi = data$HDI

hist(hdi, breaks = seq(0, 1, by=0.05)+0.025, xlim = c(0,1),
     col = "lightgrey", border = "white", main = NULL)
rug(hdi); box()

GMM = Mclust(hdi)
summary(GMM)
GMM$entropy = with(GMM, -rowSums(z * ifelse(z > 0, log(z), 0)))
GMM$NEC = with(GMM, ifelse(G == 1, 0, sum(entropy)/(n * log(G))))
GMM$NEC

GMMB = MclustBounded(hdi, G = 1:5, lbound = 0, ubound = 1)
summary(GMMB$BIC)
GMMB$entropy = with(GMMB, -rowSums(z * ifelse(z > 0, log(z), 0)))
GMMB$NEC = with(GMMB, ifelse(G == 1, 0, sum(entropy)/(n * log(G))))
GMMB$NEC

plot(as.densityMclustBounded(GMMB), what = "density", data = hdi,
     breaks = seq(0, 1, by=0.05)+0.025, xlim = c(0,1))
rug(hdi)

x0 = seq(0, 1, length = 201)
cdens = predict(as.densityMclustBounded(GMMB),
                newdata = x0, what = "cdens", logarithm = TRUE)
cdens = exp(sweep(cdens, MARGIN = 2, FUN = "+", 
                  STATS = log(GMMB$parameters$pro)))
hist(hdi, probability = TRUE, breaks = seq(0, 1, by=0.05)+0.025, 
     xlim = c(0,1), ylim = range(0,cdens),
     col = "lightgrey", border = "white", main = NULL)
rug(hdi); box()
matplot(x0, cdens, type = "l", add = TRUE,
        col = mclust.options("classPlotColors"), lty = 1)

