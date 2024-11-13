# install.packages(c("mclust", "mclustAddons"))
library(mclustAddons)

enzyme = read.csv("enzyme.csv", comment.char = "#")[,1]
xbreaks = seq(0, max(enzyme)*1.1, by=0.5)
hist(enzyme, breaks = 21, col = "lightgrey", border = "white", main = NULL); box()
rug(enzyme)

summary(mclustBIC(enzyme, modelNames = "V"))
summary(mclustICL(enzyme, modelNames = "V"))

GMM = Mclust(enzyme, modelNames = "V")
summary(GMM)

plot(as.densityMclust(GMM), what = "density",
     data = enzyme, xlab = "enzyme", breaks = 21)
rug(enzyme)

GMM$entropy = with(GMM, -rowSums(z * ifelse(z > 0, log(z), 0)))
GMM$NEC = with(GMM, ifelse(G == 1, 0, sum(entropy)/(n * log(G))))
GMM$NEC


GMMB = MclustBounded(enzyme, G = 1:5, modelNames = "V", lbound = 0, criterion = "BIC")
summary(GMMB$BIC)
GMMB = MclustBounded(enzyme, G = 1:5, modelNames = "V", lbound = 0, criterion = "ICL")
summary(GMMB$ICL)
summary(GMMB, parameters = TRUE)

plot(as.densityMclustBounded(GMMB), what = "density",
     data = enzyme, breaks = 21)
rug(enzyme)

GMMB$entropy = with(GMMB, -rowSums(z * ifelse(z > 0, log(z), 0)))
GMMB$NEC = with(GMMB, ifelse(G == 1, 0, sum(entropy)/(n * log(G))))
GMMB$NEC

x0 = seq(min(xbreaks), max(xbreaks), length = 1001)
cdens = predict(as.densityMclustBounded(GMMB), 
                newdata = x0, what = "cdens", logarithm = TRUE)
cdens = exp(sweep(cdens, MARGIN = 2, FUN = "+", 
                  STATS = log(GMMB$parameters$pro)))
hist(enzyme, breaks = 21, probability = TRUE,
     col = "lightgrey", border = "white", main = NULL); box()
rug(enzyme)
matplot(x0, cdens, type = "l", add = TRUE,
        col = mclust.options("classPlotColors"), lty = 1)

