# install.packages(c("mclust", "mclustAddons"))
library(mclustAddons)

wholesale = read.csv("wholesale.csv", comment.char = "#")
X = wholesale[,-(1:2)]
summary(X)
pairs(X, gap = 0)

GMM = Mclust(X, G = 2)
summary(GMM, parameters = TRUE)
clPairs(GMM$data, GMM$classification)
table(wholesale$Channel, Cluster = GMM$classification)
adjustedRandIndex(wholesale$Channel, GMM$classification)
GMM$entropy = with(GMM, -rowSums(z * ifelse(z > 0, log(z), 0)))
GMM$nce = with(GMM, ifelse(G == 1, 0, sum(entropy)/(n * log(G))))
GMM$nce

GMMB = MclustBounded(X, G = 2, lbound = rep(0,ncol(X)))
summary(GMMB, parameters = TRUE)
clPairs(GMMB$data, GMMB$classification)
table(wholesale$Channel, Cluster = GMMB$classification)
adjustedRandIndex(wholesale$Channel, GMMB$classification)
GMMB$nce
