


pv <- read.csv("/Users/leeanderegg/Desktop/HV_PVcurves_20200304.csv")
plot(WP~RWC, pv, type="n", ylab="Water Potential (MPa)", xlim=c(80,100))
for(i in 1:length(unique(pv$sample))){
  lines(WP~RWC, pv[which(pv$sample==i),], col=type)
}
abline(lm(WP~RWC, pv), lwd=3, col="blue")
abline(v=100, lwd=2, lty=3)
