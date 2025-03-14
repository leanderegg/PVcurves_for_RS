######## Plotting Figures for Presentations/Manuscript####################
# requires running Preliminary_Analysis_contributed... to get all the data in



#_________________________________________________________________________________
########### Example PV Curve Figures ##################################

mypal1 <- brewer.pal(n=5,"Dark2")
palette(mypal1)

######## . RWC PV curves #############
quartz(width=4, height=4)
par(mgp=c(2.5,1,0), cex.lab=1.1, font.lab=1)
plot(Psi~I(R*100), ecs[which(ecs$Species=="Quercus douglasii"),], col=factor(Species), type="b", ylab=expression(paste(Psi," (MPa)")), xlab="% water loss (1-RWC)", ylim=c(-8.5,0), yaxs="i")
points(Psi~I(R*100), ecs[which(ecs$Species!="Quercus douglasii"),], col=2, type="b")
text(x=c(30, 35), y=c(-2.4,-7.2),labels = c("giant redwood","blue oak"), col=c(2,1))

#quartz.save("Results/RWC_PVcurve_example_clean_v1.pdf", type="pdf")

lines(y=ecs$TLP[1:2], x=c(-5,(1-ecs$totRWCtlp[1])*100), col=1, lwd=2)
lines(y=ecs$TLP[c(nrow(ecs)-1, nrow(ecs))], x=c(-5,(1-ecs$totRWCtlp[nrow(ecs)])*100), col=2, lwd=2)

#quartz.save("Results/RWC_PVcurve_example_TLP_v1.pdf", type="pdf")
lines(y=c(-9,ecs$TLP[1]), x=rep((1-ecs$totRWCtlp[1])*100,2), col=1, lty=2, lwd=2)
lines(y=c(-9,ecs$TLP[nrow(ecs)]), x=rep((1-ecs$totRWCtlp[nrow(ecs)])*100,2), col=2, lty=2, lwd=2)
#quartz.save("Results/RWC_PVcurve_example_RWCTLP_v1.pdf", type="pdf")



######## . RWC instead of 1-R PV curves #############
quartz(width=4, height=4)
par(mgp=c(2.5,1,0), cex.lab=1.1, font.lab=1)
plot(Psi~I((1-R)*100), ecs[which(ecs$Species=="Quercus douglasii"),], col=factor(Species), type="b", ylab=expression(paste(Psi," (MPa)")), xlab="Relative Water Content (RWC, %)", ylim=c(-8.5,0), yaxs="i")
points(Psi~I((1-R)*100), ecs[which(ecs$Species!="Quercus douglasii"),], col=2, type="b")
text(x=c(70, 65), y=c(-2.4,-7.2),labels = c("giant redwood","blue oak"), col=c(2,1))

#quartz.save("Results/RWC_PVcurve_example_clean_v2.pdf", type="pdf")

lines(y=ecs$TLP[1:2], x=c(-5,(ecs$totRWCtlp[1])*100), col=1, lwd=2)
lines(y=ecs$TLP[c(nrow(ecs)-1, nrow(ecs))], x=c(-5,(ecs$totRWCtlp[nrow(ecs)])*100), col=2, lwd=2)

#quartz.save("Results/RWC_PVcurve_example_TLP_v2.pdf", type="pdf")
lines(y=c(-9,ecs$TLP[1]), x=rep((ecs$totRWCtlp[1])*100,2), col=1, lty=2, lwd=2)
lines(y=c(-9,ecs$TLP[nrow(ecs)]), x=rep((ecs$totRWCtlp[nrow(ecs)])*100,2), col=2, lty=2, lwd=2)
#quartz.save("Results/RWC_PVcurve_example_RWCTLP_v2.pdf", type="pdf")



#### Figure in raw water/area space
quartz(width=4, height=4)
par(mgp=c(2.5,1,0), cex.lab=1.1, font.lab=1)
plot(Psi~g_h20_m2, ecs[which(ecs$Species=="Quercus douglasii"),], col=factor(Species), type="b", ylab=expression(paste(Psi," (MPa)")), xlab="g Water (per m2 leaf)", ylim=c(-8.5,0), yaxs="i", xlim=c(90,730))
points(Psi~g_h20_m2, ecs[which(ecs$Species!="Quercus douglasii"),], col=2, type="b")
text(x=c(400, 260), y=c(-2.9,-7),labels = c("giant redwood","blue oak"), col=c(2,1))

#quartz.save("Results/WA_PVcurve_example_clean_v1.pdf", type="pdf")


# SWC lines
lines(y=c(-9,1), x=rep((ecs$SWC[1]*ecs$`Dry mass`[1]/ecs$Fresh_area_m2[1]),2), col=1, lty=2, lwd=2)
lines(y=c(-9,1), x=rep((ecs$SWC[nrow(ecs)]*ecs$`Dry mass`[nrow(ecs)]/ecs$Fresh_area_m2[nrow(ecs)]),2), col=2, lty=2, lwd=2)

#quartz.save("Results/WA_PVcurve_example_SWA_v1.pdf", type="pdf")

# TLP lines
lines(y=c(-9,ecs$TLP[1]), x=rep((ecs$totRWCtlp[1]*ecs$SWC[1]*ecs$`Dry mass`[1]/ecs$Fresh_area_m2[1]),2), col=1, lty=2, lwd=2)
lines(y=ecs$TLP[1:2], x=c(0,ecs$totRWCtlp[1]*ecs$SWC[1]*ecs$`Dry mass`[1]/ecs$Fresh_area_m2[1]), col=1, lwd=2)
#redwood
lines(y=c(-9,ecs$TLP[nrow(ecs)]), x=rep((ecs$totRWCtlp[nrow(ecs)]*ecs$SWC[nrow(ecs)]*ecs$`Dry mass`[nrow(ecs)]/ecs$Fresh_area_m2[nrow(ecs)]),2), col=2, lty=2, lwd=2)
lines(y=ecs$TLP[c(nrow(ecs)-1, nrow(ecs))], x=c(0,(ecs$totRWCtlp[nrow(ecs)]*ecs$SWC[nrow(ecs)]*ecs$`Dry mass`[nrow(ecs)]/ecs$Fresh_area_m2[nrow(ecs)])), col=2, lwd=2)

#quartz.save("Results/WA_PVcurve_example_full_v1.pdf", type="pdf")




########## Map Figure ###############
quartz(width=4, height=3)
maps::map(database="world", col="black")
locs <- fullpvs %>% group_by(Lat, Lon) %>% summarise(nCurves = n(), nSpecies=length(unique(Species)))
points(Lat~Lon, locs, pch=16, cex=sqrt(nSpecies/pi)/2, col="green")
mtext("Full Dataset (size = # species)")
mtext(paste(nrow(fullpvs),"total obs and",length(unique(fullpvs$Species)), "spp"), side=1, cex=.8)
mtext(paste("plus", length(which(is.na(fullpvs$Lat)))," obs and",length(unique(fullpvs$Species[which(is.na(fullpvs$Lat))])), "spp without locations"), side=1, cex=.8)




####### Taxonomic Variance Decomp with rarified dataset ##########
quartz(width=5, height=3.5)
par(mgp=c(3,.7,0), cex.lab=1.3, cex.axis=1.1, mfrow=c(1,1), mar=c(6,2,3,6), oma=c(0,3,0,0))

mypal <- brewer.pal(n=9, "Set1")
colchoices <- c(1,2,4,3,6)
cols <-rev(c(mypal[colchoices[c(1,2,3)]],"#CCCCCC"))#brewer.pal(11, "RdBu")[c(11,9,1, 6)]
# "#CCCCCC" "#984EA3" "#377EB8" "#E41A1C"
#cols <- brewer.pal(11, "RdBu")[c(1,11,9,7)]
# cols <- "#B2182B" "#053061" "#F7F7F7" "#4393C3"
#"#67001F" "#053061" "#4393C3" "#D1E5F0"
barplot(as.matrix(traitvars_rar_scaled[,-7]),beside=F,legend.text = F,xpd = T, names.arg = c("TLP","RWCtlp","LMA", "SWC","SWA","C[T]", "TWL"), las=2,args.legend = list(x=4, y=1.3, ncol=2), col = paste0(cols,"99"), ylab="Proportion of total Variance", xlab="")
mtext("Proportion of Total Variance", side=2, line=2.8)

#legend(xpd=T, x = 0, y=2.3, legend=c("W/Spp", "Btw Spp", "Btw Genera", "Btw Families"), fill=paste0(cols,"CC")[c(4,3,2,1)], ncol=2, bty="n",  cex=1.2)
legend(xpd=NA, x = 8.5, y=0.7, legend=rev(c("btw Fams","btw Gen","btw Spp","w/in Spp")), fill=rev(paste0(cols,"99")), ncol=1, bty="n",  cex=1)




####### Predictor Type Variance Decomp with rarified dataset ##########
quartz(width=5, height=3.5)
par(mgp=c(3,.7,0), cex.lab=1.3, cex.axis=1.1, mfrow=c(1,1), mar=c(6,2,3,6.5), oma=c(0,3,0,0))

mypal <- brewer.pal(n=9, "Set1")
# "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628" "#F781BF" "#999999"

barplot(height=t(as.matrix(data.frame(test.rand))), beside = F, las=2, col=c(mypal[1:5], "white",mypal[6]), ylab="% Variance Explained",
        names.arg = c("TLP","SWA","C[T]","TWL"),)
mtext("% Variance Explained",side = 2, line=2.5, cex=1.1)
legend(xpd=NA, x = 4.7, y=0.99, legend=rev(colnames(test.rand)), fill=rev(c(mypal[1:5], "white",mypal[6])), ncol=1, bty="n",  cex=1)


######### SWC vs LMA ################

quartz(width=4, height=4)
par(mgp=c(2.5,1,0), cex.lab=1.2, font.lab=1, oma=c(0,1.5,0,0))
plot(SWA~LMA, fullpvs, pch=16, col=factor(Gymno.Angio), log="xy", ylab="SWA \n(g H2O m-2)", xpd=NA)
legend("bottomright", legend=c("Gymno","Angio"), pch=16, col=mypal[c(2,1)], bty="n", xpd=NA)
#quartz.save("Results/SWC-LMA_v1.pdf", type="pdf")

quartz(width=4, height=4)
par(mgp=c(2.5,1,0), cex.lab=1.2, font.lab=1, oma=c(0,1.5,0,0))
plot(water_loss.m2~LMA, fullpvs, pch=16, col=factor(Gymno.Angio), log="xy", ylab="TWL g water m-2\n(sat to TLP)", xpd=NA)
#legend("bottomright", legend=c("Gymno","Angio"), pch=16, col=mypal[c(2,1)], bty="n", xpd=NA)
quartz.save("Results/TWL-LMA_v1.pdf", type="pdf")


quartz(width=4, height=4)
par(mgp=c(2.5,1,0), cex.lab=1.2, font.lab=1, oma=c(0,1.5,0,0))
plot(CFT.g.m2~LMA, fullpvs, pch=16, col=factor(Gymno.Angio), log="xy", ylab=expression(paste(C[T]," (g*",MPa^-1,"*",m^-2,")")), xpd=NA)
abline(lm(log(CFT.g.m2,base=10)~I(log(LMA,base=10)), fullpvs[which(fullpvs$Gymno.Angio=="Gymnosperm"),]), lwd=2, col=2)
abline(lm(log(CFT.g.m2, base=10)~I(log(LMA,base=10)), fullpvs[which(fullpvs$Gymno.Angio=="Angiosperm"),]), lwd=2, col=1, lty=2)

#legend("bottomright", legend=c("Gymno","Angio"), pch=16, col=mypal[c(2,1)], bty="n", xpd=NA)
quartz.save("Results/CFT-LMA_v1.pdf", type="pdf")
