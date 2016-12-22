aromatics_data = data.frame(rate=c(1.06, 0.79, 0.82, 0.89, 1.05, 0.95, 0.65, 1.15, 1.12),solvent = "Aromatics")
chloroalkanes_data = data.frame(rate=c(1.58, 1.12, 1.45, 0.91, 0.57, 0.83, 1.16, 0.43),solvent="Chloroalkanes")
ester_data = data.frame(rate=c(0.29, 0.43, 0.06, 0.06, 0.51, 0.09, 0.44, 0.10, 0.17, 0.55, 0.53, 0.17, 0.61, 0.34, 0.60),solvent="Esters")

allData <- rbind(aromatics_data,chloroalkanes_data, ester_data)

means = tapply(allData$rate,allData$solvent,mean);means
sdevs = tapply(allData$rate,allData$solvent,sd)

# Dot plot 
stripchart(rate~solvent, data=allData, method="stack", vertical=TRUE,
           pch=1, cex=1.5, xlab="solvent", ylab="rate", main="Dotplots")
title(sub="pre-analysis plot", adj=0, cex=5/6)
mtext("Example")
points(c(1,2,3),tapply(allData$rate,allData$solvent,mean),col=2,pch=8)
abline(h=mean(allData$rate),col=3)
legend(3,250, c("Observations", " Trt Mean","Grand Mean"), col = c(1,2,3), text.col= "black",
       lty=c(0,0,1),pch=c(1,8,NA),bg='gray90')

attach(allData)

m1=aov(rate~solvent)
summary(m1)

# Almost all conditions hold true
source("http://www.stat.ufl.edu/~athienit/check.R")
check(m1)

#### Obtain Tukey's Comparisons among levels of treatment
alloy.Tukey=TukeyHSD(m1, "solvent")
print(alloy.Tukey)
plot(alloy.Tukey, sub="Solvent Data", adj=0) 
mtext("Tukey Honest Significant Differences",side=3,line=0.5)
