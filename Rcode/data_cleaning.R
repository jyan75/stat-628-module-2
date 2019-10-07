rm(list=ls())
require(car)
require("MVA")
require("biwt")

bodyfat <- read.table('../data/BodyFat.csv',header = T,sep = ',')
plot(bodyfat$BODYFAT,1/bodyfat$DENSITY,type = 'n',xlab="BODYFAT",ylab="1/DENSITY",xaxt="n",yaxt="n");axis(1,cex.axis=0.5);axis(2,cex.axis=0.8)
text(bodyfat$BODYFAT,1/bodyfat$DENSITY,bodyfat$IDNO,cex=0.5)

par(mfrow=c(1,2))
plot(bodyfat$ADIPOSITY,bodyfat$WEIGHT/bodyfat$HEIGHT^2,type = 'n',xlab="ADIPOSITY",ylab="WEIGHT/HEIGHT^2")
text(bodyfat$ADIPOSITY,bodyfat$WEIGHT/bodyfat$HEIGHT^2,bodyfat$IDNO,cex=0.5)
plot(bodyfat[-42,]$ADIPOSITY,bodyfat[-42,]$WEIGHT/bodyfat[-42,]$HEIGHT^2,type = 'n',xlab="ADIPOSITY",ylab="WEIGHT/HEIGHT^2")
text(bodyfat[-42,]$ADIPOSITY,bodyfat[-42,]$WEIGHT/bodyfat[-42,]$HEIGHT^2,bodyfat[-42,]$IDNO,cex=0.4)

bodyfat[42,'HEIGHT'] <- sqrt(bodyfat[42,'WEIGHT']/bodyfat[42,'ADIPOSITY']*703)
bodyfat[162,'HEIGHT'] <- sqrt(bodyfat[162,'WEIGHT']/bodyfat[162,'ADIPOSITY']*703) 
bodyfat[220,'HEIGHT'] <- sqrt(bodyfat[220,'WEIGHT']/bodyfat[220,'ADIPOSITY']*703) 

data.frame(Quartiles=quantile(bodyfat[-c(182,96,76,48),]$BODYFAT))
options(repr.plot.width=3.5, repr.plot.height=3.5)
boxplot(bodyfat[-c(182,96,76,48),]$BODYFAT,main="Boxplot of BODYFAT")

options(repr.plot.width=6, repr.plot.height=6)
pairs(bodyfat[-c(48,76,96,182),3:17], 
      panel = function(x,y, ...) {
        text(x, y, bodyfat$IDNO,cex = 1, pos = 2)
        bvbox(cbind(x,y), add = TRUE,method = "robust")
      })

set.seed(2)
x <- sample(1:252,18)
y <- x[!x %in% c(48,76,96,182)]
options(repr.plot.width=5, repr.plot.height=5)
subdata <- bodyfat[unique(c(39,41,212,y)),c('IDNO',"ADIPOSITY","BODYFAT","CHEST","NECK",'HEIGHT','WRIST')]
stars(subdata[,c("ADIPOSITY","BODYFAT","CHEST","NECK",'HEIGHT','WRIST')],
      nrow=5,ncol=5,labels=subdata$IDNO)

options(repr.plot.width=6, repr.plot.height=6)
bodyfat$IDNO=NULL
lm.dirte <- lm(BODYFAT ~ ., data=subset(bodyfat[-c(39,41,48,76,96,182),],select=-DENSITY))
par(mfcol=c(2,2))
plot(lm.dirte)
options(repr.plot.width=8, repr.plot.height=4)
par(mfcol=c(1,2))
lm.dirte.hats = hatvalues(lm.dirte)
plot(lm.dirte.hats, type = "h", ylab = "Leverage",main="Leverage") 
text(lm.dirte.hats, cex = 1)
abline(h=2*15/(252-6), lty = 2,col=2) 
plot(lm.dirte, which=4)
abline( h = 4/(252-15-6),lty=2 ,col=2)

write.csv(bodyfat,"../data/cleanfile.csv")



