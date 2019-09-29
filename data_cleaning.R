rm(list=ls())

library(car)
bodyfat.raw=read.csv('data/BodyFat.csv')
bodyfat.raw$IDNO=NULL
lm.body_dense=lm(bodyfat.raw$BODYFAT~I(1/bodyfat.raw$DENSITY))
par(mfcol=c(2,2))
plot(lm.body_dense)

lm.dirte <- lm(BODYFAT ~ ., data=subset(bodyfat.raw[-c(48,96,76),],select=-DENSITY))
par(mfcol=c(2,2))
plot(lm.dirte)

#detect influential observations
#leverage
par(mfcol=c(1,2))
lm.dirte.hats = hatvalues(lm.dirte)
plot(lm.dirte.hats, type = "h", ylab = "Leverage") 
text(lm.dirte.hats, labels = 1:249, cex = 1) 
abline(h=2*15/(252-3), lty = 2,col=2) 
#cook's distance
plot(lm.dirte, which=4)
abline( h = 4/(252-15-3),lty=2 ,col=2)
#42, 39, 86 are influential points

colMeans(bodyfat.raw)#This is for checking whether the data is normal.
print(bodyfat.raw[42,])#height
print(bodyfat.raw[39,])#weight
print(bodyfat.raw[86,])#age, but can be save

lm.dirte1 <- lm(BODYFAT ~ ., data=subset(bodyfat.raw[-c(42,39,48,96,76),],select=-DENSITY))
par(mfcol=c(2,2))
plot(lm.dirte1)

# detect influential observations again
#leverage
par(mfcol=c(1,2))
lm.dirte1.hats = hatvalues(lm.dirte1)
plot(lm.dirte1.hats, type = "h", ylab = "Leverage") 
text(lm.dirte1.hats, labels = 1:247, cex = 1) 
abline(h=2*15/(252-3), lty = 2,col=2) 
#cook's distance
plot(lm.dirte1, which=4)
abline( h = 4/(252-15-5),lty=2)
# 221 is an obvious influential point
print(bodyfat.raw[221,])#age and ankle

#outlier
outlierTest(lm.dirte1)
print(bodyfat.raw[224,])#bode fat is low, we choose to reserve

clean.bodyfat=bodyfat.raw[-c(39,42,48,96,76),]
write.csv(clean.bodyfat,file='cleanfile.csv',row.names = F)
