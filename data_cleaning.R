library(car)
a=read.csv('BodyFat.csv')
a$IDNO=NULL
lm.body_dense=lm(a$BODYFAT~I(1/a$DENSITY))
par(mfcol=c(2,2))
plot(lm.body_dense)

lm.dirte <- lm(BODYFAT ~ ., data=subset(a[-c(48,96,76)],select=-DENSITY))
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

colMeans(a)#This is for checking whether the data is normal.
print(a[42,])#height
print(a[39,])#weight
print(a[86,])#age, but can be save

lm.dirte1 <- lm(BODYFAT ~ ., data=subset(a[-c(42,39,48,96,76),],select=-DENSITY))
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
print(a[221,])#age and ankle

#outlier
outlierTest(lm.dirte1)
print(a[224,])#bode fat is low, we choose to reserve

clean.a=a[-c(39,42,48,96,76),]
write.csv(clean.a,file='cleanfile.csv',row.names = F)
