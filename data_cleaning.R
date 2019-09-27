a=read.csv('BodyFat.csv')
a$IDNO=NULL
lm.body_dense=lm(a$BODYFAT~I(1/a$DENSITY))
par(mfcol=c(2,2))
plot(lm.body_dense)
lm.dirte <- lm(BODYFAT ~ ., data=subset(a,select=-DENSITY))
par(mfcol=c(2,2))
plot(lm.dirte)
#leverage
plot(lm.dirte, which=4)
abline( h = 4/(252-15),lty=2 )

colMeans(a)#This is for checking whether the data is normal.
print(a[42,])#height
print(a[39,])#weight

print(a[86,])#age, but can be save
lm.dirte1 <- lm(BODYFAT ~ ., data=subset(a[c(-42,-39),],select=-DENSITY))
par(mfcol=c(2,2))
plot(lm.dirte1)

plot(lm.dirte1, which=4)
abline( h = 4/(252-15-2),lty=2)
# The most noticable is still leverage
print(a[221,])#age and ankle

#outlier
library(car)
outlierTest(lm.dirte1)
print(a[224,])#bode fat is low, we choose to reserve

clean.a=a[-c(39,42),]
write.csv(clean.a,file='cleanfile.csv',row.names = F)
