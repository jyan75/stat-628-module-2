rm(list=ls())

require(car)
require("MVA")
require("biwt")

bodyfat <- read.table('data/BodyFat.csv',header = T,sep = ',')
colnames(bodyfat)
attach(bodyfat)
#"IDNO" "BODYFAT"   "DENSITY"   "AGE"       "WEIGHT"    "HEIGHT"    "ADIPOSITY"
#"NECK"      "CHEST"     "ABDOMEN"   "HIP"       "THIGH"     "KNEE"     
#"ANKLE"     "BICEPS"    "FOREARM"   "WRIST"    

plot(BODYFAT,1/DENSITY,type = 'n')
text(BODYFAT,1/DENSITY,IDNO)
abline(v=c())

bodyfat[216,]#keep, very big ADIPOSITY and ABDOMEN
bodyfat[172,]#keep, very small ADIPOSITY and ABDOMEN

#Drop, 48 76 96 182

plot(ADIPOSITY,WEIGHT/HEIGHT^2,type = 'n')
text(ADIPOSITY,WEIGHT/HEIGHT^2,IDNO)

bodyfat[41,]#keep, very big BODYFAT and ABDOMEN
bodyfat[39,]#keep, very big BODYFAT and ABDOMEN

boxplot(BODYFAT,main="Bodyfat")

boxplot(WEIGHT,main="Weight")

plot(HEIGHT,type = 'n')
text(IDNO,y=HEIGHT,labels = IDNO)
bodyfat[42,'HEIGHT'] <- sqrt(bodyfat[42,'WEIGHT']/bodyfat[42,'ADIPOSITY']*703) #Recalculate weight based on ADIPOSITY and WEIGHT 
bodyfat[162,'HEIGHT'] <- sqrt(bodyfat[162,'WEIGHT']/bodyfat[162,'ADIPOSITY']*703) #Recalculate weight based on ADIPOSITY and WEIGHT 
bodyfat[220,'HEIGHT'] <- sqrt(bodyfat[220,'WEIGHT']/bodyfat[220,'ADIPOSITY']*703) #Recalculate weight based on ADIPOSITY and WEIGHT 

pairs(bodyfat[-c(48,76,96,182),])

pairs(bodyfat[-c(48,76,96,182),], 
      panel = function(x,y, ...) {
        text(x, y, IDNO,cex = 1, pos = 2)
        bvbox(cbind(x,y), add = TRUE,method = "robust")
      })
#we will check 39 41 212

set.seed(2)
x <- sample(1:252,22)
y <- x[!x %in% c(48,76,96,182)]

subdata <- bodyfat[unique(c(39,41,212,y)),c('IDNO',"ADIPOSITY","BODYFAT","CHEST","NECK",'HEIGHT','WRIST')]
stars(subdata[,c("ADIPOSITY","BODYFAT","CHEST","NECK",'HEIGHT','WRIST')],
      nrow=5,ncol=5,labels=subdata$IDNO)

# Preorganize the data( look at 9-16 variables)
require(ggplot2)
bodyfat.raw=read.csv('data/BodyFat.csv')

p <- ggplot(bodyfat, aes(x=ABDOMEN)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(ABDOMEN)),
              color="blue", linetype="dashed", size=1)

#bodyfat[bodyfat$ABDOMEN==max(bodyfat$ABDOMEN),] Fat 39

p <- ggplot(bodyfat, aes(x=HIP)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(HIP)),
              color="blue", linetype="dashed", size=1)

#bodyfat[bodyfat$HIP>140,] Fat 39

p <- ggplot(bodyfat, aes(x=THIGH)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(THIGH)),
              color="blue", linetype="dashed", size=1)

#bodyfat[bodyfat$THIGH>80,] Fat 39

p <- ggplot(bodyfat, aes(x=KNEE)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(KNEE)),
              color="blue", linetype="dashed", size=1)

#bodyfat[bodyfat$KNEE>48,] FAT 39

p <- ggplot(bodyfat, aes(x=ANKLE)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(ANKLE)),
              color="blue", linetype="dashed", size=1)

#bodyfat[bodyfat$ANKLE>30,] They should be normal(average 27cm).

p <- ggplot(bodyfat, aes(x=BICEPS)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(BICEPS)),
              color="blue", linetype="dashed", size=1)

#bodyfat[bodyfat$BICEPS>40,] 39

p <- ggplot(bodyfat, aes(x=FOREARM)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(FOREARM)),
              color="blue", linetype="dashed", size=1)

ggplot(bodyfat, aes(BODYFAT, FOREARM)) + geom_point()

#bodyfat[bodyfat$FOREARM<22,] 175, It is normal

p <- ggplot(bodyfat, aes(x=WRIST)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(WRIST)),
              color="blue", linetype="dashed", size=1)

#bodyfat[bodyfat$WRIST>21,] 39, 41

ggplot(bodyfat, aes(BODYFAT, WRIST)) + geom_point()

#bodyfat.raw<-bodyfat[-c(39,41,48,76,96,182),]


bodyfat$IDNO=NULL

lm.dirte <- lm(BODYFAT ~ ., data=subset(bodyfat[-c(39,41,48,76,96,182),],select=-DENSITY))
par(mfcol=c(2,2))
plot(lm.dirte)

#detect influential observations
#leverage
par(mfcol=c(1,2))
lm.dirte.hats = hatvalues(lm.dirte)
plot(lm.dirte.hats, type = "h", ylab = "Leverage") 
text(lm.dirte.hats, labels = 1:256, cex = 1)
abline(h=2*15/(252-6), lty = 2,col=2) 
#cook's distance
plot(lm.dirte, which=4)
abline( h = 4/(252-15-6),lty=2 ,col=2)
#42, 40 and 86 are influential points

colMeans(bodyfat)#This is for checking whether the data is normal.
print(bodyfat[42,])#height, we delete it
print(bodyfat[86,])#age and ankle, but can be reserve
print(bodyfat[40,])#ankle and chest, we reserve it

lm.dirte1 <- lm(BODYFAT ~ ., data=subset(bodyfat[-c(42,39,41,48,76,96,182),],select=-DENSITY))
par(mfcol=c(2,2))
plot(lm.dirte1)

# detect influential observations again
#leverage
par(mfcol=c(1,2))
lm.dirte1.hats = hatvalues(lm.dirte1)
plot(lm.dirte1.hats, type = "h", ylab = "Leverage") 
text(lm.dirte1.hats, labels = 1:245, cex = 1) 
abline(h=2*15/(252-3), lty = 2,col=2) 
#cook's distance
plot(lm.dirte1, which=4)
abline( h = 4/(252-15-7),lty=2)
# 221 is an obvious influential point
print(bodyfat[221,])#age and ankle, we choose to reserve it.

#outlier
outlierTest(lm.dirte1)
#There is no outlier

clean.bodyfat=bodyfat[-c(42,39,41,48,76,96,182),]
write.csv(clean.bodyfat,file='data/cleanfile.csv',row.names = F)
