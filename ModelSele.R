bodyfat.dat=read.csv('cleanfile.csv')
View(bodyfat.dat)

library(leaps)
#function definitions
#my.regsub selects variables using aic and cp
my.regsub <- function(matrix,y,nbest,method,nvmax=8){
  n<-length(y)
  p<-ncol(matrix)+1
  temp <- regsubsets(matrix,y,nbest=nbest,method=method,nvmax=nvmax)
  temp.mat <- cbind(summary(temp)$which,summary(temp)$cp,
                    n*log(2*pi)+n*log(summary(temp)$rss/(n-(2:(nvmax+1))))+n+(2:(nvmax+1))+2,
                    summary(temp)$bic,
                    1-summary(temp)$rss/sum((y-mean(y))^2))
  dimnames(temp.mat)[[2]] <- c(dimnames(summary(temp)$which)[[2]],
                               "cp", "aic", "bic_drop")
  return(temp.mat)
}

# lm.mod1 <- lm(BODYFAT ~ .,data=subset(bodyfat.dat,select = -DENSITY))
# step(lm.mod1, direction = "both", k = 2)

regsub.mat<-my.regsub(bodyfat.dat[,-c(1,2)],bodyfat.dat$BODYFAT,nbest=1,method = "exhaustive")
regsub.mat[which.min(regsub.mat[,"aic"]),]
#Keep AGE, HEIGHT, NECK, CHEST, ABDOMEN, FOREARM, WRIST under AIC criteron
regsub.mat[which.min(regsub.mat[,"cp"]),]
#Keep AGE, HEIGHT, NECK, CHEST, ABDOMEN, FOREARM, WRIST under Mallow's Cp criteron
regsub.mat[which.min(regsub.mat[,"bic_drop"]),]
#Keep WEIGHT, ABDOMEN, WRIST under BIC criteron
plot(regsub.mat[,"r_square"],type="b",
     main=bquote("Variable selection by"~R^2),
     xlab="Number of variables",
     ylab=expression(R^2)
     )

