bodyfat.dat=read.csv("data/cleanfile.csv")

library(leaps)
library(caret)
#function definitions
#my.regsub selects variables using aic and cp
my.regsub <- function(matrix,y,nbest,method,nvmax=8){
  n<-length(y)#sample size
  k=(2:(nvmax+1))#possible numbers of coefficients
  temp <- regsubsets(matrix,y,nbest=nbest,method=method,nvmax=nvmax)
  temp.mat <- cbind(summary(temp)$which,summary(temp)$cp,
                    n*log(2*pi)+n*log(summary(temp)$rss/(n-k))+n+k+2,
                    n*log(2*pi)+n*log(summary(temp)$rss/(n-k))+n-k+(k+1)*log(n),
                    1-summary(temp)$rss/sum((y-mean(y))^2))
  dimnames(temp.mat)[[2]] <- c(dimnames(summary(temp)$which)[[2]],
                               "cp", "aic", "bic","r_square")
  return(temp.mat)
}

regsub.mat<-my.regsub(bodyfat.dat[,-c(1,2)],bodyfat.dat$BODYFAT,nbest=1,method = "exhaustive")
regsub.mat[which.min(regsub.mat[,"aic"]),]
#Keep AGE, HEIGHT, NECK, CHEST, ABDOMEN, FOREARM, WRIST under AIC criteron
regsub.mat[which.min(regsub.mat[,"cp"]),]
#Keep AGE, HEIGHT, NECK, CHEST, ABDOMEN, FOREARM, WRIST under Mallow's Cp criteron
regsub.mat[which.min(regsub.mat[,"bic"]),]
#Keep WEIGHT, ABDOMEN, WRIST under BIC criteron
plot(regsub.mat[,"r_square"],type="b",
     main=bquote("Variable selection by"~R^2),
     xlab="Number of variables",
     ylab=expression(R^2)
)
#model1
set.seed(123)
train_control <- trainControl(method="cv", number=4)
model1 <- train(BODYFAT~AGE+HEIGHT+NECK+CHEST+ABDOMEN+FOREARM+WRIST,data=bodyfat.dat, 
               trControl=train_control, 
               method="lm",metric="RMSE")
print(model1)
#model2
model2 <- train(BODYFAT~WEIGHT+ABDOMEN+WRIST,data=bodyfat.dat, 
               trControl=train_control, 
               method="lm",metric="RMSE")
print(model2)
#model3
model3<- train(BODYFAT~WEIGHT+ABDOMEN,data=bodyfat.dat, 
                trControl=train_control, 
                method="lm",metric="RMSE")
print(model3)
