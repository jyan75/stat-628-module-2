
#I choose to include  WEIGHT, ABDOMEN, WRIST which was choosen by both AIC and BIC criteria, and
#satisfy the rule of thumb
#regression diagnostic
final.model <- lm(BODYFAT~WEIGHT + ABDOMEN + WRIST,data=bodyfat.dat)
summary(final.model)
par(mfrow = c(2,3))
plot(final.model, which = 1:6)

#Independence:Assume independence as we collect the data on randomly chosen individual
#linearity:
pairs(bodyfat.dat[,c("BODYFAT","WEIGHT","ABDOMEN","WRIST")])


##residual plots: no non-linear trend, can be considered as equal variance

##Response Normality:
MASS::boxcox(bodyfat.dat$BODYFAT ~ bodyfat.dat$WEIGHT+bodyfat.dat$ABDOMEN+bodyfat.dat$WRIST)
#lambda=1 is in the 95% confidence interval, so we don't do transformation to fit normality
#although the upper tail and power tail of qqplot is not good

##see whether has outlier(y too large or too small)
which(abs(rstudent(final.model))>2.576) # 2.576 is the 0.01 quantial of N(0,1)
which(abs(rstandard(final.model))>2.576)
car::outlierTest(final.model)

##see whether has high leverage point(x as a outlier with h too large)
(2*4/dim(bodyfat.dat)[1]) # 2p/n
unname(which(hatvalues(final.model)>0.03252033) )
#high leverage point doesn't mean outlier, we don't delete

##see high influential point(Cook's distance large)
4/(dim(bodyfat.dat)[1]-4) #4/n-p
unname(which(cooks.distance(final.model)>0.01652893))
sum(which(cooks.distance(final.model)>0.5))
#high leverage point doesn't mean outlier, we don't delete

#or just influence.measures(final.model)
