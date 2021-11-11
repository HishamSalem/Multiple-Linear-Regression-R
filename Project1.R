attach(carmileage)
#Remove the first column and the last column
data1=carmileage[,-c(1)]
dim(data1)

#examine each of the variables 
summary(data1)
sapply(data1,sd)

hist(MPG)
boxplot(MPG)
hist(WT)
boxplot(WT)
hist(HP)
boxplot(HP)
hist(VOL)
boxplot(VOL)
hist(SP)
boxplot(SP)


car_data=data1[,-c(1,2)]
boxplot(car_data)

table(MPG)/length(WT)
car_data2=data1[,-c(4,5)]
boxplot(car_data2)


#Examine the relationships between pairs of variables
pairs(data1)
cor(data1)
cor.test(HP,MPG)
cor.test(WT,MPG)
cor.test(SP,MPG)
cor.test(VOL,MPG)

carmile.lm=lm(MPG~HP+WT+SP,data=data1)
summary(carmile.lm)

#Y intercept 194.12962
#Horse Power--> HP=0.40518
#Weight -->WT=-0.99037
#Speed --> SP= -1.32000 
#MPG=194.12962+0.40518HP-1.92210WT-1.32000SP

#Examine Residuals:
#Plot the residuals vs. the predicted values y_hat to see if there is evidence of a curved (rather than linear)relation
plot(fitted(carmile.lm),residuals(carmile.lm))
abline(0,0)
#Plot the residuals vs. each of the explanatory variables to check if the variances of the residuals are constant
plot(HP,residuals(carmile.lm),xlab="HP",ylab="Residuals")
abline(0,0)
plot(WT,residuals(carmile.lm),xlab="WT",ylab="Residuals")
abline(0,0)
plot(SP,residuals(carmile.lm),xlab="SP",ylab="Residuals")
abline(0,0)
#nomal quantile plot for residuals to check normality of residuals
qqnorm(residuals(carmile.lm),xlab="Normal Score", ylab="Residuals")
qqline(residuals(carmile.lm))
