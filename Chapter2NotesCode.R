



#Example: Comparing two different groups and seeing if they have the same distributional parameters
rm(list=ls(all = TRUE))
dat = read.table("C:/Users/benma/OneDrive/Documents/A Stellies/A2024/Semester1/ActSci 311/RCode/GLMs/ChronicMedicalConditions.txt", h=TRUE)
?attach
?str
attach(dat)
str(dat)
head(dat)
summary(Count[Region==1])
summary(Count[Region==2])
sd(Count[Region==1])
sd(Count[Region==2])
xtabs(~Count + Region, data=dat)


# Model 1:
#note how the poisson model just has an intercept
mod1 = glm(Count~1,family = poisson(link = 'identity'))
summary(mod1)

#Furthermore, the sum of squares of this model (which will enable us to compare) can be found as:
yhat1 = fitted.values(mod1)
yhat1 = predict(mod1)
r1 = (Count-yhat1)/sqrt(yhat1)
dat$r1 = r1
head(dat)
sum(r1^2) #this follows a chi-squared: (O-E)^2/E



#Model2
mod2 = glm(Count~factor(Region),family = poisson(link = 'identity'))
mod2

yhat2 = fitted.values(mod2)
r2 = (Count-yhat2)/sqrt(yhat2)
dat$r2 = r2
head(dat)
sum(r2^2)

#our hypothesis test
X1 = sum(r1^2)
X1
X2 = sum(r2^2)
X2
1-pchisq(X1 - X2,1)
detach(dat)




#Example: Comparing two different groups
rm(list=ls(all = TRUE))
dat = read.table("C:/Users/benma/OneDrive/Documents/A Stellies/A2024/Semester1/ActSci 311/RCode/GLMs/Birthweight.txt", h=TRUE)
attach(dat)
head(dat)
tail(dat)
c(mean(birth_weight[sex == 'male']),
  mean(birth_weight[sex == 'female']),
  mean(gestational_age[sex == 'male']),
  mean(gestational_age[sex == 'female']))
c(sd(birth_weight[sex == 'male']),
  sd(birth_weight[sex == 'female']),
  sd(gestational_age[sex == 'male']),
  sd(gestational_age[sex == 'female']))
boxplot(birth_weight~sex, data = dat, ylab = 'Birth weight (g)')
boxplot(gestational_age~sex, data = dat, ylab = 'Age (weeks)')
plot(birth_weight~gestational_age, pch = c(15,16)[(sex == 'female')+1],
     col = c('black','blue')[(sex == 'female')+1],
     main = '',
     xlab = 'Gestational age (weeks)', ylab = 'Birth weight (g)')
model_0 = glm(birth_weight ~ gestational_age + sex,
              family = gaussian(link = "identity"))
summary(model_0)
model_1 = glm(birth_weight ~ gestational_age + sex + gestational_age*sex,
              family = gaussian(link = "identity"))
summary(model_1)

par(mfrow = c(1,1))
betas0 = coefficients(model_0)
betas0
betas1 = coefficients(model_1)
betas1
x1 = seq(min(gestational_age),max(gestational_age),1/100)
y1_f = betas1[1] + betas1[2]*x1 + betas1[3]*0 + betas1[4]*0*x1
y1_m = betas1[1] + betas1[2]*x1 + betas1[3]*1 + betas1[4]*1*x1
y0_f = betas0[1] + betas0[2]*x1 + betas0[3]*0
y0_m = betas0[1] + betas0[2]*x1 + betas0[3]*1
plot(birth_weight~gestational_age,
     pch = c(15,16)[(sex == 'female')+1],
     col = c('black','blue')[(sex == 'female')+1],
     main = '', xlab = 'Gestational age (weeks)', ylab = 'Birth weight (g)')
lines(y1_f~x1,lty = 1, col = 'blue')
lines(y1_m~x1,lty = 1, col = 'black')
lines(y0_f~x1,lty = 3, col = 'blue')
lines(y0_m~x1,lty = 3, col = 'black')
legend('topleft', lty = c(1, 3,NA,NA),pch = c(NA,NA,15,16),
       legend = c('Model 1','Model 0','Male','Female'),
       col = c(1,1,1,4),bty = 'n' )
plot(model_0)

