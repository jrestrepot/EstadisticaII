install.packages("mvnormtest")
install.packages("tseries")
install.packages("MVN")
library(MVN)
library(tseries)

# MUESTRAS UNIVARIADAS

boxplot(ammonia)
hist(ammonia$Ammonia)
boxplot(aeration.rate)
hist(aeration.rate$Aeration)


x1.test <- shapiro.test(ammonia$Ammonia)
print(x1.test)

y1.test <- jb.norm.test(ammonia$Ammonia)
print(y1.test)


x2.test <- shapiro.test(aeration.rate$Aeration)
print(x2.test)

y2.test <- jb.norm.test(aeration.rate$Aeration)
print(y2.test)

# MUESTRA MULTIVARIADA

averager$V1 <- NULL
x3.test <- mvnormtest::mshapiro.test(t(averager))
print(x3.test)

y3.test <- mvn(averager, mvnTest="mardia")
print(y3.test$multivariateNormality)

