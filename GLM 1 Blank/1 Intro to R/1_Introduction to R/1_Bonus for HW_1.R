x<-(1:6)
median(x)
?sd
sd(x)
?confint
confint(x)

x
xbar<-mean(x)
x_std<-sd(x)

length(x)
xbar+((1.96)*x_std/(sqrt(length(x))))
xbar+((1.96)*x_std/(sqrt(length(x))))

library(psych)
?describe
