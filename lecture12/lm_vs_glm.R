## simple example to compare models implemented by lm or glm

## for illustration, let's take the cars dataset 
data <- cars
mod1<-lm(dist ~ speed, data = cars)
mod2<-glm(dist~speed, data = cars)

mod3 <-lm(log(dist) ~ speed, data = cars)
mod4 <-glm(dist ~ speed, 
           family = gaussian(link = 'log'),
           data = cars)
plot(fitted(mod1),fitted(mod2),
     xlab = 'lm',
     ylab = 'glm')
abline(0,1)

plot(fitted(mod3),log(fitted(mod4)),
     xlab = 'lm on log transform',
     ylab = 'glm with log link')
abline(0,1)