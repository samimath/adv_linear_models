library(ggplot2)
library(ggpubr)
library(effects)
library(mgcv)
## gam example on ozone data
data("ozone")

## visualize the data:

data(ozone, package="faraway")

p1<-ggplot(ozone, aes(x=temp, y=O3)) + geom_point(size=1) + geom_smooth()+theme_bw()
p2<-ggplot(ozone, aes(x=ibh, y=O3)) + geom_point(size=1) + geom_smooth() + 
  theme(axis.text.x = element_text(angle = 90))+theme_bw()
p3<-ggplot(ozone, aes(x=ibt, y=O3)) + geom_point(size=1) + geom_smooth()+theme_bw()

ggarrange(p1,p2,p3,nrow = 1)

## first try, let's fit a linear model and see how it works:

lmod <- lm(O3 ~temp +ibh + ibt, ozone)

## Using the effects package, 
## we can visualize individual relationships 
## between the predictors and the response 
## via their regression coefficients


par(mfrow = c(1,3))
plot(Effect("temp", lmod, partial.residuals=TRUE))
plot(Effect("ibh", lmod, partial.residuals=TRUE))
plot(Effect("ibt", lmod, partial.residuals=TRUE))


## Using mgcv, we can try fitting 
## additive models made up of spine functions: 
ammgcv <- gam(O3 ~ s(temp)+s(ibh)+s(ibt),data=ozone)
summary(ammgcv)

## let's take a look at the transformations use:
par(mfrow = c(1,3))
plot(ammgcv, residuals=TRUE, select=1)
plot(ammgcv, residuals=TRUE, select=2)
plot(ammgcv, residuals=TRUE, select=3)

##Consider a mix of linear and smoothing functions :

am1 <- gam(O3 ~ s(temp)+s(ibh),data=ozone)
am2 <- gam(O3 ~ temp+s(ibh),data=ozone)
anova(am2,am1,test="F")

