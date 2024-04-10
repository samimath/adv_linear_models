## More regression spline example:

## instead of piece-wise linear, 
## let's try cubic splines (these are called B-splines)
library(splines)
## specify 10 knots over 100 sample points:
matplot(bs(seq(0,1,length=100),df=10),
        type="l",ylab="",col='blue',lwd=2)

# repeat the process of fitting a spline regression:
nk = 5
bs_plot<-function(nk,data){
lmod <- lm(y ~ bs(x,nk),data)
plot(y ~ x, exa, col=gray(0.75),
     main = paste('num. of knots :',nk))
lines(m ~ x, data)
lines(predict(lmod) ~ x, data, lty=2, col='red')
}

par(mfrow = c(2,2))

bs_plot(nk = 4, data = exa)
bs_plot(nk = 10, data = exa)
bs_plot(nk = 20, data = exa)
bs_plot(nk = 50, data = exa)

## how to construct confidence interval?
## because B-splines uses regression method to get the coef,
## we can use that to derive confidence interval

ggplot(exa) +
  geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=m),col='red')+
  geom_smooth(aes(x=x,y=y), 
              method = 'gam',
              level = 0.99,
              formula = y~s(x,k=8))+theme_bw()


## Another way to implement B-splines:
## natural spline example:
library(splines)
## df = K+3 
fit <- lm(waiting ~ ns(eruptions, df=6), faithful)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Natural splines, 6 df)") + 
  geom_line(aes(x=eruptions, y=fitted(fit)), col='blue')+
  theme_bw()

fit2 <- lm(waiting ~ ns(eruptions, df=10), faithful)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Natural splines, 10 df)") + 
  geom_line(aes(x=eruptions, y=fitted(fit2)), col='purple')+
  theme_bw()


## local polynomial:
smr <- loess(waiting ~ eruptions, 
             span=0.75, ## window size
             degree=2,  ## locally quadratic
             family="gaussian", ## fitting by least square
             faithful #data 
)

ggplot(faithful) +
  geom_point(aes(x=eruptions,y=waiting)) +
  ggtitle("Old Faithful (Loess, span=0.75)") + 
  geom_line(aes(x=eruptions, y=fitted(smr)), col='purple')+theme_bw()

