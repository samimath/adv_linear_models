## Kernel smoothing functions
library(ggplot2)
library(KernSmooth)


## kernel  example:

x <- seq(-3,3,by=0.01)
db <- data.frame(
  x = x,
  f1 = 0.5*as.numeric(abs(x)<1),
  f2 = pmax(0, 0.75*(1-x^2)),
  f3 = dnorm(x),
  f4 = pmax(0, (1-abs(x)^3)^3/1.157143))
ggplot(db, aes(x=x)) +
  geom_line(aes(y=f1,col='Uniform')) +
  geom_line(aes(y=f2,col="Epanechnikov")) +
  geom_line(aes(y=f4,col="Tri-cube")) +
  geom_line(aes(y=f3,col="Gaussian")) +
  guides(col=guide_legend(title="Kernel")) +
  ylab("K(x)")+theme_bw()


## effect of bandwidth
h <- seq(0.01,1.5,by=0.15)
smr <- as.data.frame(matrix(NA,nrow=NROW(faithful), ncol=length(h)))
est_plot<-list()
for(i in seq_along(h))
{

  smr <- ksmooth(faithful$eruptions, 
                 faithful$waiting, 
                 kernel = 'normal',
                 bandwidth=h[i])
  est_plot[[i]]<-ggplot(faithful) +
          geom_point(aes(x=eruptions,y=waiting)) +
          ggtitle(paste("Old Faithful (Gaussian kernel, h=",h[i],")", sep="")) +
          geom_line(data=as.data.frame(smr), aes(x=x, y=y), col='blue')+theme_bw()
  
  
}
library(egg)
ggarrange(est_plot[[1]],
          est_plot[[2]],
          est_plot[[3]],
          est_plot[[4]],
          est_plot[[5]],
          est_plot[[6]],
          ncol = 3, nrow = 2)



library(KernSmooth)

## using cross-validation method to pick bandwidth

library(sm)
sm_faithful<-with(faithful,
     sm.regression(x=eruptions, 
                   y=waiting, 
                   h=h.select(eruptions, waiting) ## select bw using CV method
                   ))

sm_faithful_df <- data.frame(x=sm_faithful$eval.points,y=sm_faithful$estimate)
ggplot(faithful) + 
  geom_point(aes(x=eruptions,y=waiting)) + 
  geom_line(data=sm_faithful_df , aes(x=x,y=y), 
            col='blue')+theme_bw()
