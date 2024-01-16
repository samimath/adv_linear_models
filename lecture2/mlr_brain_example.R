## lecture 2: MLR example with IQ size data 
require(datasets) ## R library for built-in datasets 
require(stats) ## R statistical functions
require(visreg) ## Visualization of regression models
require(broom) ## Data wrangling tool 
require(dplyr) ## Data wrangling tool 
require(ggplot2)
require(psych)
data <- tibble(read.table('./lecture2/iqsize.txt', header = T))
## visually inspect the data, 
## the pairs.panels function from 'psych' package is a nice one 
## for this purpose:
pairs.panels(data, 
             method = "pearson", # correlation method
             hist.col = "forestgreen",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


fit <- lm(PIQ~., data= data)
## report model summary
summary(fit)
## report model ANOVA
anova(fit)

## check for model assumption 
## plot 1: residual (vertical) vs fitted (horizontal)
plot(fit,pch=19,which = 1)

## plot 2: residual (vertical) vs predictor (horizontal)
plot(resid(fit)~ data$Brain, pch = 19, main = 'e_i vs Brain')
abline(h=0)
plot(resid(fit)~ data$Height, pch = 19, main = 'e_i vs Height')
abline(h=0)

## plot 3: normality plot
plot(fit,pch=19,which = 2)

## plot 4: cook's distance vs leverage
plot(fit,pch=19,which = 6)


