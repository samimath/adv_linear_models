## curse of dimensionality example:
library(faraway)
data(babyfood)
babyfood[,'y']<-babyfood$disease/(babyfood$disease+babyfood$nondisease)
lmod1<-lm(y ~ sex ,
   data = babyfood)
lmod2<-lm(y ~ food ,
          data = babyfood)
lmod3<-lm(y ~ sex + food,
          data = babyfood)
lmod4<-lm(y ~ sex + food + sex*food ,
          data = babyfood)

