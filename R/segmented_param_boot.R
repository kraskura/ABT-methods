

library(segmented) # bp
library(weathermetrics) # celsius / Kelvin 

d <- read.csv("./data/exampleData/test6-10.csv")
data.breakpoint <- d[d$abt_fit=="use", c("bpm", "temp_mean","temp.K", "timepoint")]
data.breakpoint <- data.breakpoint[order(data.breakpoint$timepoint),]

# segmented parametric bootstrap -----------

set.seed(55) # set seed to replicate any random resampling
# segmented  uses bootstrap so would be subject to the minimal change if set.seed not constant.
# high N bootstrap samples should make the variation between runs VERY small


# step 1 - linear regression, no BP
breakpoint.lm <- lm(log(bpm) ~ temp.K, data = data.breakpoint)

# step 2 - finding bp using parametric bootstrap segmented package
my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K, # psi = c(), 
                    control=seg.control(n.boot = 100, # default: 10
                                        display=TRUE,
                                        # K=1, # default: K=10; the number of quantiles (or equally-spaced values) to supply as starting values for the breakpoints when the psi argument of segmented is set to NA. K is ignored when psi is different from NA.
                                        seed = 55 ,# default: seed=12345
                                        nonParam = FALSE, # default: nonParam=TRUE
                                        break.boot = 10, # default:  break.boot = 5
                                        digits = 4)) # the desidered number of decimal points of the breakpoint to be used during the iterative algorithm))
                    

# step 3: get CI
CIs<-segmented::confint.segmented(my.seg, level = 0.95)	

# step 4: get all estimate values
# yhat = y coordinate of the breakpoint
# yhat <- beta[1] + beta[2]*x + beta[3]*w;
# linear model with changepoint
summary(my.seg)

my.fitted <- fitted(my.seg)
breakpoint<-my.seg$psi[2]
breakpoint_SE<-my.seg$psi[3]
breakpoint_CI.l<-CIs[1,2]
breakpoint_CI.h<-CIs[1,3]

reg1_slopeSE<-as.data.frame(slope(my.seg))[1,2]
reg2_slopeSE<-as.data.frame(slope(my.seg))[2,2]
reg1_slope<-as.data.frame(slope(my.seg))[1,1]
reg2_slope<-as.data.frame(slope(my.seg))[2,1]
reg1_inter<-as.data.frame(intercept(my.seg))[1,1]
reg2_inter<-as.data.frame(intercept(my.seg))[2,1]

# log 
yhat1<-as.data.frame(intercept(my.seg))[1,1]+
  as.data.frame(slope(my.seg))[1,1]*my.seg$psi[2]

exp(yhat1)

# step 5: plot 
plot(my.seg, ylab="Heart rate, ln(bpm)", xlab = "Temp, 1000/K", main = "segmented:segmented")
points(y=log(data.breakpoint$bpm), x=data.breakpoint$temp.K, pch=19)
arrows(y0=yhat1, x0=my.seg$psi[2]-my.seg$psi[3], y1=yhat1, x1=my.seg$psi[2]+my.seg$psi[3], code=3, angle=90, length=0.1, col="red", lwd=3)
arrows(y0=yhat1, x0=breakpoint_CI.l, y1=yhat1, x1=breakpoint_CI.h, code=3, angle=90, length=0.1, col="darkred", lwd=3)
points(x=my.seg$psi[2], y=yhat1, col = "red")

