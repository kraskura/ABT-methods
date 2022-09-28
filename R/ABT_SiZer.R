
library(pryr) # to save plot in variable
library(SiZer) # bp
library(weathermetrics) # celsius / Kelvin 

d <- read.csv("test6-10.csv")
data.breakpoint <- d[d$abt_fit=="use", c("bpm", "temp_mean","temp.K", "timepoint")]
data.breakpoint <- data.breakpoint[order(data.breakpoint$timepoint),]

# ********************* SiZer *********************

# set.seed(55)

# could set seed - this uses bootstrap so would be subject to the minimal change. 
# high N bootstrap samples should make the variation between runs VERY small

model<-SiZer::piecewise.linear(x=data.breakpoint$temp.K,
                               y=log(data.breakpoint$bpm),
                               CI = TRUE,
                               bootstrap.samples = 1000,
                               sig.level = 0.5)


# WArnings: ********************* 
# Warning messages:
#   1: In stats::optimize(piecewise.linear.likelihood, c(low,  ... :
#                                                          NA/Inf replaced by maximum positive value

# Believe its due to low sample sizes, not 100% sure. 
# WArnings: ********************* 


# Estimates: ********************* 
# yhat <- beta[1] + beta[2]*x + beta[3]*w;
# linear model with changepoint
yhat <- model$model$coefficients[1] +
  model$model$coefficients[2]*model$change.point 

# plot
p_sz %<a-% {
  plot(model, pch=16, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "SiZer::piecewise.linear")
  abline(v=model$change.point, lty=3)
  text(x=max(x)-0.01, y = max(y)-0.03, labels = paste(round(model$change.point, 2), "ºK"), cex = 1)
  text(x=max(x)-0.01, y = max(y)-0.07, labels = paste(round(kelvin.to.celsius(1000/model$change.point), 2), "ºC"), cex = 1)
  arrows(y0=yhat, x0=model$intervals[1,1], y1=yhat, x1=model$intervals[2,1], code=3, angle=90, length=0.1, col="red", lwd=3)
  points(x=model$change.point, y=yhat, col = "red")
  model$intervals[1,1]
}
p_sz

# fitted data
d$SZbreakpoint<-model$change.point # x of BP
d$SZbreakpoint_y<-yhat # y of BP
d$SZbreakpoint_CI_low<-model$intervals[1,1] 
d$SZbreakpoint_CI_high<-model$intervals[2,1]


