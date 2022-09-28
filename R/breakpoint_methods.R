


library(tidyverse)
library(pryr)
library(readxl)
library(ggsci)
library(weathermetrics)
# install.packages("strucchange")
# breakpoints:
library(strucchange)
# http://cran.r-project.org/doc/Rnews/Rnews_2008-1.pdf
library(segmented)
library(SiZer)

source("/Users/kristakraskura/Github_repositories/Plots-formatting/ggplot_format.R")

abt.analysis(abt.file<-"/Users/kristakraskura/Github_repositories/ABT-methods/data/Oct14_2021_test8_perchsize_analysed.xlsx", testID="test8")
abt.analysis(abt.file<-"/Users/kristakraskura/Github_repositories/ABT-methods/data/Oct13_2021_test6_perchsize_analysed.xlsx", testID="test6")
abt.analysis(abt.file="/Users/kristakraskura/Github_repositories/ABT-methods/data/Oct18_2021_test10_perchsize_analysed.xlsx", "test10")

abt.analysis <- function(abt.file, testID){
  data.bpm<-as.data.frame(read_xlsx(abt.file, sheet = 1, col_names =TRUE) )
  colnames(data.bpm)<-c("start_time", "end_time",
                        "bmp_Ch1", "bmp_Ch2", "bmp_Ch5", "bmp_Ch6",
                        "bmp_Ch1_SD", "bmp_Ch2_SD", "bmp_Ch5_SD", "bmp_Ch6_SD",
                        "comment", "timepoint")
  
  
  # sanity check:
  if(!grepl(testID, abt.file)){
    stop("testID is not matching the test")
  }
  
  # heart rate data ------
  data.bpm$start_time_min<-as.POSIXlt(data.bpm$start_time, format = "%H:%M:%OS")
  data.bpm$end_time_min<-as.POSIXlt(data.bpm$end_time, format = "%H:%M:%OS")
  
  data.bpm$start_time_min<- round((as.numeric(data.bpm$start_time_min - as.POSIXct("0:0:0", format = "%H:%M:%OS"))),3)
  data.bpm$end_time_min<- round((as.numeric(data.bpm$end_time_min - as.POSIXct("0:0:0", format = "%H:%M:%OS"))),3)
  
  data.bpm$bpm_duration <- (data.bpm$end_time_min - data.bpm$start_time_min) / 0.016667
  
  data.bpm.L<-gather(data.bpm[, c(3:6, 12:15 )], channel, bpm, 1:4, factor_key=TRUE)
  data.bpm.L.SD<-gather(data.bpm[, c(7:10,  12:15)], channel, bpm, 1:4, factor_key=TRUE)
  
  # d.bpm<-cbind(data.bpm.L, data.bpm.L.SD) # sanity check
  # d.bpm[,4] == d.bpm[,10] # sanity check
  data.bpm.L<-cbind(data.bpm.L, data.bpm.L.SD[, 6]) # sanity check
  data.bpm.L$channel<-as.factor(substr(data.bpm.L$channel, start = 5, stop = 7))
  
  names(data.bpm.L)[names(data.bpm.L) == 'data.bpm.L.SD[, 6]'] <- 'bpm.SD'
  
  # temperature data -----
  data.t<-as.data.frame(read_xlsx(abt.file, sheet = 2, col_names = TRUE) )
  
  data.t$start_time_min_TEMP<- data.t$h_start*60 + data.t$min_start + data.t$sec_start/60
  data.t$end_time_min_TEMP<- data.t$h_end*60 + data.t$min_end + data.t$sec_end/60
  
  start_times<-gather(data.t[, c("Temp_Ch1_start30", "Temp_Ch2_start30", "Temp_Ch5_start30", "Temp_Ch6_start30", "start_time_min_TEMP", "timepoint")], channel, temp, c(1:4) , factor_key=TRUE)
  end_times<-gather(data.t[, c("Temp_Ch1_end1", "Temp_Ch2_end1", "Temp_Ch5_end1", "Temp_Ch6_end1", "end_time_min_TEMP", "timepoint")], channel, temp, c(1:4) , factor_key=TRUE)
  start_times$channel<-substr(start_times$channel, start = 6, stop = 8)
  end_times$channel<-substr(end_times$channel, start = 6, stop = 8)
  data.t <- cbind(start_times, end_times)
  
  if(all((data.t[, c(2)] == data.t[, 6]) & (data.t[, c(3)] == data.t[, 7]))){
    data.t<-data.t[, -c(2, 3)]
  }
  data.t$temp_diff <- (end_times$temp - start_times$temp)
  data.t$temp_mean <- (end_times$temp + start_times$temp)/2
  
  plot1.t <- ggplot(data.t, aes(x = start_time_min_TEMP, y = temp_mean, group = channel, color = channel))+
    geom_point()+
    geom_line()+
    scale_color_jama()
  ggformat(plot1.t, x_title = "Time (min)", y_title = expression(Temperature~degree*C), print = FALSE)
  
  
  # combining data ------
  data<-merge(data.bpm.L, data.t, by=c("channel", "timepoint"))
  data$temp.K<-1000/celsius.to.kelvin(data$temp_mean)
  data$testID<-testID
  
  # take out the missing data (the ones indicated with -999)
  data<-data[(which(data$bpm > 0)),]
  
  
  
  
  
  
  # Channel specific ABT data analysis -----------------
  data.ch.split<-split(data, data$channel)
  # par(mfrow=c(3,4))
  
  # Find HRpeak 
  for (k in 1:length(data.ch.split)){
    d<-as.data.frame(data.ch.split[k])
    
    if(nrow(d)<1){next}
    names(d)<-names(data)
    d$abt_fit<-"use"
    d$Tpeak.K<-d[which(d$bpm == max(d$bpm)), "temp.K"]
    d$Tpeak<-d[which(d$bpm == max(d$bpm)), "temp_mean"]
    d$HRpeak<-d[which(d$bpm == max(d$bpm)), "bpm"]
    d$abt_fit[d$temp_mean > d$Tpeak[1]]<-"NOTuse"
    
    write.csv(file = paste(testID,"-" ,i, ".csv", sep=""), d)
    # write.csv(file = "dataset_example2.csv", d)
    # write.csv(file = "dataset_example3.csv", d)
    # write.csv(file = "dataset_example4.csv", d)
    
    # breakpoints specific *********************
    data.breakpoint<-d[d$abt_fit=="use", ]
    data.breakpoint <- data.breakpoint[order(data.breakpoint$timepoint),]
    
    # ********************* segmented *********************
    breakpoint.lm<-lm(log(bpm) ~ temp.K, data = data.breakpoint)
    # breakpoint # 1
    
    # loop:
    # N boots, K, psi, set.seed
    my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K,
                        # psi = c(3.38),
                        n.boot = 10000, control=seg.control(display=FALSE, K=1))
    
    # summary stats 
    summary(my.seg)
    my.seg$psi
    slope(my.seg)
    intercept(my.seg)
    # fitted data
    my.fitted <- fitted(my.seg)
    d$breakpoint<-my.seg$psi[2]
    d$breakpoint_y<-as.data.frame(intercept(my.seg))[1,1]+
      as.data.frame(slope(my.seg))[1,1]*my.seg$psi[2]
    d$breakpoint_SE<-my.seg$psi[3]
    
    d$reg1_slopeSE<-as.data.frame(slope(my.seg))[1,2]
    d$reg2_slopeSE<-as.data.frame(slope(my.seg))[2,2]
    d$reg1_slope<-as.data.frame(slope(my.seg))[1,1]
    d$reg2_slope<-as.data.frame(slope(my.seg))[2,1]
    d$reg1_inter<-as.data.frame(intercept(my.seg))[1,1]
    d$reg2_inter<-as.data.frame(intercept(my.seg))[2,1]
    
    yhat1<-as.data.frame(intercept(my.seg))[1,1]+
      as.data.frame(slope(my.seg))[1,1]*my.seg$psi[2]
    yhat2<-as.data.frame(intercept(my.seg))[2,1]+
      as.data.frame(slope(my.seg))[2,1]*my.seg$psi[2]
    if(round(yhat1, 2) == round(yhat2,2)){
      message("At bp: y1 & yt2 are the same: ",round(yhat1, 2) == round(yhat2,2))  
    }else{
      message("At bp: y1 & yt2 are the same: FALSE") 
    }
    
    # get breakpoint 
    # get SE breakpoint
    # get slope1, slope2
    # get SE of slopes
    # intercept
    # get SE of intercepts
    # bootstrap samples
    
    y<-log(data.breakpoint$bpm)
    x<-data.breakpoint$temp.K
    
    p_seg %<a-% {
      plot(my.seg, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "segmented:segmented")
      points(x,y, pch=19)
      abline(v=my.seg$psi[2], lty=3)
      text(x=max(x)-0.01, y = max(y)-0.03, labels = paste(round(my.seg$psi[2], 2), "ºK"), cex = 1)
      text(x=max(x)-0.01, y = max(y)-0.07, labels = paste(round(kelvin.to.celsius(1000/my.seg$psi[2]), 2), "ºC"), cex = 1)
      arrows(y0=yhat1, x0=my.seg$psi[2]-my.seg$psi[3], y1=yhat1, x1=my.seg$psi[2]+my.seg$psi[3], code=3, angle=90, length=0.1, col="red", lwd=3)
      points(x=my.seg$psi[2], y=yhat1, col = "red")
    }
    # ********************* segmented *********************
    
    
    
    # ********************* strucchange *********************
    # strucchange::breakpoints
    # strucchange::Fstats
    # not good... ? 
    # ********************* strucchange*********************
   
    
    # ********************* SiZer *********************
    model<-SiZer::piecewise.linear(data.breakpoint$temp.K, log(data.breakpoint$bpm), CI = TRUE, bootstrap.samples = 100)
    par(mfrow=c(1,1))
    # plot(model)
    # yhat <- beta[1] + beta[2]*x + beta[3]*w;
    yhat <- model$model$coefficients[1] +
      model$model$coefficients[2]*model$change.point 
    # +model$model$coefficients[3]*(x - model$change.point)
    
    p_sz %<a-% {
      plot(model, pch=16, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "SiZer::piecewise.linear")
      abline(v=model$change.point, lty=3)
      text(x=max(x)-0.01, y = max(y)-0.03, labels = paste(round(model$change.point, 2), "ºK"), cex = 1)
      text(x=max(x)-0.01, y = max(y)-0.07, labels = paste(round(kelvin.to.celsius(1000/model$change.point), 2), "ºC"), cex = 1)
      arrows(y0=yhat, x0=model$intervals[1,1], y1=yhat, x1=model$intervals[2,1], code=3, angle=90, length=0.1, col="red", lwd=3)
      points(x=model$change.point, y=yhat, col = "red")
      model$intervals[1,1]
    }
    
    # print(model)
    
    # model$x
    # model$y
    # plot(model$x, model$y)
    
    # fitted data
    d$SZbreakpoint<-model$change.point
    d$SZbreakpoint_y<-yhat
    d$SZbreakpoint_CI2.5<-model$intervals[1,1]
    d$SZbreakpoint_CI97.5<-model$intervals[2,1]
    
    # specify sign level
    # CI
    # bootstrap samples
    
    # ********************* SiZer end *********************
    
    
    # *********** manual estimates **************************
    # source: https://www.r-bloggers.com/2012/08/r-for-ecologists-putting-together-a-piecewise-regression/
    
    # x<-data.breakpoint$temp.K
    # y<-log(data.breakpoint$bpm)
    search_min<-min(data.breakpoint$temp.K)
    search_max<-max(data.breakpoint$temp.K)
    
    breaks <- x[which(x >= search_min & x <= search_max)] # define T range in which to search the break 
    mse <- numeric(length(breaks)) # make numeric vector of this
    
    par(mfrow=c(3,4))
    # plot.new()
    for(i in 2:length(breaks)){ # loop over the search value vector
      piecewise1 <- lm(y ~ x*(x < breaks[i]) + x*(x>=breaks[i])) # first part finds beta (slope) for all values below the selected search value, the second part does it for the rest of the values. 
      mse[i] <- summary(piecewise1)[6] # Residual standard error or 'sigma'
      
      piecewise0 <- lm(y ~ x*(x < bp.manual) + x*(x > bp.manual))
      # plot(x,y, pch=16)
      # curve((piecewise0$coefficients[1] + piecewise0$coefficients[3]) + (piecewise0$coefficients[2] + piecewise0$coefficients[5])*x, add=T, from=1, to=15, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K")
      # curve((piecewise0$coefficients[1] + piecewise0$coefficients[4]) + piecewise0$coefficients[2] *x, add=T, lty=2, from=1, col = "blue", to=max(x))
      # abline(v=breaks[i], lty=3)
      #   # points(x= (solve(matrix(c((piecewise0$coefficients[2]), -1,(piecewise0$coefficients[2] + piecewise0$coefficients[5]), -1), byrow = T, nrow = 2), c(-1*(piecewise0$coefficients[1] + piecewise0$coefficients[4]), -1*(piecewise0$coefficients[1] + piecewise0$coefficients[3]))))[1],
      #        y= (coord <- solve(matrix(c((piecewise0$coefficients[2]), -1, (piecewise0$coefficients[2] + piecewise0$coefficients[5]), -1), byrow = T, nrow = 2), c(-1*(piecewise0$coefficients[1] + piecewise0$coefficients[4]), -1*(piecewise0$coefficients[1] + piecewise0$coefficients[3]))))[2],
      #        col = "red")
    }
    mse <- as.numeric(mse[2:length(breaks)])
    bp.manual<-breaks[which(mse==min(mse))]
    piecewise2 <- lm(y ~ x*(x < bp.manual) + x*(x > bp.manual))
    # summary(piecewise2)
    
    # int<-piecewise2$coefficients[1] # intercept
    int1<-piecewise2$coefficients[1] + piecewise2$coefficients[3]
    slope1<-piecewise2$coefficients[2] + piecewise2$coefficients[5]
    int2<-piecewise2$coefficients[1] + piecewise2$coefficients[4]
    slope2<-piecewise2$coefficients[2] 
    
    # find actual interception between the two regression lines
    A <- matrix(c(slope2, -1,
                  slope1, -1), byrow = T, nrow = 2)
    
    b <- c(-int2, -int1)
    if(any(is.na(A))){
      
      p_man %<a-% { 
        plot(x,y, pch=16, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "MANUAL")
        curve((int1) + (slope1)*x, add=T, from=1, to=15)
        curve((int2) + slope2*x, add=T, from=1, to=max(x))
        abline(v=bp.manual, lty=3, col = "red")
        points(x=coord[1], y=coord[2], col = "red")
        text(x=max(x)-0.01, y = max(y)-0.03, labels = paste(round(bp.manual, 2), "ºK"), cex = 1)
        text(x=max(x)-0.01, y = max(y)-0.07, labels = paste(round(kelvin.to.celsius(1000/bp.manual), 2), "ºC"), cex = 1)
      }# r
      
      # add parameters:
      d$MANbreakpoint<-NA
      d$MANbreakpoint_SE<-NA
      d$MANbreakpoint_EMPIRICAL<-bp.manual
      d$MANbreakpoint_y<-NA
      
      
      d$MANreg1_slopeSE<- NA
      d$MANreg2_slopeSE<-NA
      d$MANreg1_slope<- NA
      d$MANreg2_slope<- NA
      d$MANreg1_inter<- NA
      d$MANreg2_inter<- NA
     
    }else{
      
      (coord <- solve(A, b))
      p_man %<a-% { 
        plot(x,y, pch=16, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "MANUAL")
        curve((int1) + (slope1)*x, add=T, from=1, to=15)
        curve((int2) + slope2*x, add=T, from=1, to=max(x))
        abline(v=bp.manual, lty=3, col = "red")
        points(x=coord[1], y=coord[2], col = "red")
        text(x=max(x)-0.01, y = max(y)-0.03, labels = paste(round(bp.manual, 2), "ºK"), cex = 1)
        text(x=max(x)-0.01, y = max(y)-0.07, labels = paste(round(kelvin.to.celsius(1000/bp.manual), 2), "ºC"), cex = 1)
      }# rect(xleft=par("usr")[1], ybottom=par("usr")[3], 
      #      xright=par("usr")[2],ytop=par("usr")[4], 
      #      lwd=2, border="green", xpd=TRUE)
      
      # add parameters:
      d$MANbreakpoint<-coord[1]
      d$MANbreakpoint_SE<-NA
      d$MANbreakpoint_EMPIRICAL<-bp.manual
      d$MANbreakpoint_y<-coord[2]
      
      d$MANreg1_slopeSE<- NA
      d$MANreg2_slopeSE<-NA
      d$MANreg1_slope<- slope1
      d$MANreg2_slope<- slope2
      d$MANreg1_inter<- int1
      d$MANreg2_inter<- int2
    
    }
    
    (coord <- solve(A, b))
    
    p_man %<a-% { 
      plot(x,y, pch=16, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "MANUAL")
      curve((int1) + (slope1)*x, add=T, from=1, to=15)
      curve((int2) + slope2*x, add=T, from=1, to=max(x))
      abline(v=bp.manual, lty=3, col = "red")
      points(x=coord[1], y=coord[2], col = "red")
      text(x=max(x)-0.01, y = max(y)-0.03, labels = paste(round(bp.manual, 2), "ºK"), cex = 1)
      text(x=max(x)-0.01, y = max(y)-0.07, labels = paste(round(kelvin.to.celsius(1000/bp.manual), 2), "ºC"), cex = 1)
    }# rect(xleft=par("usr")[1], ybottom=par("usr")[3], 
    #      xright=par("usr")[2],ytop=par("usr")[4], 
    #      lwd=2, border="green", xpd=TRUE)
    
    # add parameters:
    d$MANbreakpoint<-coord[1]
    d$MANbreakpoint_SE<-NA
    d$MANbreakpoint_EMPIRICAL<-bp.manual
    d$MANbreakpoint_y<-coord[2]
    
    
    d$MANreg1_slopeSE<- NA
    d$MANreg2_slopeSE<-NA
    d$MANreg1_slope<- slope1
    d$MANreg2_slope<- slope2
    d$MANreg1_inter<- int1
    d$MANreg2_inter<- int2
    # *********** manual estimates **************************
    
    
    # breakpoint plot
    # png(plotname.bp, width=6, height=5, res=200, units='in')
    
    if(k ==1){
      data.NEW<-d
    }else{
      data.NEW<-rbind(d, data.NEW) 
    }
    
    # if(i == 1){
    #   data.f<-d
    # }else{
    #   data.f<-rbind(d, data.f)
    # }
    # 
    
    # png(plotname, width=20, height=5, units="in",  res=200)
    par(mfrow=c(3,1))
    p_seg
    p_sz
    p_man
    # dev.off()
  }
  
  plotname.bp<-paste("./ABT_figures/ABT_breakpoint_", testID, data.breakpoint$channel[1], "-", Sys.Date(), ".png")
  # data.NEW
  
  
  
  
  # breakpoint()
  plot_bp<-ggplot(data = data.NEW, aes(temp.K, log(bpm), fill = abt_fit, group = channel, label = kelvin.to.celsius(1000/temp.K)))+
    geom_line( color = "black", size=0.5)+
    geom_point(pch =21, color = "black", size=3)+
    scale_fill_grey()+
    facet_grid(.~channel)+
    geom_point(aes(x =data.NEW$breakpoint, y =  data.NEW$breakpoint_y), color = "red", fill = "green", size = 4, pch = 23)+
    geom_point(aes(x =data.NEW$SZbreakpoint, y =  data.NEW$SZbreakpoint_y), color = "red", fill = "blue",  size = 2, pch = 21)+
    geom_point(aes(x =data.NEW$MANbreakpoint, y =  data.NEW$MANbreakpoint_y), color = "red", fill = "yellow",  size = 4, pch = 25)
    # geom_text(aes(x = max(data.NEW$temp.K)-0.03, y =  max(log(data.NEW$bpm))-c(0.03, 0.05, 0.07)), color = "darkred", size=3)

    # geom_vline(mapping=aes(xintercept = breakpoint),color = "black", alpha = 0.5)
    # geom_abline(mapping=aes( slope =reg1_slope, intercept = reg1_inter),  lty = "dotted")+
    # geom_abline(mapping=aes( slope =reg2_slope, intercept = reg2_inter),  lty =  "dotted")
  ggformat(plot_bp,x_title = expression(paste(Arrhenius~temperature~(10^3*K^-1))), y_title = expression(paste(italic(ln)(f[Hmax]))), print = TRUE)
  
  
  # plots
  plot1 <- ggplot(data, aes(x = temp_mean, y = bpm, group=channel))+
    geom_point()+
    geom_errorbar(aes(ymin = bpm - bpm.SD, ymax = bpm + bpm.SD, group=channel))+
    geom_line()+
    # scale_color_uchicago()+
    geom_vline(xintercept = 22, lty = 2, color="grey")+
    geom_vline(xintercept = 24, lty = 2, color="red")+
    ylim(70, 180)+
    xlim(15, 28)
  ggformat(plot1, y_title = expression(Max~heart~rate~(bpm)), x_title = expression(Temperature~degree*C), print = FALSE )
  
  # plot2 <- ggplot(data, aes(x = temp.K, y = log(bpm), group=channel))+
  #   geom_point()+
  #   geom_line()
  # # ylim(70, 180)
  # # scale_color_uchicago()+
  # # geom_vline(xintercept = 22, lty = 2, color="grey")+
  # # geom_vline(xintercept = 24, lty = 2, color="red")
  # ggformat(plot2, x_title = expression(paste(Arrhenius~temperature~(10^3*K^-1))), y_title = expression(paste(italic(ln)(f[Hmax]))), print = TRUE)
  # 
  if (!dir.exists("./ABT_figures/")){
    message("In current wd a new folder is being created:  ./ABT_figures/ ")
    dir.create(file.path("./ABT_figures/"), recursive = TRUE)
  }
  if (!dir.exists("./ABT_data/")){
    message("In current wd a new folder is being created:  ./ABT_data/ ")
    dir.create(file.path("./ABT_data/"), recursive = TRUE)
  }

  plotname1.t<-paste("./ABT_figures/temperature_trace_", testID,"-", Sys.Date(), ".png")
  plotname1<-paste("./ABT_figures/ABT_trace_", testID, "-", Sys.Date(), ".png")

  png(plotname1.t, width=5, height=4, res=200, units='in')
  print(plot1.t)
  dev.off()
  png(plotname1, width=5, height=5, res=200, units='in')
  print(plot1)
  dev.off()



  write.csv(file = paste("./ABT_data/ABT_data_analysed", testID,"-", Sys.Date(), ".csv"), data.NEW)
  return(data.frame(data.NEW))
}

