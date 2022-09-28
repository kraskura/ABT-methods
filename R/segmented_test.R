
d <- read.csv("dataset_example3.csv")
data.breakpoint <- d[d$abt_fit=="use", ]
data.breakpoint <- data.breakpoint[order(data.breakpoint$timepoint),]

# BOOTS = 1000
# psi_settings = TRUE
# PSI.VAL = 3.38
# SET.SEED.VAL = 125

# "seg.control_adjust"
# "NOval_NOseed"

segmented_estimate<-function(data.breakpoint, BOOTS, psi_settings = "NOval_NOseed", PSI.VAL=NA, SET.SEED.VAL = 12345, run.data, displ.run, print_plot = FALSE){
  
    approachRUN = paste(BOOTS,"-",PSI.VAL,"-", SET.SEED.VAL,"-", psi_settings, sep="")
    # run.data<-as.data.frame(matrix(nrow=1, ncol = 12))
    # names(run.data)<-c("method", "bp_K", "bp_C", "y_bp", "error_bp", "unitError",
    #             "slope1", "slope2", "inter1", "inter2", "approach", "set.seed")
    # 
    
    # ********************* segmented *********************
    
    breakpoint.lm <- lm(log(bpm) ~ temp.K, data = data.breakpoint)
    # breakpoint # 1
    
  
    if(psi_settings == "YESval_NOseed"){
      my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K,
                          psi = c(PSI.VAL))
    }
    
    if(psi_settings == "NOval_YESseed"){
      my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K, control=seg.control(seed = SET.SEED.VAL))
    }
    
    if(psi_settings == "NOval_NOseed"){
      my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K)
    }
    
    if(psi_settings == "YESval_YESseed"){
      my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K,
                          psi = c(PSI.VAL), control = seg.control(seed = SET.SEED.VAL))
    }
    
    # if(psi_settings == "YESval_YESseed"){
    #   my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K,
    #                       psi = c(PSI.VAL), control = seg.control(display=displ.run))
    # }
    
    if(psi_settings == "seg.control_adjust"){
      my.seg <- segmented(breakpoint.lm, seg.Z = ~ temp.K, control=seg.control(n.boot = BOOTS,
                                                                               display=TRUE,
                                                                               K=1,
                                                                               seed = SET.SEED.VAL))
    }
    
    # summary stats 
    # summary(my.seg)
    # my.seg$psi
    # slope(my.seg)
    # intercept(my.seg)
    # fitted data
    if(nrow(as.data.frame(my.seg$coefficients))<3){
      print(SET.SEED.VAL)
      
      plot(my.seg, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "segmented:segmented")
      points(x,y, pch=19)
      # abline(v=my.seg$psi[2], lty=3)
    }
    
    my.fitted <- fitted(my.seg)
    d$breakpoint<-my.seg$psi[2]
    d$breakpoint_SE<-my.seg$psi[3]

    d$reg1_slopeSE<-as.data.frame(slope(my.seg))[1,2]
    d$reg2_slopeSE<-as.data.frame(slope(my.seg))[2,2]
    d$reg1_slope<-as.data.frame(slope(my.seg))[1,1]
    d$reg2_slope<-as.data.frame(slope(my.seg))[2,1]
    d$reg1_inter<-as.data.frame(intercept(my.seg))[1,1]
    d$reg2_inter<-as.data.frame(intercept(my.seg))[2,1]
    # 
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
    
    if(print_plot){
      plot(my.seg, ylab="Heart rate, log10(bpm)", xlab = "Temp, 1000/K", main = "segmented:segmented")
      points(x,y, pch=19)
      abline(v=my.seg$psi[2], lty=3)
      text(x=max(x)-0.01, y = max(y)-0.03, labels = paste(round(my.seg$psi[2], 2), "ºK"), cex = 1)
      text(x=max(x)-0.01, y = max(y)-0.07, labels = paste(round(kelvin.to.celsius(1000/my.seg$psi[2]), 2), "ºC"), cex = 1)
      arrows(y0=yhat1, x0=my.seg$psi[2]-my.seg$psi[3], y1=yhat1, x1=my.seg$psi[2]+my.seg$psi[3], code=3, angle=90, length=0.1, col="red", lwd=3)
      points(x=my.seg$psi[2], y=yhat1, col = "red")
    }
    
    # ggplot(data = data.breakpoint, aes(x = temp.K, y = log(bpm)))+
    #   geom_point()+
    #   geom_abline(slope = as.numeric(run.data$slope1), intercept = as.numeric(run.data$inter1), col = "black", alpha = 0.3)+
    #   geom_abline(slope = as.numeric(run.data$slope2), intercept = as.numeric(run.data$inter2), col = "blue", alpha = 0.3)+
    #   theme_classic()
    
    values<-as.data.frame(t(c("segmented",my.seg$psi[2],kelvin.to.celsius(1000/my.seg$psi[2]) , yhat1,my.seg$psi[3], 
                "SE", as.data.frame(slope(my.seg))[1,1], as.data.frame(slope(my.seg))[2,1],
                as.data.frame(intercept(my.seg))[1,1], as.data.frame(intercept(my.seg))[2,1],
                approachRUN, SET.SEED.VAL)))
    names(values)<-c("method", "bp_K", "bp_C", "y_bp", "error_bp", "unitError",
                     "slope1", "slope2", "inter1", "inter2", "approach", "set.seed")
    # run.data$method[1]<-"segmented"
    # run.data$bp_K[1]<-my.seg$psi[2]
    # run.data$bp_C[1]<-kelvin.to.celsius(1000/my.seg$psi[2])
    # run.data$y_bp[1]<-yhat1
    # run.data$error_bp[1]<-my.seg$psi[3]
    # run.data$unitError[1]<-"SE"
    # run.data$slope1[1]<-as.data.frame(slope(my.seg))[1,1]
    # run.data$slope2[1]<-as.data.frame(slope(my.seg))[2,1]
    # run.data$inter1[1]<-as.data.frame(intercept(my.seg))[1,1]
    # run.data$inter2[1]<-as.data.frame(intercept(my.seg))[2,1]
    # run.data$approach[1]<-approachRUN
    # run.data$set.seed[1]<-SET.SEED.VAL
    #   
    run.data<-rbind(run.data, values)
   
    return(run.data)
}
  
run.data<-as.data.frame(matrix(nrow=0, ncol = 12))
names(run.data)<-c("method", "bp_K", "bp_C", "y_bp", "error_bp", "unitError",
                   "slope1", "slope2", "inter1", "inter2", "approach", "set.seed")
loopruns<-100
for(i in 1:loopruns){
  run.data<-segmented_estimate(run.data=run.data, BOOTS = 1000, SET.SEED.VAL = sample(100,1, replace=T), 
                               psi_settings = "YESval_YESseed", data.breakpoint = data.breakpoint, 
                               print_plot = FALSE, displ.run = FALSE)
  if(i == loopruns){
      print(ggplot(data = data.breakpoint, aes(x = temp.K, y = log(bpm)))+
      geom_point()+
      geom_abline(slope = as.numeric(run.data$slope1), intercept = as.numeric(run.data$inter1), col = "black", alpha = 0.3)+
      geom_abline(slope = as.numeric(run.data$slope2), intercept = as.numeric(run.data$inter2), col = "blue", alpha = 0.3)+
      geom_vline(xintercept = as.numeric(run.data$bp_K), alpha = 0.2, color = "red")+
      geom_point(data = run.data, mapping = aes(x=as.numeric(bp_K), y=as.numeric(y_bp)), color = "red", pch=5, size=4)+
      geom_pointrange(data = run.data, mapping = aes(x = as.numeric(bp_K), y = yhat , xmin=as.numeric(bp_K) - as.numeric(error_bp) , xmax=as.numeric(bp_K) + as.numeric(error_bp)), color = "red")+
      theme_classic())
 
  }
}
# 
var(run.data$bp_K)
# 22
# 120
# 45.
# 8

# segmented_estimate(run.data=run.data, BOOTS = 1000, SET.SEED.VAL = 22, 
#                    psi_settings = "seg.control_adjust", data.breakpoint = data.breakpoint, 
#                    print_plot = FALSE)
# 
# 
