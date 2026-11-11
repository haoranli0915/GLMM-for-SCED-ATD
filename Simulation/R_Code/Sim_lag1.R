setwd("/users/6/haoranli/ATD/ATD1/n1=30,n2=4")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
start <- Sys.time()

library(foreach)
library(doParallel)
library(glmmTMB)
#library(GLMMadaptive)
#library(gee)
#library(geesmv)
#library(saws)
#library(msm)
#library(MASS)

reps=1000
n1_list<-c(30)
n2_list<-c(4)
Intercept_list <- c(log(59.77),log(59.49))
Trend_list <- c(log(1.00))
IE1_list <- c(log(1.00),log(1.30))
IE2_list <- c(log(1.00),log(1.30))
Interaction1_list <- c(log(1.00),log(1.05)) 
Interaction2_list <- c(log(1.00),log(1.05))
Trt1Lag1_list <- c(log(1.00),log(1.20))
Trt2Lag1_list <- c(log(1.00),log(1.20))
Var_int_list <- c(0.1)
Var_trend_list <- c(0)
Var_level1_list <- c(0.1)
Var_level2_list <- c(0.1)
Var_interaction1_list <- c(0)
Var_interaction2_list <- c(0)
Correlation_list <- c(0)
Var_res_list<- c(0.008, 0.017)
Dispersion_list <- c(120,60)
AR_list <- c(-0.5,0.5)

conditions <- length(IE1_list)*length(IE2_list)*length(Interaction1_list)*length(Interaction2_list)*
  length(Trt1Lag1_list)*length(Trt2Lag1_list)*length(Dispersion_list)*length (AR_list)

design <- numeric()
for (n1 in n1_list) {
  for (n2 in n2_list) {
    for (Intercept in Intercept_list) {
      for (Trend in Trend_list) {
        for (IE1 in IE1_list) {
          for (IE2 in IE2_list) {
            for (Interaction1 in Interaction1_list) {
              for (Interaction2 in Interaction2_list) {
                for(Trt1Lag1 in Trt1Lag1_list) {
                  for(Trt2Lag1 in Trt2Lag1_list) {
                    for (Var_int in Var_int_list) {
                      for (Var_trend in Var_trend_list) {
                        for (Var_level1 in Var_level1_list) {
                          for (Var_level2 in Var_level2_list) {
                            for (Var_interaction1 in Var_interaction1_list) {
                              for (Var_interaction2 in Var_interaction2_list) {
                                for (Correlation in Correlation_list) {
                                  for (AR in AR_list) {
                                    if (Intercept == Intercept_list[1]) {Var_res = Var_res_list[1]} 
                                    if (Intercept == Intercept_list[1]) {Dispersion = Dispersion_list[1]}
                                    if (Intercept == Intercept_list[2]) {Var_res = Var_res_list[2]} 
                                    if (Intercept == Intercept_list[2]) {Dispersion = Dispersion_list[2]}
                                    design <-rbind(design,matrix(rep(c(n1,n2,Intercept,Trend,IE1,IE2,Interaction1,Interaction2,Trt1Lag1,Trt2Lag1,
                                                                       Var_int,Var_trend,Var_level1,Var_level2,Var_interaction1,Var_interaction2,
                                                                       Correlation,AR,Var_res,Dispersion),reps),nrow = reps,byrow = T))
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
colnames(design) <- c("Series length","Cases","Intercept","Trend","Immediate_effect1","Immediate_effect2", 
                      "Interaction1","Interaction2","Trt1Lag1","Trt2Lag1","Var_int","Var_trend","Var_level1","Var_level2",
                      "Var_interaction1","Var_interaction2","Correlation","AR","Var_res","Dispersion")

registerDoParallel(cores=64)
load(file = paste0("Data_lag1.Rdata"))

Sim <- foreach(
  k = 1:(reps * conditions),
  .combine = "rbind",
  .packages = "glmmTMB",
  .inorder = TRUE
) %dopar% {
  
  write(k, "Counter_Sim.txt")
  
  pars <- design[k, ]
  data <- Data[[k]]
  
  # --- Fit model 1 ---
  m1 <- try(glmmTMB(
    Y ~ 1 + Time + Trt1 + Trt2 + Time1_Trt1 + Time2_Trt2 +
      (1 + Trt1 + Trt2 || IDLEVEL2),
    data = data,
    family = poisson
  ), silent = TRUE)
  
  if (inherits(m1, "try-error")) {
    return(c(pars, rep(NA, 74)))
  }
  
  # --- Fit model 2 ---
  m2 <- try(glmmTMB(
    Y ~ 1 + Time + Trt1 + Trt2 + Time1_Trt1 + Time2_Trt2 +
      Trt1Lag1 + Trt2Lag1 +
      (1 + Trt1 + Trt2 || IDLEVEL2) +
      ar1(as.factor(Session) + 0 | IDLEVEL2),
    data = data,
    family = poisson
  ), silent = TRUE)
  
  if (inherits(m2, "try-error")) {
    return(c(pars, rep(NA, 74)))
  }
  
  # --- Fit model 3 ---
  m3 <- try(glmmTMB(
    Y ~ 1 + Time + Trt1 + Trt2 + Time1_Trt1 + Time2_Trt2 +
      Trt1Lag1 + Trt2Lag1 +
      (1 + Trt1 + Trt2 || IDLEVEL2),
    data = data,
    family = poisson
  ), silent = TRUE)
  
  if (inherits(m3, "try-error")) {
    return(c(pars, rep(NA, 74)))
  }
  
  
  # --- Extract coefficients efficiently ---
  get_stats <- function(model) {
    coefs <- summary(model)$coefficients$cond
    
    est <- coefs[, "Estimate"]
    se  <- coefs[, "Std. Error"]
    p   <- coefs[, "Pr(>|z|)"]
    
    # Wald CI (fast replacement for confint)
    lower <- est - 1.96 * se
    upper <- est + 1.96 * se
    
    c(
      AIC(model),
      BIC(model),
      est,
      se,
      p,
      lower,
      upper
    )
  }
  
  out <- c(get_stats(m1), get_stats(m2), get_stats(m3))
  
  return(c(pars, out))
}

#Rows<-unname(which(abs(Sim[,14])>100))
#Sim[Rows,14]<-NA
#Rows<-unname(which(abs(Sim[,15])>50))
#Sim[Rows,15]<-NA

## Poisson ##
AIC.Poisson <- as.vector(by(Sim[,21], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
BIC.Poisson <- as.vector(by(Sim[,22], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))  

bias.IE1.Poisson <- as.vector(by((Sim[,25]-Sim[,5]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))  
bias.IE2.Poisson <- as.vector(by((Sim[,26]-Sim[,6]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.IE1.Poisson <-  as.vector(by((Sim[,25]-Sim[,5])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.IE2.Poisson <-  as.vector(by((Sim[,26]-Sim[,6])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.IE1.Poisson <- as.vector(by(Sim[,43]<Sim[,5] & Sim[,49]>Sim[,5], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.IE2.Poisson <- as.vector(by(Sim[,44]<Sim[,6] & Sim[,50]>Sim[,6], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.IE1.Poisson <- as.vector(by(Sim[,37]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.IE2.Poisson <- as.vector(by(Sim[,38]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))

bias.interaction1.Poisson <- as.vector(by((Sim[,27]-Sim[,7]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
bias.interaction2.Poisson <- as.vector(by((Sim[,28]-Sim[,8]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.interaction1.Poisson <-  as.vector(by((Sim[,27]-Sim[,7])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.interaction2.Poisson <-  as.vector(by((Sim[,28]-Sim[,8])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.interaction1.Poisson <- as.vector(by(Sim[,45]<Sim[,7] & Sim[,51]>Sim[,7], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.interaction2.Poisson <- as.vector(by(Sim[,46]<Sim[,8] & Sim[,52]>Sim[,8], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.interaction1.Poisson <- as.vector(by(Sim[,39]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.interaction2.Poisson <- as.vector(by(Sim[,40]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))

convergence.rate.Poisson <- 1-as.vector(by(Sim[,21], ceiling(1:(conditions*reps)/reps),function(x) sum(is.na(x))))/reps

## Poisson lag1AR1 ##
AIC.Lag1AR1 <- as.vector(by(Sim[,53], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
BIC.Lag1AR1 <- as.vector(by(Sim[,54], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))  

bias.IE1.Lag1AR1 <- as.vector(by((Sim[,57]-Sim[,5]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))  
bias.IE2.Lag1AR1 <- as.vector(by((Sim[,58]-Sim[,6]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.IE1.Lag1AR1 <-  as.vector(by((Sim[,57]-Sim[,5])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.IE2.Lag1AR1 <-  as.vector(by((Sim[,58]-Sim[,6])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.IE1.Lag1AR1 <- as.vector(by(Sim[,81]<Sim[,5] & Sim[,89]>Sim[,5], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.IE2.Lag1AR1 <- as.vector(by(Sim[,82]<Sim[,6] & Sim[,90]>Sim[,6], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.IE1.Lag1AR1 <- as.vector(by(Sim[,73]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.IE2.Lag1AR1 <- as.vector(by(Sim[,74]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))

bias.interaction1.Lag1AR1 <- as.vector(by((Sim[,59]-Sim[,7]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
bias.interaction2.Lag1AR1 <- as.vector(by((Sim[,60]-Sim[,8]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.interaction1.Lag1AR1 <-  as.vector(by((Sim[,59]-Sim[,7])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.interaction2.Lag1AR1 <-  as.vector(by((Sim[,60]-Sim[,8])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.interaction1.Lag1AR1 <- as.vector(by(Sim[,83]<Sim[,7] & Sim[,91]>Sim[,7], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.interaction2.Lag1AR1 <- as.vector(by(Sim[,84]<Sim[,8] & Sim[,92]>Sim[,8], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.interaction1.Lag1AR1 <- as.vector(by(Sim[,75]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.interaction2.Lag1AR1 <- as.vector(by(Sim[,76]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))

convergence.rate.Lag1AR1 <- 1-as.vector(by(Sim[,53], ceiling(1:(conditions*reps)/reps),function(x) sum(is.na(x))))/reps

## Poisson lag1 ##
AIC.Lag1 <- as.vector(by(Sim[,95], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
BIC.Lag1 <- as.vector(by(Sim[,96], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))  

bias.IE1.Lag1 <- as.vector(by((Sim[,99]-Sim[,5]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))  
bias.IE2.Lag1 <- as.vector(by((Sim[,100]-Sim[,6]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.IE1.Lag1 <-  as.vector(by((Sim[,99]-Sim[,5])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.IE2.Lag1 <-  as.vector(by((Sim[,100]-Sim[,6])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.IE1.Lag1 <- as.vector(by(Sim[,123]<Sim[,5] & Sim[,131]>Sim[,5], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.IE2.Lag1 <- as.vector(by(Sim[,124]<Sim[,6] & Sim[,132]>Sim[,6], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.IE1.Lag1 <- as.vector(by(Sim[,115]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.IE2.Lag1 <- as.vector(by(Sim[,116]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))

bias.interaction1.Lag1 <- as.vector(by((Sim[,101]-Sim[,7]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
bias.interaction2.Lag1 <- as.vector(by((Sim[,102]-Sim[,8]), ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.interaction1.Lag1 <-  as.vector(by((Sim[,101]-Sim[,7])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
MSE.interaction2.Lag1 <-  as.vector(by((Sim[,102]-Sim[,8])^2, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.interaction1.Lag1 <- as.vector(by(Sim[,125]<Sim[,7] & Sim[,133]>Sim[,7], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
cp.interaction2.Lag1 <- as.vector(by(Sim[,126]<Sim[,8] & Sim[,134]>Sim[,8], ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.interaction1.Lag1 <- as.vector(by(Sim[,117]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))
power.interaction2.Lag1 <- as.vector(by(Sim[,118]<0.05, ceiling(1:(conditions*reps)/reps),function(x) mean(x,na.rm=T)))

convergence.rate.Lag1 <- 1-as.vector(by(Sim[,95], ceiling(1:(conditions*reps)/reps),function(x) sum(is.na(x))))/reps


results<-cbind(unique(design),
               AIC.Poisson,BIC.Poisson,bias.IE1.Poisson,bias.IE2.Poisson,MSE.IE1.Poisson,MSE.IE2.Poisson,
               cp.IE1.Poisson,cp.IE2.Poisson,power.IE1.Poisson,power.IE2.Poisson,
               bias.interaction1.Poisson,bias.interaction2.Poisson,MSE.interaction1.Poisson,MSE.interaction2.Poisson,
               cp.interaction1.Poisson,cp.interaction2.Poisson, power.interaction1.Poisson,power.interaction2.Poisson,convergence.rate.Poisson,
               
               AIC.Lag1AR1,BIC.Lag1AR1,bias.IE1.Lag1AR1,bias.IE2.Lag1AR1,MSE.IE1.Lag1AR1,MSE.IE2.Lag1AR1,
               cp.IE1.Lag1AR1,cp.IE2.Lag1AR1,power.IE1.Lag1AR1,power.IE2.Lag1AR1,
               bias.interaction1.Lag1AR1,bias.interaction2.Lag1AR1,MSE.interaction1.Lag1AR1,MSE.interaction2.Lag1AR1,
               cp.interaction1.Lag1AR1,cp.interaction2.Lag1AR1, power.interaction1.Lag1AR1,power.interaction2.Lag1AR1,convergence.rate.Lag1AR1,
               
               
               AIC.Lag1,BIC.Lag1,bias.IE1.Lag1,bias.IE2.Lag1,MSE.IE1.Lag1,MSE.IE2.Lag1,
               cp.IE1.Lag1,cp.IE2.Lag1,power.IE1.Lag1,power.IE2.Lag1,
               bias.interaction1.Lag1,bias.interaction2.Lag1,MSE.interaction1.Lag1,MSE.interaction2.Lag1,
               cp.interaction1.Lag1,cp.interaction2.Lag1, power.interaction1.Lag1,power.interaction2.Lag1,convergence.rate.Lag1)

colnames(Sim) <- c("Series length","Cases","Intercept","Trend","Immediate_effect1","Immediate_effect2", 
                   "Interaction1","Interaction2","Trt1Lag1","Trt2Lag1","Var_int","Var_trend","Var_level1","Var_level2",
                   "Var_interaction1","Var_interaction2","Correlation","AR","Var_res","Dispersion",
                   
                   "AIC.Poisson","BIC.Poisson",
                   "Est.int.Poisson","Est.trend.Poisson","Est.IE1.Poisson","Est.IE2.Poisson","Est.interaction1.Poisson","Est.interaction2.Poisson",
                   "SE.int.Poisson","SE.trend.Poisson","SE.IE1.Poisson","SE.IE2.Poisson","SE.interaction1.Poisson","SE.interaction2.Poisson",
                   "Pvalue.int.Poisson","Pvalue.trend.Poisson","Pvalue.IE1.Poisson","Pvalue.IE2.Poisson","Pvalue.interaction1.Poisson","Pvalue.interaction2.Poisson",
                   "lowbound.int.Poisson","lowbound.trend.Poisson",
                   "lowbound.IE1.Poisson","lowbound.IE2.Poisson","lowbound.interaction1.Poisson","lowbound.interaction2.Poisson",
                   "highbound.int.Poisson","highbound.trend.Poisson",
                   "highbound.IE1.Poisson","highbound.IE2.Poisson","highbound.interaction1.Poisson","highbound.interaction2.Poisson",
                   
                   "AIC.Lag1AR1","BIC.Lag1AR1",
                   "Est.int.Lag1AR1","Est.trend.Lag1AR1","Est.IE1.Lag1AR1","Est.IE2.Lag1AR1","Est.interaction1.Lag1AR1","Est.interaction2.Lag1AR1","Est.T1L1.Lag1AR1","Est.T2L1.Lag1AR1",
                   "SE.int.Lag1AR1","SE.trend.Lag1AR1","SE.IE1.Lag1AR1","SE.IE2.Lag1AR1","SE.interaction1.Lag1AR1","SE.interaction2.Lag1AR1","SE.T1L1.Lag1AR1","SE.T2L1.Lag1AR1",
                   "Pvalue.int.Lag1AR1","Pvalue.trend.Lag1AR1","Pvalue.IE1.Lag1AR1","Pvalue.IE2.Lag1AR1","Pvalue.interaction1.Lag1AR1","Pvalue.interaction2.Lag1AR1","Pvalue.T1L1.Lag1AR1","Pvalue.T2L1.Lag1AR1",
                   "lowbound.int.Lag1AR1","lowbound.trend.Lag1AR1",
                   "lowbound.IE1.Lag1AR1","lowbound.IE2.Lag1AR1","lowbound.interaction1.Lag1AR1","lowbound.interaction2.Lag1AR1","lowbound.T1L1.Lag1AR1","lowbound.T2L1.Lag1AR1",
                   "highbound.int.Lag1AR1","highbound.trend.Lag1AR1",
                   "highbound.IE1.Lag1AR1","highbound.IE2.Lag1AR1","highbound.interaction1.Lag1AR1","highbound.interaction2.Lag1AR1","highbound.T1L1.Lag1AR1","highbound.T2L1.Lag1AR1",
                   
                   "AIC.Lag1","BIC.Lag1",
                   "Est.int.Lag1","Est.trend.Lag1","Est.IE1.Lag1","Est.IE2.Lag1","Est.interaction1.Lag1","Est.interaction2.Lag1","Est.T1L1.Lag1","Est.T2L1.Lag1",
                   "SE.int.Lag1","SE.trend.Lag1","SE.IE1.Lag1","SE.IE2.Lag1","SE.interaction1.Lag1","SE.interaction2.Lag1","SE.T1L1.Lag1","SE.T2L1.Lag1",
                   "Pvalue.int.Lag1","Pvalue.trend.Lag1","Pvalue.IE1.Lag1","Pvalue.IE2.Lag1","Pvalue.interaction1.Lag1","Pvalue.interaction2.Lag1","Pvalue.T1L1.Lag1","Pvalue.T2L1.Lag1",
                   "lowbound.int.Lag1","lowbound.trend.Lag1",
                   "lowbound.IE1.Lag1","lowbound.IE2.Lag1","lowbound.interaction1.Lag1","lowbound.interaction2.Lag1","lowbound.T1L1.Lag1","lowbound.T2L1.Lag1",
                   "highbound.int.Lag1","highbound.trend.Lag1",
                   "highbound.IE1.Lag1","highbound.IE2.Lag1","highbound.interaction1.Lag1","highbound.interaction2.Lag1","highbound.T1L1.Lag1","highbound.T2L1.Lag1"
                   )

write.csv(results, file="Results_TMB_lag1.csv", row.names = FALSE)
write.csv(Sim, file="Results_TMB_raw_lag1.csv", row.names = FALSE)
save.image(file="Results_TMB_lag1.RData")
sessionInfo()

end <- Sys.time()
end - start
