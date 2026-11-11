setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
start <- Sys.time()
set.seed(770)
library(mvtnorm)

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

Conditions <- length(IE1_list)*length(IE2_list)*length(Interaction1_list)*length(Interaction2_list)*
  length(Trt1Lag1_list)*length(Trt2Lag1_list)*length(Dispersion_list)*length (AR_list)
Data<- list()

sample_ATD_block <- function(n_per_cond = 10, n_conds = 3) {
  
  # Each block contains every condition exactly once
  blocks <- replicate(
    n_per_cond,
    sample(0:(n_conds - 1)),
    simplify = FALSE
  )
  
  seq <- unlist(blocks)
  total_len <- length(seq)
  
  # Identify first appearances
  first_control    <- which(seq == 0)[1]
  first_treatment1 <- which(seq == 1)[1]
  first_treatment2 <- which(seq == 2)[1]
  
  design <- data.frame(
    Session   = 1:total_len,
    Condition = seq,
    Trt1      = as.integer(seq == 1),
    Trt2      = as.integer(seq == 2)
  )
  
  design$Time  <- design$Session - first_control
  design$Time1 <- design$Session - first_treatment1
  design$Time2 <- design$Session - first_treatment2
  
  design$Time1_Trt1 <- design$Time1 * design$Trt1
  design$Time2_Trt2 <- design$Time2 * design$Trt2
  
  design$Trt1Lag1 <- c(0, head(design$Trt1, -1))
  design$Trt2Lag1 <- c(0, head(design$Trt2, -1))
  
  return(design)
}

Data_generation <- function(n1, n2, Intercept, Trend, IE1, IE2, Interaction1, Interaction2, Trt1Lag1, Trt2Lag1, Var_int, Var_trend,
                            Var_level1, Var_level2, Var_interaction1, Var_interaction2, Correlation, AR, Var_res){
  IDLEVEL2 <- rep((1:n2), each = n1)
  random_designs <- do.call(
    rbind,
    replicate(
      n2,
      sample_ATD_block(),
      simplify = FALSE
    )
  )
  
  G00 <- rep(Intercept, times = length(IDLEVEL2))
  G10 <- rep(Trend, times = length(IDLEVEL2))
  G20 <- rep(IE1, times = length(IDLEVEL2))
  G30 <- rep(IE2, times = length(IDLEVEL2))
  G40 <- rep(Interaction1, times = length(IDLEVEL2))
  G50 <- rep(Interaction2, times = length(IDLEVEL2))
  G60 <- rep(Trt1Lag1, times = length(IDLEVEL2))
  G70 <- rep(Trt2Lag1, times = length(IDLEVEL2))
  ranef <- rmvnorm(n = n2, sigma = matrix(c(Var_int, 0, 0, 0, 0, 0, 
                                            0, Var_trend, 0, 0, 0, 0, 
                                            0, 0, Var_level1, 0, 0, 0,
                                            0, 0, 0, Var_level2, 0, 0,
                                            0, 0, 0, 0, Var_interaction1, 0,
                                            0, 0, 0, 0, 0, Var_interaction2), ncol = 6))
  U0 <- rep(ranef[1:n2,1], each=n1)
  U1 <- rep(ranef[1:n2,2], each=n1)
  U2 <- rep(ranef[1:n2,3], each=n1)
  U3 <- rep(ranef[1:n2,4], each=n1)
  U4 <- rep(ranef[1:n2,5], each=n1)
  U5 <- rep(ranef[1:n2,6], each=n1)
  SD_noise <- sqrt(Var_res*(1-AR^2))
 
  mu <- exp(G00+U0+(G10+U1)*random_designs$Time+(G20+U2)*random_designs$Trt1+(G30+U3)*random_designs$Trt2
            +(G40+U4)*random_designs$Time1_Trt1+(G50+U5)*random_designs$Time2_Trt2
            +G60*random_designs$Trt1Lag1+G70*random_designs$Trt2Lag1 + c(arima.sim(list(order = c(1,0,0),ar = AR), n = n1, sd=SD_noise),
                                                                         arima.sim(list(order = c(1,0,0),ar = AR), n = n1, sd=SD_noise),
                                                                         arima.sim(list(order = c(1,0,0),ar = AR), n = n1, sd=SD_noise),
                                                                         arima.sim(list(order = c(1,0,0),ar = AR), n = n1, sd=SD_noise)))
  Y <- c(rpois(n=length(IDLEVEL2), lambda=mu))
  data_all<-as.data.frame(cbind(Y,mu,random_designs,IDLEVEL2))
  return(data_all)
}

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

for (k in 1:(Conditions*reps)) {
  n1 <- design[k,1]
  n2 <- design[k,2]
  Intercept <- design[k,3]
  Trend <- design[k,4]
  IE1 <- design[k,5]
  IE2 <- design[k,6]
  Interaction1 <- design[k,7]
  Interaction2 <- design[k,8]
  Trt1Lag1 <- design[k,9]
  Trt2Lag1 <- design[k,10]
  Var_int <- design[k,11]
  Var_trend <- design[k,12]
  Var_level1 <- design[k,13]
  Var_level2 <- design[k,14]
  Var_interaction1 <- design[k,15]
  Var_interaction2 <- design[k,16]
  Correlation <- design[k,17]
  AR <- design[k,18]
  Var_res <- design[k,19]
  
  Data[[k]] <- Data_generation(n1, n2, Intercept, Trend, IE1, IE2, Interaction1, Interaction2, Trt1Lag1, Trt2Lag1,
                               Var_int, Var_trend, Var_level1, Var_level2, Var_interaction1, Var_interaction2, Correlation, AR, Var_res)
  
  cat("Just completed", k, "of", Conditions*reps, "Replications", "(","remaining", Conditions*reps-k,")", "\n")
  
}

save(Data,file=paste0("Data_lag1.Rdata"))
end <- Sys.time()
end-start