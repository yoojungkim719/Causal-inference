################################################################################
### basic setting
rm(list=ls())
cat("\014")
setwd("C:/R/Rproject/metalearners/1. simulation")



################################################################################
### library
library(randomForest)
library(Metrics)



################################################################################
for (nums in c(300, 500, 1000)) {
  
  for (ps in c(0.1, 0.2, 0.5)) {
    
    RMSE = as.data.frame(matrix(rep(NA, 500*4), ncol=4))
    colnames(RMSE) = c("S", "T", "X", "DR")
    BIAS = as.data.frame(matrix(rep(NA, 500*4), ncol=4))
    colnames(BIAS) = c("S", "T", "X", "DR")
    
    ites.sl = c()
    ites.tl = c()
    ites.xl = c()
    ites.drl = c()
    
    for (k in 1:500) {
      
      ### data generation
      set.seed(k)
      N = nums
      d = 10
      X = matrix(rnorm(N*d, 0, 1), nrow=N)
      beta = rep(1, d)
      ps = ps
      Z = rbinom(N, size=1, p=ps)
      mu0 = X %*% as.matrix(beta, nrow=d)
      mu1 = mu0 + 4
      Y = ifelse(Z==0, mu0, mu1) + rnorm(1, 0, 1)
      
      data = as.data.frame(cbind(X, Z, Y))
      
      
      ### S-learner
      data.sl = data
      sl = randomForest(formula = Y~.,
                        data = data.sl,
                        ntree = 500,
                        nodesize = 5)
      sl0 = cbind(data.sl[, c(1:10)], Z=rep(0, nrow(data.sl)))
      sl1 = cbind(data.sl[, c(1:10)], Z=rep(1, nrow(data.sl)))
      data.sl$ite.sl = predict(sl, newdata=sl1, type="response") - predict(sl, newdata=sl0, type="response")
      ites.sl = cbind(ites.sl, data.sl$ite.sl)
      
      
      ### T-learner
      data.tl = data
      tl.0 = randomForest(formula = Y~.,
                          data = data[data$Z==0, -11],
                          ntree = 500,
                          nodesize = 5)
      tl.1 = randomForest(formula = Y~.,
                          data = data[data$Z==1, -11],
                          ntree = 500,
                          nodesize = 5)
      data.tl$ite.tl = predict(tl.1, newdata=data.tl[, c(1:10)], type="response") - predict(tl.0, newdata=data.tl[, c(1:10)], type="response")
      ites.tl = cbind(ites.tl, data.tl$ite.tl)
      
      
      ### X-learner
      xl.0 = randomForest(formula = Y~.,
                          data = data[data$Z==0, -11],
                          ntree = 500,
                          nodesize = 5)
      xl.1 = randomForest(formula = Y~.,
                          data = data[data$Z==1, -11],
                          ntree = 500,
                          nodesize = 5)
      data.xl = data
      # imputed treatment effect
      data.xl$imputed = ifelse(data.xl$Z==0,
                               predict(xl.1, newdata=data.xl[, c(1:10)], type="response") - data.xl$Y, # d0
                               data.xl$Y - predict(xl.0, newdata=data.xl[, c(1:10)], type="response")) # d1
      # imputed treatment effect ~ Xs
      xl.d0 = randomForest(formula = imputed~.,
                           data = data.xl[data.xl$Z==0, -c(11:12)],
                           ntree = 500,
                           nodesize = 5)
      xl.d1 = randomForest(formula = imputed~.,
                           data = data.xl[data.xl$Z==1, -c(11:12)],
                           ntree = 500,
                           nodesize = 5)
      xl.psmodel = glm(formula=Z~., data=data[, c(1:11)], family="binomial")
      data.xl$ps = predict(xl.psmodel, newdata=data.xl[, c(1:10)], type="response")
      tau0 = predict(xl.d0, newdata=data.xl[, c(1:10)], type="response")
      tau1 = predict(xl.d1, newdata=data.xl[, c(1:10)], type="response")
      data.xl$ite.xl = data.xl$ps*tau0 + (1-data.xl$ps)*tau1
      ites.xl = cbind(ites.xl, data.xl$ite.xl)
      
      
      ### DR-learner
      data.drl = data
      randoms = sample(x=c(1:2), size=nrow(data.drl), replace=T, prob=c(1/2, 1/2))
      data.drl$r = randoms
      
      attach(data.drl)
      ## 1st
      psmd1 = glm(formula=Z~., data=data.drl[r==1, c(1:11)], family="binomial")
      # (r=1) propensity score estimation
      ps1 = predict(psmd1, newdata=data.drl[, c(1:10)], type="response")
      # (r=1) outcome estimation
      om1.0 = randomForest(formula = Y~.,
                           data = data.drl[(r==1)&(Z==0), c(1:10, 12)],
                           ntree = 500,
                           nodesize = 5)
      om1.1 = randomForest(formula = Y~.,
                           data = data.drl[(r==1)&(Z==1), c(1:10, 12)],
                           ntree = 500,
                           nodesize = 5)
      # pseudo outcome
      mu1.0 = predict(om1.0, newdata = data.drl[, c(1:10)], type="response")
      mu1.1 = predict(om1.1, newdata = data.drl[, c(1:10)], type="response")
      data.drl$pseudo1 = ifelse(Z==0,
                                ((mu1.0-Y)/(1-ps1))+mu1.1-mu1.0,
                                ((Y-mu1.1)/ps1)+mu1.1-mu1.0)
      # (r=2) pseudo outcome regression
      phi1 = randomForest(formula = pseudo1~.,
                          data = data.drl[r==2, c(1:10, 14)],
                          ntree = 500,
                          nodesize = 5)
      taudrl1 = predict(phi1, newdata=data.drl[, c(1:10)], type="response")
      
      # 2nd
      psmd2 = glm(formula=Z~., data=data.drl[r==2, c(1:11)], family="binomial")
      # (r=2) propensity score estimation
      ps2 = predict(psmd2, newdata=data.drl[, c(1:10)], type="response")
      # (r=2) outcome estimation
      om2.0 = randomForest(formula = Y~.,
                           data = data.drl[(r==2)&(Z==0), c(1:10, 12)],
                           ntree = 500,
                           nodesize = 5)
      om2.1 = randomForest(formula = Y~.,
                           data = data.drl[(r==2)&(Z==1), c(1:10, 12)],
                           ntree = 500,
                           nodesize = 5)
      # pseudo outcome
      mu2.0 = predict(om2.0, newdata = data.drl[, c(1:10)], type="response")
      mu2.1 = predict(om2.1, newdata = data.drl[, c(1:10)], type="response")
      data.drl$pseudo2 = ifelse(Z==0,
                                ((mu2.0-Y)/(1-ps2))+mu2.1-mu2.0,
                                ((Y-mu2.1)/ps2)+mu2.1-mu2.0)
      # (r=1) pseudo outcome regression
      phi2 = randomForest(formula = pseudo2~.,
                          data = data.drl[r==1, c(1:10, 15)],
                          ntree = 500,
                          nodesize = 5)
      taudrl2 = predict(phi2, newdata=data.drl[, c(1:10)], type="response")
      detach(data.drl)
      
      # final prediction
      r1 = sum(data.drl$r==1)
      r2 = sum(data.drl$r==2)
      data.drl$ite.drl = (taudrl1*r1 + taudrl2*r2) / nrow(data.drl)
      ites.drl = cbind(ites.drl, data.drl$ite.drl)
      
      
      ### ALL
      psall = glm(formula=Z~., data=data[, c(1:11)], family="binomial")$fitted.values
      ystar = ifelse(data$Z==1, data$Y/psall, data$Y/(psall-1))
      ITES = as.data.frame(cbind(ystar, data.sl$ite.sl, data.tl$ite.tl,
                                 data.xl$ite.xl, data.drl$ite.drl))
      names(ITES) = c("ystar", "ITE.sl", "ITE.tl", "ITE.xl", "ITE.drl")
      
      
      ### RMSE
      RMSE[k, 1] = rmse(ystar, ITES$ITE.sl) # S-learner
      RMSE[k, 2] = rmse(ystar, ITES$ITE.tl) # T-learner
      RMSE[k, 3] = rmse(ystar, ITES$ITE.xl) # X-learner
      RMSE[k, 4] = rmse(ystar, ITES$ITE.drl) # DR-learner
      
      ### BIAS
      BIAS[k, 1] = bias(ystar, ITES$ITE.sl)^2 # S-learner
      BIAS[k, 2] = bias(ystar, ITES$ITE.tl)^2 # T-learner
      BIAS[k, 3] = bias(ystar, ITES$ITE.xl)^2 # X-learner
      BIAS[k, 4] = bias(ystar, ITES$ITE.drl)^2 # DR-learner
      
    }
    
    eval(parse(text=paste0( "write.csv(RMSE, 'RCT result/rmse(", nums, ", ", ps, ").csv'",", row.names=F)")))
    eval(parse(text=paste0( "write.csv(BIAS, 'RCT result/bias(", nums, ", ", ps, ").csv'",", row.names=F)")))
    eval(parse(text=paste0( "write.csv(ites.sl, 'RCT result/ite.sl(", nums, ", ", ps, ").csv'",", row.names=F)")))
    eval(parse(text=paste0( "write.csv(ites.tl, 'RCT result/ite.tl(", nums, ", ", ps, ").csv'",", row.names=F)")))
    eval(parse(text=paste0( "write.csv(ites.xl, 'RCT result/ite.xl(", nums, ", ", ps, ").csv'",", row.names=F)")))
    eval(parse(text=paste0( "write.csv(ites.drl, 'RCT result/ite.drl(", nums, ", ", ps, ").csv'",", row.names=F)")))
    
    print(paste0("n=", nums, " ps=", ps, " completed"))
    
  }
  
}



