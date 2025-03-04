################################################################################
### RCT
rm(list=ls())
dev.off()
cat("\014")
setwd("C:/R/Rproject/metalearners/1. simulation")



### CATE
for (nums in c(300, 500, 1000)) {
  
  for (ps in c(0.1, 0.2, 0.5)) {
    
    ## cate
    eval(parse(text=paste0("SL = read.csv('RCT result/ite.sl(", nums, ", ", ps, ").csv', header=T)")))
    eval(parse(text=paste0("TL = read.csv('RCT result/ite.tl(", nums, ", ", ps, ").csv', header=T)")))
    eval(parse(text=paste0("XL = read.csv('RCT result/ite.xl(", nums, ", ", ps, ").csv', header=T)")))
    eval(parse(text=paste0("DRL = read.csv('RCT result/ite.drl(", nums, ", ", ps, ").csv', header=T)")))
    
    CATE = cbind(apply(SL, 2, mean), apply(TL, 2, mean),
                 apply(XL, 2, mean), apply(DRL, 2, mean))
    colnames(CATE) = c("S", "T", "X", "DR")
    
    eval(parse(text=paste0("write.csv(CATE, 'RCT evaluation/cate(", nums, ", ", ps, ").csv', row.names=F)")))
    
  }
  
}


### CATE boxplot
for (nums in c(300, 500, 1000)) {
  
  for (ps in c(0.1, 0.2, 0.5)) {
    
    ## cate
    eval(parse(text=paste0("cate = read.csv('RCT evaluation/cate(", nums, ", ", ps, ").csv', header=T)")))
    
    ## cate boxplot
    eval(parse(text=paste0("png('RCT evaluation/★cateplot(", nums, ", ", ps, ").png', width=800, height=600)")))
    boxplot(cate,
            xlab = "Metalearners",
            ylab = "CATE (Conditional Average Treatment Effect)",
            names = c("S-learner", "T-learner", "X-learner", "DR-learner"))
    abline(h=4, lwd=2, col="red")
    dev.off()
    
  }
  
}


### total summary
for (nums in c(300, 500, 1000)) {
  
  for (ps in c(0.1, 0.2, 0.5)) {
    
    ## cate, rmse, bias load
    eval(parse(text=paste0("cate = read.csv('RCT evaluation/cate(", nums, ", ", ps, ").csv', header=T)")))
    eval(parse(text=paste0("rmse = read.csv('RCT evaluation/rmse(", nums, ", ", ps, ").csv', header=T)")))
    eval(parse(text=paste0("bias = read.csv('RCT evaluation/bias(", nums, ", ", ps, ").csv', header=T)")))
    
    ## summary
    summary = rbind(apply(cate, 2, mean), apply(cate, 2, sd),
                    apply(rmse, 2, mean), apply(rmse, 2, sd),
                    apply(bias, 2, mean), apply(bias, 2, sd))
    rownames(summary) = c("cate.mean", "cate.sd",
                          "rmse.mean", "rmse.sd",
                          "bias.mean", "bias.sd")
    
    ## save
    eval(parse(text=paste0("write.csv(summary, 'RCT evaluation/★summary(", nums, ", ", ps, ").csv', row.names=T)")))
    
  }
  
}



################################################################################
### OBS
rm(list=ls())
dev.off()
cat("\014")
setwd("C:/R/Rproject/metalearners/1. simulation")


### CATE
for (nums in c(300, 500, 1000)) {
  
  ## cate
  eval(parse(text=paste0("SL = read.csv('OBS result/ite.sl(", nums, ").csv', header=T)")))
  eval(parse(text=paste0("TL = read.csv('OBS result/ite.tl(", nums, ").csv', header=T)")))
  eval(parse(text=paste0("XL = read.csv('OBS result/ite.xl(", nums, ").csv', header=T)")))
  eval(parse(text=paste0("DRL = read.csv('OBS result/ite.drl(", nums, ").csv', header=T)")))
  
  CATE = cbind(apply(SL, 2, mean), apply(TL, 2, mean),
               apply(XL, 2, mean), apply(DRL, 2, mean))
  colnames(CATE) = c("S", "T", "X", "DR")
  
  eval(parse(text=paste0("write.csv(CATE, 'OBS evaluation/cate(", nums, ").csv', row.names=F)")))
  
}


### CATE boxplot
for (nums in c(300, 500, 1000)) {
  
  ## cate
  eval(parse(text=paste0("cate = read.csv('OBS evaluation/cate(", nums, ").csv', header=T)")))
  
  ## cate boxplot
  eval(parse(text=paste0("png('OBS evaluation/★cateplot(", nums, ").png', width=800, height=600)")))
  boxplot(cate,
          xlab = "Metalearners",
          ylab = "CATE (Conditional Average Treatment Effect)",
          names = c("S-learner", "T-learner", "X-learner", "DR-learner"))
  abline(h=4, lwd=2, col="red")
  dev.off()
  
}


### total summary
for (nums in c(300, 500, 1000)) {
  
  ## cate, rmse, bias load
  eval(parse(text=paste0("cate = read.csv('OBS evaluation/cate(", nums, ").csv', header=T)")))
  eval(parse(text=paste0("rmse = read.csv('OBS evaluation/rmse(", nums, ").csv', header=T)")))
  eval(parse(text=paste0("bias = read.csv('OBS evaluation/bias(", nums, ").csv', header=T)")))
  
  ## summary
  summary = rbind(apply(cate, 2, mean), apply(cate, 2, sd),
                  apply(rmse, 2, mean), apply(rmse, 2, sd),
                  apply(bias, 2, mean), apply(bias, 2, sd))
  rownames(summary) = c("cate.mean", "cate.sd",
                        "rmse.mean", "rmse.sd",
                        "bias.mean", "bias.sd")
  
  ## save
  eval(parse(text=paste0("write.csv(summary, 'OBS evaluation/★summary(", nums, ").csv', row.names=T)")))
  
}







