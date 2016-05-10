library(quantmod)
library(xgboost)
data = datacollection()
data=data[complete.cases(data),]
data1 = data


########Train dataset for model
ret=data[,21:ncol(data)]
ret1=(t(apply(t(ret),2,cumsum))[,ncol(ret1)])
lm1=matrix(NA,nrow(ret),ncol = 30)
for(i in 1:nrow(ret))
{
  print(i)
x=ts(as.data.frame((ret[i,1:(ncol(ret)-1)])))
x=forecast(x,1)
lm1[i,1]=x$model$loglik
lm1[i,2]=x$model$aic
lm1[i,3]=x$model$bic;lm1[i,4]=x$model$aicc;lm1[i,5]=x$model$mse
lm1[i,6]=x$model$amse;lm1[i,7]=x$model$fit$value;lm1[i,8:9]=x$lower[1:2]
lm1[i,10]=x$model$sigma2;
lm1[i,11:12]=x$upper[1:2]
lm1[i,13]=x$fitted[119];lm1[i,14:(13+length(x$model$par))]=x$model$fit$par
}
ret=data[,1:20]
ret=cbind(ret,lm1[,1:15])
ret=cbind(ret,ret1)
train=cbind(ret,data[,c(21,22,139,140)])




############test dataset for model
test = datacollection(requiretestdata = T)
test=test[,-21]
colnames(test)=c(c(colnames(test[, 1:20]), as.character(paste("return", 1:119, sep = ""))))
data = test
ret=data[,21:ncol(data)]
ret1=(t(apply(t(ret),2,cumsum))[,ncol(ret1)])
lm1=matrix(NA,nrow(ret),ncol = 30)
for(i in 1:nrow(ret))
{
  print(i)
  x=ts(as.data.frame((ret[i,1:(ncol(ret)-1)])))
  x=forecast(x,1)
  lm1[i,1]=x$model$loglik
  lm1[i,2]=x$model$aic
  lm1[i,3]=x$model$bic;lm1[i,4]=x$model$aicc;lm1[i,5]=x$model$mse
  lm1[i,6]=x$model$amse;lm1[i,7]=x$model$fit$value;lm1[i,8:9]=x$lower[1:2]
  lm1[i,10]=x$model$sigma2;
  lm1[i,11:12]=x$upper[1:2]
  lm1[i,13]=x$fitted[119];lm1[i,14:(13+length(x$model$par))]=x$model$fit$par
}
ret=data[,1:20]
ret=cbind(ret,lm1[,1:15])
ret=cbind(ret,ret1)
test=cbind(ret,data[,c(21,22,139)])
