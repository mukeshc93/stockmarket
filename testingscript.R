

library(quantmod)
library(xgboost)
data = datacollection()
data=data[complete.cases(data),]
data1 = data

#########testing steps
ret=data[,21:ncol(data)]
ret1=(t(apply(t(ret),2,cumsum))[,120])
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
ret=cbind(ret,data[,c(21,22,139,140)])


ret=ret[sample(nrow(ret)),]
n=nrow(ret)
train=ret[1:round(n*0.75),]
test=ret[(round(n*0.75)+1):n,]


ml = function(n) {
  xgtrain = xgb.DMatrix(as.matrix(train[, - ncol(train)]), label = train[, ncol(train)], missing = NA)
  xgtest = xgb.DMatrix(as.matrix(test), missing = NA)
  
  
  pred = rep(0, nrow(test))
  for (i in 1:n) {
    print(i)
    set.seed(i + 12)
    bst = xgb.train(param = list(
      "objective" = "reg:linear",
      "bst:eta" = 0.09,
      "bst:max_depth" = 9,
      "subsample" = 0.900,
      "colsample_bytree" = 0.40,
      "min_child_weight" = 6,
      "nthread" = 6), data = xgtrain, nrounds = 2000)
    #
    pred = pred + predict(bst, xgtest)
  }
  pred = pred / i
  return(pred)
}
pred = ml(8)
plot(pred, type = "h")
error = (test[, ncol(test)] - ((pred)))
print(mean(abs(error)));
print(mean(abs(pred)));
print(mean(abs(test[, ncol(test)])))
plot(test[, ncol(test)], type = "h") 
points(pred, col = 2)

act = NULL
pre = NULL
for (j in 1:nrow(test)) {
  act[j] = ifelse(test[j, ncol(test)] < 0, 0, 1)
  pre[j] = ifelse(pred[j] < 0, 0, 1)
}
table(act, pre)
random = cbind(act, pre)
ra = nrow(test) - nrow(random[which(rowSums(random) == 1),]);
print(ra / nrow(test))