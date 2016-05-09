library(quantmod)
library(xgboost)
#data = datacollection()
#data=data[complete.cases(data),]
#data1 = data

#data=rd(data)
#write.csv(data,"data.csv",row.names = F)

#test = datacollection(requiretestdata = T)
#tdata1 = test
#testdata=rd(testdata)
#write.csv(testdata,"testdata.csv",row.names = F)

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
#pred = rep(0, nrow(test))
#for (i in 1:n) {
#set.seed(i+12)
#pred = pred+predict(bst, xgtest)
#  }
pred = ml(2)
plot(pred, type = "h")
error = (test[, ncol(test)] - ((pred)))
print(mean(abs(error)));
print(mean(abs(pred)));
print(mean(abs(test[, ncol(test)])))
plot(test[, ncol(test)], type = "h") #,ylim=c(-0.5,2))
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
upstocks = test[as.logical(act), ncol(data)]
write.csv(upstocks, "upstock.csv")

downstocks = test[as.logical(pre), ncol(data)]