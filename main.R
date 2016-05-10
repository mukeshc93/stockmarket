library(quantmod)
library(xgboost)


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

pred = (ml(2))
plot(pred, type = "h")
pred1=as.data.frame(pred)
act = NULL

for (j in 1:nrow(pred1)) {
    act[j] = ifelse(pred1[j, ] < 0, 0, 1)
}

upstocks = as.data.frame(cbind(row.names(test[as.logical(act),]),pred1[as.logical(act),]))
write.csv(upstocks, "upstock.csv")

downstocks = as.data.frame(cbind(row.names(test[!as.logical(act),]),pred1[!as.logical(act),]))
write.csv(downstocks, "downstock.csv")
