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

pred = as.data.frame(ml(10))
plot(pred, type = "h")

act = NULL

for (j in 1:nrow(pred)) {
    act[j] = ifelse(pred[j, ] < 0, 0, 1)
}

upstocks = as.data.frame(cbind(row.names(test[as.logical(act),]),pred[as.logical(act),]))
write.csv(upstocks, "upstock.csv")

downstocks = as.data.frame(cbind(row.names(test[!as.logical(act),]),pred[!as.logical(act),]))
write.csv(downstocks, "downstock.csv")
