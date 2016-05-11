
######################### xgboost cv and train

features = colnames(alldata)[-c(2:24,27,29,47:52)]  #remove text features 

cv.nround <- 5000
cv.nfold <- 3
param_cv <- list(max.depth=8, eta=0.03,  subsample = 1, colsample_bytree = 0.7, gamma = 1, min_child_weight = 1, scale_pos_weight = 1, silent=0, objective='reg:linear', eval_metric = 'rmse')
#param_cv <- list(max.depth=7, eta=0.01,  subsample = 0.9, colsample_bytree = 0.6, gamma = 0.3, min_child_weight = 2, scale_pos_weight = 1, silent=0, objective='reg:linear', eval_metric = 'rmse')
mincv_nrounds = array(0,c(5,2) )
for (i in 1:5) {
  folds <- createFolds(labels$relevance, k = 3, list = TRUE)
  # bst.cv = xgb.cv(data = data.matrix(X[,-c(2:20, 22,23,87,88, 96, 97)]), verbose = TRUE, print.every.n = 1, early.stop.round = 3, params = param_cv, label = data.matrix(y), nrounds = cv.nround, folds = folds ) #  nfold=cv.nfold, stratified = TRUE)
  bst.cv = xgb.cv(data = data.matrix(X[,features]), verbose = TRUE, print.every.n = 1, early.stop.round = 10, params = param_cv, label = data.matrix(y), nrounds = cv.nround, folds = folds ) #  nfold=cv.nfold, stratified = TRUE)
  mincv_nrounds[i,1] = min(bst.cv$test.rmse.mean)
  mincv_nrounds[i,2] = which.min(bst.cv[, test.rmse.mean])
}
nround = sum(mincv_nrounds[,2])/5
max_nround = max(mincv_nrounds[,2])
min_cv = sum(mincv_nrounds[,1])/5
min_cv
max_nround
nround


# train xgboost
set.seed(1)
#param <- list(max.depth=8, eta=0.03,  subsample = 1, colsample_bytree = 0.7, gamma = 1, min_child_weight = 1, scale_pos_weight = 1, silent=0, objective='reg:linear', eval_metric ='rmse')
param = param_cv
nround = 1809 #558
bst = xgboost(data = data.matrix(X[,features]), label = data.matrix(y), param = param, nrounds = nround)
xgb.save(bst,'C:/R/homedepot/temp_xgb.model')
# bst <- xgb.load('C:/R/homedepot/temp_xgb.model')



## predict values in train set for debugging purposes
#y_pred = predict(bst, data.matrix(X[,features]) )
#y_pred <- ifelse(y_pred>3,3,y_pred)
#y_pred <- ifelse(y_pred<1,1,y_pred)

#y_error = ((y-y_pred)^2)^0.5
#debug = cbind(y_pred, labels[,-1], y_error, X)
#write_csv(temp,"C:/R/homedepot/temp.csv")


# predict values in test set
y_pred = predict(bst, data.matrix(Xtest[,features]) )
y_pred <- ifelse(y_pred>3,3,y_pred)
y_pred <- ifelse(y_pred<1,1,y_pred)

#chcount_xtest = sapply(debug_test$search_term_stem1,nchar)
#debug_test = cbind(y_pred, chcount_xtest, Xtest)


submission <- data.frame(id=test$id,relevance=y_pred)
write_csv(submission,"C:/R/homedepot/submission.csv")
print(Sys.time()-t)

