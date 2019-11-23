# Punto 2

library(readr)
data_banknote_authentication <- read_csv("data_banknote_authentication.txt", col_names = FALSE)


########################### Bagging ########################### 

bag.car = randomForest(churn~., car, mtry = (dim(car)[2]-2))

imp = importance(bag.car)
imp
varImpPlot(bag.car)

pred_bag = predict(bag.car, test, type="prob")

roc_bag=roc(High.test,pred_bag[,2])
plot(roc_bag)
auc_bag=auc(roc_bag)
auc_bag
