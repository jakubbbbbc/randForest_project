library(rpart)
library(randomForest)
library(randForest)

# parameters
ntree = 10
m_frac = 0.2
minsplit = 20

# experiments for planes df
form = target ~ .
df_train = df_planes_train
df_test = df_planes_test

# form = target ~ . - city
# df_train = df1
# df_test = df2

fit1 = rpart(form, data=df_train, method = 'anova', control=list(minsplit=minsplit))
pred1 = predict(fit1, data=df_test)
pred1 = pred1 - min(pred1)
pred1 = pred1 / max(pred1)
pred1[pred1<0.5] = 0
pred1[pred1>=0.5] = 1
acc1 = mean(df_test$target == pred1)

fit2 = randForest(form, data=df_train, ntree=ntree, m_frac=m_frac, minsplit=minsplit)
pred2 = predict(fit2, df_test)
acc2 = mean(df_test$target == pred2)

fit3 <- randomForest(form, data=df_train, ntree=500)
pred3 = predict(fit3, df_test, type = 'class', )
acc3 = mean(df_test$target == pred3)


# cross validation for jobs df
k = 5
rows = nrow(df_jobs) / k
accs1 = double(k) # for rpart
accs2 = double(k) # for custom randForest
accs3 = double(k) # for randomForest
for (i in 1:k) {

  test_rows = seq(rows*(i-1)+1, rows*i)
  df_train = df_jobs[-test_rows,]
  df_test = df_jobs[test_rows,]
  
  
  fit1 = rpart(form, data=df_train, method = 'anova', control=list(minsplit=minsplit))
  pred1 = predict(fit1, data=df_test)
  pred1 = pred1 - min(pred1)
  pred1 = pred1 / max(pred1)
  pred1[pred1<0.5] = 0
  pred1[pred1>=0.5] = 1
  accs1[i] = mean(df_test$target == pred1)


  fit2 = randForest(form, data=df_train, ntree=ntree, m_frac=m_frac, minsplit=minsplit)
  pred2 = predict(fit2, df_test)
  accs2[i] = mean(df_test$target == pred2)
  
  fit3 <- randomForest(form, data=df_train, ntree=500)
  pred3 = predict(fit3, df_test, type = 'class', )
  accs3[i] = mean(df_test$target == pred3)
  
}
acc_avg1 = mean(accs1)
acc_avg2 = mean(accs2)
acc_avg3 = mean(accs3)

