set.seed(1234)
x=rnorm(200, mean=2, sd=1.3)

error=rnorm(200,0,sd=5.9)
y=1+1.1*x-0.2*x^2+0.09*x^3+0.06*x^4+error
plot(x,y)
x2 = x^2
x3 = x^3
x4 = x^4

fit = lm(y~x)
fit = lm(y~x+x2+x3+x4)
summary(fit)

## b
train = x[1:120]
ytrain = y[1:120]
test = x[121:200]
ytest = y[121:200]


xtrain <- data.frame(x = train, 
                     x2 = train^2, 
                     x3 = train^3, 
                     x4 = train^4, 
                     x5 = train^5, 
                     x6 = train^6, 
                     x7 = train^7, 
                     x8 = train^8)



xtest <- data.frame(x = test, 
                     x2 = test^2, 
                     x3 = test^3, 
                     x4 = test^4, 
                     x5 = test^5, 
                     x6 = test^6, 
                     x7 = test^7, 
                     x8 = test^8)

fit1 = lm(ytrain~x, data = xtrain)
fit2 = lm(ytrain~x+x2, data = xtrain)
fit3 = lm(ytrain~x+x2+x3, data = xtrain)
fit4 = lm(ytrain~x+x2+x3+x4, data = xtrain)
fit5 = lm(ytrain~x+x2+x3+x4+x5, data = xtrain)
fit6 = lm(ytrain~x+x2+x3+x4+x5+x6, data = xtrain)
fit7 = lm(ytrain~x+x2+x3+x4+x5+x6+x7, data = xtrain)
fit8 = lm(ytrain~x+x2+x3+x4+x5+x6+x7+x8, data = xtrain)

mse = c()
pred1 = predict(fit1, xtest)
mse[1]=mean((ytest-pred1)^2)

pred2 = predict(fit2, xtest)
mse[2]=mean((ytest-pred2)^2)

pred3 = predict(fit3, xtest)
mse[3]=mean((ytest-pred3)^2)

pred4 = predict(fit4, xtest)
mse[4]=mean((ytest-pred4)^2)

pred5 = predict(fit5, xtest)
mse[5]=mean((ytest-pred5)^2)

pred6 = predict(fit6, xtest)
mse[6]=mean((ytest-pred6)^2)

pred7 = predict(fit7, xtest)
mse[7]=mean((ytest-pred7)^2)

pred8 = predict(fit8, xtest)
mse[8]=mean((ytest-pred8)^2)

plot(mse)
lines(mse)


#### Train
mse_train = c()
pred1 = predict(fit1, xtrain)
mse_train[1]=mean((ytrain-pred1)^2)

pred2 = predict(fit2, xtrain)
mse_train[2]=mean((ytrain-pred2)^2)

pred3 = predict(fit3, xtrain)
mse_train[3]=mean((ytrain-pred3)^2)

pred4 = predict(fit4, xtrain)
mse_train[4]=mean((ytrain-pred4)^2)

pred5 = predict(fit5, xtrain)
mse_train[5]=mean((ytrain-pred5)^2)

pred6 = predict(fit6, xtrain)
mse_train[6]=mean((ytrain-pred6)^2)

pred7 = predict(fit7, xtrain)
mse_train[7]=mean((ytrain-pred7)^2)

pred8 = predict(fit8, xtrain)
mse_train[8]=mean((ytrain-pred8)^2)

lines(mse_train)
