library(ggplot2)
library(tidyverse)
library(mosaic)
library(modelr)
library(rsample)
library(MASS)
library(caret)
library(foreach)
library(FNN)

capmetro_UT <- read.csv("~/GitHub/DATA MINING/ECO395M/data/capmetro_UT.csv",header = TRUE)
capmetro_UT = mutate(capmetro_UT,
                     day_of_week = factor(day_of_week,
                                          levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month,
                                    levels=c("Sep", "Oct","Nov")))
capmetro_UT_summ = capmetro_UT %>%
  group_by(hour_of_day,day_of_week,month) %>%
  summarize(mean_boarding = mean(boarding))

ggplot(data=capmetro_UT_summ)+
  geom_line(aes(x = hour_of_day,y=mean_boarding,color = capmetro_UT_summ$month))+
  labs(title = "average boarding at each hour of day in Sep,Oct,Nov")+
  facet_wrap(~day_of_week)

ggplot(data=capmetro_UT)+
  geom_point(aes(x=temperature, y=boarding, color=weekend))+
  labs(title = "boardings vs temperature")+
  facet_wrap(~hour_of_day)





data(SaratogaHouses)
SaratogaHouses_sum = mutate(SaratogaHouses,
                            log_landValue = log(landValue))
lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
                 fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=SaratogaHouses_sum)
lm_step = step(lm_medium,
               scope=~(. + log_landValue + sewer + newConstruction)^3)
rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}
n = nrow(SaratogaHouses_sum)
n_train = round(0.8*n)
n_test = n - n_train
rmse = do(100)*{
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = SaratogaHouses_sum[train_cases,]
  saratoga_test = SaratogaHouses_sum[test_cases,]
  lm1 = update(lm_medium, data=saratoga_train)
  lm2 = update(lm_step, data=saratoga_train)
  yhat_test1 = predict(lm1, saratoga_test)
  yhat_test2 = predict(lm2, saratoga_test)
  c(rmse(saratoga_test$price, yhat_test1),
    rmse(saratoga_test$price, yhat_test2))
}

colMeans(rmse)

SaratogaHouses_model = model.matrix(~lotSize + age + landValue + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + sewer + centralAir + newConstruction - 1,
                                    data=SaratogaHouses)
SaratogaHouses_scale = scale(SaratogaHouses_model)
k_grid = seq(2,50, by=2)

k = foreach(i = 1:n, .combine='rbind') %dopar% {
  Saratoga_train = SaratogaHouses_scale[-i,]
  Saratoga_test = SaratogaHouses_scale[i,]
  price_train = SaratogaHouses$price[-i]
  price_test = SaratogaHouses$price[i]
  knn_mse2 = foreach(k = k_grid, .combine='c') %dopar% {
    models = knn.reg(Saratoga_train,Saratoga_test,price_train,k)
    (price_test-models$pred)^2}
  knn_mse2
} 
krmse = sqrt(colMeans(k))

min(krmse)



german_credit <- read.csv("~/GitHub/DATA MINING/ECO395M/data/german_credit.csv", header=TRUE)

german_credit_good = german_credit %>%
  filter(history == 'good')
table(german_credit_good$Default) 
good_prob = 53/sum(table(german_credit_good$Default))

german_credit_poor = german_credit %>%
  filter(history == 'poor')
table(german_credit_poor$Default) 
poor_prob = 197/sum(table(german_credit_poor$Default))

german_credit_terrible = german_credit %>%
  filter(history == 'terrible')
table(german_credit_terrible$Default) 
terrible_prob = 50/sum(table(german_credit_terrible$Default))

prob = cbind(good_prob,poor_prob,terrible_prob)

barplot(prob,main="default probability",xlab="history",ylab="probability")

logit_credit = glm(Default~duration+amount+installment+age+history+purpose+foreign,data = german_credit,family='binomial')

coef(logit_credit) %>% round(5)
exp(-1.10759)
exp(-1.88467)

hotels_dev=read.csv("~/GitHub/DATA MINING/ECO395M/data/hotels_dev.csv",header=TRUE)
hotels_val <- read.csv("~/GitHub/DATA MINING/ECO395M/data/hotels_val.csv",header=TRUE)
lm_medium1=lm(children~market_segment+adults+customer_type+is_repeated_guest,data = hotels_dev)
getCall(lm_medium1)
lm_medium2=lm(children~.-(arrival_date)-(children),data = hotels_dev)
getCall(lm_medium2)

lm0=lm(children ~ 1, data=hotels_dev)
lm_forward=step(lm0, direction='forward',scope=~(lead_time+stays_in_weekend_nights+poly(adults,2)+average_daily_rate+meal+market_segment+customer_type+is_repeated_guest)^2)

getCall(lm_forward)

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

hotels_dev_split = initial_split(hotels_dev, prop = 0.8)
hotels_dev_train = training(hotels_dev_split)
hotels_dev_test = testing(hotels_dev_split)

lm_hotels_dev_train1 = lm(children ~ market_segment+adults+customer_type+is_repeated_guest,data = hotels_dev_train)
lm_hotels_dev_train2 = lm(children ~.-(arrival_date)-(children),data = hotels_dev_train)
lm_hotels_dev_train3 = lm(children ~ average_daily_rate + market_segment + 
                            meal + poly(adults, 2) + customer_type + lead_time + is_repeated_guest + 
                            average_daily_rate:market_segment + average_daily_rate:poly(adults, 
                                                                                        2) + market_segment:poly(adults, 2) + average_daily_rate:meal + 
                            poly(adults, 2):customer_type + average_daily_rate:customer_type + 
                            market_segment:customer_type + meal:poly(adults, 2) + average_daily_rate:lead_time + 
                            poly(adults, 2):lead_time + market_segment:lead_time + meal:lead_time + 
                            customer_type:lead_time + meal:customer_type + average_daily_rate:is_repeated_guest + 
                            lead_time:is_repeated_guest + poly(adults, 2):is_repeated_guest, 
                          data = hotels_dev_train)

yhat1=predict(lm_hotels_dev_train1,hotels_dev_test)
yhat2=predict(lm_hotels_dev_train2,hotels_dev_test)
yhat3=predict(lm_hotels_dev_train3,hotels_dev_test)
baseline1_rmse=RMSE(yhat1, hotels_dev_test$children)
baseline2_rmse=RMSE(hotels_dev_test$children,yhat2)
mymodel_rmse=RMSE(yhat3, hotels_dev_test$children)

c(baseline1_rmse,baseline2_rmse,mymodel_rmse)

lm_val = update(lm_forward, data=hotels_dev)
hotel_val_hat= predict(lm_val, hotels_val)

rmse(hotels_val$children,hotel_val_hat)

phat_hotel_val= predict(lm_forward, hotels_val, type='response')
thresh_grid = 0.5
thresh_grid = seq(0.95, 0.05, by=-0.005)
roc_curve = foreach(thresh = thresh_grid, .combine='rbind') %do% {
  yhat_test = ifelse(phat_hotel_val >= thresh, 1, 0)
  # FPR, TPR for linear model
  confusion_out = table(y = hotels_val$children, yhat = yhat_test)
  outcome = data.frame(TPR = confusion_out[2,2]/sum(hotels_val$children==1),
                       FPR = confusion_out[1,2]/sum(hotels_val$children==0))
  rbind(outcome)
} %>% as.data.frame()
ggplot(roc_curve) + 
  geom_line(aes(x=FPR, y=TPR)) + 
  theme_bw(base_size = 10)

sum=apply(matrix(phat_hotel_val, 250, 20), 2, sum)

sum2=apply(matrix(hotels_val$children, 250, 20), 2, sum)

number=c(1:20)
all=data.frame(cbind(number,sum,sum2))

ggplot(data=all)+
  geom_line(aes(x=number,y=sum,color='red'))+
  geom_line(aes(x=number,y=sum2,color='blue'))+
  scale_color_discrete(labels = c("actual","predict"))














