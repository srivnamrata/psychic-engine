
setwd("D:\\R\\Ln Regression")
### read the train file 
t=read.csv("train.csv")

store=read.csv("store.csv")

str(t)

head(store)

library(lubridate)
### convert date to  YMD date format
t$Date2=ymd(t$Date)

str(t)

## delete the date variable 
t$Date=NULL

### 
train=t[t$Date2<ymd("20150601"),]
test=t[t$Date2>ymd("20150531"),]

### exploratory data analysis 

library(sqldf)
avg_sales_date=sqldf("select date2,avg(sales) as sales from train group by date2")

### check the trends 

jan2014 = avg_sales_date[ avg_sales_date$Date2 == ymd("20140101") , ]

head(avg_sales_date)

library(ggplot2)
ggplot(data=avg_sales_date,aes(Date2,sales))+geom_line()

### avg sales for each store 

avg_sales_store=sqldf("select store,avg(sales) as store_avg_sales from train group by store")


head(avg_sales_store )


## Explore the store sales distribution
summary(avg_sales_store$sales)
hist(avg_sales_store$store_avg_sales)

### Check with te test file ( using avg predictions) 
test2=merge(test,avg_sales_store,by="Store")
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)


### Average sale for day of a week
avg_sales_dow=sqldf("select dayofweek,avg(sales) as sales from train
                    group by dayofweek")
head(avg_sales_dow, 7)

ggplot(data=avg_sales_dow,aes(DayOfWeek,sales))+ geom_bar(stat="identity")

### Per store per weekday
avg_sales_store_dow=sqldf("select store,dayofweek,avg(sales) as store_avg_sales 
                          from train group by store,dayofweek")

head(avg_sales_store_dow, 7)

### 
test2=merge(test,avg_sales_store_dow,by=c("Store","DayOfWeek"))
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)


### check store status OPEN or CLOSED

avg_sales_open=sqldf("select open,avg(sales) as sales from train
                     group by open")

avg_sales_open

avg_sales_store_dow_open=sqldf("select store,dayofweek,open,
                               avg(sales) as store_avg_sales 
                               from train group by store,dayofweek,open")

## test this with store , dayofweek, and open/close status
test2=merge(test,avg_sales_store_dow_open,by=c("Store","DayOfWeek","Open"))
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)


avg_sales_promo=sqldf("select Promo,avg(sales) as sales from train
                      group by Promo")

avg_sales_promo

#### with promotions 
avg_sales_store_dow_open_promo=sqldf("select store,dayofweek,open,promo,
                                     avg(sales) as store_avg_sales 
                                     from train group by store,dayofweek,open,promo")

## with promos 
test2=merge(test,avg_sales_store_dow_open_promo,by=c("Store","DayOfWeek","Open","Promo"))
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)



avg_sales_statehol=sqldf("select stateholiday,avg(sales) as sales from train
                         group by stateholiday")

avg_sales_statehol


avg_sales_store_dow_open_promo_statehol=sqldf("select store,dayofweek,open,promo,
                                              stateholiday,avg(sales) as store_avg_sales 
                                              from train group by store,dayofweek,open,
                                              promo,stateholiday")


test2=merge(test,avg_sales_store_dow_open_promo_statehol,by=c("Store","DayOfWeek","Open","Promo","StateHoliday"))
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)



avg_sales_schoolhol=sqldf("select schoolholiday,avg(sales) as sales from train
                          group by schoolholiday")

avg_sales_schoolhol


avg_sales_store_dow_open_promo_statehol_schoolhol=sqldf("select store,dayofweek,open,promo,
                                                        stateholiday,schoolholiday,avg(sales) as store_avg_sales 
                                                        from train group by store,dayofweek,open,
                                                        promo,stateholiday,schoolholiday")


test2=merge(test,avg_sales_store_dow_open_promo_statehol_schoolhol,by=c("Store","DayOfWeek","Open","Promo","StateHoliday","SchoolHoliday"))
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)


### merge train file with store file 
train2=merge(train,store,by="Store")
t=merge(test,store,by="Store")


#### This is the least RMSE we have calculated 
avg_sales_store_dow_open_promo_statehol_schoolhol_asmnt=sqldf("select store,dayofweek,open,promo,
                                                              stateholiday,assortment,avg(sales) as store_avg_sales 
                                                              from train2 group by store,dayofweek,open,
                                                              promo,stateholiday,assortment")

### with store assortment 
test2=merge(t,avg_sales_store_dow_open_promo_statehol_schoolhol_asmnt,by=c("Store","DayOfWeek","Open","Promo","StateHoliday","Assortment"))
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)

str(train2)
ggplot(data=train2, aes(Assortment, Sales)) + geom_boxplot()

test2=test2[rev(order(test2$error)),]
avg_sales_store=sqldf("select store,avg(sales) as store_avg_sales from train group by store")


#### add avg_sales_store to the train file 
train3=merge(train2,avg_sales_store,by="Store")
train3$DayOfWeek=as.factor(train3$DayOfWeek)
test2$DayOfWeek=as.factor(test2$DayOfWeek)
train3$Store=as.factor(train3$Store)
test2$Store=as.factor(test2$Store)
train3$month=month(train3$Date)
test2$month=month(test2$Date2)

train4=train3[train3$month>5 & train3$month<8,]
gc()

lm=lm((Sales)~DayOfWeek+Open+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment,data=train3)
#lm=glm((Sales)~Open+Promo,data=train3)


summary(lm)

test2$prediction=(predict(lm,test2))
test2$error=(test2$prediction-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)



avg_sales_dow_open_promo_statehol_schoolhol_asmnt=sqldf("select dayofweek,open,promo,
                                                              stateholiday,assortment,avg(sales) as store_avg_sales 
                                                              from train2 group by dayofweek,open,
                                                              promo,stateholiday,assortment")


test2=merge(t,avg_sales_dow_open_promo_statehol_schoolhol_asmnt,by=c("DayOfWeek","Open","Promo","StateHoliday","Assortment"))
test2$error=(test2$store_avg_sales-test2$Sales)^2
sqrt(mean(test2$error))
sqrt(mean(test2$error))/mean(test2$Sales)

rank=quantile(avg_sales_store$store_avg_sales,probs=seq(0,1,0.1))
rank
for(i in 1:nrow(avg_sales_store)){
  for(j in 2:length(rank)){
    
    if(avg_sales_store$store_avg_sales[i]<rank[[j]]){
      avg_sales_store$final_rank[i]=j
      break
    }
    
  }
}

train4=merge(train3,avg_sales_store,by="Store")
test3=merge(test2,avg_sales_store,by="Store")

train4$final_rank=as.factor(train4$final_rank)
test3$final_rank=as.factor(test3$final_rank)

test3$DayOfWeek = as.factor(test3$DayOfWeek)
lm=lm((Sales)~final_rank+DayOfWeek+Open+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+Promo2,data=train4)


summary(lm)
test3$prediction=(predict(lm,test3))
test3$error=(test3$prediction-test3$Sales)^2
sqrt(mean(test3$error))
sqrt(mean(test3$error))/mean(test3$Sales)

##########################################################
##########################################################
#####Treating all Na in the file
sum(is.na(train3))
sum(is.na(train3[,4]))
sum(is.na(train3[,16]))
table(train3$Promo2SinceWeek)

###Replace all the missing values in variable Promo2SinceWeek
train3$Promo2SinceWeek[is.na(train3$Promo2SinceWeek)]=0

###Replace all the missing values in df by a variable
train3[is.na(train3)]=99

train3$promosinceyears=year(train3$Date2)-train3$Promo2SinceYear

head(train3$promosinceyears)
train3$promosinceyears[is.na(train3$promosinceyears)]=0

summary(train3$promosinceyears)

test3[is.na(test3)]=99

test3$promosinceyears=year(test3$Date2)-test3$Promo2SinceYear

head(test3$promosinceyears)
test3$promosinceyears[is.na(test3$promosinceyears)]=0

summary(test3$promosinceyears)

### Avoid promosinceyears value less than 0
train3$promosinceyears=ifelse(train3$promosinceyears>0,train3$promosinceyears,0)
test3$promosinceyears=ifelse(test3$promosinceyears>0,test3$promosinceyears,0)

### Including the newly created variable in the model

cor(train3$Promo2SinceWeek,train3$promosinceyears)
