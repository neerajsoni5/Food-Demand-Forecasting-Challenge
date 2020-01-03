library(data.table)
library(stringr)
library(readxl)
library(forecast)
library(plyr)
library(dplyr)
library(reshape)
library(plyr)
library(tidyr)
library(broom)

setwd("C:\\Users\\n.kumar.soni\\OneDrive - Accenture\\additional\\demandfocsting")

#mapping<-as.data.frame(read_excel("Mapping.xlsx"),sheet=1)
#mapping$ISBN13<-as.character(mapping$ISBN13)

#season<-as.data.frame(read_excel("Seasonality_Return.xlsx"),sheet=1)
#season$ISBN13<-as.character(season$ISBN13)
#season<-season[!duplicated(season$ISBN13),]

#long_season<-melt(season,id.vars=c("ISBN13"))
#names(long_season)[3]<-"seasnality"

#June_seg<-as.data.frame(fread("C:\\Users\\n.kumar.soni\\OneDrive - Accenture\\Pearson\\July_Reporting\\Pre_Tunning\\US_July_Reports\\INC5051151_Data_L2_US_Mahak\\segments_July_US.csv"))
#names(June_seg) 
#June_seg<-June_seg[,c(1:4)] 
#June_seg$ISBN<-as.character(June_seg$ISBN)

#setwd("C:\\Users\\n.kumar.soni\\OneDrive - Accenture\\additional\\ReturnFCST\\")

Clubbed_data<-as.data.frame(fread("train_final.csv"))
train<-Clubbed_data[Clubbed_data$Flag=="train",]
train$Flag<-NULL

test<-Clubbed_data[Clubbed_data$Flag=='test',]
test$Flag<-NULL

#Complete Sereis
#week<-as.data.frame(c(1:52))
#week<-as.data.frame(unique(Clubbed_data$week))
#unique_center_id<-as.data.frame(unique(Clubbed_data$center_id))
#unique_meal_id<-as.data.frame(unique(Clubbed_data$meal_id))
week<-as.data.frame(unique(train$week))
names(week)[1]<-"week"

centre_meal<-unique(train[,c("center_id","meal_id")])
list_final<-merge(centre_meal,week)
#weekBycentre<-merge(week,unique_center_id)
#add_meal<-merge(weekBycentre,unique_meal_id)
#names(add_meal)[1:3]<-c("week","center_id","meal_id")

#week_df<-unique(Clubbed_data$week)
#f_week<-data.frame(week_df,week_index=c(1:52,1:52,1:51))
#final_data<-merge(add_meal,f_week,by.x=c("week"),by.y=c("week_df"),all.x = T)

#final_data_v2<-merge(final_data,Clubbed_data[,c(1:9,16)],by=c("week","center_id","meal_id"),all.x=T)

#final_data_v2$Flag<-ifelse(final_data_v2$week<146,"train","test")

#final_data_v2[is.na(final_data_v2)]<-0
trainfin<-merge(list_final,train[,c(2,3,4,6,7,8,9,10,15)],all.x=T)

#Create a Key for Split
trainfin$key<-paste(trainfin$center_id,trainfin$meal_id,sep="_")

#Check
#sum_centreMeal<-aggregate(num_orders~center_id+meal_id,data=final_data_v2,sum)

#train1<-final_data_v2[final_data_v2$Flag=="train",]
#train1$Flag<-NULL

#test1<-final_data_v2[final_data_v2$Flag=="test",]
#test1$Flag<-NULL

#split.by<-c('week_index',"center_id","meal_id")
trainfin[is.na(trainfin)]<-0
split.by<-c("center_id","meal_id")

#a<-aggregate(num_orders~center_id+meal_id,data=train1,sum)
#a<-a[which(a$num_orders>0),]
#trainfin<-merge(train1,a[,c(1:2)],by=c("center_id","meal_id"),all.x=T)
#train1<-train1[which(train1$num_orders>0),]
#b<-aggregate(id~center_id+meal_id,data=train1,length)

train$key<-paste(train$center_id,train$meal_id,sep="_")
#train$key<-NULL
#Data.preprocess.model.list_ts<-split(trainfin[,c(7)],trainfin$key)
#Data.preprocess.model.list<-split(train[,c(1,2,3,4,5,7,8,15)],train[,split.by])
#Data.preprocess.model.list<-split(train[,c(2,310)],train[,split.by])

#Data.preprocess.model.list<-split(trainfin[,c(7)],trainfin$key)


Data.preprocess.model.list<-split(trainfin[,c(9)],trainfin$key)
#xreg.list<-split(trainfin[,c(1,2,4,5,6,7)],trainfin$key)

#single 
autoarima <- lapply(Data.preprocess.model.list, function(x){auto.arima(ts(x,frequency=52))})
##Data frame with model validation parameter
ts3_models <- lapply(Data.preprocess.model.list, function(x) auto.arima(x) %>% glance) %>% do.call(rbind,.)
ts3_models$key<-row.names(ts3_models)

#ts5_models <- lapply(autoarima, function(x) (x$arma) %>% glance) %>% do.call(rbind,.)

#p<-autoarima[[1]]$model$phi
#d<-autoarima[[1]]$model$theta
#q<-autoarima[[1]]$model$Delta

##model names
model.names<-lapply(autoarima, function(x){(x)$arma})
model<-as.data.frame(do.call(rbind,model.names))
model$key<-row.names(model)
names(model)[1:7]<-c("p", "q", "P", "Q", "m", "d", "D")

##Extract accuracy parameters
sum<-lapply(autoarima,function(x){summary(x)})
model.error<-as.data.frame(do.call(rbind,sum))

#model.error<-do.call(rbind,sum)
#model.error<-data.frame(Reduce(rbind, sum))


##Aggrgate model parameters
model.output<-cbind(model,model.error)
model.output<-merge(model.output,ts3_models,by="key",all.x=T)

forec<-lapply(lms,function(x){forecast(x,h=10)$mean})

master<-as.data.frame(do.call(rbind,forec))
master$key<-row.names(master)
mapping<-data.frame(variable=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"),week=c(146:155))

library(reshape2)
long<-melt(master,id.vars ="key")
long<-merge(long,mapping,by="variable",all.x=T)
test$key<-paste(test$center_id,test$meal_id,sep="_")
#aab<-t(do.call(rbind,forec))

submission<-merge(test,long,by=c("key","week"),all.x=T)
sub<-submission[,c("id","value")]
names(sub)[1:2]<-c("id","num_orders")

sub$num_orders<-ifelse(sub$num_orders<0,0,sub$num_orders)


write.csv(sub,"submission_v1.csv",row.names = F)



###############################################################3
########apply xreg

#w <- fourier(qty, K=3)
#fit3 <- auto.arima(qty, xreg=cbind(price,promo,fw))

##xreg should be numeric vector or numeric matrix. No data frame , else you need to do dummy coding
Data.preprocess.model.list<-split(trainfin[,c(9)],trainfin$key)

#factor<-c("center_id","meal_id","category","cuisine")
#trainfin$center_id<-as.factor(trainfin$center_id)
#trainfin$meal_id<-as.factor(trainfin$meal_id)
#trainfin$category<-as.factor(trainfin$category)
#trainfin$cuisine<-as.factor(trainfin$cuisine)

trainfin$base_price<-as.integer(trainfin$base_price)
#xreg.list<-split(trainfin[,c(4,5,6)],trainfin$key)
xreg.list<-split(trainfin[,c(4)],trainfin$key)

#single 
for (i in 1:length(xreg.list)){
autoarima <- lapply(Data.preprocess.model.list, function(x){auto.arima(ts(x,frequency = 52),xreg=data.matrix(xreg.list[[i]]))})
}

##Data frame with model validation parameter
ts3_models_xreg <- lapply(Data.preprocess.model.list, function(x) auto.arima(x) %>% glance) %>% do.call(rbind,.)
ts3_models_xreg$key<-row.names(ts3_models)



##model names
model.names_xreg<-lapply(autoarima, function(x){(x)$arma})
model<-as.data.frame(do.call(rbind,model.names_xreg))
model$key<-row.names(model)
names(model)[1:7]<-c("p", "q", "P", "Q", "m", "d", "D")

##Extract accuracy parameters
sum<-lapply(autoarima,function(x){summary(x)})
model.error_xreg<-as.data.frame(do.call(rbind,sum))

#model.error<-do.call(rbind,sum)
#model.error<-data.frame(Reduce(rbind, sum))


##Aggrgate model parameters
model.output<-cbind(model,model.error_xreg)
model.output<-merge(model.output,ts3_models_xreg,by="key",all.x=T)

xreg_test<-test[,c("center_id","meal_id","base_price","emailer_for_promotion","homepage_featured")]
xreg_test$key<-paste(xreg_test$center_id,xreg_test$meal_id,sep="_")

xreg.list.test<-split(xreg_test[,c(3,4,5)],xreg_test$key)



#ParkingForecast=forecast(bestfit,xreg=cbind(model.matrix(~Parking.Test$WeekDay)[,-1],
 #                                           Parking.Test$AvgTrafficFlow,
  #                                          forecast::fourier(ParkingTS1, K=4)))
for (i in 1:length(xreg.list.test)){
forec<-lapply(autoarima,function(x){forecast(x,xreg=data.matrix(xreg.list.test[[i]]),h=1)$mean})
}

master<-as.data.frame(do.call(rbind,forec))
master$key<-row.names(master)
mapping<-data.frame(variable=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"),week=c(146:155))

library(reshape2)
long<-melt(master,id.vars ="key")
long<-merge(long,mapping,by="variable",all.x=T)
test$key<-paste(test$center_id,test$meal_id,sep="_")
#aab<-t(do.call(rbind,forec))

submission<-merge(test,long,by=c("key","week"),all.x=T)
sub<-submission[,c("id","value")]
names(sub)[1:2]<-c("id","num_orders")

sub$num_orders<-ifelse(sub$num_orders<0,0,sub$num_orders)


write.csv(sub,"submission_v1.csv",row.names = F)

#########################Complete






##Extract accuracy parameters
sum<-lapply(autoarima,function(x){accuracy(x)})
model.error<-as.data.frame(do.call(rbind,sum))


ply<-do.call(rbind,sum)
a3<-data.frame(Reduce(rbind, sum))
a4<-data.frame(Reduce(rbind, autoarima))


method<-lapply(autoarima,function(x){cbind(x)})
aa<-do.call(rbind,autoarima)



ts4_models <-lapply(autoarima, function(x) accuracy(x) %>% glance) %>% do.call(rbind,.)
ts4_models <-lapply(autoarima, function(x) {accuracy(x)})
error<-as.data.frame(do.call(rbind,ts4_models))
#%>% glance) %>% do.call(rbind,.)

#c<-c(lms[[1]]$coef,lms[[1]]$sigma2)
method<-do.call(rbind,autoarima)
aa<-do.call(rbind,method)
aab<-t(do.call(rbind,method))
forec<-lapply(lms,function(x){forecast(x,h=10)$mean})

master<-as.data.frame(do.call(rbind,forec))
master$key<-row.names(master)
mapping<-data.frame(variable=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"),week=c(146:155))

library(reshape2)
long<-melt(master,id.vars ="key")
long<-merge(long,mapping,by="variable",all.x=T)
test$key<-paste(test$center_id,test$meal_id,sep="_")
#aab<-t(do.call(rbind,forec))

submission<-merge(test,long,by=c("key","week"),all.x=T)
sub<-submission[,c("id","value")]
names(sub)[1:2]<-c("id","num_orders")

sub$num_orders<-ifelse(sub$num_orders<0,0,sub$num_orders)


write.csv(sub,"submission_v1.csv",row.names = F)


for (i in 1:length(forec)){
  a<-names(forec[i])
  
  key<-rbind(a)
}


ts<-list(NA)
for (i in 1:length(Data.preprocess.model.list)){
  paste("ts_",names(Data.preprocess.model.list)[i])<-ts(Data.preprocess.model.list[[i]][8],frequency=52)
  ts<-paste("ts_",names(Data.preprocess.model.list)[i])
}
####################3


library(dplyr)
library(forecast)
model_fits <- group_by(train, c("center_id")) %>% do(fit=auto.arima(.$num_orders))


)a<-as.data.frame(Data.preprocess.model.list[1])
b<-(cbind(a$X1.center_id,a$X1.meal_id,a$X1.checkout_price))
f1<-auto.arima(ts(a$X1.num_orders),xreg=b)
f2<-auto.arima(ts(a$X1.num_orders))
pred1 = forecast(f1,xreg=b) # forecast is from forecast package
fit1_acry   = accuracy(pred1) #   accuracy from forecast package see page number 10

pred2 = forecast(f2) # forecast is from forecast package
fit2_acry   = accuracy(pred2)

ts<-list()
for (i in 1:length(Data.preprocess.model.list)){
  ts_i <- ts(Data.preprocess.model.list[[i]][8],frequency=52) 
  ts<-data.frame(ts_i)
  
  #auto_arima<-auto.arima(ts)
}

lms <- lapply(Data.preprocess.model.list, function(x){lm(log.tranfer.lag4~log.transfer.gross+ses_flag ,data=x)})

lms<-lapply(Data.preprocess.model.list_ts, function(x){auto.arima(ts(x))})





#Data.preprocess<-na.omit(fin.merge.lead)
#Data.preprocess$Returns_Vol<-NULL
Data.preprocess$month<-as.numeric(substr(Data.preprocess$year,6,7))

Data.preprocess$ses_flag<-ifelse(Data.preprocess$month %in% c(7,8,11,12),1,0)
#Data.preprocess<-merge(Data.preprocess,long_season,by.x=c("ISBN13","month"),by.y=c("ISBN13","variable"),all.x=T)


#linearity bet pred and Response
aggregated<-aggregate(cbind(value,lag4_return)~year,data=fin.merge.lead,sum)
scatter.smooth(x=aggregated$value, y=aggregated$lag4_return, main="Gross ~ Return")

##Normality of target & Independent variable
library(e1071)  # for skewness function
library(ggplot2)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(log(aggregated$lag4_return)), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'

##alternate
ggplot(aggregated, aes(lag4_return)) + geom_density(fill="blue")
ggplot(aggregated, aes(log(lag4_return))) + geom_density(fill="blue")

plot(density(log(aggregated$value)), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'

ggplot(aggregated, aes((value))) + geom_density(fill="blue")

####Check Correlation
cor(aggregated$value, aggregated$lag4_return)

Data.preprocess$value<-ifelse(Data.preprocess$value==0,1,Data.preprocess$value)
Data.preprocess$lag4<-ifelse(Data.preprocess$lag4_return==0,1,Data.preprocess$lag4_return)

Data.preprocess$log.transfer.gross<-log(Data.preprocess$value)
Data.preprocess$log.tranfer.lag4<-log(Data.preprocess$lag4)
#Data.preprocess<-merge(Data.preprocess,mapping,by="ISBN13",all.x=T)

Data.preprocess.model<-Data.preprocess[,c(1,12,13,10)]

Data.preprocess.model.list<-split(Data.preprocess.model,Data.preprocess.model$ISBN13)

lms <- lapply(Data.preprocess.model.list, function(x){lm(log.tranfer.lag4~log.transfer.gross+ses_flag ,data=x)})

#Data.preprocess.arima<-Data.preprocess[,c(1,6,11)]
#Data.preprocess.arima.list<-split(Data.preprocess.arima[,c(2:3)],Data.preprocess.arima$ISBN13)

#arima <- lapply(Data.preprocess.arima.list, function(x){auto.arima(x)})


cf<-sapply(lms, coef)
cf_t<-as.data.frame(t(cf))
cf_t$ISBN<-row.names(cf_t)
# if you need more info, get full summary call. now you can get whatever, like:
summaries <- lapply(lms, summary)
recof<-sapply(summaries, function(x) c(r_sq = x$r.squared, 
                                       adj_r_sq = x$adj.r.squared))


write.csv(cf_t,"coefficict_flag.csv")
#res<-sapply(lms,r.squared)
#pre<-predict(lms[[1]],3)

#summaries <- sapply(lms, summary)
## ...coefficents with p values:
#aa<-sapply(summaries, function(x) x$coefficients[, c(1,4)])
# ...or r-squared values
#sapply(summaries, function(x) c(r_sq = x$r.squared, 
#                              adj_r_sq = x$adj.r.squared))

##########################################################################
###########Calculate Return Probablity by Logit
logit.Data<-Data.preprocess[,c(1,6,7,8)]
logit.Data$Return<-ifelse(logit.Data$lag4_return>0,1,0)
logit.Data$month<-as.numeric(substr(logit.Data$year,6,7))
logit.Data.process<-logit.Data[,c(1,6,3,5)]

fit.logit<-glm(Return~month+value,data=logit.Data.process,family=binomial(link='logit') )

probabilities <- predict(fit.logit,newdata=logit.Data.process,type='response')

logit.Data.process$prediction<-probabilities

logit.Data.process$response<-ifelse(logit.Data.process$prediction>=0.5,1,0)

table(logit.Data.process$Return,logit.Data.process$response)





















































fin.merge.long <- melt(fin.merge, id.vars = c("ISBN13","PPID","Product Family","Segment","Geography"))
fin.merge.long$variable<-as.character(fin.merge.long$variable)

fin.merge.long$year<-trimws(substr(fin.merge.long$variable,nchar(fin.merge.long$variable)-7,nchar(fin.merge.long$variable)))
fin.merge.long$Flag<-trimws(str_extract(fin.merge.long$variable,pattern = "GROSS VOLUME|RETURNS VOLUME "))
#long_Gross <- melt(fin.merge.Gross, id.vars = c("ISBN13","PPID","Product Family","Segment","Geography"))
#long_Return<-melt(fin.merge.Return, id.vars = c("ISBN13","PPID","Product Family","Segment","Geography"))

fin.merge.long<-fin.merge.long[,c(1:5,8,9,7)]
names(fin.merge.long)

fin.merge.wide<-dcast(fin.merge.long,c("ISBN13","PPID","Product Family","Segment"
                                       ,"Geography","year")~Flag,value.var="value")


library(reshape2)
fin.merge.wide<-reshape(fin.merge.long,idvar=c("ISBN13","PPID","Product Family","Segment"
                                               ,"Geography","year"),timevar="Flag",v.names="value",direction="wide")


library(forecast)
print(system.time({ 
  sales <- read.csv("C:\\Users\\Red Eye\\Desktop\\ARIMA\\Sales History1.csv", header = FALSE)
  sales <- ts(sales[,-1], f = 12, s = c(2016,4))
  h <- 3
  fit <- apply(sales, 2, auto.arima)
  fcast <- lapply(fit, forecast, h = h)
  fcast <- sapply(fcast,"[",4)
  fcast <- (as.data.frame(fcast))
  write.csv(t(fcast), file = "Sales Forecast.csv")  
}))