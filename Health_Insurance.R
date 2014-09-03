health_raw<-read.table(file='C:\\Users\\Kevin\\Desktop\\ds_test_final.txt',sep = '\t', header = T)
#Check size of dataset to predict on
sum(is.na(health_raw$hicov))
#without labels
health_pred<-health_raw[is.na(health_raw$hicov),]
#with labels
health<-health_raw[!is.na(health_raw$hicov),]
# attach label to end
hicov<-health$hicov
health$hicov<-NULL
health$hicov<-hicov


#train and test sets
nData<-dim(health)[1]
train_ind<-sample(1:nData,floor(nData*.6))
health_train<-health[train_ind,]
health_test<-health[-train_ind,]

#some exploratory data analysis
plot(density(health_train$agep),main = 'Distribution of Age')

#check for missing values
apply(is.na(health_train),2,sum)
write.table(apply(is.na(health_pred),2,sum),file = "Civis_predict.csv",sep = ",",col.names = T)
#calculate amount of training data left of if indiscriminately remove rows 
#with missing data
missing_byrow<-apply(is.na(health_train),1,sum)
sum(missing_byrow==0) 

#convert to factor
continuous<-c("agep","semp","puma","wkhp")
View(health_train[,continuous])
categorical<-setdiff(names(health),continuous)

for (counter in 1:length(categorical)){
  health_train[,categorical[counter]]<-as.factor(health_train[,categorical[counter]])
  health_pred[,categorical[counter]]<-as.factor(health_pred[,categorical[counter]])
}

#fit logistic regression
health_reg<-glm(formula =  hicov~agep+cit+dis+fs+mar+np+puma+rac1p+sex+st+type, family = binomial(logit),data = health_train)

#predict with logistic regression
predictions<-predict(health_reg,newdata=health_pred,type = "response")
threshold = .5
#True means that they are predicted to not have health care
write.table(predictions, file = "civis_predictions.csv", sep = ",",col.names = F)
