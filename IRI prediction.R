data<-read.csv("data_1.csv")
sapply(data,function(x)sum(length(which(is.na(x)))))
NAremove <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
data_2<-NAremove(data, "Speed.Limit")
sapply(data_2,function(x)sum(length(which(is.na(x)))))
data_3<-NAremove(data_2,"DAY_PRECIPITATION")
sapply(data_3,function(x)sum(length(which(is.na(x)))))
f<-duplicated(data_3)#This check if we have some duplicate rows in the data
table(f)
#No duplicates in the rows


library('Amelia')
missmap(data_3,main="Missing values in the data") 

#Replacing missing values by mean/median
install.packages("Hmisc")
library("Hmisc")
data_3$Thickness<-with(data_3, impute(Thickness,mean))
data_3$MAX_DAY_WIND_SPD<-with(data_3, impute(MAX_DAY_WIND_SPD, mean))
f<-duplicated(data_3)#This check if we have some duplicate rows in the data
table(f)

str(data_3)
data_3$Speed.Limit <- factor(data_3$Speed.Limit)
data_3$Thickness<-as.numeric(data_3$Thickness)
data_3$MAX_DAY_WIND_SPD<-as.numeric(data_3$MAX_DAY_WIND_SPD)
#Splitting data into test and train (80% and 20%)
set.seed(101)
indexes <-sample(1:nrow(data_3), size=0.20*nrow(data_3))

# Split data
test <- data_3[indexes,]
dim(test)
train <- data_3[-indexes,]
dim(train)



#-----#-------------------#-------------
#Correlation plot
install.packages("corrplot")
library("corrplot")
png(height=1200, width=1500, pointsize=15, file="overlap.png")
data_cor<-data_3[-4]
data_cor<-data_cor[2:11]
data_cor$MAX_DAY_WIND_SPD<-as.numeric(data_cor$MAX_DAY_WIND_SPD)
corr<-cor(data_cor)
corrplot(corr,method="circle",title="Correlation Matrix for predictors",number.cex = 1)

#-----#-------------------#-------------
#Violin Plot
install.packages("vioplot")
library("vioplot")
vioplot(data_3$MEAN_DAY_TEMP, data_3$MAX_DAY_TEMP, data_3$MIN_DAY_TEMP, data_3$MAX_DAY_WIND_SPD ,data_3$Thickness, data_3$Mean_Day_Humidity, col='Green', names=c('Mean_Day_Temp','Max_Day_Temp','Min_Day_Temp','MAX_DAY_WIND_SPD','Thickness', 'Mean_Day_Humidity'))
vioplot(data_3$DAY_PRECIPITATION, data_3$SLOPE_VARIANCE,col='Green', names=c( 'Day_Precipitation','Slope Variance'))
vioplot(data_3$AADTT,data_3$KESAL_YEAR,names=c('AADTT','KESAL_Year'), col='Green')
vioplot(data_3$MRI,names=c('MRI'), col='Green')


#install.packages("ggplot2")
#library(ggplot2)
#ggplot(data_3, aes(x=SLOPE_VARIANCE))+geom_violin(trim=FALSE) + labs(title="Plot of MRI  by Slope Variance",x="Slope Variance", y = "MRI")
#ggplot(data_3, aes(x=Thickness, y=MRI))+geom_violin(trim=FALSE) + labs(title="Plot of MRI  by Thickness",x="Thickness", y = "MRI")
#ggplot(data_3, aes(x=Speed.Limit, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Speed Limit",x="Speed Limit", y = "MRI")
#ggplot(data_3, aes(x=DAY_PRECIPITATION, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Day Precipitation",x="Day Precipitation", y = "MRI")
#ggplot(data_3, aes(x=MEAN_DAY_TEMP, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Mean Day Temperature",x="Mean Day Temperature", y = "MRI")
#ggplot(data_3, aes(x=MAX_DAY_TEMP, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Max Day Temperature",x="Max Day Temperature", y = "MRI")
#ggplot(data_3, aes(x=MIN_DAY_TEMP, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Min Day Temperature",x="Min Day Temperature", y = "MRI")
#ggplot(data_3, aes(x=MAX_DAY_WIND_SPD, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Max day Wind Speed",x="Max day Wind Speed", y = "MRI")
#ggplot(data_3, aes(x=AADTT, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by AADTT",x="AADTT", y = "MRI")
#ggplot(data_3, aes(x=Mean_Day_Humidity, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Mean Day Humidity",x="Mean Day Humidity", y = "MRI")
#ggplot(data_3, aes(x=KESAL_YEAR, y=MRI))+geom_violin(trim=FALSE)+labs(title="Plot of MRI  by Kesal Year",x="Kesal Year", y = "MRI")

#-------#---------#_------------
stat<- function(reg.object,responseVar,train, test){
  pred.train<-predict(reg.object,train)
  pred.test<-predict(reg.object,test)
  RMSE.train<-sqrt(mean((pred.train- train[,responseVar])^2))
  RMSE.test<-sqrt(mean((pred.test- test[,responseVar])^2))
  SSE<-(sum((pred.train-train[,responseVar])^2))
  mean_vec<- rep(mean(train[,responseVar]), nrow(train))
  SST<- sum((train[,responseVar]- mean_vec)^2)
  R.square<- 1-SSE/SST
  stat_vec<- c(RMSE.train, RMSE.test, R.square)
  return(stat_vec)
}
#-----#---------#----------#_-------#----------
# Mean only model
y_pred<-mean(data_3$MRI)
RMSEdata<-sqrt(sum((y_pred-data_3$MRI)^2)/nrow(data_3))

#Fitting a linear model into the data
lm<-lm(MRI~.,data=train)
summary(lm) 
or_resid<-resid(lm)
qqnorm(or_resid,ylab="Residuals",xlab="Normal Scores") 
qqline(or_resid)# From this plot data seems to be approximately normally distributed. 
plot(predict(lm),or_resid) # Residual plot against response suggests constant variance


linear.reg<- lm(MRI~.,data=train)
summary(linear.reg)
stats_linear_reg<-stat(linear.reg,"MRI",train,test)
#Checking QQ plot of the data
qqnorm(resid(linear.reg),ylab="Residuals",xlab="Normal Scores") 
qqline(resid(linear.reg))

#STEPWISE Regression to chose the best subset of predictors from linear
null.step<-lm(MRI~1,data=train)
full.step<- lm(MRI~., data=train)
step.linear.reg<-step(null.step,scope=list(lower=null.step,upper=full.step), direction="both")
stat(step.linear.reg, "MRI", train, test)

#Trying Box-Cox
library('MASS')
bc<-boxcox(train$MRI~ ., data=train, lambda=seq(-5,5,0.2))
trans <- bc$x[which.max(bc$y)]
train_boxcox<-train
train_boxcox$MRI<-train_boxcox$MRI^trans
boxcox<- lm(MRI~.,data=train_boxcox)
#Best lambda comes to be close to 1, so I think w don't need box cox transformation
#Checking QQ plot of the data
qqnorm(resid(boxcox),ylab="Residuals",xlab="Normal Scores") 
qqline(resid(boxcox))


#Ridge Regression

ridge.reg<-lm.ridge(MRI~., data=train, lambda=seq(0.001,100,0.01)) #how to chose lambda????????
plot(ridge.reg) #gives the value of best lambda
summary(ridge.reg)
best.lambda<-ridge.reg$lambda[which.min(ridge.reg$GCV)]
#Ridge regression suggests lambda value to be 5.231
ridge.reg.final<-lm.ridge(MRI~., data=train, lambda=best.lambda)
summary(ridge.reg.final)

df<-train
df$MRI<-rep(1,nrow(df))
df<-data.matrix(df,rownames.force = NA)
coeff_ridge<-coefficients(ridge.reg.final)
coeff_ridge[is.na(coeff_ridge)]<-0
pred.train.ridge<- df*coeff_ridge

df1<-test
df1$MRI<-rep(1,nrow(df1))
df1<-data.matrix(df1,rownames.force = NA)
coeff_ridge1<-coefficients(ridge.reg.final)
coeff_ridge1[is.na(coeff_ridge)]<-0
pred.test.ridge<- df1*coeff_ridge1

RMSE.ridge.train<-sqrt(mean((pred.train.ridge- train$MRI)^2))
RMSE.ridge.test<-sqrt(mean((pred.test.ridge- test$MRI)^2))

mean_vector<-rep(mean(train$MRI), nrow(train))
SSE<-(sum((pred.train.ridge-train$MRI)^2))
SST<-sum((train$MRI- mean_vector)^2)
R.square.ridge<- 1-SSE/SST

#---------#_--------------#----------#_------------

install.packages("glmnet")
library(glmnet)
lasso.train<-train[,2:12]
lasso.test<-test[,2:12]
lasso.train.matrix<-data.matrix(lasso.train,rownames.force = NA)
lasso.test.matrix<-data.matrix(lasso.test,rownames.force = NA)

lasso.reg<-cv.glmnet(x=lasso.train.matrix, y=train$MRI,alpha=1,nlambda=100)
plot(lasso.reg)
lasso.reg.final<-glmnet(x=lasso.train.matrix, y=train$MRI,alpha=1,lambda=lasso.reg$lambda.1se)# Best lamba 0.1

lasso.train.pred<-predict.glmnet(lasso.reg.final, lasso.train.matrix)
lasso.test.pred<- predict.glmnet(lasso.reg.final, lasso.test.matrix)

RMSE.lasso.train<-sqrt(sum((lasso.train.pred-train$MRI)^2)/nrow(train))
RMSE.lasso.test<- sqrt(sum((lasso.test.pred-test$MRI)^2)/nrow(test))

mean_vector<-rep(mean(train$MRI), nrow(train))
SSE<-(sum((lasso.train.pred-train$MRI)^2))
SST<-sum((train$MRI- mean_vector)^2)
R.square.lasso<- 1-SSE/SST

#Lasso doesnot give much improvement over linear

#--#----#_--------#-----------#_---------#-------------#_--------
#General Additive Model
#Using MGCV package to fit GAM
install.packages("mgcv")
library("mgcv")

gam_model<-gam(MRI ~ s(SLOPE_VARIANCE)+ Speed.Limit +s(Thickness)+s(DAY_PRECIPITATION)+s(MEAN_DAY_TEMP)+
                 s(MAX_DAY_TEMP)+s(MIN_DAY_TEMP)+s(MAX_DAY_WIND_SPD)+s(AADTT)+s(Mean_Day_Humidity)+s(KESAL_YEAR), data=train, select=TRUE)
plot(gam_model,pages=8,residuals=FALSE,all.terms=TRUE,shade=TRUE,shade.col=2)
summary(gam_model)
stats_gam<-stat(gam_model, "MRI", train, test)


#---------#----------#-----------#_---------#-------------#_--------
#---------------6 CART (rpart) ------------------

install.packages("rpart")
library(rpart)
cart<-rpart(MRI~.,train, method="anova")
summary(cart)
plot(cart)
text(cart, cex=.75)
# No of optimal cross_validation  
cp_table<-printcp(cart)
plotcp(cart) 
cp_table<-data.frame(cp_table)
index_CP<-min(cp_table$xerror) == cp_table$xerror
best_CP<-cp_table$CP[index_CP]
# Pruning the tree
pruned_cart<-prune(cart,cp =  best_CP)
plot(pruned_cart)
text(pruned_cart, cex=.75)
stats_cart<-stat(pruned_cart, "MRI", train, test)

#---------#----------#-----------#_---------#-------------#_--------

# Bagged CART
install.packages("ipred")
library(ipred)
bagging<-bagging(MRI~., data=data_3, na.action=na.rpart)
print(bagging)
stats_bagging<-stat(bagging, "MRI", train, test)
#bagging_tuned<-bagging(MRI~., data=data_3, na.action=na.rpart,ntree=100)
#print(bagging_tuned)
#stats_bagging_tunned<-stat(bagging_tuned, "MRI", train, test)

#---------#----------#-----------#_---------#-------------#_--------
install.packages("randomForest")
library("randomForest")
rf_1<-randomForest(MRI~., data=train,OOB=T)
stats_rf_1<-stat(rf_1,"MRI",train,test)
plot(rf_1)
rf_2<- randomForest(MRI~., data=train, ntree=1000, mtry=sqrt(10))
stats_rf_2<-stat(rf_2,"MRI",train,test)
rf_3<-randomForest(MRI~., data=train,ntree=1000, mtry=sqrt(10), importance=TRUE)
stats_rf_3<-stat(rf_3,"MRI",train,test)

cv_rf<-rfcv(train[,-1],train[,1],cv.folds=10)

random_forest<-rf_3
varImpPlot(random_forest,sort = TRUE,n.var = 10, nrow(rf_final$importance),type=1)


stat(rf_3,"MRI", train,test)

# Actual vs.Fit # training set
predicted_value<-predict(rf_3, train)
plot(predicted_value~train$MRI, col=18, pch=18, lwd=0.25)
abline(0,1)
title("Random Forest Actual vs. Fit")

# Actual vs.Predicted # test set
predicted_value<-predict(rf_3, test)
plot(predicted_value~test$MRI, col=18, pch=18, lwd=0.25)
abline(0,1)
title("Random Forest Actual vs. Predicted")

par(mfrow=c(2,5))
partialPlot(rf_3,pred.data=train,x.var=SLOPE_VARIANCE, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=KESAL_YEAR, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=AADTT, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=MAX_DAY_TEMP, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=MAX_DAY_WIND_SPD, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=Thickness, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=Speed.Limit, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=Mean_Day_Humidity, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=MEAN_DAY_TEMP, ylab="MRI")
partialPlot(rf_3,pred.data=train,x.var=MIN_DAY_TEMP, ylab="MRI")

#---------#----------#-----------#_---------#-------------#_--------

install.packages("earth")
library(earth)

mars_1<-earth(MRI~., data=train, nk=10, degree=2,pmethod="cv", nfold=10, penalty=-1)
stats_mars_1<-stat(mars_1,"MRI",train,test)

mars_2<-earth(MRI~., data=train, nk=10, degree=1,pmethod="cv", nfold=10, penalty=1)
stats_mars_2<-stat(mars_2,"MRI",train,test)

mars_3<-earth(MRI~., data=train, nk=30, degree=1,pmethod="cv", nfold=10, penalty=-1 )
stats_mars_3<-stat(mars_3,"MRI",train,test)

mars_4<-earth(MRI~., data=train, nk=10, degree=1, penalty=-1 )
stats_mars_4<-stat(mars_4,"MRI",train,test)

mars_5<-earth(MRI~., data=train, nk=10, degree=1, penalty=-1 )
stats_mars_5<-stat(mars_5,"MRI",train,test)

# Mars_3 performs the best

# Actual vs.Fit # training set
predicted_value<-predict(mars_3, train)
plot(predicted_value~train$MRI, col=18, pch=18, lwd=0.25)
abline(0,1)
title("MARS Actual vs. Fit")

# Actual vs.Predicted # test set
predicted_value<-predict(mars_3, test)
plot(predicted_value~test$MRI, col=18, pch=18, lwd=0.25)
abline(0,1)
title("MARS Actual vs. Predicted")

evimp(mars_3)

#---------#----------#-----------#_---------#-------------#_--------
install.packages("bartMachine")
library("bartMachine")

# BART
bart_1<-bartMachine(train[,-1],train[,1], num_trees = 100,num_burn_in=10, q=0.75, k=1,serialize = TRUE)
predicted_value<-predict(bart_1,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_1,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_1<-c(RMSE_train, RMSE_test, R_square)

bart_2<-bartMachine(train[,-1],train[,1], num_trees = 100,num_burn_in=10, q=0.75, k=2,serialize = TRUE)
predicted_value<-predict(bart_2,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_2,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_2<-c(RMSE_train, RMSE_test, R_square)


bart_3<-bartMachine(train[,-1],train[,1], num_trees = 100,num_burn_in=10, q=0.99, k=1,serialize = TRUE)
predicted_value<-predict(bart_3,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_3,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_3<-c(RMSE_train, RMSE_test, R_square)


bart_4<-bartMachine(train[,-1],train[,1], num_trees = 100,num_burn_in=10, q=0.99, k=2,serialize = TRUE)
predicted_value<-predict(bart_4,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_4,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_4<-c(RMSE_train, RMSE_test, R_square)


bart_5<-bartMachine(train[,-1],train[,1], num_trees = 100,num_burn_in=100, q=0.75, k=1,serialize = TRUE)
predicted_value<-predict(bart_5,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_5,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_5<-c(RMSE_train, RMSE_test, R_square)


bart_6<-bartMachine(train[,-1],train[,1], num_trees = 20,num_burn_in=100, q=0.75, k=2,serialize = TRUE)
predicted_value<-predict(bart_6,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_6,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_6<-c(RMSE_train, RMSE_test, R_square)


bart_7<-bartMachine(train[,-1],train[,1], num_trees = 20,num_burn_in=100, q=0.99, k=1,serialize = TRUE)
predicted_value<-predict(bart_7,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_7,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_7<-c(RMSE_train, RMSE_test, R_square)


bart_8<-bartMachine(train[,-1],train[,1], num_trees = 20,num_burn_in=100, q=0.99, k=2,serialize = TRUE)
predicted_value<-predict(bart_8,train[,-1])
RMSE_train<-sqrt(sum((predicted_value-train[,1])^2)/nrow(train))
SST<-sum((train[,1]-mean(train[,1]))^2)
SSE<-sum((predicted_value-train[,1])^2)
R_square<-1-SSE/SST
predicted_value<-predict(bart_8,test[,-1])
RMSE_test<-sqrt(sum((predicted_value-test[,1])^2)/nrow(test))
stats_bart_8<-c(RMSE_train, RMSE_test, R_square)

best_model<-bartMachineCV(X=train[,-1],y=train[,1],Xy=NULL, num_tree_cvs =100,k_cvs=c(1,2),k_folds=10, nu_q_cvs=list(c(10,0.75),c(10,0.99),c(100,0.75),c(100,0.99)))

# The best bart model is bart_3

#---------#----------#-----------#----------#-------------#_--------
# PCA 
# Based on lab done
install.packages("devtools")
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
data_pca<-data_3
data_pca$MAX_DAY_WIND_SPD<-as.numeric(data_pca$MAX_DAY_WIND_SPD)
data_pca<-data_pca[,-which(names(data_pca)=="MRI")]
data_pca<-data_pca[,-which(names(data_pca)=="Speed.Limit")]
data.pca <- prcomp(data_pca, scale. = TRUE)
speed_limit<-as.factor(data_3$Speed.Limit)
ggbiplot(data.pca, obs.scale = 3, var.scale = 1, varname.size = 4, varname.adjust = 1, varname.abbrev = FALSE,
         groups = speed_limit, ellipse = TRUE, circle = FALSE) + scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
dpca<-data_pca
names(dpca)<-c("Slope", "Thick","Preci", "Mean Temp", "Max Temp", "Min Temp", "Wind", "AADTT", "Humidity", "Kesal")
dapca <- prcomp(dpca, scale. = TRUE)
ggbiplot(dapca, obs.scale = 3, var.scale = 1, varname.size = 4, varname.adjust = 2, 
         alpha = 0.8, varname.abbrev = FALSE,
         groups = speed_limit, ellipse = TRUE, circle = FALSE) + scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')




# Remove Factor variable
train_pca<-train[-4]
pca<-prcomp(train_pca, center=TRUE, scale.=TRUE)
print(pca)
plot(pca, type='l')
sumary(pca)
# Biplot
install.packages("devtools")
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
speed<-train$Speed.Limit
tiff("pca plot.tiff",width = 10,height = 10, units = "in", res = 500 )
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              groups = speed, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
dev.off()
#---------#----------#-----------#_---------#-------------#_--------
# Bagged MARS
install.packages("caret")
library(caret)
library(earth)
bagged_earth<-bagEarth(MRI~., data=trainingset, B=10)


# Cross Validation 
# Linear regression


# Mapping
# Using ggmap
install.packages("ggplot2")
library(ggplot2)


a<-sapply(MRI, function(x)floor(x))
whiMRI[991]
range(MRI)
index.max(MRI)

#---------#----------#-----------#_---------#-------------#_--------
# SVM
library(e1071)
tuneResult <- tune(svm, MRI ~ .,  data = train,ranges = list(epsilon = seq(0,1,0.1), cost = seq(0.1,1,10)))
#Bset Epsilon 0.1, cost 0.1 
svmfit <- svm(MRI~., data=train, epsilon=0.1, cost=0.1)
predictedY <- predict(svmfit, train)
error<-train$MRI-predictedY
RMSE.SVM<-sqrt(mean(error^2))

predictedY <- predict(svmfit, test)
error<-test$MRI-predictedY
RMSE.SVM.test<-sqrt(mean(error^2))


SSE<-(svmfit$residuals)^2
mean_vec<- rep(mean(train$MRI), nrow(train))
mean_vec1<-as.vector(rep(0,nrow(train)))
for (i in 1:nrow(train)){
  mean_vec1[i]<-(train$MRI[i]-mean(train$MRI))^2
}
SST<- sum((train$MRI - mean_vec)^2)
SST1<-sum(mean_vec1)
Rsquare.SVM<- 1- sum(SSE)/SST1

sum(SSE)

