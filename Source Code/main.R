# Import Libraries
library(class) #used for KNN
library(nnet) #used for multinomial logistic regression
library(infotheo) # for calculating entropy
#install.packages("devtools")
library(ggplot2)
# install.packages("randomForest")
library(randomForest) #for random forest

# Set Working Directory
setwd("C:/Users/telmo/Desktop/BeanProject")

# Import Scripts
source('train_test_split.r')
source('normalize.r')
source('accuracy.r')
source('macro_recall.r')
source('macro_precision.r')
source('macro_f1.r')
source('print_results.r')
source('MI.r')
source('CCMI.r')
source('MICC.r')

# Load and View Dataset
data <- read.csv("Dry_Bean_Dataset.csv",header=T,stringsAsFactors = T)
data$Class <- as.factor(data$Class)
View(data)
cat("# of datapoints:",dim(data)[1])
cat("# of parameters:",dim(data)[2]-1)
cat("Parameter names:\n",names(data[,1:16]))

# Preliminary Data Analysis
sum(is.na(data)) # any missing values?
summary(data)
round(cor(data[,1:16]),3)

# Splitting data in Training (70%) and Test (30%)
train<-train_test_split(data,0.7)[[1]]
test<-train_test_split(data,0.7)[[2]]

#outlier analysis
boxplot(data$Area,data$ConvexArea, ylab= "Pixel count", xlab="Area     Convex Area")
boxplot(data[2:4], ylab= "Pixel count")
boxplot(data[9:12], ylab= "Pixel count")

#####################################################################################
#####################################################################################
# FEATURE SELECTION
#####################################################################################
#####################################################################################




# Convert Class to Numerical to compute entropy() using infotheo package
# 'BARBUNYA' = 1
# 'BOMBAY' = 2
# 'CALI' = 3
# 'DERMASON' = 4
# 'HOROZ' = 5
# 'SEKER' = 6
# 'SIRA' = 7

data_new<-data
data_new$Class <- as.numeric(data_new$Class)
for (i in 1:length(data_new$Class)) {
  if (data_new$Class[i]=='BARBUNYA') {
    data$Class[i]<-1
  }else if (data_new$Class[i]=='BOMBAY'){
    data_new$Class[i]<-2
  }else if (data_new$Class[i]=='CALI'){
    data_new$Class[i]<-3
  }else if (data_new$Class[i]=='DERMASON'){
    data_new$Class[i]<-4
  }else if (data_new$Class[i]=='HOROZ'){
    data_new$Class[i]<-5
  }else if (data_new$Class[i]=='SEKER'){
    data_new$Class[i]<-6
  }else if (data_new$Class[i]=='SIRA'){
    data_new$Class[i]<-7
  }
}

# Standardized data to avoid the deltas problem
data_stand <- scale(data[,1:16])
data_stand <- cbind(data_stand,data_new[,17])
colnames(data_stand)[17] <- "Class"
data_stand <- as.data.frame(data_stand)

a<-CCMI(data_stand,16)
cat("Features in decreasing order of relevance (using CCMI):\n",a,"\n")

a<-MICC(data_stand,16)
cat("Features in decreasing order of relevance (using MICC):\n",a,"\n")





#####################################################################################
#####################################################################################
# K-NEAREST NEIGHBOURS
#####################################################################################
#####################################################################################





train_std<-train_test_split(data_stand,0.7)[[1]]
test_std<-train_test_split(data_stand,0.7)[[2]]

#############################################
# ALL FEATURES - NOT NORMALIZED VS NORMALIZED
#############################################

# knn original data
knn_pred <- knn(train[,1:16],test[,1:16],train[,17],k=3)
cat("KNN Results (original):")
print_results(test[,17],knn_pred)

# knn standardized data
knn_pred <- knn(train_std[,1:16],test_std[,1:16],train[,17],k=3)
cat("KNN Results (standardized):")
print_results(test[,17],knn_pred)

# knn normalized data
data_norm<-normalize(data)
data_norm['Class']<-data[,17]

norm_train<-train_test_split(data_norm,0.7)[[1]]
norm_test<-train_test_split(data_norm,0.7)[[2]]

knn_pred <- knn(norm_train[,1:16],norm_test[,1:16],norm_train[,17],k=20)
cat("KNN Results (normalized):")
print_results(test[,17],knn_pred)


###################################
#  kNN using CCMI feature selection
###################################

# using 1/8 of the features (2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
features <- c(2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
num_features <- 2

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,9],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)

cat("KNN Results (2 predictors using CCMI):")
print_results(test_std[,NCOL(test_std)],knn_pred)


# using 1/3 of the features (2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
features <- c(2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
num_features <- 5

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,9],data_stand[,14],data_stand[,10],data_stand[,13],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)

cat("KNN Results (5 predictors using CCMI):")
print_results(test_std[,NCOL(test_std)],knn_pred)

#################

# using 1/2 of the features (2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
features <- c(2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
num_features <- 8

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,9],data_stand[,14],data_stand[,10],data_stand[,13],data_stand[,12],data_stand[,4],data_stand[,5],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)

cat("KNN Results (8 predictors using CCMI):")
print_results(test_std[,NCOL(test_std)],knn_pred)


###################################
#  kNN using MICC feature selection
###################################

# using 1/8 of the features (2 6 13 11 14 16 8 3 4 9 15 10 1 5 7 12)
features <- c(2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
num_features <- 2

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,6],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)
cat("KNN Results (5 predictors using MICC):")
print_results(test_std[,NCOL(test_std)],knn_pred)

#################

# using 1/3 of the features (2 6 13 11 14 16 8 3 4 9 15 10 1 5 7 12)
features <- c(2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
num_features <- 5

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,6],data_stand[,13],data_stand[,11],data_stand[,14],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)
cat("KNN Results (5 predictors using MICC):")
print_results(test_std[,NCOL(test_std)],knn_pred)

#################

# using 1/2 of the features (2 6 13 11 14 16 8 3 4 9 15 10 1 5 7 12)
features <- c(2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
num_features <- 8

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,6],data_stand[,13],data_stand[,11],data_stand[,14],data_stand[,16],data_stand[,8],data_stand[,3],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# knn normalized data
knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)
cat("KNN Results (8 predictors using MICC):")
print_results(test[,17],knn_pred)




#####################################################################################
#####################################################################################
# MULTINOMIAL LOGISTIC REGRESSION (MLR)
#####################################################################################
#####################################################################################



###################################
#  MLR W/ ALL FEATURES
###################################

multinom_model <- multinom(Class ~ ., data = train)

# train set
mlr_predictions_train <- predict(multinom_model, newdata=train)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (All features) (Training Set):")
print_results(train$Class,mlr_predictions_train)

# test set
mlr_predictions_test <- predict(multinom_model, newdata=test)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (All features) (Test Set):")
print_results(test$Class,mlr_predictions_test)


###################################
#  MLR using CCMI feature selection
###################################


# using 1/8 of the features (2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
features <- c(2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
num_features <- 2

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,9],data_stand[,17])
colnames(df) <- c('2','9','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (CCMI-2) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (CCMI-2) 92.02(Test Set):")
print_results(test_std$Class,mlr_predictions_test)

#################

# using 1/3 of the features (2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
features <- c(2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
num_features <- 5

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,9],data_stand[,14],data_stand[,10],data_stand[,13],data_stand[,17])
colnames(df) <- c('2','9','14','10','13','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (CCMI-5) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (CCMI-5) (Test Set):")
print_results(test_std$Class,mlr_predictions_test)

#################

# using 1/2 of the features (2 8 1 7 9 3 13 4 5 12 15 6 14 11 16 10)
features <- c(2,9,14,10,13,12,4,5,8,15,1,6,7,16,3,11)
num_features <- 8

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,9],data_stand[,14],data_stand[,10],data_stand[,13],data_stand[,12],data_stand[,4],data_stand[,5],data_stand[,17])
colnames(df) <- c('2','9','14','10','13','12','4','5','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (CCMI-8) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (CCMI-8) (Test Set):")
print_results(test_std$Class,mlr_predictions_test)


###################################
#  MLR using MICC feature selection
###################################


# using 1/8 of the features (2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
features <- c(2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
num_features <- 2

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,6],data_stand[,17])
colnames(df) <- c('2','6','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (MICC-2) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (MICC-2) (Test Set):")
print_results(test_std$Class,mlr_predictions_test)

#################

# using 1/3 of the features (2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
features <- c(2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
num_features <- 5

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,6],data_stand[,13],data_stand[,11],data_stand[,14],data_stand[,17])
colnames(df) <- c('2','6','13','11','14','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (MICC-5) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (MICC-5) (Test Set):")
print_results(test_std$Class,mlr_predictions_test)

#################

# using 1/2 of the features (2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
features <- c(2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)
num_features <- 8

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,6],data_stand[,13],data_stand[,11],data_stand[,14],data_stand[,16],data_stand[,8],data_stand[,3],data_stand[,17])
colnames(df) <- c('2','6','13','11','14','16','8','3','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (MICC-8) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (MICC-8) (Test Set):")
print_results(test_std$Class,mlr_predictions_test)




#####################################################################################
#####################################################################################
# PRINCIPAL COMPONENT ANALYSIS
#####################################################################################
#####################################################################################




# scale = true -> standardized data
data.cpca <- prcomp(data[,1:16],scale.=TRUE, retx=TRUE)
summary(data.cpca)
a<-round(data.cpca$sdev^2,3)
cat("Eigenvalues:",a)
cat("Sum of eigenvalues:",sum(a))

# eigenvectors 
eigenvectors<-data.cpca$rotation 
round(eigenvectors[,1:3],3)

screeplot(data.cpca, npcs=16,main="Variance (Principal Component)", type="lines",cex=0.8)
abline(h=1,col="green")

# eigenvectors of all principal components
eigenvectors<-data.cpca$r
print(round(eigenvectors , digits = 3))

# loadings correlation with original features
round(cor(scale(data[,1:16]),data.cpca$x),3)
       
# Projections of our data on the first two principal components
dtp <- data.frame('Class' = data$Class, data.cpca$x[,1:2]) # the first 2 components are selected
ggplot(data = dtp) + 
  geom_point(aes(x = PC1, y = PC2, col = Class)) + 
  theme_minimal() 

# Creating the (first three) principal components
# First we extract the eigenvectors from the loading dataframe
v1 <- rep(NA,16)
v2 <- rep(NA,16)
v3 <- rep(NA,16)

for (i in 1:16) {
  v1[i] <- eigenvectors[i,1]
  v2[i] <- eigenvectors[i,2]
  v3[i] <- eigenvectors[i,3]
}

# Calculate the data projections by taking the linear combinations of the standardized columns (there's certainly a better way to code this)
PC1 <- v1[1]*data_stand[,1]+v1[2]*data_stand[,2]+v1[3]*data_stand[,3]+v1[4]*data_stand[4]+v1[5]*data_stand[5]+v1[6]*data_stand[,6]+v1[7]*data_stand[,7]+v1[8]*data_stand[8]+v1[9]*data_stand[,9]+v1[10]*data_stand[,10]+v1[11]*data_stand[,11]+v1[12]*data_stand[,12]+v1[13]*data_stand[,13]+v1[14]*data_stand[,14]+v1[15]*data_stand[,15]+v1[16]*data_stand[,16]
PC2 <- v2[1]*data_stand[,1]+v2[2]*data_stand[,2]+v2[3]*data_stand[,3]+v2[4]*data_stand[4]+v2[5]*data_stand[5]+v2[6]*data_stand[,6]+v2[7]*data_stand[,7]+v2[8]*data_stand[8]+v2[9]*data_stand[,9]+v2[10]*data_stand[,10]+v2[11]*data_stand[,11]+v2[12]*data_stand[,12]+v2[13]*data_stand[,13]+v2[14]*data_stand[,14]+v2[15]*data_stand[,15]+v2[16]*data_stand[,16]
PC3 <- v3[1]*data_stand[,1]+v3[2]*data_stand[,2]+v3[3]*data_stand[,3]+v3[4]*data_stand[4]+v3[5]*data_stand[5]+v3[6]*data_stand[,6]+v3[7]*data_stand[,7]+v3[8]*data_stand[8]+v3[9]*data_stand[,9]+v3[10]*data_stand[,10]+v3[11]*data_stand[,11]+v3[12]*data_stand[,12]+v3[13]*data_stand[,13]+v3[14]*data_stand[,14]+v3[15]*data_stand[,15]+v3[16]*data_stand[,16]
# Creating a dataframe 
data_pca <- data.frame (PC1,PC2,PC3,data$Class)
colnames(data_pca) <- c('PC1','PC2','PC3','Class')
View(data_pca)

#################
#  kNN using PCA
#################

train_pca<-train_test_split(data_pca,0.7)[[1]]
test_pca<-train_test_split(data_pca,0.7)[[2]]

knn_pred <- knn(train_pca[,1:NCOL(train_pca)-1],test_pca[,1:NCOL(test_pca)-1],train_pca[,NCOL(train_pca)],k=20)
cat("KNN Results (from PCA):")
print_results(test_pca[,NCOL(test_pca)],knn_pred)

#################
#  MLR using PCA
#################

# train set
multinom_model <- multinom(Class ~ ., data = train_pca)
mlr_predictions_train <- predict(multinom_model, newdata=train_pca)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (from PCA) (Training Set):")
print_results(train_pca$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_pca)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (from PCA) (Test Set):")
print_results(test_pca$Class,mlr_predictions_test)





#####################################################################################
#####################################################################################
# RANDOM FOREST (RF)
#####################################################################################
#####################################################################################





# reconstructing the standardized dataframe
data_stand <- scale(data[,1:16])
data_stand <- data.frame (data_stand,data$Class)
colnames(data_stand)[17] <- "Class"

# train/test split
train_std<-train_test_split(data_stand,0.7)[[1]]
test_std<-train_test_split(data_stand,0.7)[[2]]

####################
# RF W/ ALL FEATURES
####################

a1<-randomforest_model <-  randomForest(formula = Class ~ ., data = data_stand, importance = TRUE)
a2<-randomforest_model <-  randomForest(formula = Class ~ ., data = data_stand, importance = TRUE)
a3<-randomforest_model <-  randomForest(formula = Class ~ ., data = data_stand, importance = TRUE)
a4<-randomforest_model <-  randomForest(formula = Class ~ ., data = data_stand, importance = TRUE)
a5<-randomforest_model <-  randomForest(formula = Class ~ ., data = data_stand, importance = TRUE)
x1<-a1$importance[129:144]
x2<-a2$importance[129:144]
x3<-a3$importance[129:144]
x4<-a4$importance[129:144]
x5<-a5$importance[129:144]
y<- (x1+x2+x3+x4+x5)/5
cat("MeandecreaseGini average over 5 runs of random forest:",round(y,3))
# by inspection
cat("Selected features using random forest: (2,15,12,13,4,3,7,6,5,8,1,11,14,16,10,9)")

round(randomforest_model$importance,3)

# classifcation with random forest (which is not required)
#predTrain <- predict(randomforest_model, train_std, type = "Class")
#predTest <- predict(randomforest_model, test_std, type = "Class")
#
#cat("Random Forest (w/ all features) (Train Set):")
#print_results(train_std$Class,predTrain)
#
#cat("Random Forest (w/ all features) (Test Set):")
#print_results(test_std$Class,predTest)

#################################
#  kNN using RF feature selection
#################################

# using 1/8 of the features
features <- c(2,15,12,13,4,3,7,6,5,8,1,11,14,16,10,9)
num_features <- 2

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,15],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)

cat("KNN Results (2 predictors using RF):")
print_results(test_std[,NCOL(test_std)],knn_pred)


# using 1/3 of the features
features <- c(2,15,12,13,4,3,7,6,5,8,1,11,14,16,10,9)
num_features <- 5

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,15],data_stand[,12],data_stand[,13],data_stand[,4],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)

cat("KNN Results (5 predictors using RF):")
print_results(test_std[,NCOL(test_std)],knn_pred)

#################

# using 1/2 of the features
features <- c(2,15,12,13,4,3,7,6,5,8,1,11,14,16,10,9)
num_features <- 8

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,15],data_stand[,12],data_stand[,13],data_stand[,4],data_stand[,3],data_stand[,7],data_stand[,6],data_stand[,17])

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

knn_pred <- knn(train_std[,1:(NCOL(df)-1)],test_std[,1:(NCOL(df)-1)],train_std[,(num_features+1)],k=3)

cat("KNN Results (8 predictors using RF):")
print_results(test_std[,NCOL(test_std)],knn_pred)


#################################
#  MLR using RF feature selection
#################################


# using 1/8 of the features
features <- c(2,15,12,13,4,3,7,6,5,8,1,11,14,16,10,9)
num_features <- 2

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,15],data_stand[,17])
colnames(df) <- c('2','15','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (RF-2) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (RF-2) 92.02(Test Set):")
print_results(test_std$Class,mlr_predictions_test)

#################

# using 1/3 of the features
features <- c(2,15,12,13,4,3,7,6,5,8,1,11,14,16,10,9)
num_features <- 5

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,15],data_stand[,12],data_stand[,13],data_stand[,4],data_stand[,17])
colnames(df) <- c('2','15','12','13','4','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (RF-5) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (RF-5) (Test Set):")
print_results(test_std$Class,mlr_predictions_test)

#################

# using 1/2 of the features
features <- c(2,15,12,13,4,3,7,6,5,8,1,11,14,16,10,9)
num_features <- 8

feat <- features[1:num_features]

df<-data.frame(data_stand[,2],data_stand[,9],data_stand[,14],data_stand[,10],data_stand[,13],data_stand[,12],data_stand[,4],data_stand[,5],data_stand[,17])
colnames(df) <- c('2','15','12','13','4','3','7','6','Class')

train_std<-train_test_split(df,0.7)[[1]]
test_std<-train_test_split(df,0.7)[[2]]

# train set
multinom_model <- multinom(Class ~ ., data = train_std)
mlr_predictions_train <- predict(multinom_model, newdata=train_std)
#train$ClassPredictedMLR <- mlr_predictions_train
cat("Multinomial Logistic Regression Results (RF-8) (Training Set):")
print_results(train_std$Class,mlr_predictions_train)

#test set
mlr_predictions_test <- predict(multinom_model, newdata=test_std)
#test$ClassPredictedMLR <-  mlr_predictions_test
cat("Multinomial Logistic Regression Results (RF-8) (Test Set):")
print_results(test_std$Class,mlr_predictions_test)






#################################
#################################
#  EXTRA: PCA FROM MICC SELECTION
#################################
#################################

# MICC Features: (2,6,13,11,14,16,8,3,4,9,15,10,1,5,7,12)


# data_final <- data.frame (data[,2],data[,6],data[,13],data[,11],data[,14],data$Class)
# colnames(data_final) <- c('2','6','13','11','14','Class')
# View(data_final)
# 
# data_final.cpca <- prcomp(data_final[,1:5],scale.=TRUE, retx=TRUE)
# summary(data_final.cpca)
# a<-round(data_final.cpca$sdev^2,3)
# cat("Eigenvalues:",a)
# cat("Sum of eigenvalues:",sum(a))
# 
# # eigenvectors 
# eigenvectors<-data_final.cpca$rotation 
# round(eigenvectors[,1:3],3)
# 
# screeplot(data_final.cpca, npcs=5,main="Variance (Principal Component)", type="lines",cex=0.8)
# abline(h=1,col="green")
# 
# # eigenvectors of all principal components
# eigenvectors<-data_final.cpca$r
# print(round(eigenvectors , digits = 3))
# 
# # loadings correlation with original features
# round(cor(scale(data[,1:16]),data_final.cpca$x),3)
# 
# # Projections of our data on the first two principal components
# dtp <- data.frame('Class' = data_final$Class, data_final.cpca$x[,1:2]) # the first 2 components are selected
# ggplot(data = dtp) + 
#   geom_point(aes(x = PC1, y = PC2, col = Class)) + 
#   theme_minimal() 


###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
