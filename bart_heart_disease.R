install.packages('dbarts')
install.packages('readr')
install.packages('corrplot')
library(dbarts)
library(readr)
library(corrplot)
library(ggplot2)
library(dplyr)
library(reshape2)
rm(list=ls())

# age: patient's age in years 
# sex: patient's gender (0: Female, 1: Male) (binary variable)
# cp: type of chest pain experienced by the patient (categorical variable)
# trestbps: patient's resting blood pressure in mm Hg (continuous variable)
# chol: patient's cholesterol level in mg/dl (continuous variable)
# fbs: fasting blood sugar level of the patient (> 120 mg/dl = 1, else = 0) (binary variable)
# restecg: resting electrocardiography results of the patient (categorical variable)
# thalach: maximum heart rate achieved by the patient (continuous variable)
# exang: exercise-induced angina (0: No, 1: Yes) (binary variable)
# oldpeak: exercise-induced ST depression relative to rest (continuous variable)
# slope: slope of the ST segment during exercise (categorical variable)
# ca: number of major blood vessels colored by fluoroscopy (discrete variable)
# thal: thallium stress test (thalassemia) (categorical variable)
# target: presence of heart disease (0: No disease, 1: Heart disease) (binary variable)

# EDA
myData <- read.csv("heart.csv")
head(myData)
dim(myData) # 1024 14
summary(myData)
str(myData)

## missing value analysis
sum(is.na(myData))

## categorical features - unique value analysis
unique(myData$sex)
unique(myData$cp)
unique(myData$fbs)
unique(myData$restecg)
unique(myData$exang)
unique(myData$slope)
unique(myData$ca)
unique(myData$thal)
unique(myData$target)

## age, trestbps, chol, thalach, oldpeak
numeric_list = c('age', 'trestbps','chol','thalach','oldpeak', 'target')
categorical_list = c("sex","cp","fbs","restecg","exang","slope","ca","thal","target")

## correlation analysis
c <- round(cor(myData),2)
corrplot(c, method = "number")


boxplot(myData[, c('age', 'chol', 'thalach', 'trestbps')], 
				main="Boxplot of Age, Cholesterol, Max Heart Rate, and Resting Blood Pressure",
				xlab="Variables", ylab="Values", 
				col=c("red","blue","green","purple"))

## data preparation
for (col in numeric_list) {
	q1 <- quantile(myData[[col]], 0.25)
	q3 <- quantile(myData[[col]], 0.75)
	iqr <- q3 - q1
	lower_bound <- q1 - 1.5 * iqr
	upper_bound <- q3 + 1.5 * iqr
	myData[[col]] <- ifelse(myData[[col]] < lower_bound | myData[[col]] > upper_bound, mean(myData[[col]], na.rm = TRUE), myData[[col]])
}


## Categorical Feature Analysis
df_categoric <- myData[, categorical_list]

for (i in categorical_list) {
	p <- ggplot(data = df_categoric, aes(x = .data[[i]], fill = as.factor(target))) +
		geom_bar(position = "dodge") +
		labs(title = i) +
		theme_minimal()
	
	print(p)  # Printing or plotting the plot object
}
par(mfrow = c(1, 1))

## Numeric Feature Analysis
# 이따 해보기

## Standardization
myData[numeric_list[-length(numeric_list)]] <- myData[numeric_list[-length(numeric_list)]] %>% mutate_all(~(scale(.) %>% as.vector))


# selecting test dataset (30%)
testsize=as.integer(nrow(myData)*0.3)
test_index=sort(sample(1:nrow(myData),testsize,replace=F)) #random sampling for test data set
myData_test = myData[test_index, ]
myData_train = myData[-test_index, ]


# Modeling with BART
fits1 <- bart2(target ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = myData_train, keepTrees = TRUE)
fitted_test1 <- predict(fits1, newdata = myData_test, type = 'ppd', n.trees = 75)
dim(fitted_test1)
y_pred <- apply(fitted_test1, 2, mean)
y_pred

threshold <- 0.5 

y_pred_binary <- ifelse(y_pred >= threshold, 1, 0)
y_pred_binary
myData_test$target

sum(y_pred_binary-myData_test$target) # only 2 incorrect among test dataset.
