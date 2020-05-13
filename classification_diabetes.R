library(rpart)
library(ggplot2)
library(GGally)
library(corrplot)
library(dplyr)
library(magrittr)
library(randomForest)
library(rpart.plot)

diabetes <- read.csv("diabetes.csv")

diabetes$Outcome <- factor(diabetes$Outcome, levels = c(0,1), labels = c("Healthy", "Patient"))
names(diabetes)[7]<-"Pedigree"

pairs(diabetes[,1:8], panel = panel.smooth)
corrplot(cor(diabetes[, -9]), type = "lower", method = "number")

# Variable transformation
diabetes <- mutate(diabetes, body_type = ifelse(BMI<18.5,"Underweight",
                                                ifelse(BMI>=18.5 & BMI < 25,"Normal",
                                                       ifelse(BMI>=25&BMI<30,"Overweight",
                                                              ifelse(BMI>=30,"Obese","N/A")))))

diabetes <- mutate(diabetes, glucose_test_pred = ifelse(Glucose<140,"Normal",
                                                        ifelse(Glucose>=140 & Age < 200,"prediabetes",
                                                               ifelse(Age>=200,"diabetes","N/A"))))

diabetes <- mutate(diabetes, age_group = ifelse(Age<=30,"0-30",
                                                ifelse(Age>=31 & Age < 45,"31-45",
                                                       ifelse(Age>=45,"45-81","N/A"))))

diabetes_clean <-diabetes[which(diabetes$BMI>0),]
diabetes_clean <-diabetes_clean[which(diabetes_clean$BloodPressure>0),]
diabetes_clean <- diabetes_clean[which(diabetes_clean$Glucose>0),]
diabetes_clean <- diabetes_clean[which(diabetes_clean$Insulin>0),]
summary(diabetes_clean)

# Visualization

g <- ggplot(diabetes_clean, aes(Pedigree))
g + geom_density(aes(fill=factor(Outcome)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Pedigree Diabetes function for healthy and diabetic patients",
       x="Pedigree diabetes function",
       fill="Outcome")

g <- ggplot(diabetes_clean, aes(Outcome, BMI))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="BMI for healthy and diabetic patients",
       x="Outcome",
       y="BMI")

ggplot(diabetes_clean, aes(x=BloodPressure, y=Glucose)) + 
  geom_point(aes(col=Outcome, size=age_group)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Skin Thickness Vs Glucose level", 
       y="Glucose level", 
       x="Skin Thickness", 
       title="Scatterplot")

g <- ggplot(diabetes_clean, aes(Outcome, BloodPressure))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="Blood Pressure for healthy and diabetic patients",
       x="Outcome",
       y="Blood Pressure")

g <- ggplot(diabetes_clean, aes(age_group, Glucose))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="Glucose levels for different age groups",
       x="Age group",
       y="Glucose level")

diabetes_t <- diabetes_clean[-c(2,6,8)]
set.seed(5678)

# train-validate split
train <- sample(nrow(diabetes_t), 0.8*nrow(diabetes_t))

diabetes_train <- diabetes_t[train,]
diabetes_validate <- diabetes_t[-train,]

table(diabetes_train$Outcome)
table(diabetes_validate$Outcome)

# decision tree model
set.seed(5678)
d_tree <- rpart(Outcome ~ ., data=diabetes_train, method="class",
               parms=list(split="information"))
summary(d_tree)

rpart.plot(d_tree)

d_tree_pred <- predict(d_tree, diabetes_validate, type="class")
d_tree_table <- table(diabetes_validate$Outcome, d_tree_pred, dnn=c("Actual", "Predicted"))
d_tree_table
mean(d_tree_pred==diabetes_validate$Outcome) 

# The tree diagram above is quite complictaed. 
# To make it easier to understand, we need to prune the tree. 
# Let's check the optimum cp and corresponding depth to prune the tree.
d_tree$cptable
plotcp(d_tree)

opt <- which.min(d_tree$cptable[,"xerror"])
cp <- d_tree$cptable[opt, "CP"]

d_tree.pruned <- prune(d_tree, cp)

# Display pruned decision tree.  The true values follow the left branches.
rpart.plot(d_tree.pruned)

table(diabetes_train$Outcome)/nrow(diabetes_train)

rpart.plot(d_tree.pruned, type = 2, box.palette = c("green", "red"), fallen.leaves = TRUE)

# predict evaluates the application of a model to a data frame
d_tree.pred <- predict(d_tree.pruned, diabetes_validate, type="class")
d_tree.perf <- table(diabetes_validate$Outcome, d_tree.pred, dnn=c("Actual", "Predicted"))
d_tree.perf
mean(d_tree.pred==diabetes_validate$Outcome) # accuracy 78.481
# Model is underfitted, We will not use this one

# random forest model

diabetes_rf <- diabetes_clean[-c(10,11,12)]
set.seed(5678)
train <- sample(nrow(diabetes_rf), 0.8*nrow(diabetes_rf))

diabetes_train <- diabetes_rf[train,]
diabetes_validate <- diabetes_rf[-train,]

d_tree_rf <- randomForest(Outcome ~ ., data=diabetes_train)
predicted_tree <- predict(d_tree_rf, diabetes_validate)

d_tree_rf_perf <- table(diabetes_validate$Outcome, predicted_tree, dnn=c("Actual", "Predicted"))
d_tree_rf_perf
mean(predicted_tree==diabetes_validate$Outcome)
# mean is lesser than from decision tree classification: 82.278

importance(d_tree_rf, type=2)
