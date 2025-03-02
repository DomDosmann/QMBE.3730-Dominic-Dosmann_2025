## Assignment 3/2/25 

data <- read.csv("loan_default_data_set.csv")  
install.packages("install.")

library(tidyverse)       
library(caret)           
library(ggplot2)         
library(corrplot)        
library(caTools)         
library(e1071) 
library(DMwR2)

str(data)

# Question 1 EDA

dim(data)  #2,000 rows and 21 columns

# Question 2 Missing Varaibles

missing_vals <- colSums(is.na(data))
missing_vals
      # Rep income and Percentage card over 50
df_clean <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
     
# Question 3 duplicates & Datatypes

duplicates <- data[duplicated(data), ]
nrow(duplicates)
 # 0 duplicates
sapply(df_clean, class)

# Question 4 handling duplicates and wrong data types

df_clean <- df_clean %>%
  distinct()

df_clean$rep_education <- as.factor(df_clean$rep_education)

# Question 5 plotting 2 variables
ggplot(df_clean, aes(x = credit_age, y = rep_income)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Credit Age vs Rep Income", x = "Credit Age", y = "Rep Income")
 # There is no coorelation between Rep Income and Credit Age
ggplot(df_clean, aes(x = tot_balance, y = avg_bal_cards)) +
  geom_point(color = "red", alpha = 0.7) +
  labs(title = "Total Balance vs Average Balance on Cards", x = "Total Balance", y = "Average Balance on Cards")
 # Positive Linear coorelation between Average Balance on Cards and Total Balance

# Question 6 distribution of education levels
education_distribution <- df_clean %>%
  group_by(rep_education) %>%
  summarise(count = n()) %>%
  arrange(count)

education_distribution
  # Graduate has the smallest count therefore it is underrepresented compared to High school and College.

# Question 7 Balancing 

table(df_clean$Def_Ind)
 # Imbalanced but I dont know how to fix this issue

# Question 8 Plot distribution of rep income

ggplot(df_clean, aes(x = rep_income)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Rep Income", x = "Rep Income", y = "Frequency")
  #Little to no Skew on the distribution of re income. It is approximately normal.
skewness(df_clean$rep_income)

# Question 9 Group default status (“Def_Ind”) by education level (“rep_education”)

education_default <- df_clean %>%
  group_by(rep_education) %>%
  summarise(default_rate = mean(Def_ind == 1))
  
education_default
  # High school education is most likely to default on loans with a 0.116 or 11.6% chance

# Question 10 Anything Else stand out?
 The higher the education the less likely someone is to default on their loan. This could be false though because of the lack of data from the graduate category.

 # Quesion 1 
 model <- glm(Def_ind ~ ., family = binomial, data)
 
 # Make predictions on the testing set
 predictions <- predict(model, newdata = data, type = "response")
 predicted_classes <- ifelse(predictions > 0.5, 1, 0)
 
 # Evaluate the model
 confusion <- confusionMatrix(as.factor(predicted_classes), as.factor(data$Def_ind))
 
 # Extract accuracy, precision, and recall
 accuracy <- confusion$overall['Accuracy']
 precision <- confusion$byClass['Pos Pred Value']
 recall <- confusion$byClass['Sensitivity']
 
 accuracy # 0.9850
 precision # 0.9868
 recall # 0.9966
 # all of them are almost 100%
 
 # Question 2
 confusion
# has a kappa value of 0.9098 and a p-Value of 0.03887. very few false negative and false positives
 
 # Question 3
#CHAT GPT (Don't know how) 
 predictions_prob <- predict(model, newdata = data, type = "response")
 roc_curve <- roc(testing_set$Def_Ind, predictions_prob)
 plot(roc_curve, col = "blue", main = "ROC Curve")
 auc_value <- auc(roc_curve)
 print(paste("AUC:", auc_value))
 # The AUC model is 0.97 and close to one which means it is an effective model.

# Question 4
 importance <- summary(model)$coefficients
 importance <- as.data.frame(importance)
 importance <- importance[order(abs(importance$Estimate), decreasing = TRUE), ]
 importance
   # Avg balance of cards and number of accounts past due 6 months have the greatest importance 
 
 # Question 5
    # I prefer k nearest neighbors model because it is easier for me to interpret and I work more effecient with it because I have used it before.