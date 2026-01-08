#Case Study 3
#Section A, Team #11
#Ankit Chandekar, Jinghua He, Abigail Miller
#Muhammad Memon, Nikki Reddy, Katherine Zhang


########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
########################################################
source("DataAnalyticsFunctions.R")
# read data into R
election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)

##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
# write.csv(electionDataTrain, "electionDataTrain.csv")
# write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)
###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###

###
### Question 1: Provide a visualization (very flexible format, 
### it does not need to be related to the election)
library(ggplot2)
df <- read.csv("ElectionDataAlone.csv", check.names = FALSE, stringsAsFactors = FALSE)

# candidate vote shares 
df$ObamaShare <- df$Obama / df$TotalVote
df$ClintonShare <- df$Clinton / df$TotalVote

# find ethnicity % columns
find_col <- function(pattern) {
  m <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
  if (length(m)) m[1] else NA
}

cols <- list(
  White   = find_col("^\\s*white"),
  Black   = find_col("^\\s*black"),
  Hispanic= find_col("hispanic|latino"),
  Asian   = find_col("^\\s*asian"),
  Native  = find_col("native|american\\s*indian")
)

cols <- cols[!is.na(unlist(cols))]

# build long data
long <- do.call(rbind, lapply(names(cols), function(lbl) {
  pct <- df[[ cols[[lbl]] ]]
  # standardize: if given 0–100, convert to 0–1
  div <- if (max(pct, na.rm = TRUE) <= 1) 1 else 100
  pct <- pct / div
  
  rbind(
    data.frame(Ethnicity = lbl, Percent = pct, Candidate = "Obama",  Share = df$ObamaShare),
    data.frame(Ethnicity = lbl, Percent = pct, Candidate = "Clinton",Share = df$ClintonShare)
  )
}))
long <- long[is.finite(long$Percent) & is.finite(long$Share), ]


ggplot(long, aes(x = Percent, y = Share, color = Candidate)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c(Obama = "blue", Clinton = "red")) +
  facet_wrap(~ Ethnicity, scales = "free_x") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Candidate Vote Share vs. Ethnicity % (by county)",
    x = "Ethnicity share (0–1)",
    y = "Vote share"
  ) +
  theme_minimal()


### Question 2: Prediction. No additional script beyond the cleaning up the data
### provided above. (Hint: FIPS variable bahaves as an identifier for each observation
### so you might not want to include it in a linear regression.)
###

# Using a 16-fold cross validation to evaluate model performance
set.seed(25)

#Target variable
n <- nrow(election_data_train)
K <- 16

folds <- sample(rep(1:K, length.out = nrow(election_data_train)))

rmse_values <- numeric(K)

# NULL Model

# CV loop
for (k in 1:K) {
  train_idx <- which(folds != k)
  valid_idx <- which(folds == k)
  
  # Train and validation sets
  train_data <- election_data_train[train_idx, ]
  valid_data <- election_data_train[valid_idx, ]
  
  # Null model 
  model_null <- lm(Obama_margin_percent ~ 1, data = train_data)
  
  # Predict on validation set
  preds <- predict(model_null, newdata = valid_data)
  
  # RMSE
  rmse_values[k] <- sqrt(mean((valid_data$Obama_margin_percent - preds)^2))}


print(paste("Mean RMSE of NULL Model across 16 folds:", round(mean(rmse_values), 4)))

null_rmse <- mean(rmse_values)


# Linear Regression

# Dropping columns that 
drop_cols <- c("County", "State", "FIPS", "ElectionDate",
               "Obama", "Clinton", "TotalVote",
               "Obama_margin", "Obama_wins")


for (k in 1:K) {
  train_set <- election_data_train[folds != k, ]
  valid_set <- election_data_train[folds == k, ]
  
  # Dropping unwanted columns
  train_set <- train_set[, !(names(train_set) %in% drop_cols)]
  valid_set <- valid_set[, !(names(valid_set) %in% drop_cols)]
  
  # Fit linear regression
  model <- lm(Obama_margin_percent ~ ., data = train_set)
  
  # Predict 
  preds <- predict(model, newdata = valid_set)
  
  # Compute RMSE
  rmse_values[k] <- sqrt(mean((valid_set$Obama_margin_percent - preds)^2))
  
}


# Print mean RMSE
print(paste("Mean RMSE across 16 folds:", round(mean(rmse_values), 4)))

lm_rmse <- mean(rmse_values)

#Lasso Regression

for (k in 1:K) {
  train_set <- election_data_train[folds != k, ]
  valid_set <- election_data_train[folds == k, ]
  
  # Drop unwanted columns
  train_set <- train_set[, !(names(train_set) %in% drop_cols)]
  valid_set <- valid_set[, !(names(valid_set) %in% drop_cols)]
  
  # Creating matrix for lasso
  x_train <- model.matrix(Obama_margin_percent ~ ., data = train_set)[, -1]
  y_train <- train_set$Obama_margin_percent
  
  x_valid <- model.matrix(Obama_margin_percent ~ ., data = valid_set)[, -1]
  x_valid <- x_valid[, colnames(x_train), drop = FALSE]
  y_valid <- valid_set$Obama_margin_percent
  
  # Fit Lasso model with cross-validation to choose lambda
  cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
  best_lambda <- cv_lasso$lambda.min
  
  # Predict
  preds <- predict(cv_lasso, newx = x_valid, s = best_lambda)
  
  # Compute RMSE
  rmse_values[k] <- sqrt(mean((y_valid - preds)^2))
}

# Print mean RMSE
print(paste("Mean RMSE across 16 folds:", round(mean(rmse_values), 4)))

lasso_rmse <- mean(rmse_values)


# Random Forest 

for (k in 1:K) {
  train_set <- election_data_train[folds != k, ]
  valid_set <- election_data_train[folds == k, ]
  
  # Drop unwanted columns
  train_set <- train_set[, !(names(train_set) %in% drop_cols)]
  valid_set <- valid_set[, !(names(valid_set) %in% drop_cols)]
  
  # Fit Random Forest
  model_rf <- randomForest(Obama_margin_percent ~ .,data = train_set,
                           mtry = floor((ncol(train_set) - 1) / 3),  # one-third of predictors
                           ntree = 500,                              # number of trees
                           importance = TRUE )     # stores variable importance making the predicition better
  
  # Predict
  preds_rf <- predict(model_rf, newdata = valid_set)
  
  # Compute RMSE
  rmse_values[k] <- sqrt(mean((valid_set$Obama_margin_percent - preds_rf)^2))
  
}

# Print mean RMSE
print(paste("Mean RMSE across 16 folds:", round(mean(rmse_values), 4)))

rf_rmse <- mean(rmse_values)

rmse_table <- data.frame(
  Model = c("Null", "Linear Regression", "Lasso", "Random Forest"),
  RMSE = c(null_rmse, lm_rmse, lasso_rmse, rf_rmse)
)

# Print as a table
print(rmse_table)

library(knitr)
kable(rmse_table, caption = "RMSE Comparison Across Models")



# Using random forest (best model) to predict target variable on the test set.
exclude_vars <- c("County", "State", "FIPS", "ElectionDate", "Obama",
                  "Clinton", "TotalVote", "Obama_margin", "Obama_wins",
                  "Obama_margin_percent")

predictors <- names(election_data_train)[!(names(election_data_train) %in% exclude_vars)]

# Create the formula
rf_formula <- as.formula(
  paste("Obama_margin_percent ~", paste(predictors, collapse = " + "))
)

# Train the Random Forest model
rf_final_model <- randomForest(formula = rf_formula, data = election_data_train,
                               ntree = 500, mtry = floor(length(predictors) / 3), importance = TRUE)

# View model summary and important variables
print(rf_final_model)

# Predict on the test data
rf_test_preds <- predict(rf_final_model, newdata = election_data_test)

# Add predictions to test dataset
election_data_test$Predicted_ObamaMarginPercent <- rf_test_preds

# Save results to CSV
write.csv(election_data_test, "testingset_with_predictions.csv", row.names = FALSE)





#####################################
### Question 3: Keep in mind that unsupervised learning 
### is used to explore the data. Feel free to consider only a subset of the 
### demographic variables. 
###

install.packages("fastDummies")
library(fastDummies)

set.seed(1)

# drop null columns
drop_cols <- c("TotalVote", "Clinton", "Obama")

# create a list of categorical variables
# do not include county or vote date
cat_cols <- c("State", "Region", "ElectionType")

# create dummies
election_with_dummies <- dummy_cols(election_data,
                                    select_columns = cat_cols,
                                    remove_selected_columns = TRUE,
                                    remove_first_dummy = TRUE)


# keep numeric columns 
all_numeric <- election_with_dummies[sapply(election_with_dummies, is.numeric)]
all_numeric <- all_numeric[ , !(names(all_numeric) %in% drop_cols), drop = FALSE]

#standardize numbers
all_scaled <- scale(all_numeric)

# fill null values with 0
# null likely means that that description does not apply for a person
colSums(is.na(election_with_dummies))
election_with_dummies[is.na(election_with_dummies)] <- 0


# 4 clusters
k <- 4  
km_res <- kmeans(all_scaled, centers = k, nstart = 25)

# append labels to dataset
# column that appends cluster assignment to each observation
election_with_dummies$cluster <- factor(km_res$cluster)


# shows how many observations are in each cluster
print(km_res$size)
# In-Sample R^2 of 0.13 (for explaining deviance of x)
print(1 - sum(km_res$tot.withinss) / km_res$totss)

# variance explained by each PC
pca <- prcomp(all_scaled)
# Sort by abs(PC1) first, then abs(PC2)
rotation_sorted <- pca$rotation[order(-abs(pca$rotation[,1]), -abs(pca$rotation[,2])), 1:2]
# Show top 20
head(rotation_sorted, 20)


# run principal analysis component on the scaled features 
pca <- prcomp(all_scaled)
# plot the clusters based on two principal components
plot(pca$x[,1:2], col = election_with_dummies$cluster,
     pch = 19, cex = 0.6,
     xlab = "PC1", ylab = "PC2",
     main = paste("K-Means Clustering With All Variables"))

# plot clusters based on average income and percentage of people with a bachelors degree 
plot(election_with_dummies$Bachelors, 
     election_with_dummies$AverageIncome,
     col = election_with_dummies$cluster, 
     pch = 19, cex = 0.6,
     xlab = "Percentage of People with Bachelor's Degree",
     ylab = "Average Income ($)",
     main = "Clusters by Education vs Average Income")
legend("topleft", legend = levels(election_with_dummies$cluster),
       col = 1:length(levels(election_with_dummies$cluster)), pch = 19)


#####################################
### Question 4. First part: impact of changing hispanic demographic
###
#### 
HispanicSimple <- glm( Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(HispanicSimple)


### Question 4. First part: impact of changing black demographic
 
BlackSimple <- glm( Obama_margin_percent ~ Black, data = election_data_train )
summary(BlackSimple)
####

####
### Question 4. Second part
####
#### Model with 1771 controls to measure the impact of 1% larger Hispanic demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Hispanic

#### Model with 1771 controls to measure the impact of 1% larger Black demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Black
####



#####################################
#### Question 5:
# Hispanic Demographic
# Obama's margin against Hispanic %
Hispanic <- lm( Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(Hispanic)

# Black Demographic
# Obama's margin againsts Black %
Black <- lm( Obama_margin_percent ~ Black, data = election_data_train )
summary(Black)


lm_model <- lm(Obama_margin_percent ~ . -Obama_wins -Obama_margin -FIPS -ElectionDate -TotalVote -Clinton -Obama, data = election_data_train)
summary(lm_model)

options(max.print = 8000)
summary(lm_model)

