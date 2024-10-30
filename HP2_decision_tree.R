library(dplyr)
library(rpart)
library(rpart.plot)

combined_data$Type <- as.factor(combined_data$Type)

combined_data <- combined_data %>%
  mutate(Type = ifelse(Type == "Ally", "Ally", "Enemy"))  # Group by type

tree_model <- rpart(Type ~ House + Loyalty, 
                    data = combined_data, 
                    method = "class",
                    control = rpart.control(minsplit = 10, 
                                            minbucket = 5, 
                                            maxdepth = 5, 
                                            cp = 0.01))

rpart.plot(tree_model, 
           type = 4, 
           extra = 104, 
           fallen.leaves = TRUE, 
           main = "Decision Tree: Allies vs Enemies Grouped by House",
           cex = 0.8, 
           branch = 0.3,
           box.palette = c("skyblue", "lightcoral"))  



# Print the complexity parameter table
printcp(tree_model)

# Optionally, evaluate model performance (if you choose to split the data)
set.seed(123)
train_index <- sample(1:nrow(tree_data), 0.7 * nrow(tree_data))
train_data <- tree_data[train_index, ]
test_data <- tree_data[-train_index, ]

tree_model_train <- rpart(Type ~ House + Loyalty, data = train_data, method = "class", control = rpart.control(cp = 0.01))

predictions <- predict(tree_model_train, test_data, type = "class")

confusion_matrix <- table(predictions, test_data$Type)
print(confusion_matrix)


# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Calculate precision and recall for each class
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)

# F1 Score for each class
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")




