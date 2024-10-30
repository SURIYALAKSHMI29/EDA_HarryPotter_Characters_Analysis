
install.packages("class")
library(class)

combined_data$House <- as.factor(combined_data$House)
combined_data$Loyalty <- as.factor(combined_data$Loyalty)
combined_data_knn <- model.matrix(~ House + Loyalty - 1, data = combined_data)
combined_data_knn <- as.data.frame(combined_data_knn)

combined_data_knn$Type <- combined_data$Type

set.seed(123)
sample_indices <- sample(1:nrow(combined_data_knn), 0.7 * nrow(combined_data_knn))
train_data_knn <- combined_data_knn[sample_indices, ]
test_data_knn <- combined_data_knn[-sample_indices, ]

train_x <- train_data_knn[, -ncol(train_data_knn)]
train_y <- train_data_knn$Type
test_x <- test_data_knn[, -ncol(test_data_knn)]
test_y <- test_data_knn$Type

cat("Rows in train_x:", nrow(train_x), "\n")
cat("Length of train_y:", length(train_y), "\n")


k_value <- 25
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = k_value)

knn_conf_matrix <- table(knn_pred, test_y)
print(knn_conf_matrix)

knn_accuracy <- sum(diag(knn_conf_matrix)) / sum(knn_conf_matrix)
knn_precision <- diag(knn_conf_matrix) / rowSums(knn_conf_matrix)
knn_recall <- diag(knn_conf_matrix) / colSums(knn_conf_matrix)

# F1 Score for each class
knn_f1_score <- 2 * (knn_precision * knn_recall) / (knn_precision + knn_recall)

# Print metrics
cat("Accuracy:", knn_accuracy, "\n")
cat("Precision:", knn_precision, "\n")
cat("Recall:", knn_recall, "\n")
cat("F1 Score:", knn_f1_score, "\n")
