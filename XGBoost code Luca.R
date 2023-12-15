#Installing all the packages
install.packages("jsonlite")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("hash")
install.packages("numbers")
install.packages("xgboost")
library(xgboost)
library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)
library(hash)
library(numbers)

#Loading in the data
dataset <- fromJSON("C:\\Users\\Luca Sloekers\\Downloads\\TVs-all-merged\\TVs-all-merged.json")
data_frame <- bind_rows(lapply(dataset, as.data.frame), .id = "observation_id")
data <- as.matrix(data_frame)

#Setting up everything for the bootstrap
iterations <- 5
F1_XGB <- numeric(5)
F1_XGB_clustered <- numeric(5)
F1_XGB_total <- numeric(5)

#the beginning of the code is the same as for LSH. For a more detailed commented version, I refer to the LSH code. 

for(iteration in 1:iterations){
  set.seed(iteration) 
  num_observations_to_select <- 1000
  selected_indices <- sample(1:nrow(data), num_observations_to_select, replace = FALSE)
  data_matrix <- data[selected_indices, ]
  model_ID <- data_matrix[, 1]
  
  count_duplicates <- c()
  binary_duplicates <- c()
  for(j in 1:length(model_ID)){
    count_duplicates[j] <- 0
    for(k in 1:length(model_ID))
      if(model_ID[j] == model_ID[k]){
        count_duplicates[j] <- count_duplicates[j] + 1
      }
    if(count_duplicates[j]>1){
      binary_duplicates[j] <- 1
    } else{
      binary_duplicates[j] <- 0
    }
  }
  
  
  #DATA CLEANING-------------------------------------------------------------------------------------------------------------------
  non_numeric_words <- unlist(strsplit(gsub("[^A-Za-z]+", " ", data_frame), "\\s+"))
  word_frequency <- table(non_numeric_words)
  print(word_frequency)
  sorted_word_freq <- sort(word_frequency, decreasing = TRUE)
  print(sorted_word_freq)
  make_lowercase <- function(matrix){
  result_matrix <- matrix(, nrow = nrow(matrix), ncol = ncol(matrix))
  
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      result_matrix[i, j] <- tolower(matrix[i, j])
    }
  }
  return(result_matrix)
  }

  
  lowercase_matrix <- make_lowercase(data_matrix)

  non_numeric_words <- unlist(strsplit(gsub("[^A-Za-z]+", " ", lowercase_matrix), "\\s+"))

  word_frequency_lower <- table(non_numeric_words)
  print(word_frequency_lower)
  sorted_word_freq_lower <- sort(word_frequency_lower, decreasing = TRUE)
  print(sorted_word_freq_lower)

  inconsistent_inch <- c("Inch", "inches", '"', "-inch", " inch", "inch")
  standardize_inch <- function(text_vector, inconsistent_inch) {
  
  for (inch in inconsistent_inch) {
    text_vector <- gsub(inch, "inch", text_vector, ignore.case = TRUE)
  }
  
  text_vector <- gsub('["“”]', 'inch', text_vector)
  
  return(text_vector)
  }

  dataset_inches <- standardize_inch(lowercase_matrix, inconsistent_inch)
  print(dataset_inches)

  inconsistent_hertz <- c("Hertz", "hertz", "Hz", "HZ", "hz", "-hz", "hz")
  standardize_hertz <- function(text_vector, inconsistent_hertz) {
  for (hertz in inconsistent_hertz) {
    text_vector <- gsub(hertz, "hz", text_vector, ignore.case = TRUE)
  }
  
  return(text_vector)
  }

 dataset_inches_hertz <- standardize_hertz(dataset_inches, inconsistent_hertz)
 dataset_consistent <- as.matrix(dataset_inches_hertz)
 colnames(dataset_consistent) <- colnames(data_matrix)
 title_vector <- dataset_consistent[, 356]
 print(title_vector)
 
 #Model words--------------------------------------------------------------------------------------------------------------
 Modelword_title <- "([a-zA-Z0-9]*(([0-9]+[^0-9,]+)|([^0-9,]+[0-9]+))[a-zA-Z0-9]*)"
 
 title_tokens <- strsplit(title_vector, "\\s+")
 model_words_per_title <- sapply(title_tokens, function(tokens) {
   valid_tokens <- tokens[grep(Modelword_title, tokens, perl = TRUE)]
   if (length(valid_tokens) >= 2) {
     cleaned <- gsub("[^a-zA-Z0-9]", "", valid_tokens)
     cleaned <- gsub("\\s+", "", cleaned)
     paste(cleaned, collapse = " ")
   } else {
     NA
   }
 })
 
 print(model_words_per_title)
 modelwords_title_matrix <- as.matrix(model_words_per_title)
 print(modelwords_title_matrix)
 
 Modelword_keyvalue <- "(^\\d+(\\.\\d+)?[a-zA-Z]+$|^\\d+(\\.\\d+)?$)"
 

 keyvalue_matrix <- dataset_consistent[, 5:355]
 print(keyvalue_matrix)
 
 model_words_keyvalues_matrix <- apply(keyvalue_matrix, c(1, 2), function(keyvalue_pair) {
   valid_pair <- grepl(Modelword_keyvalue, keyvalue_pair, perl = TRUE)
   
   if (valid_pair) {
     cleaned_pair <- gsub("[^0-9.]", "", keyvalue_pair)
     cleaned_pair
   } else {
     NA
   }
 })
 
 print(model_words_keyvalues_matrix)
 
 #Binary vectors -----------------------------------------------------------------------------------------------------
 MW_titles <- unique(unlist(strsplit(as.character(modelwords_title_matrix), " ")))
 print(MW_titles)
 MW_keyvalues <- unique(as.vector(model_words_keyvalues_matrix))
 print(MW_keyvalues)
 MW <- unique(c(MW_titles, MW_keyvalues))
 MW <- MW[!is.na(MW)]
 print(MW)
 
 binary_matrix <- matrix(0, nrow = nrow(modelwords_title_matrix), ncol = length(MW), dimnames = list(NULL, MW))
 
 model_words_split <- strsplit(modelwords_title_matrix, " ")
 
 max_words <- max(sapply(model_words_split, length))

 final_modelwords_title_matrix <- matrix("", nrow = length(modelwords_title_matrix), ncol = max_words)
 
 for (i in 1:length(modelwords_title_matrix)) {
   final_modelwords_title_matrix[i, 1:length(model_words_split[[i]])] <- model_words_split[[i]]
 }
 
 for (i in 1:nrow(final_modelwords_title_matrix)) {
   model_words_title_i <- unlist(strsplit(as.character(final_modelwords_title_matrix[i, ]), " "))
   binary_matrix[i, MW %in% model_words_title_i] <- 1
 }
 
 for (i in 1:nrow(model_words_keyvalues_matrix)) {
   model_words_keyvalues_i <- unlist(strsplit(as.character(model_words_keyvalues_matrix[i, ]), " "))
   binary_matrix[i, MW %in% model_words_keyvalues_i] <- 1
 }
 
 print(binary_matrix)

 k <- 1500
 is_prime <- function(n) {
   length(divisors(n)) == 2
 }
 
 generate_p <- function(prime){
   prime_candidate <- sample(1500:10000,1)
   while(!is_prime(prime_candidate)){
     prime_candidate <- prime_candidate + 1
   }
   return(prime_candidate)
 }
 
 random_hash_function <- function(k) {
   a <- sample(1:(k-1), 1)
   b <- sample(1:(k-1), 1)
   
   p <- generate_p(k)
   
   return(function(x) {
     return((a + b * x) %% p)
   })
 }
 
 
 generate_minhash_signatures <- function(binary_matrix, num_hashes) {
   num_instances <- nrow(binary_matrix)
   num_features <- ncol(binary_matrix)
   
   k <- floor(num_features *(1500/ncol(binary_matrix)))
   
   minhash_signatures <- matrix(Inf, nrow = num_hashes, ncol = num_instances)
   
   for (h in 1:num_hashes) {
     hash_function <- random_hash_function(k)
     
     for (i in 1:num_instances) {
       set_indices <- which(binary_matrix[i, ] == 1)
       if (length(set_indices) == 0) {
         minhash_signatures[h, i] <- 0
       } else {
         minhash_signatures[h, i] <- min(hash_function(set_indices))
       }
     }
   }
   
   return(minhash_signatures)
 }
 
 set.seed(123)  
 num_hashes <- floor(ncol(binary_matrix) * (1500/ncol(binary_matrix)))
 minhash_signatures <- generate_minhash_signatures(binary_matrix, num_hashes)
 
 print(minhash_signatures)
 
 #XGBoost----------------------------------------------------------------------------------------------
 #Generate the X and y variable for XGBoost
 X<- binary_matrix
 Y<- as.matrix(binary_duplicates)

  set.seed(123)

  # Determine the number of rows for training (80%) and test (20%)
  num_rows <- nrow(binary_matrix)
  num_train <- round(0.8 * num_rows)
  
 # Generate random indices for training set
 train_indices <- sample(1:num_rows, num_train, replace = FALSE)

# Create training and test sets
X_train <- as.matrix(X[train_indices, ])
Y_train <- Y[train_indices]

X_test <- X[-train_indices, ]
Y_test <- Y[-train_indices]

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = Y_test)

# Set parameters for XGBoost
params <- list(
  objective = "binary:logistic",  # for binary classification
  eval_metric = "logloss" 
)

# Train the XGBoost model
xgb_model <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = TRUE)

# Make predictions on the test set
y_pred <- predict(xgb_model, dtest)
# Make a binary vector of the predictions
y_pred_binary <- ifelse(y_pred > 0.5, 1, 0)

# Evaluate the performance of the model on the test set
confusion_matrix <- table(y_pred_binary, Y_test)
print(confusion_matrix)

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

F1_XGB[iteration] <- f1_score

#We also run the model once on the entire dataset to see what happens here.
y_pred_total <- predict(xgb_model, X)
y_pred_binary_total <- ifelse(y_pred_total > 0.5, 1, 0)
confusion_matrix_total <- table(y_pred_binary_total, Y)
print(confusion_matrix_total)
precision_total <- confusion_matrix_total[2, 2] / sum(confusion_matrix_total[, 2])
recall_total <- confusion_matrix_total[2, 2] / sum(confusion_matrix_total[2, ])
f1_score_total <- 2 * (precision_total * recall_total) / (precision_total + recall_total)

F1_XGB_total[iteration] <- f1_score_total

#Trying to perform the k-means on the XGBoost output-------------------------------------------------------
# Select rows where XGBoost predicts a duplicate (y equals 1)
duplicate_indices_XGB <- which(y_pred_binary_total == 1)

minhash_signatures_transposed <-t(minhash_signatures)
selected_rows_xgboost <- minhash_signatures_transposed[duplicate_indices_XGB, ]

# Perform clustering on the selected rows from XGBoost predictions
num_clusters_xgboost <- floor(length(duplicate_indices_XGB) / 2) 
kmeans_result_xgboost <- kmeans(selected_rows_xgboost, centers = num_clusters_xgboost)

pair_matrix_clusters_XGB <- matrix(0, ncol = 2)
for (cluster_number in unique(kmeans_result_xgboost$cluster)) {
  cluster_indices <- which(kmeans_result_xgboost$cluster == cluster_number)
  cluster_size <- length(cluster_indices)
  
  # Check if there are at least 2 elements in the cluster
  if (cluster_size >= 2) {
    pairs_in_cluster <- combn(cluster_indices, 2)
    # Add the pairs to the pair_matrix_clusters
    pair_matrix_clusters_XGB <- rbind(pair_matrix_clusters_XGB, t(pairs_in_cluster))
  }
}

indices_selected_rows_pairs_XGB <- pair_matrix_clusters_XGB[-1, ]
pair_matrix_clusters_final_XGB <- matrix(0, nrow = nrow(indices_selected_rows_pairs_XGB), ncol = ncol(indices_selected_rows_pairs_XGB))

for (i in 1:nrow(indices_selected_rows_pairs_XGB)) {
  for (j in 1:ncol(indices_selected_rows_pairs_XGB)) {
    index_from_duplicate_indices <- indices_selected_rows_pairs_XGB[i, j]
    pair_matrix_clusters_final_XGB[i, j] <- duplicate_indices_XGB[index_from_duplicate_indices]
  }
}

total_duplicates_xgboost <- 0
model_id_duplicates_xgboost <- matrix(0, nrow = nrow(pair_matrix_clusters_final_XGB), ncol = ncol(pair_matrix_clusters_final_XGB))
for (i in 1:nrow(pair_matrix_clusters_final_XGB)) {
  row_number_true_matrix_pair1_xgboost <- pair_matrix_clusters_final_XGB[i, 1]
  row_number_true_matrix_pair2_xgboost <- pair_matrix_clusters_final_XGB[i, 2]
  model_id_duplicates_xgboost[i, 1] <- data_matrix[row_number_true_matrix_pair1_xgboost, 1]
  model_id_duplicates_xgboost[i, 2] <- data_matrix[row_number_true_matrix_pair2_xgboost, 1]
  total_duplicates_xgboost <- total_duplicates_xgboost + 1
}

found_pairs_xgboost <- 0
for (i in 1:nrow(model_id_duplicates_xgboost)) {
  if (model_id_duplicates_xgboost[i, 1] == model_id_duplicates_xgboost[i, 2]) {
    found_pairs_xgboost <- found_pairs_xgboost + 1
  } 
}

Df_xgboost <- found_pairs_xgboost
Nc_xgboost <- length(pair_matrix_clusters_final_XGB)
Dn_xgboost <- total_duplicates_xgboost

PQ_xgboost <- Df_xgboost / Nc_xgboost
PC_xgboost <- Df_xgboost / Dn_xgboost
F1value_xgboost <- (2 * PQ_xgboost * PC_xgboost) / (PQ_xgboost + PC_xgboost)
F1_XGB_clustered[iteration] <- F1value_xgboost


}
print(F1_XGB)
meanF1_XGB <- mean(F1_XGB)
print(meanF1_XGB)
print(F1_XGB_clustered)
meanF1_XGB_clustered <- mean(F1_XGB_clustered)
print(meanF1_XGB_clustered)
print(F1_XGB_total)
meanF1_XGB_total <- mean(F1_XGB_total)
print(meanF1_XGB_total)
