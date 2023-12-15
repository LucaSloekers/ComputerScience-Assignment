#Installing all the needed packages
install.packages("jsonlite")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("hash")
install.packages("numbers")
library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)
library(hash)
library(numbers)

#Loading in the data and starting the bootstrap. 
dataset <- fromJSON("C:\\Users\\Luca Sloekers\\Downloads\\TVs-all-merged\\TVs-all-merged.json")
data_frame <- bind_rows(lapply(dataset, as.data.frame), .id = "observation_id")
data <- as.matrix(data_frame)
F1_LSH <- numeric(5)
F1_clustering <- numeric(5)
iterations <- 5

for(iteration in 1:iterations){
  set.seed(iteration) 
  
  #Selecting the data for the bootstrap iteration
  num_observations_to_select <- 1000
  selected_indices <- sample(1:nrow(data), num_observations_to_select, replace = FALSE)
  data_matrix <- data[selected_indices, ]
  
  #Determining the total number of duplicates in the chosen dataset.
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
  
#DATA CLEANING---------------------------------------------------------------------------------------------------------
  #We take out the non numeric words.
  non_numeric_words <- unlist(strsplit(gsub("[^A-Za-z]+", " ", data_frame), "\\s+"))
  # Count the frequency of each word to get an overview of most occuring inconsistencies 
  word_frequency <- table(non_numeric_words)
  print(word_frequency)
  sorted_word_freq <- sort(word_frequency, decreasing = TRUE)
  print(sorted_word_freq)

  #convert the matrix to lowercase
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

  #get the non numeric words from the lowercase matrix.
  non_numeric_words <- unlist(strsplit(gsub("[^A-Za-z]+", " ", lowercase_matrix), "\\s+"))
  word_frequency_lower <- table(non_numeric_words)
  print(word_frequency_lower)
  sorted_word_freq_lower <- sort(word_frequency_lower, decreasing = TRUE)
  print(sorted_word_freq_lower)

  #Replacing all the different verions of inch an hertz in the dataset to one 
  inconsistent_inch <- c("Inch", "inches", '"', "-inch", " inch", "inch")
  standardize_inch <- function(text_vector, inconsistent_inch) {
  for (inch in inconsistent_inch) {
    text_vector <- gsub(inch, "inch", text_vector, ignore.case = TRUE)
  }
  
  # Replace double quotes with "inch"
  text_vector <- gsub('["“”]', 'inch', text_vector)
  
  return(text_vector)
}

# Apply the function to the lowercase matrix
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

#create the matrix with the consistent values.
dataset_consistent <- as.matrix(dataset_inches_hertz)
colnames(dataset_consistent) <- colnames(data_matrix)
title_vector <- dataset_consistent[, 356]
print(title_vector)

#Model words--------------------------------------------------------------------------------------------------------------
#First we make the model words for the title
Modelword_title <- "([a-zA-Z0-9]*(([0-9]+[^0-9,]+)|([^0-9,]+[0-9]+))[a-zA-Z0-9]*)"
title_splitparts <- strsplit(title_vector, "\\s+")
model_words_per_title <- sapply(title_splitparts, function(tokens) {
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

#Now, we make the model words for the keyvalues.
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

# Print the result
print(model_words_keyvalues_matrix)

#Binary vectors -------------------------------------------------------------------------------------------------------------------
#We use the model words to create binary vectors for all the products. 

MW_titles <- unique(unlist(strsplit(as.character(modelwords_title_matrix), " ")))
print(MW_titles)
MW_keyvalues <- unique(as.vector(model_words_keyvalues_matrix))
print(MW_keyvalues)
MW <- unique(c(MW_titles, MW_keyvalues))
MW <- MW[!is.na(MW)]
print(MW)

# Initialize the binary matrix
binary_matrix <- matrix(0, nrow = nrow(modelwords_title_matrix), ncol = length(MW), dimnames = list(NULL, MW))

# Fill in the binary matrix based on model words from titles
model_words_split <- strsplit(modelwords_title_matrix, " ")

max_words <- max(sapply(model_words_split, length))
final_modelwords_title_matrix <- matrix("", nrow = length(modelwords_title_matrix), ncol = max_words)

for (i in 1:length(modelwords_title_matrix)) {
  final_modelwords_title_matrix[i, 1:length(model_words_split[[i]])] <- model_words_split[[i]]
}

# Fill in the binary matrix 
for (i in 1:nrow(final_modelwords_title_matrix)) {
  model_words_title_i <- unlist(strsplit(as.character(final_modelwords_title_matrix[i, ]), " "))
  binary_matrix[i, MW %in% model_words_title_i] <- 1
}
for (i in 1:nrow(model_words_keyvalues_matrix)) {
  model_words_keyvalues_i <- unlist(strsplit(as.character(model_words_keyvalues_matrix[i, ]), " "))
  binary_matrix[i, MW %in% model_words_keyvalues_i] <- 1
}

print(binary_matrix)

#Defining duplicate candidates --------------------------------------------------------------------------

# Function to generate a random prime number greater than k. This will be p in the random hash function. 
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

#Generate the random hash functions
random_hash_function <- function(k) {
  # Generate random integers a and b
  a <- sample(1:(k-1), 1)
  b <- sample(1:(k-1), 1)
  
  # Generate a random prime number greater than k
  p <- generate_p(k)
  
  return(function(x) {
    return((a + b * x) %% p)
  })
}

#Making the minhashing signatures
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

#setting the number of hashes and performing the minhashing
num_hashes <- floor(ncol(binary_matrix) * (1500/ncol(binary_matrix)))
minhash_signatures <- generate_minhash_signatures(binary_matrix, num_hashes)
print(minhash_signatures)

#LSH------------------------------------------------------------------------------------------------
lsh_algorithm <- function(minhash_signatures, bands, rows ) {
  num_hashes <- nrow(minhash_signatures)
  num_instances <- ncol(minhash_signatures)
  
  # Check if the number of hashes is divisible by bands
  if (num_hashes %% bands != 0) {
    stop("N needs to be r x b")
  }
  
  rows_per_band <- num_hashes / bands
  buckets <- vector("list", length = bands)
  for (i in 1:bands) {
    buckets[[i]] <- list()
  }
  
  for (band in 1:bands) {
    start_row <- (band - 1) * rows_per_band + 1
    end_row <- band * rows_per_band
    band_signatures <- minhash_signatures[start_row:end_row, ]
    
    # Check if band_signatures is empty
    if (length(band_signatures) == 0) {
      stop("Band signatures matrix is empty.")
    }
    
    # Hash each column in the band
    hashed_columns <- apply(band_signatures, 2, function(col) {
      paste(col, collapse = '')
    })
    
    for (product in 1:num_instances) {
      # Use band_string as an identifier for the bucket
      bucket_identifier <- hashed_columns[product]
      buckets[[band]][[bucket_identifier]] <- c(buckets[[band]][[bucket_identifier]], product)
    }
  }
  candidate_pairs <- list()
  for (band in 1:bands) {
    for (bucket in buckets[[band]]) {
      if (length(bucket) > 1) {
        # Generate all pairs within the bucket
        pairs <- combn(bucket, 2)
        candidate_pairs <- union(candidate_pairs, list(pairs))
      }
    }
  }
  return(candidate_pairs)
}

#Initialise best bands and rows and F1 value for tuning the bands and rows in LSH
best_bands <- NULL
best_rows <- NULL
best_f1 <- 0

set.seed(123) 
for(divisor in divisors(1500)){
  if (divisor %in% c(1, 1500)) {
    next  # Skip 1 and 1500
  }
  
  bands <- divisor
  rows <- 1500/bands
  candidate_pairs_tune <- lsh_algorithm(minhash_signatures, bands, rows)
  #Flatten the list into a matrix with two columns
  pair_matrix_tune <- matrix(unlist(candidate_pairs_tune), ncol = 2, byrow = TRUE)
  
  model_id_vector <- data_matrix[, 1]
  #creating count
  total_duplicates_LSH_tuning <- 0
  model_id_count <- table(model_id_vector)
  model_id_count_matrix <- as.matrix(model_id_count)
  for (i in 1:nrow(model_id_count_matrix)) {
    if (model_id_count_matrix[i, 1] == 2) {
      total_duplicates_LSH_tuning <- total_duplicates_LSH_tuning + 1
    } else if (model_id_count_matrix[i, 1] == 3) {
      total_duplicates_LSH_tuning <- total_duplicates_LSH_tuning + 3
    } else if (model_id_count_matrix[i, 1] == 4) {
      total_duplicates_LSH_tuning <- total_duplicates_LSH_tuning + 6
    }
  }
  
  #creating found pairs
  model_id_duplicates <- matrix(0, nrow = nrow(pair_matrix_tune), ncol = ncol(pair_matrix_tune))
  for (i in 1:nrow(pair_matrix_tune)) {
    row_number_true_matrix_pair1 <- pair_matrix_tune[i, 1]
    row_number_true_matrix_pair2 <- pair_matrix_tune[i, 2]
    model_id_duplicates[i, 1] <- data_matrix[row_number_true_matrix_pair1, 1]
    model_id_duplicates[i, 2] <- data_matrix[row_number_true_matrix_pair2, 1]
  }
  
  found_pairs <-0
  model_id_duplicates_counted <- matrix(0, nrow = nrow(model_id_duplicates), ncol = 1)
  for (i in 1:nrow(model_id_duplicates)) {
    if (model_id_duplicates[i, 1] == model_id_duplicates[i, 2]) {
      model_id_duplicates_counted[i, 1] <- 1
      found_pairs <- found_pairs + 1
    } else {
      model_id_duplicates_counted[i, 1] <- 0
    }
  }
  
  # Add the counted duplicates as the third column in model_id_duplicates so we can see which ones are duplicates
  model_id_duplicates <- cbind(model_id_duplicates, model_id_duplicates_counted)
  
  #Calculate the F1
  D_f <- 0
  N_c <- 0
  D_n <- 0
  PQ <- 0
  PC <- 0
  
  D_f <- found_pairs
  N_c <- length(pair_matrix_tune)
  D_n <- total_duplicates_LSH_tuning
  
  PQ = D_f / N_c
  PC = D_f/D_n
  
  F1value = (2*PC*PQ)/(PQ+PC)
  

 # Check if this combination gives a better F1 score
 if (F1value > best_f1) {
  best_f1 <- F1value
  best_bands <- bands
  best_rows <- rows
}
}

candidate_pairs <- lsh_algorithm(minhash_signatures, best_bands, best_rows)
# Flatten the list into a matrix with two columns
pair_matrix <- matrix(unlist(candidate_pairs), ncol = 2, byrow = TRUE)
print(pair_matrix)


#Evaluation----------------------------------------------------------------------------------------------------
#calculating how many modelID's are the same
model_id_vector <- data_matrix[, 1]

#creating count
total_duplicates <- 0
model_id_count <- table(model_id_vector)
model_id_count_matrix <- as.matrix(model_id_count)
for (i in 1:nrow(model_id_count_matrix)) {
  if (model_id_count_matrix[i, 1] == 2) {
    total_duplicates <- total_duplicates + 1
  } else if (model_id_count_matrix[i, 1] == 3) {
    total_duplicates <- total_duplicates + 3
  } else if (model_id_count_matrix[i, 1] == 4) {
    total_duplicates <- total_duplicates + 6
  }
}

#creating found pairs
model_id_duplicates <- matrix(0, nrow = nrow(pair_matrix), ncol = ncol(pair_matrix))
for (i in 1:nrow(pair_matrix)) {
  row_number_true_matrix_pair1 <- pair_matrix[i, 1]
  row_number_true_matrix_pair2 <- pair_matrix[i, 2]
  model_id_duplicates[i, 1] <- data_matrix[row_number_true_matrix_pair1, 1]
  model_id_duplicates[i, 2] <- data_matrix[row_number_true_matrix_pair2, 1]
}

found_pairs <-0
model_id_duplicates_counted <- matrix(0, nrow = nrow(model_id_duplicates), ncol = 1)
for (i in 1:nrow(model_id_duplicates)) {
  if (model_id_duplicates[i, 1] == model_id_duplicates[i, 2]) {
    model_id_duplicates_counted[i, 1] <- 1
    found_pairs <- found_pairs + 1
  } else {
    model_id_duplicates_counted[i, 1] <- 0
  }
}
model_id_duplicates <- cbind(model_id_duplicates, model_id_duplicates_counted)
print(model_id_duplicates)

#calculating the F1
D_f <- found_pairs
N_c <- length(pair_matrix)
D_n <- total_duplicates

PQ = D_f / N_c
PC = D_f/D_n

F1value = (2*PC*PQ)/(PQ+PC)
print(F1value)
F1_LSH[iteration] <- F1value

}

print(F1_LSH)
print(F1_clustering)


