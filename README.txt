READ ME file "Scalable Product Duplicate Detection: Comparing Locality-Sensitive Hashing (LSH) and EXtreme Gradient Boosting (XGBoost) with Improved Efficiency and Accuracy" - by Luca Sloekers (500863)

The project consists of two programs, written in R-studio. 
The first code is called "LSH code Luca", which uses the LSH method for duplicate detection.
The second code is called "XGBoost code Luca", which uses XGBoost for duplicate detection.

For both codes, it is important that you download the dataset that is used in this research. This dataset can be found on: https://personal.eur.nl/frasincar/datasets/TVs-all-merged.zip. Make sure that the path to the dataset when loading in the data in the code is correct and corresponds to the location of your dataset. 
Furthermore, it is important that you install all the needed packages, which are stated at the beginning of the codes.

For LSH, the code consists of the following parts:
- Selecting the data for the bootstrap
- Cleaning the data
- Constructing the model words
- Creating binary matrix
- Performing Minhashing
- Performing LSH
- Calculating the F1-value
(- Performing k-means clustering and calculating that F1-value*)

For XGBoost, the code consists of the following parts:
- Selecting the data for the bootstrap
- Cleaning the data
- Constructing the model words
- Creating binary matrix
- Performing Minhashing
- Dividing the data into X and y and training and test data
- Constructing XGBoost
- Training XGBoost
- Evaluating XGBoost using the test data
- Calculating the F1-value
(- Performing k-means clustering and calculating that F1-value*)

*This is not discussed in the paper and the obtained results are not relevant.

This paper provides results where XGBoost shows improvements in duplicate detection on both accuracy and efficiency. 
Possible suggestions for improvements:
- Tuning the XGBoost hyperparameters
- Combining LSH or XGBoost with other types of clustering algorithms
- Combinging LSH and XGBoost together
