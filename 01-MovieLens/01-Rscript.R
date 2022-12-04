##########################################################
# Create edx and final_holdout_test sets 
##########################################################

### This initial section is provided by the EdX course to generate the data sets.

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)




##########################################################
# Inspecting the data
##########################################################

### Load the required libraries

library(caret)
library(tidyverse)


### Data structure

# Number of rows
inspect_data_structure <- tibble(FEATURE = "Number of rows",
                                 VALUE = nrow(edx))

# Number of columns
inspect_data_structure <- inspect_data_structure %>%
                          bind_rows(
                            tibble( FEATURE = "Number of columns",
                                    VALUE = ncol(edx)))

# Minimum rating
inspect_data_structure <- inspect_data_structure %>%
  bind_rows(
    tibble( FEATURE = "Minimum rating",
            VALUE = min(edx$rating)))

# Maximum rating
inspect_data_structure <- inspect_data_structure %>%
  bind_rows(
    tibble( FEATURE = "Maximum rating",
            VALUE = max(edx$rating)))

# Quantity of different movies
inspect_data_structure <- inspect_data_structure %>%
  bind_rows(
    tibble( FEATURE = "Quantity of different movies",
            VALUE = length(unique(edx$movieId))))

# Quantity of different users
inspect_data_structure <- inspect_data_structure %>%
  bind_rows(
    tibble( FEATURE = "Quantity of different users",
            VALUE = length(unique(edx$userId))))

# Quantity of different genres
genres <- str_extract_all(unique(edx$genres), "[^|]+") %>%
  unlist() %>%
  unique()

inspect_data_structure <- inspect_data_structure %>%
  bind_rows(
    tibble( FEATURE = "Quantity of different genres",
            VALUE = length(genres)))



##########################################################
# Building the recommendation system
##########################################################

### Creating functions used later in the code

# RMSE:
# Calculates the Root Mean Square Error based on a
# true_ratings and a predicted_ratings vector of values

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



### Set random seed

set.seed(1993)



### Split the train and the tuning set

p = 0.1

tuning_index <- createDataPartition(y = edx$rating, times = 1,
                                    p = p, list = FALSE)
train_set <- edx[-tuning_index,]
tuning_set <- edx[tuning_index,]

# Let's ensure that all the movieId's and userId's from the training set and the
# tunning set exist in the final_holdout_test
train_set <- train_set %>%
  semi_join(final_holdout_test, by = "movieId") %>%
  semi_join(final_holdout_test, by = "userId")

tuning_set <- tuning_set %>%
  semi_join(final_holdout_test, by = "movieId") %>%
  semi_join(final_holdout_test, by = "userId") %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")



### Building the recommendation system


# Method 1: Just the average --------------------------------------------

mu_hat <- mean(train_set$rating)

# Calculate the RMSE for this method
mth_1 <- RMSE(final_holdout_test$rating, mu_hat)

# Store the RMSE in a result table
rmse_results <- tibble( METHOD=paste("Just the average (mu_hat =",mu_hat,")"),
                        RMSE = mth_1)

# rmse_results %>% knitr::kable()


# Method 2: Movie effect -------------------------------------------------------

movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

predicted_ratings <- final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu_hat + b_i) %>%
  .$pred

# Ensuring that the predicted ratings are not out of the bounds (0.5 <= pred <= 5)
predicted_ratings <- ifelse(predicted_ratings < 0.5,0.5,predicted_ratings)
predicted_ratings <- ifelse(predicted_ratings > 5,5,predicted_ratings)

# In case some rows doesn't have predicted ratings replace with the average
predicted_ratings[is.na(predicted_ratings)] <- mu_hat

# Calculate the RMSE for this method
mth_2 <- RMSE(final_holdout_test$rating, predicted_ratings)

# Store the RMSE in a result table
rmse_results <- bind_rows(rmse_results,
                          tibble( METHOD=paste("Movie effect"),
                                  RMSE = mth_2 ))
# rmse_results %>% knitr::kable()

# Remove stores results (if necessary)
# rmse_results <- rmse_results[-2,]


# Method 3: Movie and User effect -------------------------------------------------------

movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

# Ensuring that the predicted ratings are not out of the bounds (0.5 <= pred <= 5)
predicted_ratings <- ifelse(predicted_ratings < 0.5,0.5,predicted_ratings)
predicted_ratings <- ifelse(predicted_ratings > 5,5,predicted_ratings)

# In case some rows doesn't have predicted ratings replace with the average
predicted_ratings[is.na(predicted_ratings)] <- mu_hat

# Calculate the RMSE for this method
mth_3 <- RMSE(final_holdout_test$rating, predicted_ratings)

# Store the RMSE in a result table
rmse_results <- bind_rows(rmse_results,
                          tibble( METHOD=paste("Movie + User effect"),
                                  RMSE = mth_3 ))
# rmse_results %>% knitr::kable()

# Remove stores results (if necessary)
# rmse_results <- rmse_results[-2,]


# Method 4: Movie, User and Genre effect ---------------------------------------

genres_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))

predicted_ratings <- final_holdout_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred

# Ensuring that the predicted ratings are not out of the bounds (0.5 <= pred <= 5)
predicted_ratings <- ifelse(predicted_ratings < 0.5,0.5,predicted_ratings)
predicted_ratings <- ifelse(predicted_ratings > 5,5,predicted_ratings)

# In case some rows doesn't have predicted ratings replace with the average
predicted_ratings[is.na(predicted_ratings)] <- mu_hat

# Calculate the RMSE for this method
mth_4 <- RMSE(final_holdout_test$rating, predicted_ratings)

# Store the RMSE in a result table
rmse_results <- bind_rows(rmse_results,
                          tibble( METHOD=paste("Movie, User and Genres effect"),
                                  RMSE = mth_4 ))
# rmse_results %>% knitr::kable()


# Method 5: Regularized movie, user and genres effect with lambda = 3 ---------------------------

lambda <- 3

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda), n_i = n()) 

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+lambda), n_i = n()) 

genres_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+lambda), n_i = n()) 

predicted_ratings <- final_holdout_test %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred

# Ensuring that the predicted ratings are not out of the bounds (0.5 <= pred <= 5)
predicted_ratings <- ifelse(predicted_ratings < 0.5,0.5,predicted_ratings)
predicted_ratings <- ifelse(predicted_ratings > 5,5,predicted_ratings)

# In case some rows doesn't have predicted ratings replace with the average
predicted_ratings[is.na(predicted_ratings)] <- mu_hat

# Calculate the RMSE for this method
mth_5 <- RMSE(final_holdout_test$rating, predicted_ratings)

# Store the RMSE in a result table
rmse_results <- bind_rows(rmse_results,
                          tibble( METHOD=paste("Reg. Movie, User and Genres Effect (lambda =",lambda,")"),
                                  RMSE = mth_5 ))
# rmse_results %>% knitr::kable()


# Method 6: Regularized movie, user and genres effect with lambda tuning ---------------

# Tuning lambda value

# Note: the process is calculated repeatedly by increasing the precision on each
# run. This is done in order to reduce the time it requires to process.

i_precision <- c(1,0.1,0.02)
i_interval <- c(2,6)

lambda <- sapply(i_precision, function(precision){
  lambdas <- seq(i_interval[1], i_interval[2], precision)
  rmses_temp <- sapply(lambdas, function(l){
    b_i <- train_set %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat)/(n()+l))
    b_u <- train_set %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
    b_g <- train_set %>% 
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%
      group_by(genres) %>%
      summarize(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+l))
    predicted_ratings <- 
      tuning_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      left_join(b_g, by = "genres") %>%
      mutate(pred = mu_hat + b_i + b_u + b_g) %>%
      .$pred
    return(RMSE(tuning_set$rating, predicted_ratings))
  })
  
  # qplot(lambdas, rmses_temp)  # Just for debugging purposes
  
  lambda_temp <- lambdas[which.min(rmses_temp)]

  i_interval <<- c(lambda_temp-precision,lambda_temp+precision)
  
  return(lambda_temp)
  
  # Clean up
  rm(lambda_temp,rmses_temp,lambdas)
})

# Clean up
# rm(i_interval,i_precision)

# Here we obtain the tuned lambda
lambda <- lambda[length(lambda)]

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda), n_i = n()) 

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+lambda), n_i = n()) 

genres_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+lambda), n_i = n()) 

predicted_ratings <- final_holdout_test %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred

# Ensuring that the predicted ratings are not out of the bounds (0.5 <= pred <= 5)
predicted_ratings <- ifelse(predicted_ratings < 0.5,0.5,predicted_ratings)
predicted_ratings <- ifelse(predicted_ratings > 5,5,predicted_ratings)

# In case some rows doesn't have predicted ratings replace with the average
predicted_ratings[is.na(predicted_ratings)] <- mu_hat

# Calculate the RMSE for this method
mth_6 <- RMSE(final_holdout_test$rating, predicted_ratings)

# Store the RMSE in a result table
rmse_results <- bind_rows(rmse_results,
                          tibble( METHOD=paste("Reg. Movie, User and Genres Effect Tuned (lambda =",lambda,")"),
                                  RMSE = mth_6 ))
# rmse_results %>% knitr::kable()

