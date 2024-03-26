##########################################################
# Create edx and final_holdout_test sets 
##########################################################

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
  
# Let's load some additional packages to aid our analysis
library(data.table)
library(lubridate)
  
# Let's conduct an exploratory analysis of the data
head(edx)
dim(edx)
str(edx)
summary(edx)

# To see the number of users and movies
edx %>%
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
  
# What is the mean rating and mean rating per user?
mean(edx$rating)
mean_per_user <- edx %>%
  group_by(userId) %>%
  summarise(mean_rating = mean(rating))
mean(mean_per_user$mean_rating)
  
# Let's create a plot to visualise the distribution of ratings
edx %>%
  group_by(rating) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", colour = "black") +
  ggtitle("Rating Distribution") +
  xlab("Rating") +
  ylab("Occurrences Count")
# We see that users tend to favour higher ratings and whole number ratings
  
# Let's create another plot to visualise the number of movies rated per user
edx %>%
  group_by(userId) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", colour = "black") +
  ggtitle("Ratings per user") +
  xlab("Number of ratings per User") +
  ylab("Count") +
  scale_x_log10()
# We see that most users rate less than 100 movies
  
# Let's create another plot to visualise the number of ratings per movie
edx %>%
  group_by(movieId) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", colour = "black") +
  ggtitle("Ratings per movie") +
  xlab("Number of ratings per Movie") +
  ylab("Count") +
  scale_x_log10()
# We see that most movies are rated between 10 and 1000 times
  
# Let's see the top 10 rated movies
top10 <- edx %>% 
  group_by(movieId) %>%
  summarise(title = title[1], rating = mean(rating)) %>%
  top_n(10, rating) %>%
  arrange(desc(rating)) %>%
  slice(1:10)
top10
  
# These are pretty obscure movies. Let's see the number of ratings each movie has.
top10 %>%
  inner_join(edx, by = "movieId") %>%
  group_by(movieId, title.x) %>%
  summarise(count = n())
# Each has a pretty low number of ratings.

# Let's see if the same is true for the bottom 10 rated movies
bottom10 <- edx %>% 
  group_by(movieId) %>%
  summarise(title = title[1], rating = mean(rating)) %>%
  top_n(-10, rating) %>%
  arrange(rating) %>%
  slice(1:10)
bottom10

bottom10 %>%
  inner_join(edx, by = "movieId") %>%
  group_by(movieId, title.x) %>%
  summarise(count = n())
# It is still broadly true, with 6/10 of the worst rated movies having 1 or 2 ratings only.

# From str(edx), we see that the timestamp is an integer.
# Let's extract the year and conduct a yearly rating count
edx <- edx %>% 
  mutate(year = year(as_datetime(timestamp, origin = "1970-01-01")))
edx %>%
  group_by(year) %>%
  summarize(count = n())

# Let's create a plot to visualise the distribution of ratings per year
edx %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  ggplot(aes(x = year, y = n)) +
  geom_boxplot() +
  ggtitle("Ratings by Year") +
  scale_y_sqrt() +
  xlab("Year") +
  ylab("Number of Ratings")
# We see that newer movies have fewer ratings, as they haven't had sufficient time to accumulate ratings

# Let's create a plot to visualise the average distribution of ratings for each year
edx %>% 
  group_by(year) %>%
  summarise(avg = mean(rating)) %>%
  ggplot(aes(x = year, y = avg)) +
  geom_bar(stat = "identity", fill = "skyblue", colour = "black") + 
  ggtitle("Average rating per year") +
  xlab("Year") +
  ylab("Average rating")
# Pretty uniform ratings average each year. 
# We don't need to be concerned that newer movies with fewer ratings are rated more poorly.
# The anomaly is 1995, which only has 2 ratings.

# Now let's take a look at genres
edx %>% group_by(genres) %>%
  summarise(n())

# Many (or most) movies are listed as having multiple genres. 
# Let's look at the spread of individual genres.
genres <- c("Action", "Adventure", "Animation", 
            "Children", "Comedy", "Crime", 
            "Documentary", "Drama", "Fantasy", 
            "Film-Noir", "Horror", "Musical", 
            "Mystery", "Romance", "Sci-Fi", 
            "Thriller", "War", "Western")

genres_count <- data.frame(Genres = genres, Count = sapply(genres, function(x){
    sum(str_detect(edx$genres, x))
  })
)
genres_count %>% arrange(desc(Count))
# The most popular genre has almost 4 million ratings while the least popular has under 100,000 ratings.

# Let's create a plot to visualise genre popularity
genres_count %>%
  ggplot(aes(x = Count, y = reorder(Genres, Count))) +
  geom_bar(stat = "identity", width = 0.6, fill = "lightblue") +
  ggtitle("Genre Popularity") +
  xlab("Number of Ratings") +
  ylab("Genres")

# Let's look at the average rating for each genre
genres_rating <- data.frame(Genres = genres, Rating = sapply(genres, function(x){
  mean(edx %>%
    filter(str_detect(genres, x)) %>%
    pull(rating))
  })
)
genres_rating

# Now for the plot
genres_rating %>% ggplot(aes(x = Genres, y = (Rating))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  ggtitle("Average Rating by Genre") +
  xlab("Genre") +
  ylab("Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# There is some observable differences in average ratings for different genres

# Now that we are done with our exploratory analysis, let's create our training and test set
# Test set will be 10% of edx data
set.seed(123)
test_index <- createDataPartition(edx$rating, times = 1, p = 0.1, list = FALSE)
test_set <- edx[test_index, ]
train_set <- edx[-test_index, ]

# To make sure userId and movieId in test set are also in training set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Loading the rmse function from Metrics package
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
library(Metrics)

# Let's dive in! Here's the first model: average of all ratings
mu <- mean(train_set$rating)
mu

rmse_base <- rmse(test_set$rating, mu)
rmse_results <- tibble(method = "Mean baseline", rmse = rmse_base)
rmse_results

# Factoring in a movie bias
b_i <- train_set %>%
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

pred_i <- mu + test_set %>% 
  left_join(b_i, by="movieId") %>%
  pull(b_i)

rmse_i <- rmse(pred_i, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                 rmse = rmse_i))
rmse_results

# Factoring in user bias
b_u <- train_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu - b_i))

pred_i_u <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_i_u <- rmse(pred_i_u, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effect Model",
                                 rmse = rmse_i_u))
rmse_results

# Factoring in genre effects
train_genres <- data.frame(genres = genres, rating = sapply(genres, function(x){
  mean(train_set %>%
         filter(str_detect(genres, x)) %>%
         pull(rating))
})
)
train_genres

b_g <- data.frame(genres = genres, b_g = train_genres$rating - mu)

test_set_genres <- test_set %>%
  mutate(genres_list = strsplit(genres, "\\|")) %>%
  unnest(genres_list) %>%
  left_join(b_g, by = c("genres_list" = "genres")) %>%
  group_by(userId, movieId) %>%
  summarise(b_g = sum(b_g, na.rm = TRUE))

pred_i_u_g <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(test_set_genres, by=c("userId" = "userId", "movieId" = "movieId"), 
            relationship = "many-to-many") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

rmse_i_u_g <- rmse(pred_i_u_g, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre Effect Model",
                                 rmse = rmse_i_u_g))
rmse_results

# Let's apply regularisation to further finetune our model
# To find the optimal lambda using cross validation
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
    
  b_i_reg <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i_reg = sum(rating - mu)/(n()+l))
  
  b_u_reg <- train_set %>%
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u_reg = sum(rating - b_i_reg - mu)/(n()+l))
  
  pred_reg <- test_set %>%
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(pred = mu + b_i_reg + b_u_reg) %>%
    pull(pred)
  
  return(rmse(pred_reg, test_set$rating))
})

#to observe and obtain the optimal lambda
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]

# Having established the optimal lambda
b_i_reg <- train_set %>% 
group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u_reg <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  filter(!is.na(b_i)) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

pred_reg <- test_set %>% 
  left_join(b_i_reg, by='movieId') %>%
  left_join(b_u_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_reg <- rmse(pred_reg, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularised Movie + User Effect Model",
                                 rmse = rmse_reg))
rmse_results

# Now to apply matrix factorisation using the recosystem package
install.packages("recosystem")
library(recosystem)

# converting training and test sets into recosystem input format
set.seed(123)
train_recosystem <- with(train_set, 
                         data_memory(user_index = userId,
                                     item_index = movieId,
                                     rating = rating))
test_recosystem <- with(test_set, 
                        data_memory(user_index = userId,
                                    item_index = movieId,
                                    rating = rating))

# create, tune, and train model
reco_system <- Reco()
tuning <- reco_system$tune(train_recosystem,
                           opts = list(dim = c(10, 20, 30),
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       costp_l1 = 0, 
                                       costq_l1 = 0, 
                                       nthread  = 4,
                                       niter = 10))

reco_system$train(train_recosystem, 
                  opts = c(tuning$min, nthread = 4,niter = 100))

pred_mf <-  reco_system$predict(test_recosystem, out_memory())
rmse_mf <- rmse(pred_mf, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Matrix factorisation",
                                 rmse = rmse_mf))
rmse_results

# Now to apply matrix factorisation on our final holdout set
set.seed(123)
final_recosystem <- with(final_holdout_test, 
                         data_memory(user_index = userId,
                                     item_index = movieId))
pred_final <-  reco_system$predict(final_recosystem, out_memory())
rmse_final <- rmse(pred_final, final_holdout_test$rating)
rmse_final
