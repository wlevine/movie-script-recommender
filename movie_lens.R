library('dplyr')
library('softImpute')
library('readr')

setwd('~/signal/project/')

num_reviews_to_read = 20000000
ratings_full = read_csv('ml-20m/ratings.csv', col_names = TRUE, col_types = "iidi", n_max=num_reviews_to_read)

by_user = group_by(ratings_full, userId)
user_counts = summarize(by_user, length(rating))
min(user_counts[2]) #20
#Every user has rated at least 20 movies, so that's good

by_item = group_by(ratings_full,movieId)
item_counts = summarize(by_item, length(rating))
min(item_counts[2]) #1
#Many movies only have a few ratings. Remove them:
min_ratings = 10
good_items = item_counts[1][item_counts[2] >= min_ratings,]
good_items = good_items[[1]] #convert tibble to vector
ratings_small = filter(ratings_full, movieId %in% good_items)

by_user2 = group_by(ratings_small, userId)
user_counts2 = summarize(by_user2, length(rating))
min(user_counts2[2]) #16
#Even after removing movies, all users still have at least 16 ratings.
#We should be able to remove 20% of the data and still have all users/movies having at least 2 ratings

num_users = length(unique(ratings_small$userId)) #138493
num_movies = length(unique(ratings_small$movieId)) #15451

#Since we have removed some movies, the ids will not correspond exactly to indices.
#Get indices by converting to factors and then to numerics.
#Since we didn't remove any users, we only need to this for movies, not items.
movie_factors = factor(ratings_small$movieId)
movie_levels = levels(movie_factors) #this can be used to convert indices back to the original movie id
ratings_small$movie_idx = as.integer(movie_factors)

#Make a training set with 80% of the data
train_rows = sample(nrow(ratings_small),nrow(ratings_small)*0.8)
ratings_train = ratings_small[train_rows,]
ratings_test = ratings_small[-train_rows,]

ratings_matrix = Incomplete(ratings_train$userId,ratings_train$movie_idx,ratings_train$rating)
scaled_ratings_matrix = biScale(ratings_matrix, maxit=5, trace=TRUE)

lam0 = lambda0(scaled_ratings_matrix) #406.9
lams=exp(seq(from=log(lam0),to=log(1),length=20))
results = data.frame(lambda = lams, rank = NA, rmse = NA)
fits = vector('list', length(lams))

#11:48
fit_test = softImpute(scaled_ratings_matrix, rank.max = 30, maxit=1000, lambda = 30, trace.it = TRUE)
