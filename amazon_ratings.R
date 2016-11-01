library('dplyr')
library('softImpute')
library('readr')

setwd('~/signal/project/')

num_reviews_to_read = 1000000
books_df = read_csv('amazon_data/ratings_Books.csv', col_names = c("user","item","rating","time"), col_types = "ccdi", n_max=num_reviews_to_read)

#remove all items and users that have fewer than X reviews
min_ratings_per = 10

by_user = group_by(books_df, user)
user_counts = summarize(by_user, length(rating))

good_users = user_counts[1][user_counts[2] >= min_ratings_per,]
good_users = good_users[[1]] #convert from tibble to vector
books_small = filter(books_df, user %in% good_users)

by_item = group_by(books_small,item)
item_counts = summarize(by_item, length(rating))

good_items = item_counts[1][item_counts[2] >= min_ratings_per,]
good_items = good_items[[1]]
books_smaller = filter(books_small, item %in% good_items)
#Remove items after removing users will leave us with some users with <10 reviews, but I think this is fine
#Though, we do need to remove any thing with exactly one rating, so that biScale will work

while (TRUE) {
  by_user = group_by(books_smaller, user)
  user_counts = summarize(by_user, length(rating))
  by_item = group_by(books_smaller, item)
  item_counts = summarize(by_item, length(rating))
  
  if (sum(item_counts[2] <= 1)==0 && sum(user_counts[2] <= 1)==0) {
    break
  }
  
  good_users = user_counts[1][user_counts[2] >= 2,]
  good_users = good_users[[1]]
  books_smaller = filter(books_smaller, user %in% good_users)
  
  by_item = group_by(books_smaller,item)
  item_counts = summarize(by_item, length(rating))
  
  good_items = item_counts[1][item_counts[2] >= 2,]
  good_items = good_items[[1]]
  books_smaller = filter(books_smaller, item %in% good_items)
}

#need to split into test and train

num_users = length(unique(books_smaller$user))
num_items = length(unique(books_smaller$item))

#convert to factors then to numerics, to get indices for Incomplete
users_factors = factor(books_smaller[["user"]])
users_levels = levels(users_factors)
books_smaller$user_idx = as.numeric(users_factors)

items_factors = factor(books_smaller[["item"]])
items_levels = levels(items_factors)
books_smaller$item_idx = as.numeric(items_factors)

ratings_matrix = Incomplete(books_smaller$user_idx, books_smaller$item_idx, books_smaller$rating)
scaled_ratings_matrix = biScale(ratings_matrix, maxit=5, trace=TRUE)

lam0 = lambda0(scaled_ratings_matrix) #33.6
lams=exp(seq(from=log(lam0),to=log(1),length=20))
results = data.frame(lambda = lams, rank = NA, rmse = NA)
fits = vector('list', length(lams))

rmse = function(x,y) sqrt(mean((x-y)^2))

max_rank = 30
for (i in 1:length(lams)) {
  print(i)
  if (i==1) {
    fits[[i]] = softImpute(scaled_ratings_matrix, rank.max = max_rank, maxit=1000, lambda = lams[i])
  } else {
    fits[[i]] = softImpute(scaled_ratings_matrix, rank.max = max_rank, maxit=1000, lambda = lams[i], warm.start = fits[[i-1]])
  }
  d = fits[[i]]$d
  #d contains the diagonal elements of D
  results[i,'rank'] = sum(round(d,digits=4)!=0)
  imputations = impute(fits[[i]],books_smaller$user_idx,books_smaller$item_idx)
  results[i,'rmse'] = rmse(imputations, books_smaller$rating)
}
results
best_svd = fits[[7]]

items_levels[order(best_svd$v[,1])]
