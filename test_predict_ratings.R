library('dplyr')
library('softImpute')
library('readr')
library('memoise')
library('tictoc')

setwd('~/signal/project/')

movies_df = read.csv("joined_movies_w_predictions.csv",stringsAsFactors = FALSE)
load(file="saved_objects/softimpute_fit_final_best.save")

#For some reason there are two entries for "Three Kings" (with two separate scripts). I should have noticed and removed this earlier, but I didn't.
#Remove both of them since I'm not sure which one is right.
movies_df = filter(movies_df, movieId != 2890)

#We want U*D*V^T, but V^T must be composed of our inferred ratings for our 827 movies

v = as.matrix(select(movies_df, X1_cvpred:X30_cvpred))
#This is not the efficient way to do matrix multiplication involving diagonal matrices
predictions_transformed = final_fit$u %*% diag(final_fit$d) %*% t(v)

#columns are movies, rows are users
gamma = attr(final_fit,"biScale:row")$scale
#Use this if you want to cheat and use the true scales:
#tau = movies_df$scale
#This one for using the predicted scales:
tau = movies_df$scale_cvpred
alpha = attr(final_fit,"biScale:row")$center
#beta = movies_df$center
beta = movies_df$center_cvpred

predictions = outer(gamma,tau)*predictions_transformed + outer(alpha,rep(1,length(tau))) + outer(rep(1,length(gamma)),beta)

#we need to run movie_lens.R to get ratings_full
ratings_scripts_only = ratings_full[ratings_full$movieId %in% movies_df$movieId,]
user_idx = ratings_scripts_only$userId
movieId_to_index = function (id) which(movies_df$movieId == id) #I thought memoizing this would make it go faster, but it actually makes it slower
movie_idx = sapply(ratings_scripts_only$movieId, movieId_to_index)

#These are predictions for which we have true ratings.
predictions_vec = sapply(1:length(user_idx), function(i) predictions[user_idx[i],movie_idx[i]])

rmse = function(x,y) sqrt(mean((x-y)^2))

rmse(predictions_vec,ratings_scripts_only$rating) #RMSE is 0.907

#Now let's see what happens if we use the true center and scale from biScale rather than our inferred values.
#This roughly corresponds to the case where we have enough ratings to estimate the average rating (and variance),
#but not enough to calculate the factors.

tau = movies_df$scale
beta = movies_df$center
predictions_cheating = outer(gamma,tau)*predictions_transformed + outer(alpha,rep(1,length(tau))) + outer(rep(1,length(gamma)),beta)
predictions_cheating_vec = sapply(1:length(user_idx), function(i) predictions_cheating[user_idx[i],movie_idx[i]])

rmse(predictions_cheating_vec,ratings_scripts_only$rating) #RMSE is 0.804

#
avg_rating_all = mean(ratings_scripts_only$rating)
#The most naive prediction is just predicting every user/movie as the average.
rmse(ratings_scripts_only$rating,avg_rating_all) #RMSE 1.006

#The next simplest thing is to look at it by user. Predict each user/movie rating by the average rating for that user.
by_user = group_by(ratings_scripts_only, userId)
user_means = summarize(by_user, mean(rating))
userId_to_mean_rating = memoise(function(id) user_means[user_means$userId == id,][[2]])
predictions_by_user = sapply(ratings_scripts_only$userId, userId_to_mean_rating)

rmse(ratings_scripts_only$rating,predictions_by_user) #RMSE is 0.914. This should be compared with my non-cheating RMSE.

#The next simplest thing is to predict a user/movie rating by averaging the average user rating with the average movie rating.
by_movie = group_by(ratings_scripts_only, movieId)
movie_means = summarize(by_movie, mean(rating))
movieId_to_mean_rating = memoise(function(id) movie_means[movie_means$movieId == id,][[2]])
movie_and_user_to_rating = function(userId,movieId) (userId_to_mean_rating(userId) + movieId_to_mean_rating(movieId))/2.0
predictions_by_usermovie = sapply(1:nrow(ratings_scripts_only), function(i) movie_and_user_to_rating(ratings_scripts_only$userId[i],ratings_scripts_only$movieId[i]))

#this should give (movie_avg - global_avg) + (user_avg - global_avg) + global_avg
#Basically assuming that the two deviations from the global_avg are additive.
#I think this should work better than than the first approach
predictions_by_usermovie2 = sapply(predictions_by_usermovie, function(p) 2*p - avg_rating_all)
#Should really do this using the tau and gamma from biScale. To make as direct a comparison as possible with my cheating RMSE.
alpha = attr(final_fit,"biScale:row")$center
beta = movies_df$center
movieId_to_beta = memoise(function(id) movies_df[movies_df$movieId == id,][["center"]])
predictions_by_usermovie3 = sapply(1:nrow(ratings_scripts_only), function(i) alpha[ratings_scripts_only$userId[i]] + movieId_to_beta(ratings_scripts_only$movieId[i]))

rmse(ratings_scripts_only$rating,predictions_by_usermovie) #0.876. These should be compared with my cheating RMSE.
rmse(ratings_scripts_only$rating,predictions_by_usermovie2) #0.845. These should be compared with my cheating RMSE.
rmse(ratings_scripts_only$rating,predictions_by_usermovie3) #0.846. These should be compared with my cheating RMSE.

#See the RMSE if we use the full imputations
#We  need to run movie_lens.R to get movie_levels to convert movieId to indices
#movie_levels converts indices to movieId. We want the opposite conversion.
movie_idx_final_fit = sapply(ratings_scripts_only$movieId, memoise(function (x) {
  a = which(x == movie_levels)
  ifelse(length(a)==1,a,NA)
}))
imputations = impute(final_fit,user_idx,movie_idx_final_fit)
rmse(ratings_scripts_only$rating, imputations) #0.661

#Try one last thing:
#What if I do the predictions, but with beta and tau derived from their averages value??
#columns are movies, rows are users
gamma = attr(final_fit,"biScale:row")$scale
#Use this if you want to cheat and use the true scales:
tau = rep(mean(movies_df$scale),length(movies_df$scale))
alpha = attr(final_fit,"biScale:row")$center
beta = rep(mean(movies_df$center),length(movies_df$center))

predictions_weird = outer(gamma,tau)*predictions_transformed + outer(alpha,rep(1,length(tau))) + outer(rep(1,length(gamma)),beta)

#These are predictions for which we have true ratings.
predictions_weird_vec = sapply(1:length(user_idx), function(i) predictions_weird[user_idx[i],movie_idx[i]])

rmse(predictions_weird_vec,ratings_scripts_only$rating) #RMSE is 0.931
