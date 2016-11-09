library('dplyr')
library('softImpute')
library('readr')

setwd('~/signal/project/')

movies_df = read.csv("joined_movies_w_predictions.csv",stringsAsFactors = FALSE)
load(file="saved_objects/softimpute_fit_final_best.save")

#We want U*D*V^T, but V^T must be composed of our inferred ratings for our 827 movies

v = as.matrix(select(movies_df, X1_cvpred:X30_cvpred))
predictions_transformed = final_fit$u[1:200,] %*% diag(final_fit$d) %*% t(v[1:100,])

#this requires movie_levels from the movie_lens.R file
indices = sapply(movies_df$movieId[1:100], function (x) {
  a = which(x == movie_levels)
  ifelse(length(a)==1,a,NA)
})

#columns are movies, rows are users
gamma = attr(final_fit,"biScale:row")$scale
tau = attr(final_fit,"biScale:column")$scale[indices]
alpha = attr(final_fit,"biScale:row")$center
beta = attr(final_fit,"biScale:column")$center[indices]

predictions = matrix(nrow=200,ncol=100)
for (i in 1:200) {
  for (j in 1:100) {
    predictions[i,j] = gamma[i]*tau[j]*predictions_transformed[i,j] + alpha[i] + beta[j]
  }
}
