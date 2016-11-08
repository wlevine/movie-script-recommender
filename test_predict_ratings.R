library('dplyr')
library('softImpute')
library('readr')

setwd('~/signal/project/')

movies_df = read.csv("joined_movies_w_predictions.csv",stringsAsFactors = FALSE)
load(file="saved_objects/softimpute_fit_final_best.save")

#We want U*D*V^T, but V^T must be composed of our inferred ratings for our 827 movies

v = as.matrix(select(movies_df, X1_cvpred:X30_cvpred))
predictions = final_fit$u[1:200,] %*% diag(final_fit$d) %*% t(final_fit$v[1:100,])

v[1:5,]
final_fit$v[1:5,]
