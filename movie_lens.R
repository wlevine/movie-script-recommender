library('dplyr')
library('softImpute')
library('readr')

setwd('~/signal/project/')

num_reviews_to_read = 20000000
ratings_full = read_csv('ml-20m/ratings.csv', col_names = TRUE, col_types = "iidi", n_max=num_reviews_to_read)
movies = read.csv('ml-20m/movies.csv',stringsAsFactors = FALSE)

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
scaled_ratings_matrix = biScale(ratings_matrix, maxit=10, trace=TRUE)

lam0 = lambda0(scaled_ratings_matrix) #406.9
lams = exp(seq(from=log(lam0),to=log(1),length=10))
results = data.frame(lambda = lams, rank = NA, rmse = NA)
fits = vector('list', length(lams))

#Use thresh=3e-5 (higher than default convergence fit threshold), to make the fits run faster.
for (i in 1:length(lams)) {
  if (i==1) {
    fits[[i]] = softImpute(scaled_ratings_matrix, rank.max = 30, maxit=1000, lambda = lams[i], thresh = 3e-5, trace.it = TRUE)
  } else {
    fits[[i]] = softImpute(scaled_ratings_matrix, rank.max = 30, maxit=1000, lambda = lams[i], thresh = 3e-5, trace.it = TRUE, warm.start = fits[[i-1]])
  }
}

save(fits, file="saved_objects/softimpute_fits_final.save")

rmse = function(x,y) sqrt(mean((x-y)^2))

for (i in 1:length(lams)) {
  d = fits[[i]]$d
  #d contains the diagonal elements of D
  results[i,'rank'] = sum(round(d,digits=4)!=0)
  imputations = impute(fits[[i]],ratings_test$userId,ratings_test$movie_idx)
  results[i,'rmse'] = rmse(imputations, ratings_test$rating)
}
#The best fit is with lambda=28, where the RMSE is 0.7955. The RMSE does not vary very quickly around the minimum,
#so doing only 10 values of lambda was probably sufficient.
#This is better than we did with the ml-1m dataset, where the RMSE was 0.857

#Now re-run the fit with the best lambda value, this time using the default convergence value.
#To be truly complete, once I have selected lambda, I should rerun using 100% of the original data
final_fit = softImpute(scaled_ratings_matrix, rank.max = 30, maxit = 1000, lambda = lams[5], trace.it = TRUE, warm.start = fits[[5]])
final_imputations = impute(final_fit,ratings_test$userId,ratings_test$movie_idx)
rmse(final_imputations,ratings_test$rating)
#With a lower convergence threshold, the RMSE drops from 0.7955 to 0.7946
save(final_fit, file="saved_objects/softimpute_fit_final_best.save")

list_top_and_bottom = function(factor, v,n=5) {
  o = order(v[,factor], decreasing = TRUE)
  for (i in c(1:5,(length(o)-4):length(o))) {
    cat(i, ". ", movies$title[movies$movieId == movie_levels[o[i]]], "\n",sep="")
    if (i==5) {
      cat("...\n")
    }
  }
}

list_top_and_bottom(1,final_fit$v)

#add factors on to movies data frame
movies = cbind(movies, data.frame(matrix(ncol=30,nrow=nrow(movies))))
for (i in 1:num_movies) {
  movieId = movie_levels[i]
  df_idx = which(movies$movieId == movieId)
  movies[df_idx,4:33] = final_fit$v[i,]
  if (i %% 1000 == 0) { print(i) }
}

#join together movies from the movielens database with movies we have scripts for
script_df = read.csv('raw/movie-data.txt', header=FALSE, sep="\t", stringsAsFactors = FALSE, col.names = c("title","genre","filename"))

#try to process titles so that they will be the same between the two datasets
#Possible future improvements:
#Add Year to the script_df to allow matching by year to prevent multiple matches
#Better handling of punctuation (?!:-)
#Convert digits to spelled-out-numbers
process_title = function(title) {
  title = tolower(title)
  #remove everything in parentheses
  title = gsub("\\(.*?\\)","",title)
  #remove leading and trailing 'the's and 'a's
  title = sub("^\\s*the\\s+","",title)
  title = sub("\\s+the\\s*$","",title)
  title = sub("^\\s*a\\s+","",title)
  title = sub("\\s+a\\s*$","",title)
  title = sub("^\\s*an\\s+","",title)
  title = sub("\\s+an\\s*$","",title)
  #remove trailing commas
  title = sub(",\\s*$","",title)
  #remove leading and trailing spaces
  title = sub("^\\s+","",title)
  title = sub("\\s+$","",title)
  #& to "and"
  title = sub("&","and",title)
  #multiple spaces to one space
  title = sub("\\s+"," ",title)
}

#process the titles and store them in a new column
script_df$title_p = sapply(script_df$title,process_title)
movies$title_p = sapply(movies$title,process_title)

#check for matches
num_titles_in_ml = table(movies$title_p)
script_df$matches = sapply(script_df$title_p, function(title) {
  a=num_titles_in_ml[title]
  ifelse(is.na(a),0,a)
})

#We're only interested in the movie if there's exactly one match between script and ratings db
script_df = filter(script_df, matches == 1)
script_df = select(script_df, -matches)
colnames(script_df) = c("title_script","genre_script","filename","title_p")
movies_joined = inner_join(script_df, movies, "title_p")

#Quick sanity check to make sure the movies are matched correctly by looking at their genres
genres1 = lapply(movies_joined$genre_script,function(genres) strsplit(genres,",")[[1]])
genre_matches = sapply(1:nrow(movies_joined),function(i) {
  match = FALSE
  for (genre in genres1[[i]]) {
    match = match || grepl(genre,movies_joined$genres[i])
  }
  match
})
movies_joined[!genre_matches,]
#Checking manually: 12, informant, revenant are false matches, we must remove these
#jacob's ladder, jimmy and judy, sexual life are all good matches, leave them in
movies_joined = filter(movies_joined, title_p != "12" & title_p != "informant" & title_p != "revenant")

#check briefly which of these have NaN factors (from insufficent reviews)
movies_joined[is.na(movies_joined["X1"]),"title"]

#Now I want to add in the genome information.
#Really this should probably be done before the join, but I'm doing it here to save time/memory.
genome_tags = read.csv("ml-20m/genome-tags.csv", stringsAsFactors = FALSE)
genomes_for_joined_movies = data.frame(matrix(nrow=nrow(movies_joined),ncol=nrow(genome_tags)))
#I wanted to give these columns meaningful names, but there are some issues. After cleaning up the tag names
#there are some duplicates like stop_motion and stop_motion. So instead I will just give them names G1,G2,.. (G for genome),
#while the factors are X1,X2...
colnames(genomes_for_joined_movies) = paste0("G",1:ncol(genomes_for_joined_movies))

genome_scores = read_csv('ml-20m/genome-scores.csv', col_names = TRUE, col_types = "iid")

for(i in 1:nrow(movies_joined)) {
  id = movies_joined[i,"movieId"]
  temp_df = genome_scores[genome_scores$movieId == id,]
  if (nrow(temp_df)==0) { next }
  for (row in 1:nrow(temp_df)) {
    j = temp_df[[row,"tagId"]]
    genomes_for_joined_movies[i,j] = temp_df[[row,"relevance"]]
  }
  if (i %% 50 == 0) {print(i)}
}

rowsums_na = rowSums(is.na(genomes_for_joined_movies))
sum(rowsums_na == 0)
sum(rowsums_na == 1128)
#860 have full genome information, 33 have no genome, none have partial genome

movies_joined = cbind(movies_joined, genomes_for_joined_movies)

#write out joined movies with factors
write.csv(movies_joined,file="joined_movies_w_factors_and_genomes.csv",row.names=FALSE)

#The first time around I forgot to write out the movie centers and scales which are super important.
#So do that here.
#Before doing this, we must rerun up to ?

#we need the final_fit object to get the scalings
load(file="saved_objects/softimpute_fit_final_best.save")

to_fix_up = read.csv("joined_movies_w_factors_and_genomes.csv",stringsAsFactors = FALSE)

#movie_levels converts indices to movieId. We want the opposite conversion.
indices = sapply(to_fix_up$movieId, function (x) {
  a = which(x == movie_levels)
  ifelse(length(a)==1,a,NA)
  })

to_fix_up$center = attr(final_fit,"biScale:column")$center[indices]
to_fix_up$scale = attr(final_fit,"biScale:column")$scale[indices]