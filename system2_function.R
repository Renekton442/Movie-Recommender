# system2_func.R

library(dplyr)
library(data.table)
library(Matrix)
library(plyr)
library(recommenderlab)


rate = readLines("ratings.dat")
rate = strsplit(rate, split = "::", fixed = TRUE, useBytes = TRUE)  
rate = matrix(unlist(rate), ncol = 4, byrow = TRUE)  
rate = data.frame(rate, stringsAsFactors = FALSE)  
colnames(rate) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
rate = rate[, -4]

sampleIDs = sample(rate$UserID, size = 1000)
rate = rate[rate$UserID %in% sampleIDs, ]


system2 = function(input, n = 10){
  actrats = cbind(9999, input)
  colnames(actrats) = c('UserID', 'MovieID', 'Rating')
  rate = rbind(rate, actrats)
  
  Rmat = create_rating_matrix(rate)
  actID = paste0('u', 9999)
  actRow = which(rownames(Rmat) == actID)
  
  svd.mod = Recommender(Rmat,
                        method = 'SVD',
                        parameter = list(normalize = 'center', k = 25, maxiter = 50))
  pred = predict(svd.mod, Rmat[actRow,], n = n)
  
  predlist = as(pred, "list")[[1]]
  ids = unlist(lapply(predlist, help_func))
  return(ids)
}

# retrive real ID
help_func = function(string){
  return(substr(string, 2, nchar(string)))
}

create_rating_matrix = function(ratings_df){
  
  u = paste0('u', ratings_df$UserID) #user number ...
  m = paste0('m', ratings_df$MovieID) #movie number ...
  x = ratings_df$Rating
  
  #nessecary to prevent sparseMatrix freaking out over i + j 
  tmp = data.frame(u, m, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$u), as.integer(tmp$m), x = as.integer(tmp$x))
  
  rownames(Rmat) = levels(tmp$u)
  colnames(Rmat) = levels(tmp$m)
  
  Rmat = new('realRatingMatrix', data = Rmat)
  return(Rmat)
}