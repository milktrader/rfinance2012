knn <- function(df, k=5) {

  distance <- distance.matrix(df)
  predictions <- rep(NA, nrow(df))

  for (i in 1:nrow(df)){
  
    indices <- k.nearest.neighbors(i, distance, k=k)
    predictions[i] <- ifelse(mean(df[indices, 'Label']) > 0.5 ,1,0) 
  
  }

  return(predictions)

}
