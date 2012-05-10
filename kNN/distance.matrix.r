distance.matrix <- function(df) {

  distance <- matrix(rep(NA, nrow(df) ^ 2), nrow=nrow(df))

                     for (i in 1:nrow(df))
                     {
                      for (j in 1:nrow(df)){
                      
                        distance[i,j] <- sqrt ((df[i, 'X'] - df[j, 'X']) ^ 2 + (df[i, 'Y'] - df[j, 'Y']) ^ 2 )
                      
                      } 
                     
                     }

return(distance)




}
