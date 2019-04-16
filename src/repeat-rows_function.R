
#***************************************************
# Function to repeat rows of a dataframe a specified number of time 
#***************************************************

# function defn: 
repeat.rows <- function(df, 
                        numreps) { 
      
      # input: any df
      # output: same df, duplicating rows 
      #     e.g. row number 1, 2, 3, then 1 again, 2 again, 3 again...
      
      library("magrittr")
      
      numcol <- ncol(df) # %>% print 
      orig.colnames <- colnames(df)
      
      list1 <- list()
      for (i in 1:numcol) {
            vec <- df[,i] %>% unname %>% unlist
            newvec <- rep(vec, numreps)
            
            list1[[i]] <- newvec
      }
      
      # combine vectors back into df: 
      df2 <- do.call(cbind, list1) %>% 
            as.data.frame() 
      
      
      df2 %<>% mutate(index = rep(1:nrow(df), numreps))
      
      colnames(df2) <- c(orig.colnames, "index") 
      
      return(df2)
      
}


# test fn: 
# repeat.rows(mtcars[1:3, ], 2)
# 
# repeat.rows(df1.week.schedule, 2)

