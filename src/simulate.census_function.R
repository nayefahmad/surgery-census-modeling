

#*************************************************************
# FUNCTION TO SIMULATE SURGERY UNITS CENSUS X NUMBER OF TIMES 
#*************************************************************

# function definition: 
simulate.census <- function(iterations = 2, 
                            horizon = 600, 
                            day.number = df4.full.input.schedule$day.number, 
                            surg.type = as.character(df4.full.input.schedule$surgtype), 
                            num.patients = df4.full.input.schedule$num.sda) {
      # input: 
      #     > number of iterations 
      #     > other arguments set to defaults, to be passed to census.in.surgery() 
      
      # output: 
      #     > list of vectors, each vector is the result of 1 simulation
      
      # check: 
      if (!exists("census.in.surgery")) {
            "ERROR: load function census.in.surgery"
      }
      
      
      # empty list to collect simulations: 
      census.simulations <- list()
      for (i in 1:iterations) {
            surge.census <- mapply(census.in.surgery, 
                                   horizon, 
                                   day.number, 
                                   surg.type, 
                                   num.patients)
            
            census.simulations[[i]] <- rowSums(surge.census)
            
            
      }
      
      return(census.simulations)
      
      
}


# function test: 
# sims <- simulate.census(iterations = 10)
# str(sims)
