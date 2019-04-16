


#*****************************************
# FUNCTION TO TAKE DAY NUMBER, SURG TYPE AND RETURN LOS VECTOR  
#*****************************************


# function definition: 
census.in.surgery <- function(horizon = 100, 
                           day.number, 
                           surg.type, 
                           num.patients, 
                           surgery.los.dist = df3.los.distributions){
      
      # inputs: 
      # > horizon specifies the length of output vectors 
      # > 3 vectors from the surgery schedule specifying startdate of 
      #     time in census, surg type, and num patients 
      #     >> note that num.patients arg refers to SDA patients - i.e. their LOS is >= 1
      # > surgery.los.dist has colnames variable, value, losdays 
      #     >> variable: "Ortho", "Neuro", ...
      #     >> value: probability value
      #     >> losdays: integer
      
      # output: 
      # for each patient, first generate a vector to identify which 
      # days in the census range the patient will be in the 
      # hospital 
      # Then, sum across patients to get the results for a single row 
      #     of the input schedule 
      
      library("tidyverse")
      library("glue")
      
      # generate empty census vector: 
      horizon.vec <- rep(0, horizon)
      
      # return 0 for census nights, if there were no surgeries
      if (num.patients == 0) {
            return(horizon.vec)
      }
      
      # convert surg.type arg to char
      surg.type = as.character(surg.type)
      
      
      # for each patient, generate a string of 1's to indicate 
      #     when they were in the hospital: 
      patient.census.list <- list()  # empty list 
      
      # iterate over patients to fill the list: 
      for (i in 1:num.patients) {
            # find appropriate surgery
            surg.los <- filter(df3.los.distributions, 
                                variable == glue({surg.type}))
            
            # how many nights in census? Based on historical los distribution
            random.los <- sample(surg.los$losdays, 
                                 1, 
                                 prob = surg.los$value)
            
            # for testing: 
            # print(paste0("los = ", random.los))
            # print(paste0("start.day = ", day.number))
            
            # a vector with 1 for every night a pt is in census: 
            if (random.los > 0) {
                  census.vector <- rep(1, random.los)
                  
                  # place this vector within the right timeframe: 
                  census.vec.in.horizon <- c(rep(0, day.number-1), 
                                             census.vector, 
                                             rep(0, (horizon - day.number - random.los + 1)))
                  
                  patient.census.list[[i]] <- census.vec.in.horizon
                  
            } else {
                  # if patient spends no nights in census, no change necessary: 
                  patient.census.list[[i]] <- horizon.vec
            }
      }  
      
      
      #************************************************
      # if more than 1 patient, sum their results:
      #************************************************
      patient.census.df <- do.call(cbind, patient.census.list) %>%
            unname %>%
            as.data.frame()
      
      # print("DATAFRAME FOR ALL PATIENTS")
      # print(patient.census.df)
      # str(patient.census.df)
      
      # print("ROWSUM OVER ALL PATIENTS")
      rowsum <- rowSums(patient.census.df)
      # str(rowsum)
      
      return(rowsum)
       
}




#******************************************************
# test the function: 
#******************************************************
# census.in.surgery(horizon = 200,
#                   day.number =  5,
#                   "Ortho",
#                   num.patients = 1)
# 
# census.in.surgery(horizon = 200,
#                   day.number =  1,
#                   "Gastro",
#                   num.patients = 10)
# 
# census.in.surgery(horizon = 200,
#                   day.number =  2,
#                   "Ortho",
#                   num.patients = 10)
# 
# 
# census.in.surgery(horizon = 200,
#                   day.number =  90,
#                   "Vascular",
#                   num.patients = 10)
# 
# 
# census.in.surgery(horizon = 200,
#                   day.number =  2,
#                   "Dental",
#                   num.patients = 0)
 
 
 
# using mapply: 
# mapply(census.in.surgery,
#        horizon = 600,
#        day.number = df4.full.input.schedule$day.number[1:20],
#        surg.type = as.character(df4.full.input.schedule$surgtype[1:20]),
#        num.patients = df4.full.input.schedule$num.sda[1:20])

# each column is for a row in the full schedule: 
# example: rows 1:10 in the schedule are for day 1 - ortho, neuro, uro, gyn, and gensurge
#     rows 11:20 are for day 2 - ortho, neuro, uro, gyn, and gensurge
#     etc. 


#******************************************************************
# collecting all results in a matrix: 
#******************************************************************
# surge.census <- mapply(census.in.surgery,
#                       horizon = 600,
#                       day.number = df4.full.input.schedule$day.number,
#                       surg.type = as.character(df4.full.input.schedule$surgtype),
#                       num.patients = df4.full.input.schedule$num.sda)
#  
# surge.census
# str(surge.census)  #todo: what is this?? 2D vector?
# class(surge.census)  # Ans. it's a matrix!
#  
# # final trajectory of patient census, based on the schedule: 
# rowSums(surge.census)
 
 
 
#*****************************************************
# plot result: 
#*****************************************************
# total.census.df <- as.data.frame(rowSums(surge.census)) %>%
#       mutate(day = seq(nrow(.)))
# 
# colnames(total.census.df) <- c("surgery.census",
#                                "day.number")
# 
# total.census.df %>%
#       filter(day.number < 100) %>%
#       ggplot(aes(x = day.number,
#            y = surgery.census)) +
#       geom_line(col = "dodgerblue4") +
#       geom_vline(xintercept = 84,
#                  col = "firebrick1") +
#       scale_y_continuous(limits = c(0, 200)) +
#       labs(title = "LGH - Census in surgery units for 12 weeks of given surgery schedule",
#            subtitle = paste0("Generated: ", Sys.time())) +
#       theme_classic(base_size = 14)


