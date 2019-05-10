


#********************************************
# IMPORT DATA FOR LGH SURGERY SCHEDULE SMOOTHING 
#********************************************
# 2018-08-23
# Nayef Ahmad 

library("tidyverse") 
library("here")
library("reshape2")
library("magrittr")
library("glue")

# rm(list = ls())

# todo: ---------------


# source functions: 
source(here("src", 
            "01_helper-functions.R"))



# import weekly schedule: ---------
options(readr.default_locale=readr::locale(tz="America/Los_Angeles"))

df1.week.schedule <- 
      read_csv(here("data", 
                    as.character(glue({input.schedule}))))  # input.schedule is the filename is from the master file

str(df1.week.schedule)      




# generate full schedule dataset: ----------------
# how many weeks? 
numweeks <- numweeks.param  # todo: if you haven't run the master file yet, just assign a value: numweeks.param <- 12
# how many surgery types in input data? 
num.surg.types <- unique(df1.week.schedule$surgtype) %>% length

# generate schedule: 
df4.full.input.schedule <- repeat.rows(df1.week.schedule, 
                                       numweeks) %>% 
      mutate(day.number = lapply(1:(7*numweeks), 
                                 rep, 
                                 each = num.surg.types) %>% unlist, 
             num.sda = as.character(num.sda) %>% as.integer)

str(df4.full.input.schedule)



# import probability distributions of LOS in surgery units: ------
df2.los.distributions.raw <- 
      read_csv(here("data", 
                    "surgery-LOS-by-surgery-type.csv")) 

# isolate los column: 
los <- df2.los.distributions.raw[, 1]
      
# set up prob distributions in long format: 
df3.los.distributions <- 
      df2.los.distributions.raw %>% 
      melt() %>% 
      filter(variable != "LOSDays") %>% 
      mutate(losdays = rep(los, num.surg.types) %>% 
                   unname %>% 
                   unlist) %>% 
      droplevels()


# df3.los.distributions
str(df3.los.distributions)




# generate days of week: ----------
days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
days.vec <- factor(rep(days, numweeks.param), 
                   levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))



# write output: ------------
write_csv(df4.full.input.schedule,
          here("results",
               "dst",
               "full-schedule-from-input-data.csv"))



