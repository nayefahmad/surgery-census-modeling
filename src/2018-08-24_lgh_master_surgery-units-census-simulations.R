

#*****************************************************
# LGH SURGERY CENSUS SIMULATIONS 
#*****************************************************
# 2018-08-23
# Nayef Ahmad 

library("tidyverse") 
library("here")
library("reshape2")
library("magrittr")

# rm(list = ls())

# todo: ------------------




# input paramaters: 
numweeks.param <-  48
iterations.param <- 100
warmup.cutoff.day.num <- 50
input.schedule <- "surgery-weekly-schedule.csv"

# ouput label: 
schedule.num <- 3


# load data and functions: 
source(here("src", 
            "2018-08-23_lgh_clean-data-for-or-optimization.R"))
source(here("src", 
            "census.in.surgery_function.R"))
source(here("src", 
            "simulate.census_function.R"))
source(here("src", 
            "simulation.graphs_function.R"))



# generate a list of simulations: 
sims <- simulate.census(iterations = iterations.param)


# generate graphs: 
# sim.graphs(sims, ymax = 200)

# save plots in a list: 
plots.list <- sim.graphs(sims, ymax = 50)


# calculate averages by dow: ------------------------
sims.matrix <- do.call(cbind, sims)
sims.averages <- rowSums(sims.matrix)/iterations.param

sims.averages.df <- sims.averages %>% 
      as.data.frame() %>% 
      slice(warmup.cutoff.day.num:(7*numweeks.param)) %>%  # exclude first week for warm-up; note that this doesn't include the last 14 days (cooldown period)
      mutate(dayofweek = days.vec[warmup.cutoff.day.num:(7*numweeks.param)]) %>% 
      group_by(dayofweek) %>% 
      rename(avg.census = ".") %>% 
      summarize(mean(avg.census)) %>% 
      rename(daily.avg.census = "mean(avg.census)")

sims.averages.df

# plot averages by dow: 
p1.avg.by.dow <- 
      sims.averages.df %>%
      ggplot(aes(x = dayofweek, 
                 y = daily.avg.census)) + 
      geom_bar(stat = "identity", 
               fill = "dodgerblue3") + 
      scale_y_continuous(limits = c(0,35)) + 
      labs(title = "LGH - Average census in surgery units by day of week", 
           subtitle = paste0("Based on ",
                             iterations.param, 
                             " iterations from given surgery schedule, over ", 
                             numweeks.param, 
                             " weeks \n", 
                             "Warmup period: ", 
                             warmup.cutoff.day.num -1, 
                             " days", 
                             "Schedule number: ", 
                             schedule.num)) + 
      theme_classic(base_size = 14); p1.avg.by.dow
                   




#*********************************************************************
# write outputs: 
pdf(here("results", 
         "output from src", 
         as.character(glue("2018-09-13_simulations_schedule-{schedule.num}.pdf"))), 
    width = 10)
plots.list[1:iterations.param]
dev.off()


ggsave(here("results", 
            "output from src", 
            as.character(glue("2018-09-13_avg-by-day-of-week_schedule-{schedule.num}.pdf"))), 
       p1.avg.by.dow)


write_csv(sims.averages.df, 
          here("results",
               "output from src",
               as.character(glue("2018-09-13_avg-by-day-of-week_schedule-{schedule.num}.csv"))))
