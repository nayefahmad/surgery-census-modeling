

#*******************************************************
# FUNCTION TO GENERATE GRAPHS FROM SIMULATIONS 
#*******************************************************

sim.graphs <- function(sim.list, 
                       ymax = 75, 
                       xmax = 7*numweeks.param + 14) {
      
      # input: single vector from a list of lots of simulations produced by 
      #     fn simulate.census() 
      #     > this list will prob be named "sims"
      
      # output: graphs
      
      library("tidyverse")
      library("ggplot2")
      
      plots.list <- list()
      for (i in 1:length(sim.list)) {
            total.census.df <- as.data.frame(sim.list[[i]]) %>% 
                  mutate(day = seq(nrow(.)))
            
            colnames(total.census.df) <- c("surgery.census", 
                                           "day.number")
            
            plot <- 
                  total.census.df %>% 
                  filter(day.number < xmax) %>% 
                  ggplot(aes(x = day.number, 
                             y = surgery.census)) + 
                  
                  geom_line(col = "dodgerblue4") +
                  
                  # mark cooldown and warmup periods: 
                  geom_vline(xintercept = 7*numweeks.param, 
                             col = "firebrick1") + 
                  geom_vline(xintercept = warmup.cutoff.day.num, 
                             col = "firebrick1") + 
                  
                  scale_y_continuous(limits = c(0, ymax)) + 
                  
                  labs(title = paste0("LGH - Census in surgery units for ",
                                      numweeks.param, 
                                      " weeks of given surgery schedule"), 
                       subtitle = paste0("Generated: ", 
                                         Sys.time(), 
                                         "\nIteration number: ", 
                                         i)) + 
                  theme_classic(base_size = 14)
            
            print(plot)
            
            plots.list[[i]] <- plot
            
      }
      return(plots.list)
      
            
      
}


#****************************************************************
# test the function: 
#****************************************************************
# sim.graphs(sims[1:10], ymax = 200)
