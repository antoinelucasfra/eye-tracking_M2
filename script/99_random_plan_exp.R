library(tidyverse)

# script for randomisation experimental planning

nb_stimuli <- 16
nb_consum <- 30

stimuli <- c(712,254,896,475,823,546,112,963,761,452,967,811,779,663,814,795)

consu <- 1:nb_consum

set.seed(667)

df_random <- matrix(stimuli,nrow = nb_consum, 
                    ncol = nb_stimuli,byrow = TRUE) 

df_random <- df_random %>% apply(1,sample) 
df_random <- t(df_random)

save(df_random, file = "experience/plan_exp.RData")
write.csv(df_random, file = "experience/plan_exp.csv")
