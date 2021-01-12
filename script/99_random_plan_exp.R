# script for randomisation experimental planning

nb_stimuli <- 16
nb_consum <- 30

stimuli <- LETTERS[1:nb_stimuli]
consu <- 1:nb_consum

df_random <- data

set.seed(667)

df_random <- matrix(stimuli,nrow = nb_consum, 
                    ncol = nb_stimuli,byrow = TRUE) 

df_random <- df_random %>% apply(1,sample) 
df_random <- t(df_random)

save(df_random, file = "experience/plan_exp.RData")
write.csv(df_random, file = "experience/plan_exp.csv")
