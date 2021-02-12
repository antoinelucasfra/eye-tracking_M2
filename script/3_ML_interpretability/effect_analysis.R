#### effect analysis ####

# loading labeled data 
time_user_exp  = read.csv("data/time_user_exp.csv", sep = ",", header = TRUE)
col = colnames(time_user_exp)

# create the id (like .png start name) 
time_user_exp$nom <- paste0(time_user_exp$ordre,"-",time_user_exp$nom)

# keep only selected columns
time_user <- time_user_exp %>% select(nom, starts_with("stimu"),ends_with("liking")) 

# manage colnames 
colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)] <- paste0(colnames(time_user)[grepl("stimu", colnames(time_user), fixed = T)],"_label")
colnames(time_user) <- str_replace_all(colnames(time_user),"X","stimu")

# convert into character
time_user <- as.data.frame(apply(X =  time_user, FUN = as.character, MARGIN = 2))

# make a pivot table to have the right shape of the data
time_anova <- time_user %>% 
  pivot_longer(cols = -nom,
               names_to = c("stimu",".value"),
               names_sep = "_",
               values_ptypes = list(.value=character())) %>% 
  select(-stimu)


# barplot for liking data

time_anova %>% group_by(label,liking) %>% 
  count()

time_anova %>% group_by(label,liking) %>% 
  count() %>% 
  ggplot() +
  geom_bar(aes(x=label,y=n,fill=liking), stat = "identity") +
  geom_vline()


# test for the stimuli effect
table(time_anova$label,time_anova$liking)
chisq.test(time_anova$liking,time_anova$label)


# there is a stimuli effect ! 

# test for the judge effect
table(time_anova$nom,time_anova$liking)
chisq.test(time_anova$liking,time_anova$nom)

# no judge effect 

mod <- glm(liking~label+nom,data=time_anova,family = "binomial")





