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
  count() %>% 
  ggplot() +
  geom_bar(aes(x=label,y=n,fill=liking), stat = "identity") +
  geom_hline(yintercept = 17, linetype="dashed") +
  scale_fill_manual(values = c("green","red")) +
  annotate("text",x=5,y=17.5,label="Appréciation_moyenne",size=4,parse=TRUE) +
  xlab("stimuli") +
  ylab("Nombre d'appréciations par stimuli") +
  theme_classic()
  
  
mean_c <- time_anova %>% group_by(label,liking) %>% count() %>% ungroup() %>% 
  summarise(mean_c = mean(n))

time_anova %>% group_by(nom,liking) %>% 
  count() %>% 
  ggplot() +
  geom_bar(aes(x=nom,y=n,fill=liking), stat = "identity") +
  geom_hline(yintercept = 17) +
  scale_fill_manual(values = c("green","red")) 
  # annotate("text",x=5,y=17.5,label="Average_liking",size=4,parse=TRUE)


# test for the stimuli effect
table(time_anova$label,time_anova$liking)
chisq.test(time_anova$liking,time_anova$label)


# there is a stimuli effect ! 

# test for the judge effect
table(time_anova$nom,time_anova$liking)
chisq.test(time_anova$liking,time_anova$nom)


# no judge effect 

time_anova_fct <- time_anova %>% mutate_if(is.character,as.factor)

mod <- glm(liking~nom+label,data=time_anova_fct,family = "binomial")

summary(mod)


mod_null <- glm(liking~1,data=time_anova_fct,family = "binomial")
summary(mod_null)
