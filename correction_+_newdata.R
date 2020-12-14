
##############################
##### NEW DATA : HEAT MAPS  ##
##############################
library(FactoMineR)
library(tidyverse)
library(ggplot2)

## generer des heat maps : new data 
data = read.table("data/record_correctionAndStimuli.txt")
head(data)

x =  data$V1 * 31
y = (1 - data$V2) * 17.4

df = data.frame(cbind(x,y, t = data$V3))
df$t =( df$t - df$t[1])/1000


xvec <- c(1.2, 28.4, 1.2, 28.4, 15.8 )

yvec <- c(16.4, 16.4, 1.2, 1.2, 8.7)

df_bon <- data.frame(x=xvec,y=yvec)
df_bon$name = rownames((df_bon))
head(df_bon)

# on selectionne les 1é premiere seconde pour la correction versus après pour le stimuli.
df_correction = df[df$t < 12,]
df_stimuli = df[df$t >= 12,]

# classif sur les point de correction 
acp = FactoMineR::PCA(df_correction, col.w = c(1,1,5))
res.hcpc = HCPC(acp, nb.clust = 5,  consol = TRUE)
df_correction = res.hcpc$data.clust
head(df_correction)
df_correction$clust

### si on veut réordonner une variable catégorielle en fonction d'une variable continue
###fct_reorder(df$color, df$a, min)


df_joint = full_join(df_correction, df_bon, by=c("clust"="name"))
colnames(df_joint) = c("x_eye", "y_eye", "t", "group", "x_paper", "y_paper")


# plot des points réels et des regards
ggplot() + 
  geom_point(data = df_correction, aes(x,y, color = clust)) + 
  coord_cartesian(xlim = c(0,31), ylim = c(0,30)) +
  geom_point(data = df_bon, aes(x,y, color = name), shape = 17, cex = 4)


# calculs barycentre  
barycentre_coord = df_joint %>% group_by(group) %>% 
  summarise(mean_x = mean(x_eye), 
            mean_y = mean(y_eye))

# on créé un data frame avec les nouvelles coordonnées des points du regards, translaté
# par le vecteur point papier -> barycentre
translation_mat = full_join(df_joint , barycentre_coord,by=c("group"="group"))
translation_mat$x_diff = translation_mat$x_paper - translation_mat$mean_x
translation_mat$y_diff = translation_mat$y_paper - translation_mat$mean_y

translation_mat$x_trans = translation_mat$x_eye + translation_mat$x_diff
translation_mat$y_trans = translation_mat$y_eye + translation_mat$y_diff


head(translation_mat)

# on plot les nouvelles coordonnées translaté 
ggplot() + 
  geom_point(data = translation_mat, aes(x_trans,y_trans, color = group)) + 
  coord_cartesian(xlim = c(0,31), ylim = c(0,30)) +
  geom_point(data = df_bon, aes(x,y, color = name), shape = 17, cex = 4) + 
  geom_point(data = barycentre_coord, aes(mean_x,mean_y, color = group), shape = 15, cex = 2) 

## geom density 2D 
ggplot(data=translation_mat,aes(x_trans,y_trans)) + 
  geom_point() + 
  geom_density_2d_filled(adjust = 2/3, alpha = 0.5)+
  coord_cartesian(xlim = c(0,31), ylim = c(0,30)) 


##### on applique la translation a notre JDD : 
head(translation_mat)
head(df_stimuli)
head(df_bon)

df_bon_pivot <- df_bon %>% 
  pivot_wider(names_from = name, 
              values_from = c(x,y))

barycentre_pivot <- barycentre_coord %>% 
  pivot_wider(names_from = group, 
              values_from = c(mean_x,mean_y))

dim(translation_mat)[1]
df_bon_pivot %>% slice(rep(1:n(), each = A))