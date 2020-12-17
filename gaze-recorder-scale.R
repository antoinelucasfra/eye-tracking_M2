
# Dans ce code, on applique la methode de translation par le vecteur barycentre--> cible
# nos données sont juste nos fixation sur les cible (pas de stimuli présenté)

library(FactoMineR)
library(tidyverse)
library(ggplot2)


data <- read.table("data/record_23points_correction.txt")
data_skip <- read.table("data/record_23points_correction.txt", 
                        skip = 5)
head(data)

# a adapter en fonction de la taille de notre ecran.
x =  data$V1 * 31
y = (1 - data$V2) * 17.4

df = data.frame(cbind(x,y, t = data$V3))
df$t =( df$t - df$t[1])/1000

myplot = ggplot(data = df, aes(x,y)) + 
  geom_point() + 
  coord_cartesian()
myplot

ggplot(data = df, aes(x,y, color = t)) + 
  geom_point() + 
  coord_cartesian(xlim = c(0,31), ylim = c(0,30))

# CLassifie nos points en 23 classe (car 23 cible on été regardé)
acp = FactoMineR::PCA(df, col.w = c(1,1,10))
res.hcpc = HCPC(acp, nb.clust = 23,  consol = TRUE)

df2 = res.hcpc$data.clust
colnames(df2) = c(colnames(df), "group")
head(df2)
df2$group

ggplot() + 
  geom_point(data = df2, aes(x,y, color = group)) + 
  coord_cartesian(xlim = c(0,31), ylim = c(0,30))

# vecteur des cible regardé lors de l'expé : 
xvec <- c(1.6,29.4,29.4,1.6,15.6,8,24,24,8,1.6,29.4,29.4,1.6,15.6,29.4,15.6,1.6,8,24,8,24,24,8)

yvec <- c(16.5,16.5,1,1,8.7,16.5,16.5,1,1,13,13,4.3,4.3,16.5,8.7,1,8.7,8.7,8.7,13,13,4.3,4.3)

df_bon <- data.frame(x=xvec,y=yvec)
df_bon$name = rownames((df_bon))
head(df_bon)
head(df2)

df_bon[18,]

# on joints les coordonné enregistré et les coordonnées des cible par groupe (c a d par cible)
df_joint = full_join(df2, df_bon,by=c("group"="name"))
colnames(df_joint) = c("x_eye", "y_eye", "t", "group", "x_paper", "y_paper")


# plot des points réels et des regards
ggplot() + 
  geom_point(data = df2, aes(x,y, color = group)) + 
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
# on peut faire une densité/heatmap sur ces données
ggplot(data=translation_mat,aes(x_trans,y_trans)) + 
  geom_point() + 
  geom_density_2d_filled(adjust = 2/3, alpha = 0.5)+
  coord_cartesian(xlim = c(0,31), ylim = c(0,30)) 


