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

# on selectionne les 12 premiere secondes pour la correction versus après pour le stimuli.
df_correction = df[df$t < 12,]
df_stimuli = df[df$t >= 12,]

# classif sur les point de correction 
acp = FactoMineR::PCA(df_correction, col.w = c(1,1,5))
res.hcpc = HCPC(acp, nb.clust = 5,  consol = TRUE)
df_correction = res.hcpc$data.clust
head(df_correction)

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

## geom density 2D -> usage facile ici 
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

# on duplique les lignes qu'on vient de faire 
n = dim(df_stimuli)[1]
df_bon_duplicate = df_bon_pivot[rep(seq_len(nrow(df_bon_pivot)), each = n), ]
barycentre_duplicate = barycentre_pivot[rep(seq_len(nrow(df_bon_pivot)), each = n),]

#on bind les columns des trois df créé précédement. 
df_stimuli_recardage = cbind(df_stimuli, df_bon_duplicate, barycentre_duplicate)
col = colnames(df_stimuli_recardage)

nb_clust = 5

# on créé les 5 variable distance des 5 barycentres
# code A ADAPTER SI ON A PLUS QUE 5 CLUSTER / REFLECHIR COMMENT AUTOMATISER CA ! 

for (k in 1:nb_clust){
  vec_temp = sqrt( (df_stimuli_recardage$x - df_stimuli_recardage[,(12+k)])^2 +
                    (df_stimuli_recardage$y - df_stimuli_recardage[,(17+k)])^2 )
  df_stimuli_recardage = cbind(df_stimuli_recardage, vec_temp )
}
colnames(df_stimuli_recardage) = c( col, "dist1", "dist2", "dist3", "dist4", "dist5")
df_stimuli_recardage$dist_max = apply(df_stimuli_recardage[,c( "dist1", "dist2", "dist3", 
                                                               "dist4", "dist5")], 1, FUN=max)
# on crée les 5 variable de poids (1 par cluster)
# le poid etant egual a 1- d/D 
# d la distance au barycentre, D etant la distance max a un barycentre

col = colnames(df_stimuli_recardage)
for (k in 1:nb_clust){
  vec_temp = 1 - df_stimuli_recardage[,23 + k] / df_stimuli_recardage$dist_max
  df_stimuli_recardage = cbind(df_stimuli_recardage, vec_temp )
}

colnames(df_stimuli_recardage) = c(col, "poid1", "poid2", "poid3", "poid4", "poid5")
attach(df_stimuli_recardage)

# on calcule new_x et new_y comme une translation par combinaison lineaire
# des vecteurs barycentre -> cible

df_stimuli_recardage$new_x = df_stimuli_recardage$x + ( (x_1-mean_x_1)*poid1 + (x_2-mean_x_2)*poid2  + (x_3-mean_x_3)*poid3  + 
                              (x_4-mean_x_4)*poid4  + (x_5-mean_x_5)*poid5 )
df_stimuli_recardage$new_y = df_stimuli_recardage$y + ( (y_1-mean_y_1)*poid1 + (y_2-mean_y_2)*poid2  + (y_3-mean_y_3)*poid3  + 
                                    (y_4-mean_y_4)*poid4  + (y_5-mean_y_5)*poid5 )

head(df_stimuli_recardage)

dim(df_stimuli_recardage)

# plot des points de correction et des points stimuli
ggplot() + 
  geom_point(data = df_correction, aes(x,y, color = clust)) + 
  coord_cartesian(xlim = c(0,31), ylim = c(0,30)) +
  geom_point(data = df_bon, aes(x,y, color = name), shape = 17, cex = 4)+
  geom_point(data = df_stimuli_recardage, aes(x = x, y=y), alpha = 0.3, color = "blue")
  
mycol <- c("white", "lightblue", "blue", "navy", "yellow", "red", "red4")
  
# on plot les nouvelles coordonnées translaté 
ggplot() + 
  geom_point(data = translation_mat, aes(x_trans,y_trans, color = group)) + 
  coord_cartesian(xlim = c(-5,31), ylim = c(-10,30)) +
  geom_point(data = df_bon, aes(x,y, color = name), shape = 17, cex = 4) +
  geom_point(data = df_stimuli_recardage, aes(x = new_x, y = new_y), alpha = 0.3, color = "blue")+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 31)+
  geom_hline(yintercept = 17.4)+
  geom_bin2d(data =df_stimuli_recardage,aes(x = new_x, y = new_y), 
             bins = 50, alpha = 0.6) +
  scale_fill_gradientn(colours = mycol)
  geom_point(data = df_stimuli_recardage, aes(x = new_x, y=new_y), alpha = 0.3, 
             color = "blue")



# claissification pour trouver les zones d'interet : basé sur x,y et nb clust
acp_xyNew = PCA(df_stimuli_recardage[,c("new_x", "new_y")])
classif_xyNew = HCPC(acp_xyNew, nb.clust = 5)

df_classif_xyNew = classif_xyNew$data.clust

ggplot(df_classif_xyNew, aes(x = new_x, y=new_y, color = clust))+
  coord_cartesian(xlim = c(-5,31), ylim = c(-10,30)) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 31)+
  geom_hline(yintercept = 17.4)+
  geom_point()

# claissification pour trouver les zones d'interet : basé sur x, y, t et nb clust
acp_xyNew = PCA(df_stimuli_recardage[,c("t","new_x", "new_y")])
classif_xyNew = HCPC(acp_xyNew, nb.clust = 5)

df_classif_xyNew = classif_xyNew$data.clust

ggplot(df_classif_xyNew, aes(x = new_x, y=new_y, color = clust))+
  coord_cartesian(xlim = c(-5,31), ylim = c(-10,30)) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 31)+
  geom_hline(yintercept = 17.4)+
  geom_point()




