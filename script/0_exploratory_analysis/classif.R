
source("script/requirements.R")
source("script/1_data_processing/helpers_data_process.R")
source("script/1_data_processing/load_data.R")
library(gtools)
library(stringr)



data = read.table("data/gazedata/18-damien/Result2/ScreenRecorderPath.dat", skip = 1, header = FALSE)

time_user_exp = read.table("data/time_user_exp.csv", sep = ",", header = TRUE)
col = colnames(time_user_exp)

#Position of calibration square
square_pos = read.csv("experience/25_square_position.csv", sep =";", header = TRUE, row.names = 1, dec = ".", colClasses = c("col" = "factor", "xvec" = "numeric", 'yvec' = "numeric"))
square_pos$name = rownames(square_pos)

correspondace = read.csv("experience/correspondance_stimu_couleur.csv", sep = ";")

square_pos = left_join(square_pos, correspondace, by = "col")

df_all = loader_data()


consum = levels(df_all$consu)
for (k in consum){
  df_consu_k = df_all %>% filter(consu == k)
  
  stimu = str_sort(stimu, numeric = TRUE)
  
  consumers_number = as.numeric(gsub("([0-9]+).*$", "\\1", k))
  start_time_vec = time_user_exp[time_user_exp$ordre == consumers_number ,grepl( "start_time", col, fixed = TRUE)]
  end_time_vec = time_user_exp[time_user_exp$ordre == consumers_number ,grepl( "end_time", col, fixed = TRUE)]
  name_stimu = time_user_exp[time_user_exp$ordre == consumers_number ,grepl( "stimu", col, fixed = TRUE)]
  
  for (i in 1:length(stimu)){
    
    data = df_consu_k[df_consu_k$stimu == stimu[i],1:3]

    data = remove_first_time(data,start_time_vec[1,i])
    data = remove_last_time(data,start_time_vec[1,i] + 10)
    
    data.pca = PCA(data, col.w = c(1,1,1), graph = FALSE)
    data.hcpc = HCPC(res = data.pca, nb.clust = -1, graph = FALSE)
    
    cluster = data.hcpc$data.clust
    
    ggplot()+
      geom_point(data = cluster, aes(x=x,y=y,colour = clust))+
      coord_fixed(ratio = 1, xlim = c(-10, 20), ylim = c(-10, 12)) +
      geom_point(data = square_pos[square_pos$ï..stimu == 795,], 
                 aes(x=xvec,y=yvec, color = name, fill = name), 
                 shape = 15)+
      geom_point(data = square_pos[square_pos$col == "noir",], 
                 aes(x=xvec,y=yvec, color = name, fill = name), 
                 shape = 15)+
      geom_segment(aes(x = 0, y = 9, xend = 16, yend = 9, colour = "segment"))+
      geom_segment(aes(x = 0, y = 0, xend = 0, yend = 9, colour = "segment"))+
      geom_segment(aes(x = 0, y = 0, xend = 16, yend = 0, colour = "segment"))+
      geom_segment(aes(x = 16, y = 0, xend = 16, yend = 9, colour = "segment"))+
      labs(title = paste("consumer : ", k, " stimu : ", stimu[i], " name :", name_stimu[i-1]))
  }
}



