
source("script/requirements.R")


double_loop = function(width_size= 640, 
                       height_size = 360, 
                       method = c("correction", "heatmap_uncorrected", 
                                  "heatmap_corrected","raw_image")
) {
  
  #################### LOAD DATA ###################
  
  # time user exp
  time_user_exp = read.table("data/time_user_exp.csv", sep = ",", header = TRUE)
  col = colnames(time_user_exp)
  
  # all Rdata file 
  load(file = "data/df_all.RData")
  
  #Position of calibration square
  square_pos = read.csv("experience/25_square_position.csv", sep =";", header = TRUE, 
                        row.names = 1, dec = ".", 
                        colClasses = c("col" = "factor", "xvec" = "numeric", 'yvec' = "numeric"))
  square_pos$name = rownames(square_pos)
  
  # correspondace file
  correspondance = read.csv("experience/correspondance_stimu_couleur.csv", sep = ";")
  
  # join correspondace file with square_pos by color (col)
  square_pos = left_join(square_pos, correspondance, by = "col")
  
  # data checked by flavie
  check = read.csv("experience/calibration_correlation.csv", header = TRUE,
                   sep = ";", colClasses = c(id = "factor", "stimu" = "factor", "exploitability" = "factor"))
  
  
  ################## double for loop #######################
  
  consum = levels(df_all$consu)
  
  # generate raw_image 
  
  for (k in consum){
    df_consu_k = df_all %>% filter(consu == k)
    stimu = unique(df_consu_k$stimu)
    stimu = str_sort(stimu, numeric = TRUE)
    
    consumers_number = as.numeric(gsub("([0-9]+).*$", "\\1", k))
    start_time_vec = time_user_exp[time_user_exp$ordre == consumers_number ,
                                   grepl( "start_time", col, fixed = TRUE)]
    end_time_vec = time_user_exp[time_user_exp$ordre == consumers_number ,
                                 grepl( "end_time", col, fixed = TRUE)]
    name_stimu = time_user_exp[time_user_exp$ordre == consumers_number ,
                               grepl( "stimu", col, fixed = TRUE)]
    
    add = data.frame(stimu0 = "0_correction")
    name_stimu = cbind(add, name_stimu)
    
    for (i in 1:length(stimu)){
      
      # data for the consumer K and the stimuli number i
      data = df_consu_k[df_consu_k$stimu == stimu[i],1:3]
      
      # for method = correction : 
      if (method == "correction"){
        
        data = remove_first_time(data,start_time_vec[1,i])
        data = remove_last_time(data,start_time_vec[1,i] + 10)
        
        data.pca = PCA(data, col.w = c(1,1,1), graph = FALSE)
        data.hcpc = HCPC(res = data.pca, nb.clust = -1, graph = FALSE)
        
        cluster = data.hcpc$data.clust
        
        plot = ggplot()+
          geom_point(data = cluster, aes(x=x,y=y,colour = clust))+
          
          coord_fixed(ratio = 1, xlim = c(-30, 30), ylim = c(-30, 30)) +
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
          labs(title = paste("consumer : ", k, " stimu : ", stimu[i], " name :", name_stimu[i]))
        
        
        png(file = paste0("data/inputs_ML/plot_correction/",k,"_",name_stimu[i],".png"),
            width = width_size,
            height = height_size)
        plot(plot)
        dev.off()
      }
      
      # ### method heatmaps uncorrected
      # if (method == "heatmap_uncorrected"){
      #   if (i != 1){
      #     # We are not doing the heatmaps for 0_correction
      #     
      #     data = remove_first_time(data,start_time_vec[1,i]+10)
      #     data = remove_last_time(data,end_time_vec[1,i])
      #     
      #     colnames(data) = c("new_x", "new_y", "t")
      #     
      #     heatmap_generator(data,
      #                       path_img =  paste0("experience/cockpit_utile/",
      #                                          name_stimu[i],".png"),
      #                       file_name = paste0("data/inputs_ML/heatmaps_uncorrected/",
      #                                          k,"_",name_stimu[i],".png"),
      #                       width_size = 640, height_size = 360, transparency_img = 0.6)
      #   } 
      # }
      
      ### method heatmap_corrected
      if (method == "heatmap_corrected"){
        
        if (i != 1){
          # we are not doing heatmap for 0_correction data
          
          check_value <- check[(check$id == k) & (check$stimu == name_stimu[1,i]),] 
          check_value = check_value$exploitability
          
          if (check_value %in% c("yes", "maybe")) {
            # to select only well recorded data
            
            df_calibration <- data %>%
              filter(t > start_time_vec[1,i]) %>% 
              filter(t < start_time_vec[1,i]+10)
            
            df_stimuli <- remove_first_time(data, start_time_vec[1,i]+10)
            df_stimuli <- remove_last_time(df_stimuli, end_time_vec[1,i])
            
            data_classif <- gaze_classif(df_calibration, clust_number = 5) 
            
            data_classif <- data_classif %>% 
              group_by(clust) %>% 
              summarise(mean_t = mean(t)) %>% 
              mutate(rank = rank(mean_t)) %>% 
              full_join(data_classif, by="clust") %>% 
              arrange(rank)  %>% 
              mutate(clust = rank, 
                     rank = NULL,
                     mean_t = NULL)
            
            square_pos_temp = square_pos[(square_pos$ï..stimu == name_stimu[1,i]) |
                                           (square_pos$col == "noir"),]
            
            # joindre les classes avec les vrai points 
            df_join <- full_join(data_classif, square_pos_temp, by=c("clust"="num_bis")) %>% 
              rename(x_eye = x,
                     y_eye = y, 
                     group = clust, 
                     stimu = ï..stimu) %>% 
              mutate(name = NULL) 
            
            # get correction data with barycenter correction
            df_trans <- gaze_correct_bary(df_join)
            df_bary <- unique(df_trans[,c("group","mean_x_eye","mean_y_eye","xvec","yvec")])
            
            #get only barycenter data for each observed area
            df_corrected = data.frame()
            
            dist_weight <- gaze_dist_weight_df(square_pos_temp,
                                               df_bary,
                                               df_stimuli,
                                               nb_clust = 5)
            
            # separate each object created in dist_weight
            real_pivot <- dist_weight[[1]]
            bary_pivot <- dist_weight[[2]]
            weight <- dist_weight[[4]]
            dist <- dist_weight[[3]]
            
            # execute the linear combination
            stimuli_correct <- gaze_stimuli_combi(df_stimuli,
                                                  real_pivot,
                                                  bary_pivot,
                                                  weight, 
                                                  nb_clust= 5) 
            stimuli_correct$stimu <- name_stimu[1,i]
            df_corrected = rbind(df_corrected, stimuli_correct)
            
            heatmap_generator(df_corrected,
                              path_img = paste0("experience/cockpit_utile/",name_stimu[1,i],".png"),
                              file_name = paste0("data/inputs_ML/heatmaps_corrected/",
                                                 k,"_",name_stimu[1,i],".png"),
                              add_img = TRUE,
                              width_size = width_size, height_size = height_size, transparency_img = 0.6)
          }
        }
      }
      
      ### other method here : 
      if (method == "heatmap_temporel"){
      }
      
      if (method == "raw_image"){
        if (i != 1){
          
        heatmap_generator(data,
                          path_img = paste0("experience/cockpit_utile/",name_stimu[1,i],".png"),
                          file_name = paste0("data/inputs_ML/raw_image/",
                                             k,"_",name_stimu[1,i],".png"),
                          add_img = TRUE, only_img = TRUE,
                          width_size = width_size, height_size = height_size, 
                          transparency_img = 0.6)
        
        }
        
      }
      
    }
  }
}


double_loop(method = "raw_image")


