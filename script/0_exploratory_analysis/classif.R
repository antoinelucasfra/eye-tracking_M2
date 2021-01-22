
source("script/requirements.R")
source("script/1_data_processing/helpers_data_process.R")

data = read.table("data/gazedata/18-damien/Result2/ScreenRecorderPath.dat", skip = 1, header = FALSE)

time_user = read.table("data/time_user_exp.csv")

#Position of calibration square
square_pos = read.csv("experience/25_square_position.csv", sep =";", header = TRUE, row.names = 1, dec = ".", colClasses = c("col" = "factor", "xvec" = "numeric", 'yvec' = "numeric"))
square_pos$name = rownames(square_pos)

correspondace = read.csv("experience/correspondance_stimu_couleur.csv", sep = ";")

square_pos = left_join(square_pos, correspondace, by = "col")


data = data[,1:3]

data = gaze_preprocess(data, c(9,16))

data = remove_first_time(data,4.8)
data = remove_last_time(data,4.8 + 10)

data.pca = PCA(data, col.w = c(1,1,1))

data.hcpc = HCPC(res = data.pca, nb.clust = -1)


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
  geom_segment(aes(x = 16, y = 0, xend = 16, yend = 9, colour = "segment"))



