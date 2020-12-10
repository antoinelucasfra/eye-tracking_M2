library(png)
library(grid)
library(FactoMineR)
library(ggplot2)
library(cowplot)
library(magick)
library(tidyverse)

data = read.table("./data/ScreenRecorderPath2.txt")
head(data)

# on fait la transfo en 16/9 ! et ensuite on étire 
x =  data$V1 * 16
y = (1 - data$V2) * 9
t = (data$V3 - data$V3[1])/1000

df_observed = data.frame(cbind(x,y,t))

myplot = ggplot(data = df_observed, aes(x,y,color=t)) + 
  # annotation_custom(g, xmin=0, xmax=16, ymin=-Inf, ymax=Inf) +
  geom_point() + 
  coord_cartesian(xlim = c(0,16), ylim = c(0,9)) +
  theme_cowplot()

# add the image
ggdraw() +
  draw_image("./target.png",x=0,y=0.05,scale = 0.9) +
  draw_plot(myplot)

xvec <- c(1.6,29.4,29.4,1.6,15.6,8,24,24,8,1.6,
          29.4,29.4,1.6,15.6,29.4,15.6,1.6,15,24,8,24,24,8)

yvec <- c(16.5,16.5,1,1,8.7,16.5,16.5,1,1,13,13,4.3,
          4.3,16.5,8.7,1,8.7,8.7,8.7,13,13,4.3,4.3)

df_pts_reels <- data.frame(x=xvec,y=yvec)

