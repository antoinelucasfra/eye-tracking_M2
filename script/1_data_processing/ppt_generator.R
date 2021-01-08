# script to produce a powerpoint to show at the user at the end of all stimuli
#' eye-tracking evaluations 
#' build a script that take as input 2 different paths : the one from the stimuli
#' that were looked by the people and the path from where the data are generated in the folder 
#' 

library(png)
library(officer)
library(magrittr)
library(imager)


# define the picture to write 
picture <- file.path("img/old/flav_thvnrd_LI.png")
heatmap <- file.path("img/fake_img/fake1.png")

picture_list<- list.files("img/img_test/")
heatmap_list <- list.files("img/old/")
picture_path <- file.path("img/img_test/")
heatmap_path <- file.path("img/old/")

list_heatmap <- list.files("img/fake_img/", pattern = "*.png", full.names = TRUE)
list_pictures <- list.files("img/old/flav_thvnrd_LI.png")

# create and add a slide to the slideshow 
slide_to_show <- read_pptx() %>% 
  add_slide(layout = "Two Content")

### add content into the created slide 

# add content in the left part 
slide_to_show <- ph_with(slide_to_show, 
                         value = external_img(picture), 
                         location = ph_location_left(type = "body"))

# add content in the right part 
slide_to_show <- ph_with(slide_to_show, 
                         value = external_img(heatmap), 
                         location = ph_location_right(type = "body"))

slide_to_show %>% add_slide(layout = "Two Content")
print(slide_to_show, target = "img/pic.pptx") 


#' function that is returning a powerpoint with picture and heatmap from the 2 different paths 
#' take as in put the heatmaps and pictures pat and the number of picture/slide

ppt_generate <- function(picture_path,heatmap_path,picture_list,heatmap_list,n_stimuli){
  
  slideshow <- read_pptx() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide() %>% 
    add_slide()
    
  
  for (k in 1:n_stimuli){
    picture_to_add <- paste0(picture_path,picture_list[k])
    heatmap_to_add <- paste0(heatmap_path,heatmap_list[k])
    slideshow <- slideshow %>% 
      ph_with(value = external_img(picture_to_add), 
              location = ph_location_left(type = "body")) %>% 
      ph_with(value = external_img(heatmap_to_add),
              location = ph_location_right(type = "body"))
    print(slideshow, target = "img/pic.pptx") 
    
  }
}




