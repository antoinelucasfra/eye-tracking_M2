# script to produce a powerpoint to show at the user at the end of all stimuli
#' eye-tracking evaluations 
#' build a script that take as input the path : stimuli + heatmap superposed 

library(png)
library(officer)
library(magrittr)
library(imager)


# define the path of the picture to get

# final path is : "data/heatmap_eyetrack"
picture_list <- list.files("img/fake_img/", full.names = T)

#' function that is returning a powerpoint with picture and heatmap from the path
#'
#' @param slide_object the slideshow to build 
#' @param list_pictures stimuli + heatmaps created
#' @param n_stimuli the number of picture/slide necessary (16 in total)

ppt_generate <- function(slide_object, list_pictures, n_stimuli){

  for (k in 1:n_stimuli){
    
    slideshow <- slideshow %>% add_slide()
    
    picture_to_add <- picture_list[k]

    slideshow <- slideshow %>% 
      # write the image 
      ph_with(value = external_img(picture_to_add), 
              location = ph_location_type(type = "body"), use_loc_size = T ) %>% 
      # write the image name
      ph_with(value = picture_to_add, location = ph_location_type(type = "title")) 
      
    print(slideshow, target = "img/pic.pptx") 
  }
}

# slideshow initialisation 
slideshow <- read_pptx() 

# writing the powerpoint 

ppt_generate(slideshow,picture_list,16)

