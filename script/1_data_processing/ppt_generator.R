# script to produce a powerpoint to show at the user at the end of all stimuli
#' eye-tracking evaluations 
#' build a script that take as input 2 different paths : the one from the stimuli
#' that were looked by the people and the path from where the data are generated in the folder 
#' 

library(png)
library(officer)
library(magrittr)
library(imager)


# define the path of the picture

picture_list <- list.files("img/fake_img/", full.names = T)

#' function that is returning a powerpoint with picture and heatmap from the 2 different paths 
#' take as in put the heatmaps and pictures pat and the number of picture/slide

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

ppt_generate(slideshow,picture_list,50)


