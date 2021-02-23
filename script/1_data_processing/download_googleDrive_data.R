# script to download data from a specific googledrive repo used for experimentation


consumers_name <- #user_name
path <- paste0("Eyetracking/1ter_resultats_exp/", consumers_name)
dir.create(paste0("data/gazedata/",consumers_name))
files <- drive_ls(path = path )

for (k in files$name){
  drive_path <- paste0(path,"/", k, "/ScreenRecorderPath.dat")
  path_temp <- paste0("data/gazedata/",consumers_name,"/", k)
  dir.create(path_temp)
  file_temp <- paste0(path_temp,"/ScreenRecorderPath.dat")
  drive_download(drive_path , path = file_temp, overwrite = F)
}

# check if everything is downloaded
downloaded_files <- list.files(paste0("data/gazedata/",consumers_name))

# iterate if there is missing download
files <- files %>% filter(! name %in% downloaded_files)
downloaded_files <- list.files(paste0("data/gazedata/",consumers_name))


















