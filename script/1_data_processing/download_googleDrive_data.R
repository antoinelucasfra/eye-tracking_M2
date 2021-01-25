consumers_name = "19-emma"
# consumers_name = "12-annevictoire"
path = paste0("Eyetracking/1ter_resultats_exp/", consumers_name)
dir.create(paste0("data/gazedata/",consumers_name))
files = drive_ls(path = path )
for (k in files$name){
  drive_path = paste0(path,"/", k, "/ScreenRecorderPath.dat")
  path_temp = paste0("data/gazedata/",consumers_name,"/", k)
  dir.create(path_temp)
  file_temp = paste0(path_temp,"/ScreenRecorderPath.dat")
  drive_download(drive_path , path = file_temp, overwrite = F)
}
downloaded_files <- list.files(paste0("data/gazedata/",consumers_name))
files <- files %>% filter(! name %in% downloaded_files)
files

