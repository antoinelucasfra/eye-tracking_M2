
path = paste0("Eyetracking/1ter_resultats_exp/", consumers_name)
dir.create(paste0("data/gazedata/",consumers_name))

files = drive_ls(path = path )

for (k in files$name){
  drive_path = paste0(path,"/", k, "/ScreenRecorderPath.dat")
  path_temp = paste0("data/gazedata/",consumers_name,"/", k)
  dir.create(path_temp)
  file_temp = paste0(path_temp,"/ScreenRecorderPath.dat")
  drive_download(drive_path ,path =file_temp, overwrite = FALSE)
}

