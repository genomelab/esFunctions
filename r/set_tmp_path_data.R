set_tmp_path_data <-
function() {
windows_dropboxpath = "~//Dropbox//TARGET_PROJECT//"
linux_dropboxpath = "~//Dropbox//TARGET_PROJECT//"
if(Sys.info()["sysname"][[1]]=="Windows") setwd(windows_dropboxpath) else setwd(linux_dropboxpath)
}
