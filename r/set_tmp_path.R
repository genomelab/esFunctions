set_tmp_path <-
function() {
  windows_dropboxpath = "./Dropbox//TARGET PROJECT//code//tmp//"
  linux_dropboxpath = "~//Dropbox//TARGET_PROJECT//code//tmp//"
  if(Sys.info()["sysname"][[1]]=="Windows") setwd(windows_dropboxpath) else setwd(linux_dropboxpath)
}
