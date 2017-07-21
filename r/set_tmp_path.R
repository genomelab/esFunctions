#' set_tmp_path
#' 
#' this is creating a temporary path used for sourcing
#' 
#' 
#' @keywords ~kwd1 ~kwd2
set_tmp_path <-
function() {
  windows_dropboxpath = "./Dropbox//TARGET PROJECT//code//tmp//"
  linux_dropboxpath = "~//Dropbox//TARGET_PROJECT//code//tmp//"
  if(Sys.info()["sysname"][[1]]=="Windows") setwd(windows_dropboxpath) else setwd(linux_dropboxpath)
}
