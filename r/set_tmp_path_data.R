#' Set_Tmp_Path_Data
#' 
#' This is a function that creates a temporary path directory for retrieving
#' data, behind the scene task function
#' 
#' 
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
set_tmp_path_data <-
function() {
windows_dropboxpath = "~//Dropbox//TARGET_PROJECT//"
linux_dropboxpath = "~//Dropbox//TARGET_PROJECT//"
if(Sys.info()["sysname"][[1]]=="Windows") setwd(windows_dropboxpath) else setwd(linux_dropboxpath)
}
