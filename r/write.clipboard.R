#' wirte.clipboard
#' 
#' this is for copying inputs
#' 
#' @export
#' @param x this writes an input ready to be copied
#' @param col.names this is a logical command
#' @keywords ~kwd1 ~kwd2
write.clipboard <-
function(x, col.names=FALSE) {
  zz <- pipe('pbcopy','w')
  write.table(x,file=zz,sep='\t',quote=FALSE, col.names=col.names, row.names=FALSE)
  close(zz)
}
