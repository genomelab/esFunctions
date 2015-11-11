write.clipboard <-
function(x, col.names=FALSE) {
  zz <- pipe('pbcopy','w')
  write.table(x,file=zz,sep='\t',quote=FALSE, col.names=col.names, row.names=FALSE)
  close(zz)
}
