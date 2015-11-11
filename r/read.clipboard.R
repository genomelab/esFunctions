read.clipboard <-
function(header=T, ...) {
  
  ###########################
  ## read from clipboard (useful for macs only?)
  ###########################
  read.delim(pipe('pbpaste'), header=header, ...)
}
