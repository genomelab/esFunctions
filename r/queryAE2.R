queryAE2 <-
function (keywords = NULL, species = NULL) 
  ### Faster version (does not parse details) of arraydata in ArrayExpress
  ### Gets you the names of the arrays
{
  if (is.null(keywords) && is.null(species)) 
    stop("No keywords or species specified")
  baseURL = "http://www.ebi.ac.uk/arrayexpress/xml/v2/experiments"
  qr = paste(baseURL, "?keywords=", keywords, "&species=", 
             species, sep = "")
  qr = URLencode(qr)
  queryfilename = paste("query", keywords, species, ".xml", 
                        sep = "")
  query = try(download.file(qr, queryfilename, mode = "wb"))
  x = xmlTreeParse(queryfilename)
  ID = sapply(1:length(xmlRoot(x)), function(i) unlist(xmlElementsByTagName(xmlRoot(x)[[i]], 
                                                                            "accession"))["accession.children.text.value"])
  names(ID) = NULL
  x2 = xmlTreeParse(queryfilename, useInternalNodes = TRUE)
  ra = getNodeSet(x2, "/experiments//raw[@count]")
  Rawids = sapply(ra, function(r) xmlGetAttr(r, "name"))
  Rawids = gsub(".raw.*.zip", "", Rawids)
  Raw = rep(NA, length(ID))
  names(Raw) = ID
  Raw[which(ID %in% Rawids)] = "yes"
  Raw[which(!ID %in% Rawids)] = "no"
  pr = getNodeSet(x2, "/experiments//fgem[@count]")
  Procids = sapply(pr, function(r) xmlGetAttr(r, "name"))
  Procids = gsub(".processed.*.zip", "", Procids)
  Processed = rep(NA, length(ID))
  names(Processed) = ID
  Processed[which(ID %in% Procids)] = "yes"
  Processed[which(!ID %in% Procids)] = "no"
  #date = getelt(x, node = "releasedate", element = "releasedate.children.text.value")
  #pmid = getelt(x, node = "bibliography", element = "bibliography.children.accession.children.text.value")
  #spec = getelt(x, node = "species", element = "species.children.text.value")
  #experimentdesign = getelt(x, node = "experimentdesign", element = "experimentdesign.children.text.value")
  #experimentalfactor = geteltmulti(x, node = "experimentalfactor", 
  #                                 element1 = "children.name.children.text.value", element2 = "children.value.children.text.value")
  xmlparsed = data.frame(ID = ID, Raw = Raw[ID], Processed = Processed[ID]
                         #                       ,ReleaseDate = date, PubmedID = pmid, Species = spec, 
                         #                       ExperimentDesign = experimentdesign, ExperimentFactors = experimentalfactor)
  )
  return(xmlparsed)
}
