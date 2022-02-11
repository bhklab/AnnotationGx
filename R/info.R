## ==========================================
##    FIMM Drug Data
## ==========================================


drugbankAndPubchem <- function() {
  ## Need to create a data table which maps from FIMM ID to the drugbank and pubchem ID
  ## We can use Chembl ID or the Inchikeys 
  
  target_names <- c("drugbank", "pubchem")
  
  # InChI column is chosen
  r <- (metadata$InChI)
  
  # data frame created using the wrapper function
  result <- wInchiToDatabaseID(r, target_names = target_names)
  
  # output cleaned
  result <- subset(result, select = -c(src_id))
  to_merge <- data.frame(inchikey=metadata$InChI, fimm_id=metadata$ID_Drug)
  #final <- subset(result, select = -c(src_id))
  result <- merge(result, to_merge, by="inchikey")
  return(result)
}

chemblComparison <- function() {
  ## Pre processing the data
  df <- subset(metadata, select = c(InChI, `ChEMBL ID`))
  df <- as.data.table(df)
  
  ## Removing rows with no vales
  df <- na.omit(df)
  
  ## Repeating the Inchikey values for those rows which contain multiple chembl ids
  df <- df[, unlist(strsplit(`ChEMBL ID`, split = ",")), by=InChI]
  
  t2 <- c("chembl")
  
  r2 <- wInchiToDatabaseID(df$InChI, target_names = t2)
  
  ## Comparing all the chembl ids
  final_result <- r2$src_compound_id==df$V1
  return(final_result)
}

if (sys.nframe() == 0) {
  library(readxl)
  library(data.table)
  
  metadata <- read_excel("~/Desktop/FIMM_pset_compound_metadata.xlsx")
}
