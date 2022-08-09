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
  result <- na.omit(result)
  to_merge <- na.omit(to_merge)
  #final <- subset(result, select = -c(src_id))
  result <- merge(result, to_merge, by="inchikey")
  result <- data.frame(result$fimm_id, result$database_id, result$src_compound_id, result$inchikey)
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

  r2 <- subset(r2, select = -c(src_id, database_id))

  colnames(df) <- c("inchikey", "v1")

  ## Comparing all the chembl ids
  #final_result <- r2$src_compound_id==df$V1


  df2 <- merge(r2, df, by="inchikey")
  final_result <- df2[, length(intersect(v1, src_compound_id)), by="inchikey"]

  #Shows the data frame with all inchikeys which did not have same chembl ids
  # as in data (after querying Unichem API)
  #zero_val <- final_result %>% filter(V1 == 0)

  #Shows queries from Unichem Api for inchikeys which did not match
  #actual <- wInchiToDatabaseID(zero_val$inchikey, "chembl")

  #The queries which did not return "N/A" chembl ids
  #not_na <- actual %>% filter(src_compound_id != "N/A")

  #The #queries which returned "N/A" chenbl ids
  #na <- actual %>% filter(src_compound_id == "N/A")

  return(final_result)
}