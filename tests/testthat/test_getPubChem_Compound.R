library(testthat)
library(data.table)




getPubChemStatus()

 CID <- parseJSON( httr::GET(
        "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON"))$IdentifierList$CID
q <- AnnotationGx::getPubChemAnnotation(CID, annotationType = 'ChEMBL ID', query_only = T)

parseJSON(httr::GET(q))

# dt <- fread("sandbox/GDSC2_8.4_treatmentMetadata_annotated.tsv")
# dt <- unique(dt[, .(GDSC.treatmentid, GDSC.DRUG_NAME)])

# dt[40:50]

# subdt <- dt[40:50]
# compound_nameToCIDS <- AnnotationGx::getPubChemCompound(
#     subdt[, GDSC.treatmentid],
#     from='name',
#     to='cids',
#     batch = FALSE,
#     verbose = FALSE,
#     query_only = TRUE,
#     BPPARAM = BiocParallel::MulticoreParam(workers = 10, progressbar = TRUE, stop.on.error = FALSE)
# )
# merge(subdt, compound_nameToCIDS, by.x = "GDSC.treatmentid", by.y = "name", all.x = TRUE)


# compound_nameToCIDS_2 <- AnnotationGx::getPubChemCompound(
#     subdt[, GDSC.DRUG_NAME],
#     from='name',
#     to='cids',
#     batch = FALSE,
#     verbose = FALSE,
#     BPPARAM = BiocParallel::MulticoreParam(workers = 10, progressbar = TRUE, stop.on.error = FALSE)
# )
# merge(subdt, compound_nameToCIDS_2, by.x = "GDSC.DRUG_NAME", by.y = "name", all.x = TRUE)
