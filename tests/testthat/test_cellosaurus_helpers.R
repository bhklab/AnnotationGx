library(AnnotationGx)
library(testthat)
library(checkmate)

test_that(".create_cellosaurus_queries is acting as expected", {
  queries <- AnnotationGx:::.create_cellosaurus_queries(c("ID1", "ID2", "ID3"), "Accession")
  expect_character(queries)
  expect_equal(queries, c("Accession:ID1", "Accession:ID2", "Accession:ID3"))

  queries2 <- AnnotationGx:::.create_cellosaurus_queries(c("ID1", "ID2", "ID3"), c("Accession", "Name", "Species"))
  expect_equal(queries2, c("Accession:ID1", "Name:ID2", "Species:ID3"))
})

test_that(".cellosaurus_schema is acting as expected", {
  schema <- AnnotationGx:::.cellosaurus_schema()
  expect_list(schema)
  names_list <- c("openapi", "info", "paths", "components", "tags")

  expect_names(names(schema), subset.of = names_list)
})

test_that(".build_cellosaurus_request is acting as expected", {
  request <- AnnotationGx:::.build_cellosaurus_request()

  expect_class(request, "httr2_request")
  expected <- "https://api.cellosaurus.org/search/cell-line?q=id%3AHeLa&sort=ac%20asc&fields=id%2Cac%2Chi%2Cca%2Csx%2Cag%2Cdi%2Cderived-from-site%2Cmisspelling&format=tsv&rows=1"
  expect_equal(request$url, expected)

  response <- AnnotationGx:::.perform_request(request) |> AnnotationGx:::.parse_resp_tsv(show_col_types = FALSE, skip = 14)
  expect_class(response, "spec_tbl_df")
  expect_equal(nrow(response), 1)

  request2 <- AnnotationGx:::.build_cellosaurus_request(
    query = "id:HeLa",
    to = c(
      "id", "ac", "sy", "acas", "sx", "ag", "di", "dio", "din", "dr", "cell-type",
      "derived-from-site", "misspelling", "dt", "dtc", "dtu", "dtv", "genome-ancestry"
    ),
    numResults = 2, apiResource = "search/cell-line", output = "TSV"
  )
  expect_equal(
    request2$url,
    "https://api.cellosaurus.org/search/cell-line?q=id%3AHeLa&sort=ac%20asc&fields=id%2Cac%2Csy%2Cacas%2Csx%2Cag%2Cdi%2Cdio%2Cdin%2Cdr%2Ccell-type%2Cderived-from-site%2Cmisspelling%2Cdt%2Cdtc%2Cdtu%2Cdtv%2Cgenome-ancestry&format=tsv&rows=2"
  )
  response <- AnnotationGx:::.perform_request(request2) |> AnnotationGx:::.parse_resp_tsv(show_col_types = FALSE, skip = 14)
  expect_equal(nrow(response), 2)
})


test_that("common_cellosaurus_fields returns the expected fields", {
  fields <- AnnotationGx::cellosaurus_fields(common = T, upper = T)
  expect_character(fields)
  expect_fields <- c(
    "id", "ac", "acas", "sy", "dr", "di", "din", "dio", "ox", "cc",  "sx", "ag", "oi",
    "hi", "ch", "ca",  "dt", "dtc", "dtu", "dtv", "from", "group"
  )


  expect_equal(fields, toupper(expect_fields))
})

test_that(".cellosaurus_extResources returns the expected external resources", {
  resources <- AnnotationGx:::.cellosaurus_extResources()
  expect_character(resources)

  expected_resources <- c(
    "4DN", "Abcam", "ABCD", "ABM", "AddexBio", "ArrayExpress",
    "ATCC", "BCGO", "BCRC", "BCRJ", "BEI_Resources",
    "BioGRID_ORCS_Cell_line", "BTO", "BioSample", "BioSamples",
    "cancercelllines", "CancerTools", "CBA", "CCLV", "CCRID",
    "CCTCC", "Cell_Biolabs", "Cell_Model_Passport", "CGH-DB",
    "ChEMBL-Cells", "ChEMBL-Targets", "CLDB", "CLO", "CLS",
    "ColonAtlas", "Coriell", "Cosmic", "Cosmic-CLP", "dbGAP",
    "dbMHC", "DepMap", "DGRC", "DiscoverX", "DSHB", "DSMZ",
    "DSMZCellDive", "EBiSC", "ECACC", "EFO", "EGA", "ENCODE",
    "ESTDAB", "FCDI", "FCS-free", "FlyBase_Cell_line", "GDSC",
    "GeneCopoeia", "GEO", "HipSci", "HIVReagentProgram", "Horizon_Discovery",
    "hPSCreg", "IARC_TP53", "IBRC", "ICLC", "ICLDB", "IGRhCellID",
    "IGSR", "IHW", "Imanis", "Innoprot", "IPD-IMGT/HLA", "ISCR",
    "IZSLER", "JCRB", "KCB", "KCLB", "Kerafast", "KYinno", "LiGeA",
    "LIMORE", "LINCS_HMS", "LINCS_LDP", "Lonza", "MCCL", "MeSH",
    "MetaboLights", "Millipore", "MMRRC", "NCBI_Iran", "NCI-DTP", "NHCDR",
    "NIHhESC", "NISES", "NRFC", "PerkinElmer", "PharmacoDB", "PRIDE",
    "Progenetix", "PubChem_Cell_line", "RCB", "Rockland", "RSCB", "SKIP",
    "SKY/M-FISH/CGH", "SLKBase", "TKG", "TNGB", "TOKU-E", "Ubigene",
    "WiCell", "Wikidata", "Ximbio"
  )

  expect_equal(resources, expected_resources)
})
