library(AnnotationGx)
library(testthat)
library(checkmate)


test_that(".get_cellosaurus_schema is acting as expected",{
    schema <- AnnotationGx:::.get_cellosaurus_schema()
    expect_list(schema)
    names_list <- c("openapi", "info", "paths", "components", "tags")

    expect_names(names(schema), subset.of = names_list)
})

test_that(".cellosaurus_fields is acting as expected",{
    fields <- AnnotationGx:::.cellosaurus_fields()
    expect_character(fields)

    # important fields
    important_fields <- c("id", "ac", "sy", "acas", "sx", "ag", "di", "dio", "din",
        "dr", "cell-type", "derived-from-site", "misspelling", "dt", "dtc", "dtu", "dtv", "genome-ancestry",
        "from", "resistance", "transfected")

    expect_true(all(important_fields %in% fields))
    expect_names(important_fields, subset.of = fields)
})

test_that(".build_cellosaurus_request is acting as expected",{
    request <- AnnotationGx:::.build_cellosaurus_request()

    expect_class(request,"httr2_request")
    expect_equal(request$url, "https://api.cellosaurus.org/search/cell-line?q=id%3AHeLa&fields=id%2Cac%2Chi%2Cca%2Csx%2Cag%2Cdi%2Cderived-from-site%2Cmisspelling&format=tsv&rows=1")

    response <- AnnotationGx:::.perform_request(request) |> AnnotationGx:::.parse_resp_tsv(show_col_types = FALSE, skip = 14)
    expect_class(response, "spec_tbl_df")
    expect_equal(nrow(response), 1)

    request2 <- AnnotationGx:::.build_cellosaurus_request(
        query = "id:HeLa",
        to = c("id", "ac", "sy", "acas", "sx", "ag", "di", "dio", "din", "dr", "cell-type",
            "derived-from-site", "misspelling", "dt", "dtc", "dtu", "dtv", "genome-ancestry"
        ),
        numResults = 2, apiResource = "search/cell-line", output = "TSV")
    expect_equal(request2$url,
    "https://api.cellosaurus.org/search/cell-line?q=id%3AHeLa&fields=id%2Cac%2Csy%2Cacas%2Csx%2Cag%2Cdi%2Cdio%2Cdin%2Cdr%2Ccell-type%2Cderived-from-site%2Cmisspelling%2Cdt%2Cdtc%2Cdtu%2Cdtv%2Cgenome-ancestry&format=tsv&rows=2")
    response <- AnnotationGx:::.perform_request(request2) |> AnnotationGx:::.parse_resp_tsv(show_col_types = FALSE, skip = 14)
    expect_equal(nrow(response), 2)

})
test_that(".common_cellosaurus_fields returns the expected fields", {
  fields <- AnnotationGx:::.common_cellosaurus_fields()
  expect_character(fields)
  
  expected_fields <- c("ID", "AC", "AS", "SY", "DR", "DI", "DIN", "DIO", "OX", "SX", "AG", "OI",
                      "HI", "CH", "CA", "CEL", "DT", "DTC", "DTU", "DTV", "DER", "FROM", "GROUP",
                      "KARY", "KO")
  
  expect_equal(fields, expected_fields)
})
test_that(".cellosaurus_extResources returns the expected external resources", {
  resources <- AnnotationGx:::.cellosaurus_extResources()
  expect_character(resources)
  
  expected_resources <- c("4DN", "Abcam", "ABCD", "ABM", "AddexBio", "ArrayExpress",
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
    "WiCell", "Wikidata", "Ximbio")
  
  expect_equal(resources, expected_resources)
})