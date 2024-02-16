

.get_cellosaurus_schema <- function(){
    url <- .buildURL("https://api.cellosaurus.org/openapi.json")
    request <- .build_request(url)

    resp <- .perform_request(request)
    .parse_resp_json(resp)
}


.cellosaurus_fields <- function(){
    schema <- .get_cellosaurus_schema()
    schema$components$schemas$Fields$enum
}



.build_cellosaurus_request <- function(
    from= "id", query="Hela", to = c("id", "ac", "ca", "sx", "ag", "di", "derived-from-site",  "misspelling"),
    numResults = 1, extResource = NULL, apiResource= "search/cell-line", output = "TSV"
){
    checkmate::assert_character(c(from, query, output))
    checkmate::assert_subset(to, .cellosaurus_fields())
    checkmate::assert_choice(apiResource, c("search/cell-line", "cell-line", "release-info"))
    checkmate::assert_choice(output, c("TSV", "TXT", "JSON", "XML"))

    opts <- list()
    opts$q <- {
        if (from == "dr") paste0("dr:", extResource, ";", query)
        else  paste0(from, ":", query)
    }
    opts$fields <- paste(to, collapse = ",")
    opts$format <- tolower(output)
    opts$rows <- numResults


    base_url <- "https://api.cellosaurus.org"
    url <- httr2::url_parse(base_url)
    url$path <- .buildURL(url$path, apiResource)
    url$query <- opts
    url |> httr2::url_build() |> .build_request()
}
# .warn("Parsing only available for TSV, will return raw response.")

# "cellosaurusId",
# "depmapId",
# "sangerModelId",
# "atccId",
# "cellLineName"




# 4DN|Abcam|ABCD|ABM|AddexBio|ArrayExpress|ATCC|BCGO|BCRC|BCRJ|BEI_Resources|
# BioGRID_ORCS_Cell_line|BTO|BioSample|BioSamples|cancercelllines|CancerTools|
# CBA|CCLV|CCRID|CCTCC|Cell_Biolabs|Cell_Model_Passport|CGH-DB|ChEMBL-Cells|ChEMBL-Targets|
# CLDB|CLO|CLS|ColonAtlas|Coriell|Cosmic|Cosmic-CLP|dbGAP|dbMHC|DepMap|DGRC|DiscoverX|DSHB|
# DSMZ|DSMZCellDive|EBiSC|ECACC|EFO|EGA|ENCODE|ESTDAB|FCDI|FCS-free|FlyBase_Cell_line|GDSC|
# GeneCopoeia|GEO|HipSci|HIVReagentProgram|Horizon_Discovery|hPSCreg|IARC_TP53|IBRC|ICLC|ICLDB|
# IGRhCellID|IGSR|IHW|Imanis|Innoprot|IPD-IMGT/HLA|ISCR|IZSLER|JCRB|KCB|KCLB|Kerafast|KYinno|LiGeA|
# LIMORE|LINCS_HMS|LINCS_LDP|Lonza|MCCL|MeSH|MetaboLights|Millipore|MMRRC|NCBI_Iran|NCI-DTP|NHCDR|
# NIHhESC|NISES|NRFC|PerkinElmer|PharmacoDB|PRIDE|Progenetix|PubChem_Cell_line|RCB|Rockland|RSCB|SKIP|
# SKY/M-FISH/CGH|SLKBase|TKG|TNGB|TOKU-E|Ubigene|WiCell|Wikidata|Ximbio

# Cell_Model_Passport, DepMap, ATCC, Cosmic, Cosmic-CLP