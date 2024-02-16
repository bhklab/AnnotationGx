

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


.cellosaurus_extResources <- function(){
    c("4DN", "Abcam", "ABCD", "ABM", "AddexBio", "ArrayExpress",
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
}