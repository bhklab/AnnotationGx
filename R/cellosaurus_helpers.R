.create_query_list <- function(ids, from){
    # either from has to be one field, or the same length as ids
    if(length(from) == 1) return(paste0(from,":",ids))
    if(length(from) != length(ids)){
        stop("Length of from must be 1 or the same length as ids")
    }
    lapply(1:length(ids), function(i){
        paste(from[i], ids[i])
    })
}

.build_cellosaurus_request <- function(
    query = c("id:HeLa"),
    to = c("id", "ac", "ca", "sx", "ag", "di", "derived-from-site",  "misspelling"),
    numResults = 1, apiResource= "search/cell-line", output = "TSV",
    query_only = FALSE, fuzzy = FALSE, ...
){
    # checkmate::assert_character(c(from, query, output))
    checkmate::assert_subset(to, .cellosaurus_fields())
    checkmate::assert_choice(apiResource, c("search/cell-line", "cell-line", "release-info"))
    checkmate::assert_choice(output, c("TSV", "TXT", "JSON", "XML"))

    opts <- list()
    opts$q <- paste0(query, collapse = " ")
    # if fuzzy, add a tilde to the query
    if(fuzzy) opts$q <- paste0(opts$q, "~")

    opts$fields <- paste0(to, collapse = ",")
    opts$format <- tolower(output)
    opts$rows <- numResults


    base_url <- "https://api.cellosaurus.org"
    url <- httr2::url_parse(base_url)
    url$path <- .buildURL(url$path, apiResource)
    url$query <- opts
    url <- url |> httr2::url_build()
    if(query_only) return(url)
    url |>  .build_request()
}

.get_cellosaurus_schema <- function(){
    url <- .buildURL("https://api.cellosaurus.org/openapi.json")
    request <- .build_request(url)

    resp <- .perform_request(request)
    .parse_resp_json(resp)
}

#  The definition of the Cellosaurus is provided in the following format:

#  ---------  ------------------------------  -----------------------
#  Line code  Content                         Occurrence in an entry
#  ---------  ------------------------------  -----------------------
#  ID         Identifier (cell line name)     Once; starts an entry
#  AC         Accession (CVCL_xxxx)           Once
#  AS         Secondary accession number(s)   Optional; once
#  SY         Synonyms                        Optional; once
#  DR         Cross-references                Optional; once or more
#  RX         References identifiers          Optional: once or more
#  WW         Web pages                       Optional; once or more
#  CC         Comments                        Optional; once or more
#  ST         STR profile data                Optional; twice or more
#  DI         Diseases                        Optional; once or more
#  OX         Species of origin               Once or more
#  HI         Hierarchy                       Optional; once or more
#  OI         Originate from same individual  Optional; once or more
#  SX         Sex of cell                     Optional; once
#  AG         Age of donor at sampling        Optional; once
#  CA         Category                        Once
#  DT         Date (entry history)            Once
#  //         Terminator                      Once; ends an entry
.cellosaurus_fields <- function(){
    schema <- .get_cellosaurus_schema()
    schema$components$schemas$Fields$enum
}


#  ---------  --------------------------------------  -------------------------------------------------
#  Line code  Content                                 Description
#  ---------  --------------------------------------  -------------------------------------------------
#  ID         Recommended name                        Most frequently the name of the cell line as provided in the original publication.
#  AC         Primary accession                       It is the unique identifier of the cell line. It is normally stable across Cellosaurus versions but when two entries are merged, one of the two accessions stays primary while the second one becomes secondary (see ACAS)
#  AS         Primary and secondary accession         Secondary accession are former primary accession kept here to ensure the access to a cell line via old identifiers.
#  SY         List of synonyms                        We try to list all the different synonyms for the cell line, including alternative use of lower and upper cases characters. Misspellings are not included in synonyms (see the "misspelling" tag).
#  DR         Cross-references to external resources  A cross-reference has two parts: the short name of the resource (i.e. CCLE) and an identifier used to locate a particular entry of the resource related to the cell line.
#  DI         Disease(s)                             Disease(s) suffered by the individual from which the cell line originated with its NCI Thesaurus or ORDO identifier.
#  DI         Disease(s)                             Disease(s) suffered by the individual from which the cell line originated with its NCI Thesaurus or ORDO identifier.
#  DIN        Disease(s)                             Disease(s) suffered by the individual from which the cell line originated, restricted to diseases having a NCI Thesaurus identifier.
#  DIO        Disease(s)                             Disease(s) suffered by the individual from which the cell line originated, restricted to diseases having an ORDO identifier.
#  OX         Species of origin                      Species of the individual from which the cell line originates with its NCBI taxon identifier.
#  SX         Sex of the individual                 Sex of the individual from which the cell line originates.
#  AG         Age of donor at sampling               Age at sampling time of the individual from which the cell line was established.
#  OI         Cell line(s) originating from same individual(s)  Cell line(s) originating from same individual (sister cell lines).
#  HI         Parent cell line                       Parent cell line from which the cell line originates.
#  CH         Cell line(s) originated from the cell line  Cell line(s) originated from the cell line (child cell lines).
#  CA         Category                                Category to which a cell line belongs, one of 14 defined terms. Example: cancer cell line, hybridoma, transformed cell line.
#  CEL        Cell type                               Cell type from which the cell line is derived.
#  DT         Creation date                           Creation date, last modification date and version number of the cell line Cellosaurus entry.
#  DTC        Creation date                          Creation date of the cell line Cellosaurus entry.
#  DTU        Last modification date                 Last modification date of the cell line Cellosaurus entry.
#  DTV        Version number                         Version number of the cell line Cellosaurus entry.
#  DER        Derived from site                      Body part (tissue or organ) the cell line is derived from.
#  FROM       From                                    Laboratory, research institute, university having established the cell line.
#  GROUP      Group                                   Specific group the cell line belongs to (example: fish cell lines, vaccine production cell lines).
#  KARY       Karyotype                               Information relevant to the chromosomes of a cell line (often to describe chromosomal abnormalities).
#  KO         Knockout                                Gene(s) knocked-out in the cell line and method to obtain the KO.
#  //         Terminator                             Once; ends an entry
.common_cellosaurus_fields <- function(){
    c("ID", "AC", "AS", "SY", "DR", "DI", "DIN", "DIO", "OX", "SX", "AG", "OI",
    "HI", "CH", "CA", "CEL", "DT", "DTC", "DTU", "DTV", "DER", "FROM", "GROUP",
    "KARY", "KO")
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