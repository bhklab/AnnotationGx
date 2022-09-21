#' @importFrom checkmate assertCharacter assert_subset
#' @importFrom data.table rbindlist %ilike% first as.data.table rbindlist fread
#' @importFrom rtracklayer import
#' @importFrom S4Vectors metadata metadata<- mcols
NULL


#' Retrieve the index table for the Gencode FTP web page
#'
#' @param url `character(1)` Address of Gencode FTP web page. Default is page
#'   for Gencode Human files.
#' @param recursive `logical(1)` Should the file tables be recursively retrieved
#'   for directories? Default is `FALSE`, setting to `TRUE` will be be slow due
#'   to the large number of directories to scrape.
#'
#' @return `data.table` Table of files and directories available on the
#'   Gencode FTP web page.
#'
#' @family Gencode
#'
#' @export
getGencodeFTPTable <- function(
        url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human",
        recursive=FALSE) {
    scrapeRemoteFTPFilesTable(url=url, recursive=recursive)
}


#' Retrieve a table listing all files available from Gencode for a specific
#'   database release.
#'
#' @param version `character(1)` Version number of Gencode annotations to
#'   retrieve file list for. Defaults to "latest", which is the most recent
#'   release. Use `getGencodeFTPTable()` to view available releases.
#'
#' @return `data.table` Table listing all files available for the selected
#'   `version`, along with URLs to download each file from.
#'
#' @family Gencode
#'
#' @export
getGencodeFilesTable <- function(version="latest",
        url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human") {
    stopifnot(version == "latest" || !is.na(as.integer(version)))
    url <- gsub("/$", "", url) # deal with trailing slash on url
    gencode_ftp <- getGencodeFTPTable(url=url)
    version_dir <- gsub("/$", "", gencode_ftp[Name %ilike% version, Name])
    version_url <- paste0(url, "/", version_dir)
    checkmate::assertCharacter(version_url, max.len=1, any.missing=FALSE)
    file_table <- scrapeRemoteFTPFilesTable(url=version_url, recursive=TRUE)
    file_table$gencode_version <- version_dir
    return(file_table)
}


#' Retrieve a list of files and their descriptions available for a Gencode
#' release and reference genome version.
#'
#' @param version `character(1)` Gencode version to download file list for.
#'   Defaults to "latest". See `?getGencodeFTPTable` for options.
#'   Versions prior to 10 are not currently supported.
#' @param dir `character(1)` Path to download the file to. Defaults to
#'   `tempdir()`. When this value is `tempdir()` the downloaded file is
#'   automatically deleted when the function exits.
#' @param chr `character(1)` Name of reference chromosome to fetch files for.
#'   Options are "GRCh38" (default) and "GRCh37".
#' @param url `character(1)` Address of Gencode FTP web page. Default is page
#'   for Gencode Human files.
#'
#' @return `data.table` With columns type, file, and description. Note that *
#'   in the returned file name is treated as a wildcard in `getGencodeFiles()`.
#'
#' @family Gencode
#'
#' @export
getGencodeAvailableFiles <- function(version="latest",
        chr=c("GRCh38", "GRCh37"), dir=tempdir(),
        url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human") {
    ## -- get available Gencode files
    gencode_files <- getGencodeFilesTable(version=version, url=url)
    chr <- match.arg(chr, choice=c("GRCh38", "GRCh37"))
    if (version == "latest" || as.integer(version) > 19) {
        grch37_idx <- grepl("GRCh37", gencode_files$Name)
        keep_idx <- if (chr == "GRCh37") grch37_idx else !grch37_idx
        gencode_files <- gencode_files[keep_idx, ]
    } else {
        if (as.integer(version) < 10)
            stop("Versions prior to 10 are not supported! Please download them manually.")
        if (chr == "GRCh38") {
            warning("The chr argument has been set to GRCh37 for Gencode ",
                "versions <= 19! ")
            chr <- "GRCh37"
        }
    }
    ## -- parse version number
    ver <- .infer_gencode_version(gencode_files)
    ## -- download and README and load as text
    readme_url <- gencode_files[Name %ilike% "_README", download_url]
    # fall back to use the global readme if it is missing for a specific release
    if (length(readme_url) != 1) {
        ftp_files <- getGencodeFTPTable()
        pattern <- "_README"
        pattern <- if (chr == "GRCh38") paste0(pattern, ".TXT") else
            paste0(pattern, "_GRCh37_mapping.txt")
        readme_url <- ftp_files[Name %ilike% pattern, download_url]
    }
    destfile <- file.path(dir, "_README.txt")
    if (!file.exists(destfile))
        download.file(readme_url, destfile=destfile, quiet=TRUE)
    if (dir == tempdir()) on.exit(unlink(destfile))
    readme_txt <- readLines(destfile)
    file_df <- if (grepl("lift", ver))
        .parse_lift_gencode_readme(readme_txt, ver) else
        .parse_normal_gencode_readme(readme_txt, ver)
    return(file_df)
}

#' @keywords internal
.infer_gencode_version <- function(gencode_files) {
    gencode_files[Name %ilike% "gencode\\.", data.table::first(Name)] |>
        gsub(pattern="^.*gencode\\.", replacement="") |>
        gsub(pattern="\\..*$", replacement="")
}


#' @keywords internal
.parse_normal_gencode_readme <- function(readme_txt, ver) {
    ## -- pull out the section text
    anchor_points <- c("^#Annotation files$", "^#Sequence files$",
        "^#Metadata files$", "^General format of the annotation files$")
    .vgrep <- function(patterns, string) vapply(patterns,
        FUN=function(p, x) grep(p, x), x=string, FUN.VALUE=numeric(1))
    split_idx <- .vgrep(anchor_points, string=readme_txt)
    chunks <- vector("list", length(split_idx) - 1)
    for (i in seq_len(length(split_idx) - 1)) {
        # offset of 2 on either end to remove section headers
        chunks[[i]] <- readme_txt[seq(split_idx[i] + 2, split_idx[i + 1] - 2)]
    }
    ## -- pull out the files and descriptions for each section
    file_dfs <- vector("list", length(chunks))
    for (i in seq_along(chunks)) {
        chunk_files <- c(grep("^\\d+\\. .*:", chunks[[i]]), length(chunks[[i]]))
        file_dfs[[i]] <- data.frame()
        for (j in seq_len(length(chunk_files) - 1)) {
            starti <- chunk_files[j]
            endi <- chunk_files[j + 1]
            file_name <- chunks[[i]][starti] |>
                gsub(pattern="^\\d+\\. |:$|\\s.*$", replacement="") |>
                gsub(pattern="vX", replacement=ver)
            description <- chunks[[i]][seq(starti + 1, endi - 1)] |>
                gsub(pattern="^\\s+", replacement="") |>
                paste0(collapse=" ")
            file_dfs[[i]] <- rbind(
                file_dfs[[i]],
                data.table(file=file_name, description=description)
            )
        }
    }
    ## -- clean up the file names and return
    names(file_dfs) <- c("GTF", "FASTA", "metadata")
    file_dfs[["GTF"]][, file := gsub("\\{|,gff3\\}", "", file)]
    file_dfs[["GFF3"]] <- copy(file_dfs[["GTF"]])[,
        file := gsub("gtf", "gff3", file)
    ]
    return(data.table::rbindlist(file_dfs, fill=TRUE, idcol="type"))
}


#' @keywords internal
.parse_lift_gencode_readme <- function(readme_txt, ver) {
    ## -- pull out the section text
    split_idx <- grep("^Release files$", readme_txt)
    ## -- pull out file sections
    file_sections <- readme_txt[-seq(1, split_idx)]
    chunk_idx <- c(grep("^\\*", file_sections), length(file_sections))
    file_df <- data.table()
    for (i in seq_len(length(chunk_idx) - 1)) {
        starti <- chunk_idx[i]
        endi <- chunk_idx[i + 1]
        file_name <- gsub("^\\* ", "", file_sections[starti]) |>
            gsub(pattern="vXlift..", replacement=ver)
        description <- paste0(
            gsub("^\\s+|\\s+$", "", file_sections[seq(starti + 1, endi - 1)]),
            collapse=" "
        )
        file_df <- rbind(file_df,
            data.table(file=file_name, description=description)
        )
    }
    file_df <- file_df[,
        .(file=unlist(strsplit(file, split=", "))),
        by=description
    ]
    file_df[, type := "metadata"]
    file_df[file %ilike% "\\.gtf", type := "GTF"]
    file_df[file %ilike% "\\.fa", type := "FASTA"]
    file_df[file %ilike% "\\.gff3", type := "GFF3"]
    return(file_df[])
}


#' Download files from the Gencode FTP site and load them as the appropriate
#'   Bioconductor classes.
#'
#' @description
#' File descriptions are available at https://www.gencodegenes.org/human/.
#'
#' @param file `character(1)` String name of file to download from the
#'   Gencode FTP site. See `getGencodeAvailableFiles()` for options. Supports
#'   regex to match file names. If your version is specified as "{v}" it will
#'   be interpolated from the `version` argument.
#' @param type `character(1)` One of "GTF", "GFF3", "FASTA" or "metadata".
#'   Defaults to `infer_gencode_type(file)`, which guesses based on the
#'   `file` string.
#' @param version `character(1)` Gencode version to download from.
#'   Defaults to "latest". See `?getGencodeFTPTable` for options.
#' @param dir `character(1)` Path to download the file to. Defaults to
#'   `tempdir()`. When this value is `tempdir()` the downloaded file is
#'   automatically deleted when the function exits.
#' @param url `character(1)` Address of Gencode FTP web page. Default is page
#'   for Gencode Human files.
#'
#' @family Gencode
#'
#' @details
#'
#'
#' @return `GenomicRanges` object when type="GTF", `DNAStringSet` when
#'   type="FASTA", or `data.table`/`character` (as appropriate) when
#'   type="metadata".
#'
#' @export
getGencodeFile <- function(
        file="gencode\\.{v}\\.annotation\\.gtf\\.gz",
        type=infer_gencode_type(file),
        version="latest",
        chr=c("GRCh38", "GRCh37"),
        dir=tempdir(),
        url="https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human") {
    # validate input
    chr <- match.arg(chr, choices=c("GRCh38", "GRCh37"))
    valid_files <- getGencodeAvailableFiles(version=version, chr=chr,
        dir=dir, url=url)
    # interpolate the version, if possible
    ver <- .infer_gencode_version(valid_files[, .(Name=file)])
    file <- gsub("\\{v\\}", ver, file)
    checkmate::assert_character(file, max.len=1, any.missing=FALSE)
    if (!any(grepl(file, valid_files$file))) {
        stop("Invalid file name or pattern, choose from: \n\t",
            paste0(valid_files$file, collapse="\n\t"))
    }
    # download available files for selected Gencode version
    gencode_files <- getGencodeFilesTable(version=version, url=url)
    # find the file name and url
    file_arg <- file
    exact_file <- valid_files[file %ilike% file_arg, file]
    match_row <- gencode_files[Name %ilike% exact_file, .(Name, download_url)]
    stopifnot(nrow(match_row) == 1)
    file_name <- basename(match_row[["Name"]])
    download_url <- match_row[["download_url"]]
    destfile <- file.path(dir, file_name)
    # delete the file on exit if user doesn't provide a custom directory
    if (dir == tempdir()) on.exit(unlink(destfile))
    # download and load the file
    if (!file.exists(destfile)) download.file(download_url, destfile=destfile)
    gencode_data <- switch(type,
        "GTF"=rtracklayer::import(destfile),
        "FASTA"={
            fasta <- rtracklayer::FastaFile(destfile)
            if (grepl("genome", destfile)) {
                rtracklayer::import(fasta)
            } else if (grepl("transcript", destfile)) {
                rtracklayer::import(fasta)
            } else if (grepl("translation", destfile)) {
                rtracklayer::import(fasta, type="AA")
            }
        },
        "metadata"=tryCatch({
            data.table::fread(destfile, header=FALSE)
        }, error=function(e) {
            warning("Failed to read with data.table::fread, falling back to ",
                "readLines: \n", e)
            readLines(if (grepl("\\.gz$", destfile)) gzfile(destfile) else
                destfile)
        }),
        stop("Unknown type: ", type)
    )
    if (hasMethod("metadata", class(gencode_data))) {
        metadata(gencode_data) <- c(metadata(gencode_data),
            list(file=file_name, download_url=download_url))
    } else {
        attributes(gencode_data)$source <- list(file=file_name, download_url=download_url)
    }
    return(gencode_data)
}


#' Guess the type of a Gencode file from its name
#'
#' @param file `character(1)` File name.
#'
#' @return `character(1)` One of "GTF", "GFF3", "FASTA" or "metadata" based on
#'   the file extensions of `file`.
#'
#' @family Gencode
#'
#' @export
infer_gencode_type <- function(file) {
    type <- "metadata"
    if (grepl(".gtf", file))
        type <- "GTF"
    if (grepl(".gff3", file))
        type <- "GFF3"
    if (grepl(".fa", file))
        type <- "FASTA"
    return(type)
}


#' Retrieve a GRanges object for a specific Gencode release with added metadata
#'
#' @param annotation `character(1)` Name of the annotation to retrieve. Currently
#'   only supports, and defaults to, "SwissProt". More will be implemented in
#'   the future.
#' @param ... `pairlist()` Fall through arguments to `getGencodeFile` when
#'   retreiving the annotaton files. See `?getGencodeFile` for details.
#'
#' @family Gencode
#'
#' @return `GRanges` Gencode genome annotations for the file retreived
#'   using `getGencodeFile(...)`, with annotations added from the selected
#'   Gencode metadata file.
#'
#' @export
getGencodeGRangesAnnotated <- function(annotation="SwissProt", ...) {
    annot_args <- switch(annotation,
        "SwissProt"=list(
            colnames=c("transcript_id", "uniprot_id", "uniprot_id_variant"),
            join_col=c("transcript_id")
        ),
        stop("Support for selected annotation not implement yet: ",
            annotation)
    )
    .genome <- getGencodeFile(...)
    dots <- list(...)
    dots[["file"]] <- paste0(".*", annotation, ".*")
    dots[["type"]] <- "metadata"
    annot <- do.call("getGencodeFile", args=dots)
    colnames(annot) <- annot_args[["colnames"]]
    .mcols <- data.table::as.data.table(as.data.frame(S4Vectors::mcols(.genome)))
    .mcols[, i := .I]  # capture original index, to deal with duplicated rows in join
    annotated_mcols <- merge(.mcols, annot, by=annot_args[["join_col"]],
        all.x=TRUE, sort=FALSE)
    has_duplicates <- annotated_mcols[, i %in% i[duplicated(i)]]
    duplicated_rows <- annotated_mcols[has_duplicates, ]
    dedup_rows <- duplicated_rows[,
        lapply(.SD, function(x)
            if (!all(is.na(x))) paste0(na.omit(unique(x)), collapse="|")
            else as.character(unique(x))),
        by=c("i")
    ]
    annotated_mcols <- rbind(
        annotated_mcols[!has_duplicates, ],
        dedup_rows
    )
    data.table::setorderv(annotated_mcols, "i")
    annotated_mcols[, i := NULL]
    S4Vectors::mcols(.genome) <- annotated_mcols
    return(.genome)
}