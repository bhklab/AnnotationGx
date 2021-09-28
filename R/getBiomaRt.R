getBiomaRt <- function() {

    # -- connect to ensembl to map gene symbols
    require('biomaRt')  ## FIXME:: Remove this when using as package
    ensemblRat <- useEnsembl('genes', 'rnorvegicus_gene_ensembl')
    ensemblHuman <- useEnsembl('genes', 'hsapiens_gene_ensembl')
    ratAttrs <- as.data.table(listAttributes(ensemblRat))
    humanAttrs <- as.data.table(listAttributes(ensemblHuman))

    # get symbols available for each species
    humanSymbols <- grep('symbol', humanAttrs$name, value=TRUE)
    ratSymbols <- grep('symbol', ratAttrs$name, value=TRUE)

    # function to query biomaRt
    .getBM <- function(filter, attributes, values, mart)
        getBM(filter=filter, attributes=attributes, values=values, mart=mart)

    # parallelize queries on each symbol
    humanMapping <- rbindlist(bplapply(humanSymbols, .getBM,
        attributes=c(humanSymbols, 'ensembl_gene_id'), values=pathway$symbol,
        mart=ensemblHuman))
    ratMapping <- rbindlist(bplapply(ratSymbols, .getBM,
        attributes=c(ratSymbols, 'ensembl_gene_id'), values=pathway$symbol,
        mart=ensemblRat))

    # melt so the all the symbols are in one column
    humanMapping <- melt(humanMapping, id.vars='ensembl_gene_id',
        measure.vars=humanSymbols, variable.name='symbol_type',
        value.name='symbol')
    ratMapping <- melt(ratMapping, id.vars='ensembl_gene_id',
        measure.vars=ratSymbols, variable.name='symbol_type',
        value.name='symbol')

    # merge all the symbols into a single table mapping symbol to ensembl id
    geneSymbolDT <- rbind(humanMapping, ratMapping)

}