#' Adds a new column to the drugs with ids table
#' 
#' @param drug_all assumed to be the drugs.with.ids table as data frame with 
#' rownames of unique.drugid
#' @param new_mapping assumed to be a data frame with two columns, the first 
#' one named unique.drugid, and the second one named "dataset".drugid, for the 
#' new column name in the drugs with ids table. The rownames are also assumed to 
#' match unique.drugid (in part to ensure uniqueness of this id)
#' 
#' @md
#' @export
addAnnotationColumnToDrugs <- function(drug.all, new_mapping){
​
	drug_all[[colnames(new_mapping)[2]]] <- NA_character_
	existing_drugs <- new_mapping[new_mapping[, "unique.drugid"] %in% 
		drug_all$unique.drugid, "unique.drugid"]
	new_drugs <- new_mapping[!new_mapping[,"unique.drugid"] %in% 
		drug_all$unique.drugid,"unique.drugid"]
​
	drug_all[existing_drugs, colnames(new_mapping)[2]] <- 
		new_mapping[existing_drugs,2]
	drug_all_new <- drug_all[new_drugs,]
	drug_all_new[, c("unique.drugid", colnames(new_mapping)[2])] <- 
		new_mapping[new_drugs, ]
	drug_all <- rbind(drug_all, drug_all_new)
	return(drug_all)
}