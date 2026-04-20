# Retrieve PubChem compound information

This function retrieves compound information from PubChem using the
PubChem REST API. Used by other functions to retrieve compound
information.

## Usage

``` r
getPubchemCompound(
  ids,
  from = "cid",
  to = "property",
  properties = c("Title", "InChIKey"),
  raw = FALSE,
  query_only = FALSE,
  output = "JSON",
  ...
)
```

## Arguments

- ids:

  A vector of compound identifiers.

- from:

  The source namespace of the compound identifiers. Default is 'cid'.

- to:

  The target namespace for the compound information. Default is
  'property'.

- properties:

  A character vector specifying the properties to retrieve.

- raw:

  Logical indicating whether to return the raw query results. Default is
  FALSE.

- query_only:

  Logical indicating whether to only perform the query without
  retrieving the results. Default is FALSE.

- output:

  The format of the query results. Default is 'JSON'.

- ...:

  Additional arguments to be passed to the query_pubchem_rest function.

## Value

A data.table containing the retrieved compound information.

## Examples

``` r
properties <- c("Title", "MolecularFormula", "InChIKey", "CanonicalSMILES")
getPubchemCompound(c(3672, 176870), from = "cid", to = "property", properties = properties)
#> Waiting 30s for retry backoff ■■                              
#> Waiting 30s for retry backoff ■■■■■                           
#> Waiting 30s for retry backoff ■■■■■■■■                        
#> Waiting 30s for retry backoff ■■■■■■■■■■■                     
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■                  
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■               
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■            
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■         
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■      
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Waiting 30s for retry backoff ■■                              
#> Waiting 30s for retry backoff ■■■■■                           
#> Waiting 30s for retry backoff ■■■■■■■■                        
#> Waiting 30s for retry backoff ■■■■■■■■■■■                     
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■                  
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■                
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■             
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■         
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■      
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Waiting 30s for retry backoff ■■                              
#> Waiting 30s for retry backoff ■■■■                            
#> Waiting 30s for retry backoff ■■■■■■■                         
#> Waiting 30s for retry backoff ■■■■■■■■■■                      
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■                   
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■                
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■             
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■          
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■       
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Waiting 30s for retry backoff ■■                              
#> Waiting 30s for retry backoff ■■■■                            
#> Waiting 30s for retry backoff ■■■■■■■                         
#> Waiting 30s for retry backoff ■■■■■■■■■■                      
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■                   
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■                
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■             
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■          
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■       
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Querying PubCHEM REST API.... ■■■■■■■■■■■■■■■■                  50% | ETA:  2m
#> Waiting 30s for retry backoff ■■■■                            
#> Waiting 30s for retry backoff ■■■■■■■                         
#> Waiting 30s for retry backoff ■■■■■■■■■■                      
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■                   
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■                
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■             
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■          
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■       
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Querying PubCHEM REST API.... ■■■■■■■■■■■■■■■■                  50% | ETA:  2m
#> Waiting 30s for retry backoff ■■■■                            
#> Waiting 30s for retry backoff ■■■■■■■                         
#> Waiting 30s for retry backoff ■■■■■■■■■■                      
#> Waiting 30s for retry backoff ■■■■■■■■■■■■                    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■                 
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■              
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■           
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■       
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Querying PubCHEM REST API.... ■■■■■■■■■■■■■■■■                  50% | ETA:  2m
#> Waiting 30s for retry backoff ■■■                             
#> Waiting 30s for retry backoff ■■■■■■                          
#> Waiting 30s for retry backoff ■■■■■■■■■                       
#> Waiting 30s for retry backoff ■■■■■■■■■■■■                    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■                 
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■              
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■           
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■        
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■     
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Querying PubCHEM REST API.... ■■■■■■■■■■■■■■■■                  50% | ETA:  2m
#> Waiting 30s for retry backoff ■■■                             
#> Waiting 30s for retry backoff ■■■■■■                          
#> Waiting 30s for retry backoff ■■■■■■■■■                       
#> Waiting 30s for retry backoff ■■■■■■■■■■■■                    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■                 
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■              
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■           
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■        
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■     
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> Querying PubCHEM REST API.... ■■■■■■■■■■■■■■■■                  50% | ETA:  2m
#> Querying PubCHEM REST API.... ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> [14:02:05][WARNING][AnnotationGx::getPubchemCompound]  Some queries failed. See the 'failed' object for details. 
#>    property/Title,MolecularFormula,InChIKey,CanonicalSMILES
#>                                                       <int>
#> 1:                                                       NA
#> 2:                                                       NA
```
