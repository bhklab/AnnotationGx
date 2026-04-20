# Annotate PubChem Compound

This function retrieves information about a PubChem compound based on
the provided compound ID (CID).

## Usage

``` r
annotatePubchemCompound(
  cids,
  heading = "ChEMBL ID",
  source = NULL,
  parse_function = identity,
  query_only = FALSE,
  raw = FALSE,
  nParallel = 1
)
```

## Arguments

- cids:

  The compound ID (CID) of the PubChem compound.

- heading:

  The type of information to retrieve. Default is "ChEMBL ID".

- source:

  The data source to use. Default is NULL.

- parse_function:

  A custom parsing function to process the response. Default is the
  identity function.

- query_only:

  Logical indicating whether to return the query URL only. Default is
  FALSE.

- raw:

  Logical indicating whether to return the raw response. Default is
  FALSE.

- nParallel:

  The number of parallel processes to use. Default is 1.

## Value

The annotated information about the PubChem compound.

## Examples

``` r
annotatePubchemCompound(cid = 2244)
#> Waiting 30s for retry backoff ■■                              
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
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■               
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■            
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
#> Error: Assertion on 'heading' failed: FALSE.
annotatePubchemCompound(cid = c(2244, 67890), heading = "CAS")
#> Error: Assertion on 'heading' failed: FALSE.
```
