# AnnotationGx 0.0.5.9001
* Moved the ProxyManager connection to run-time from load time
  * This should make installing the pacakge much more reliable
* Many changes to get the package passing R CMD check
  * Only warnings RE: documentation remain

# AnnotationGx 0.0.4.9001

* Added a `NEWS.md` file to track changes to the package.
* Added `getGencodeGRangesAnnotated` function in `getGencode.R`
  * This method allows retrieval of a GRanges object from gencode and automatically adds additional selected metadata
