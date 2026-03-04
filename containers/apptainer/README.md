# AnnotationGx Apptainer Container for DNAnexus

This container builds a reproducible environment for AnnotationGx.

## Why build an apptainer for ARPA deliverables?

Our current tools such as AnnotationGx and AnnotationDB may undergo:

- R version changes
- Bioconductor releases
- System libraries
- Python / Node / API dependencies
- Database drivers

Without an apptainer, we run the risk of reduced **reproducibility such as installation issues, missing libraries etc.** 

For big projects like ARPA where running on HPC is warranted, Apptainers work best as HPC environments do not allow Docker and the need for security boundaries. This also ensures that easier onboarding of other groups who want to use our tools on DNAnexus.

## Build

From this directory:

    bash build.sh

Or manually:

    sudo apptainer build annotationgx.sif annotationgx.def

## Test

    apptainer shell annotationgx.sif
    R
    library(AnnotationGx)