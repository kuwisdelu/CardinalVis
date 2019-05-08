CardinalVis
===========

Shiny interface for Cardinal

Install by running the following in R:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")

BiocManager::install("kuwisdelu/CardinalVis")
```


Get started with an example dataset by running:

```
if (!requireNamespace("CardinalWorkflows", quietly = TRUE))
    BiocManager::install("CardinalWorkflows")

library(CardinalVis)

data(pig206, package="CardinalWorkflows")
pig206 <- as(pig206, "MSImagingExperiment")

msiVis(pig206)
```
