CardinalVis
===========

Shiny interface for Cardinal

Install by running the following in R:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("kuwisdelu/CardinalVis")
```


Get started with an example dataset by running:

```
if (!requireNamespace("CardinalWorkflows", quietly = TRUE))
    BiocManager::install("CardinalWorkflows")

library(CardinalVis)

data(pig206, package="CardinalWorkflows")

msiVis(pig206)
```
