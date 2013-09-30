#! /bin/sh

## Quit immediately on error
set -e

## Want to run this as 'vagrant', so rerun if root
if [ "$(id -u)" = "0" ]; then
    sudo -u vagrant bash $0 $@
    exit 0
fi

R=${1-R}

echo '
    source("http://bioconductor.org/biocLite.R")
    install.packages(c("Rmpi", "snow"))
    biocLite(suppressUpdates=TRUE, suppressAutoUpdate=TRUE, 
             c("BiocGenerics", "methods", "isa2", "Biobase", "AnnotationDbi", 
                "Category", "genefilter", "DBI", "igraph", "GOstats",
                "GO.db", "KEGG.db", "biclust", "MASS", "xtable", "ALL", 
                "hgu95av2.db", "targetscan.Hs.eg.db", "org.Hs.eg.db",
                "GEOquery", "affy", "gcrma", "mouse4302.db")
            )
' | ${R} --no-save

