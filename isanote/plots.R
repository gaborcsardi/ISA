
library(eisa)
library(ALL)
library(hgu95av2.db)
library(affy)
library(GO.db)
data(ALL)

thr.gene <- 2.7
thr.cond <- 1.4
set.seed(1) # to get the same results, always
modules <- ISA(ALL, thr.gene=thr.gene, thr.cond=thr.cond)

# Condition plot
col <- ifelse( grepl("^B", pData(modules)$BT), "darkolivegreen", "orange")
pdf("condplot.pdf", width=10, height=5)
condPlot(modules, 1, ALL, col=col)
dev.off()

# Profile plot
png("profileplot.png")
profilePlot(modules, 2, ALL, plot="both")
dev.off()
pdf("profileplot.pdf")
profilePlot(modules, 2, ALL, plot="both")
dev.off()

# Expression plot
ep <- expPlotCreate(ALL, modules, 5)
pdf("expplot.pdf")
expPlot(ep)
dev.off()

# Another module set for the module tree

varLimit <- 0.5
kLimit <- 4
ALimit <- 5
flist <- filterfun(function(x) var(x)>varLimit, kOverA(kLimit,ALimit))
ALL.filt <- ALL[genefilter(ALL, flist), ]
ALL.filt2 <- ALL.filt[, grepl("^B", ALL.filt$BT)]

set.seed(1)
modules <- ISA(ALL.filt2, flist=NA, thr.gene=seq(2,4,by=0.5), thr.cond=1)

modules2 <- ISASweep(ALL.filt2, modules)

G <- ISASweepGraph(modules2)

GO <- ISAGO(modules2)
p.bp <- sapply(summary(GO$BP), function(x) as.integer(-log10(x$Pvalue[1])))
c.bp <- d.bp <- sapply(sigCategories(GO$BP), function(x) x[1])
d.bp[!is.na(c.bp)] <- sapply(mget(na.omit(c.bp), GOTERM), Term)
d.bp <- abbreviate(d.bp, 6)
colbar <- hcl(h=260, c=35, l=seq(30, 100, length=20))
colbar <- c("#FFFFFF", rev(colbar))
col <- colbar[p.bp]

G2 <- delete.vertices(G, match(c(19,24,27,28,29,30,32,33,35:41), V(G)$id)-1)
G2$layout <- G2$layout[V(G2)$id,]

pdf("moduletree.pdf", width=10, height=6)
ISASweepGraphPlot(G2, vertex.color=col[V(G2)$id], vertex.size2=50, 
                  vertex.label.topleft=d.bp[V(G2)$id],
                  vertex.label.topright=paste(V(G)$noFeatures, sep=",", 
                    V(G)$noSamples)[V(G2)$id])
key <- na.omit(d.bp[V(G2)$id])[unique(names(na.omit(d.bp)))]
key2 <- paste(key, sep=": ", names(key))
legend("topright", key2, bg="white")
dev.off()

