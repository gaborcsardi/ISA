
isa.in.silico <- function(num.genes=300, num.conds=50, num.tf=3, tf.per.gene=1,
                          tf.per.cond=tf.per.gene,
                          mod.gene.size=round(.5*num.genes/num.tf),
                          mod.cond.size=round(.5*num.conds/num.tf),
                          noise=0.1,
                          mod.signal=rep(1, num.tf),
                          mod.noise=rep(0, num.tf), OL=0) {

  if (max(mod.gene.size) > num.genes || max(mod.cond.size) > num.conds) {
    stop("Inconsistent data configuration")
  }

  if (length(mod.noise) != num.tf) {
    stop("Invalid `mod.noise' length")
  }
  
  mod.gene.size <- sort(rep(mod.gene.size, length=num.tf), decreasing=TRUE)
  mod.cond.size <- sort(rep(mod.cond.size, length=num.tf), decreasing=TRUE)

  mod.signal <- rep(mod.signal, length=num.tf)
  
  geneMod <- matrix(0, nr=num.genes, nc=num.tf)
  condMod <- matrix(0, nr=num.conds, nc=num.tf)

  geneP <- condP <- 0
  data <- matrix(0, nr=num.conds, nc=num.genes)

  for (i in 1:num.tf) {
    geneMod[(max(1,1+geneP-round(OL*mod.gene.size[i]))):
            (geneP+round((1-OL)*mod.gene.size[i])), i] <- sqrt(mod.signal[i])
    condMod[(max(1,1+condP-round(OL*mod.cond.size[i]))):
            (condP+round((1-OL)*mod.cond.size[i])), i] <- sqrt(mod.signal[i])
    geneP <- geneP+round((1-OL)*mod.gene.size[i])
    condP <- condP+round((1-OL)*mod.cond.size[i])

    sel.a <- which(geneMod[,i] != 0)
    sel.b <- ceiling(num.conds * matrix(runif(length(sel.a)*tf.per.gene),
                                        nr=length(sel.a), nc=tf.per.gene))
    for (j in 1:length(sel.a)) {
      for (k in seq(length=tf.per.gene)) {
        data[ sel.b[j,k], sel.a[j] ] <- mod.signal[i]
      }
    }
    sel.a <- which(condMod[,i] != 0)
    sel.b <- ceiling(num.genes * matrix(runif(length(sel.a)*tf.per.cond),
                                        nr=length(sel.a), nc=tf.per.cond))
    for (j in 1:length(sel.a)) {
      for (k in seq(length=tf.per.cond)) {
        data[ sel.a[j], sel.b[j,k] ] <- mod.signal[i]
      }
    }
  }
  
  data[] <- pmax(data, condMod %*% t(geneMod))
  data[] <- data + rnorm(length(data), mean=0, sd=noise)
  geneMod[] <- ifelse(geneMod != 0, 1, 0)
  condMod[] <- ifelse(condMod != 0, 1, 0)
  
  geneP <- condP <- 0
  for (i in 1:num.tf) {
    g.from <- (max(1,1+geneP-round(OL*mod.gene.size[i])))
    g.to <- (geneP+round((1-OL)*mod.gene.size[i]))
    c.from <- (max(1,1+condP-round(OL*mod.cond.size[i])))
    c.to <- (condP+round((1-OL)*mod.cond.size[i]))
    data[c.from:c.to, g.from:g.to] <- data[c.from:c.to, g.from:g.to] +
      rnorm( (g.to-g.from+1) * (c.to-c.from+1), mean=0, sd=mod.noise[i] )
    geneP <- geneP+round((1-OL)*mod.gene.size[i])
    condP <- condP+round((1-OL)*mod.cond.size[i])
  }
  
  list(data=data, geneMod=geneMod, condMod=condMod)
}

ppa.in.silico <- function(num.genes=300, num.drugs=100, num.conds=50,
                          num.tf0=3, num.tf1=3, num.tf2=3,
                          tf.per.gene=0, tf.per.drug=tf.per.gene,
                          tf.per.cond=tf.per.gene,
                          mod.gene.size=round(0.5*num.genes/(num.tf0+num.tf1)),
                          mod.drug.size=round(0.5*num.drugs/(num.tf0+num.tf2)),
                          mod.cond.size=round(0.5*num.conds/(num.tf0+num.tf1+num.tf2)),
                          noise.gene=.1, noise.drug=.1,
                          gene.ol=0, drug.ol=gene.ol, cond.ol=gene.ol,
                          reg.dir=matrix(1, 2, num.tf0+num.tf1+num.tf2)) {

  if (length(mod.gene.size)==1) {
    mod.gene.size = rep(mod.gene.size, num.tf0+num.tf1)
  }
  if (length(mod.drug.size)==1) {
    mod.drug.size = rep(mod.drug.size, num.tf0+num.tf2)
  }
  if (length(mod.cond.size)==1) {
    mod.cond.size = rep(mod.cond.size, num.tf0+num.tf1+num.tf2)
  }

  if (sum(mod.gene.size) > num.genes ||
      sum(mod.drug.size) > num.drugs ||
      sum(mod.cond.size) > num.conds) {
    stop("Inconsistent data configuration, modules are too large!")
  }

  num.tf <- num.tf0 + num.tf1 + num.tf2
  gene.mod <- matrix(0, num.genes, num.tf)
  drug.mod <- matrix(0, num.drugs, num.tf)
  cond.mod <- matrix(0, num.conds, num.tf)
  geneP <- drugP <- condP <- 0

  ## Common modules
  for (i in 1:num.tf0) {
    if (i==1) {
      gene.mod[1:mod.gene.size[i],i] <- reg.dir[1,i]
      drug.mod[1:mod.drug.size[i],i] <- reg.dir[2,i]
      cond.mod[1:mod.cond.size[i],i] <- 1;
    } else {
      gene.mod[ (1+geneP-round(gene.ol*mod.gene.size[i])):
                (geneP+round((1-gene.ol)*mod.gene.size[i])), i] <- reg.dir[1,i]
      drug.mod[ (1+drugP-round(drug.ol*mod.drug.size[i])):
                (drugP+round((1-drug.ol)*mod.drug.size[i])), i] <- reg.dir[2,i]
      cond.mod[ (1+condP-round(cond.ol*mod.drug.size[i])):
                (condP+round((1-cond.ol)*mod.cond.size[i])), i] <- 1
    }
    geneP <- tail(which(gene.mod[,i] != 0),1)
    drugP <- tail(which(drug.mod[,i] != 0),1)
    condP <- tail(which(cond.mod[,i] != 0),1)
  }

  ## Gene only modules
  for (i in (num.tf0+1):(num.tf0+num.tf1)) {
    gene.mod[ (1+geneP-round(gene.ol*mod.gene.size[i])):
              (geneP+round((1-gene.ol)*mod.gene.size[i])), i] <- reg.dir[1,i]
    cond.mod[ (1+condP-round(cond.ol*mod.cond.size[i])):
              (condP+round((1-cond.ol)*mod.cond.size[i])), i] <- 1
    geneP <- tail(which(gene.mod[,i] != 0),1)
    condP <- tail(which(cond.mod[,i] != 0),1)
  }

  ## Drug only modules
  for (i in (num.tf0+num.tf1+1):num.tf) {
    drug.mod[ (1+drugP-round(drug.ol*mod.drug.size[i-num.tf1])):
              (drugP+round((1-drug.ol)*mod.drug.size[i-num.tf1])), i] <- reg.dir[2,i]
    cond.mod[ (1+condP-round(cond.ol*mod.cond.size[i])):
              (condP+round((1-cond.ol)*mod.cond.size[i])), i] <- 1
    drugP <- tail(which(drug.mod[,i] != 0),1)
    condP <- tail(which(cond.mod[,i] != 0),1)
  }

  ## TF per gene, TODO
  ## TF per drug, TODO
  ## TF per cond, TODO

  ## Add noise
  data1 <- cond.mod %*% t(gene.mod)
  data1[] <- data1[] + rnorm(num.conds*num.genes, mean=0, sd=noise.gene)
  data2 <- cond.mod %*% t(drug.mod)
  data2[] <- data2[] + rnorm(num.conds*num.drugs, mean=0, sd=noise.drug)

  list(data1, data2, gene.mod, drug.mod, cond.mod)
}
