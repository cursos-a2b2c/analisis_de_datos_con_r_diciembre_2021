misdatos <- read.csv("misdatos.csv")
library(edgeR)
condiciones <- as.factor(rep(1:2, each = 5))
y <- DGEList(counts=misdatos[,2:11], 
             group = condiciones, genes = misdatos[,1])

y <- calcNormFactors(y, method="TMM")

plotMDS(y, col = rep(1:2, each = 5))

keep <- filterByExpr(y, min.count = 10)
table(keep)
y    <- y[keep, ,keep.lib.sizes=FALSE]
y    <- calcNormFactors(y, method="TMM")
plotMDS(y, col = rep(1:2, each = 5))


design <- model.matrix(~condicion, data=data.frame(condicion = condiciones))
rownames(design) <- colnames(y$counts)

y      <- estimateDisp(y, design)

fit    <- glmFit(y, design)

ds     <- glmLRT(fit)

#y = a + bx
qvalues <- p.adjust(ds$table$PValue, method = "fdr")
table(qvalues < 0.05 & abs(ds$table$logFC) > log2(1.5))
y$genes$genes[which(qvalues < 0.05 & abs(ds$table$logFC) > log2(1.5))]
