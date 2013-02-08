#Stress Tol CG

library(lattice)
library(gplots)
library(RColorBrewer)
library(preprocessCore)
library(Biobase)
library(limma)
library(pvclust)

source("http://bioconductor.org/biocLite.R")
biocLite("preprocessCore")
library(preprocessCore)

#open data files
m1dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure1
nrow(m1dat)
ncol(m1dat)
head(row.names(m1dat))
colnames(m1dat)

m2dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure2
#dup row names?
m2dat<- read.table(file.choose(), header=T, sep="\t",quote='"')
duplicated(m2dat[,1])

mHdat<- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #measure harvest

deathdat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #death and bolt

des <- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #stresstol design
nrow(des)

#diff numbers of rows?
head(row.names(des))
des.missing <- setdiff(rownames(m1dat), rownames(des))
des.missing
dat.missing <- setdiff(rownames(des), rownames(m1dat))
dat.missing

m1des<- des[rownames(m1dat),]

#merge
megadat <- merge(m1dat, m2dat, by="row.names", all.x=TRUE, all.y=TRUE)
head(megadat)
nrow(megadat)

row.names(megadat)<-megadat[,1]
megadat<-megadat[,2:9]

megadat<-merge(megadat, mHdat, by="row.names", all.x=TRUE, all.y=TRUE)
head(megadat)
row.names(megadat)<-megadat[,1]
megadat<-megadat[,2:23]

# sample breakdown
sample.size <- nrow(m1dat)
summary(des)

#pdf("images/samplebreakdownplot.pdf", paper='a4r', width=3000)
par(mfrow=c(2,2), oma=c(3,1,1,1))
plot(des$ID, main="Sample breakdown by ID", xlab="ID", ylab="Counts",mar=c(1,3,3,1))
mtext('i.',side=3,line=3, adj=0, cex=1.2)
plot(des$Pop, main="Sample breakdown for Population", xlab="Population", ylab="Counts", mar=c(1,1,3,3))
mtext('ii.',side=3,line=3, adj=0, cex=1.2)
plot(des$Treat, main="Sample breakdown for Treatment", xlab="Treatment", ylab="Counts", mar=c(1,3,3,1))
mtext('iii.',side=3,line=3, adj=0, cex=1.2)
plot(des$Origin, main="Sample breakdown for Range", xlab="Range", ylab="Counts",mar=c(1,1,3,3))
mtext('iv.',side=3,line=3, adj=0, cex=1.2)
mtext("Figure: Breakdown of samples ",side=1, line=2, adj=0,outer=TRUE, cex=0.7)
#dev.off()



#numeric megadat
megadatNum<-megadat[,c(1:6,8,9,11:13,15:18,20:22)]

#how many NAs? Which samples has the most?
sum(is.na(megadat))
summary(megadat)

#Impute "0" for "NA"
megadatNumImp<-megadatNum
megadatNumImp[is.na(megadatNumImp)]<-0
sum(is.na(megadatNumImp))

#make scaled eset (no imputed values) for visualtizations
#megadatNumscaled<-megadatNumImp
#megadatNumscaled<- t(scale(t( megadatNumscaled )))
#levels(megadatNum$LfYellow)[levels(megadatNum$LfYellow) == "n"]<-"0"
#levels(megadatNum$LfYellow)[levels(megadatNum$LfYellow) == "y"]<-"1"
#levels(megadatNum$LfYellow)[levels(megadatNum$LfYellow) == "<NA>"]<-NA
#levels(megadatNum$BoltedH)[levels(megadatNum$BoltedH) == "n"]<-"0"
#levels(megadatNum$BoltedH)[levels(megadatNum$BoltedH) == "y"]<-"1"
#Relevel factors
#megadatNum$LfYellow <- factor(megadatNum$LfYellow)
#hw2des$ArrayType <- relevel(hw2des$ArrayType, "22K")
megadatNum<-as.numeric(megadatNum)
class(megadatNum)
megadatNumImp<-as.matrix(megadatNumImp)
#correlation matrix
megadatcor<-cor(megadatNumImp)
par(mfrow=c(1,1), oma=c(1,1,1,1))
heatmap.2(megadatcor, trace="none", dendrogram="none", margin=c(4,5),col= rev(brewer.pal(9,'YlOrRd')), Rowv=NA, Colv=NA, symm=TRUE, main = "Correlation matrix")


#PCA of phenotype
#make eSet from imputed data
#Make both files have the same ordering
head(row.names(des))
des.missing <- setdiff(rownames(megadatNumImp), rownames(des))
des.missing
dat.missing <- setdiff(rownames(des), rownames(megadatNumImp))
dat.missing

m1des<- des[rownames(m1dat),]

megadatNumImp <- megadatNumImp[row.names(des),]
all(row.names(megadatNumImp)==row.names(des))
# make eSet
hw2esetImp <- new("ExpressionSet", phenoData = as(hw2des, "AnnotatedDataFrame"),exprs = as.matrix(hw2datImp))
#order eSet by array and tumor type
hw2esetImp<-hw2esetImp[,order( hw2des$ArrayType, hw2des$TumorType )]

#PCA
hw2pcs<-prcomp(exprs(hw2esetImp), center=F, scale=F)

# append the rotations for the first 10 PCs to the phenodata
pData(hw2esetImp)<- cbind(pData(hw2esetImp), hw2pcs$rotation[sampleNames(hw2esetImp),1:10])


# plot data on first two PCs, colored by array type
plot(pData(hw2esetImp)[,c("PC1","PC2")], bg=pData(hw2esetImp)$ArrayType, pch=21, cex=2, main= "PC1 and PC2 by Array Type" )
legend(  list(x=0.2,y=0.3), as.character(levels(pData(hw2esetImp)$ArrayType)), x="topright", pch =21, pt.bg=c(1,2,3,4,5))
