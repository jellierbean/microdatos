library(foreign)
library(survey)
library(Hmisc)

unzip("concen.zip")
mx06 <- read.dbf("concen.dbf")
file.remove("concen.dbf")

head(mx06)
names(mx06)

length(unique(mx06[,1]))

sum(mx06$HOG)

# deciles resumen ------

q <- 10
N <- sum(mx06$HOG) 
n <- length(unique(mx06[,1]))

deciles <- data.frame(decil=1:q,hogares=rep(NA,q),hogac = rep(NA,q))

for(i in 1:q){deciles[i,"hogares"]<-round(N/q)
              deciles[q,"hogares"] <- N-(q-1)*deciles[1,"hogares"]             
}

deciles[,"hogac"] <- cumsum(deciles[,"hogares"])

deciles

### Matriz de dummies ----

dmatrix <- as.data.frame(matrix(0,nrow=n,ncol=q))
names(dmatrix)<-paste0("d",deciles[,1])

head(dmatrix)

### Acomodar base por variable de separación

y <- mx06[,c("FOLIO","HOG","INGTOT")]

head(y)

y <- transform(y,INGTOTEX = INGTOT*HOG)

y <- y[order(y$INGTOTEX),] 

y <- transform(y,factorexac = cumsum(HOG))

### Llenar de unos ----

for(i in 1:q){
dmatrix[y$factorexac<=deciles[i,"hogac"],i] <- 1
}

for(i in q:2){
  dmatrix[,i] <- dmatrix[,i]-dmatrix[,i-1]
}

head(dmatrix)

dmatrix[6667,"d1"]
