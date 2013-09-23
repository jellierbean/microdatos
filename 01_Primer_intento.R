library(foreign)
library(survey)
library(Hmisc)

unzip("concen.zip")
mx06 <- read.dbf("concen.dbf")
file.remove("concen.dbf")

head(mx06)
names(mx06) <- tolower(names(mx06))

length(unique(mx06[,1]))

sum(mx06$hog)

# deciles resumen ------

q <- 10
N <- sum(mx06$hog) 
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

y <- mx06[,c("folio","hog","ingmon","gastot")]

head(y)

y <- y[order(y$ingmon),] 

y <- transform(y,factorexac = cumsum(hog))

### Llenar de unos ----

  #### Llenamos de unos, a partir de la acumulación del factor de expansión
for(i in 1:q){
dmatrix[y$factorexac<=deciles[i,"hogac"],i] <- 1
}

  #### Eliminamos los repetidos
for(i in q:2){
  dmatrix[,i] <- dmatrix[,i]-dmatrix[,i-1]
}

head(dmatrix)

# Índices de cambio ----


cambio <- data.frame(cambio.id=rep(NA,q))

for(i in 1:q){
cambio[i,"cambio.id"] <- tail(cumsum(y$factorexac<=deciles$hogac[i]))[5]+1
}

cambio$factorex <- y$hog[cambio$cambio.id]

cambio$factorexac <- y$factorexac[cambio$cambio.id]

cambio$hogac <- deciles$hogac

cambio$porc <- with(cambio,(factorex-(factorexac-hogac))/factorex)

cambio

for(i in 1:9){

  dmatrix[cambio[i,"cambio.id"],i] <- cambio[i,"porc"]
  dmatrix[cambio[i,"cambio.id"],i+1] <- 1-cambio[i,"porc"]
  
}

dmatrix[cambio$cambio.id,]

sum(y$hog*dmatrix$d1)
sum(y$hog*dmatrix$d10)

for(i in 1:q){
deciles$gastot[i] <- sum(y$gastot*y$hog*dmatrix[,i])/1000
}

for(i in 1:q){
  deciles$ingmon[i] <- sum(y$ingmon*y$hog*dmatrix[,i])/1000
}


deciles$gastotac <- cumsum(deciles$gastot)

deciles

