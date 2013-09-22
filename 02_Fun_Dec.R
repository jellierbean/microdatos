### Elaboración de función quintil o decil

# Datos necesarios:
# n: factor de expansión
# N: Total de población
# q: número de divisiones

# Paso 1. Elaboración de un cuadro resumen.

N <- 26000
n <- 2000
q <- 10

deciles <- data.frame(decil=1:q,hogares=rep(NA,q),hogac = rep(NA,q))

for(i in 1:q){deciles[i,"hogares"]<-round(N/q)
  deciles[q,"hogares"] <- N-(q-1)*deciles[1,"hogares"]             
}

deciles[,"hogac"] <- cumsum(deciles[,"hogares"])

deciles

### Matriz de dummies ----

dmatrix <- as.data.frame(matrix(0,nrow=n,ncol=q))
names(dmatrix)<-paste0("d",deciles[,1])

### Llenar de unos ----

dmatrix[cumsum(factorex)<N/q,1] <- 1





### Ordenar la base por la variable y

### Generar suma acumulada de factores de expansión 

### Llenar la matrix usando ifelse

### Llenar cuadro resumen con la variable a resumir

### Cómo tratar los NAs 
