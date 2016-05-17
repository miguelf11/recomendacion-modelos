library("arules")
library("dplyr")
library("arulesViz")

setwd("C:/Users/Alex/Documents/R/DM_T4/recomendacion-modelos")

periodico <- read.csv("data/periodico.csv")


## funciones
asignar <- function(valor){
  #función para asignar valores
  
  valor <- gsub(pattern = "item", x = valor, replacement = "")
  valor <- gsub(pattern = "}",x=valor,replacement = "")
  valor <- gsub(pattern = "\\{",x=valor,replacement = "")
  
  a <- as.integer(unlist(strsplit(valor,split = ",")))
  h <- "{"
  for(j in 1:length(a)){
    if(j == 1){
      h <- paste(h,valores[a[1]],sep="")
    }else{
      h <- paste(h,valores[a[j]],sep=",")
    }
  }
  h <- paste(h,"}")
  return(h)
}



### delete useless feature
periodico$X <- NULL
periodico$ID <- NULL



# eliminar bots
periodico$entry <- as.POSIXct(periodico$entry, origin = "1970-01-01")
periodico$exit <- as.POSIXct(periodico$exit, origin = "1970-01-01")
periodico <- mutate(periodico, estadia = exit - entry)

length(periodico$estadia[periodico$estadia <= 20])

periodico <- periodico[periodico$estadia > 20, ]



# Actividad 1
# creando un arreglo para asignar los valores
contenido <- c("deporte","politica","variedades","internacional","nacionales",
               "sucesos","comunidad","negocios","opinion")
contenido <- rep(contenido,each=9) 
articulo <- rep(x=1:9,times=9)
articulo <- paste0("articulo",articulo,sep="")
valores <- paste(contenido,articulo,sep = "/")
periodico$items <- sapply(periodico$articles, asignar)
rm(articulo)
rm(contenido)


# Crear matriz de incidencia
aux <- periodico$items
aux <- as.character(aux)
aux <- gsub(pattern = "}",x=aux,replacement = "")
aux <- gsub(pattern = "\\{",x=aux,replacement = "")
aux <- gsub(pattern = " ",x=aux,replacement = "")

# construyo la matriz de transacciones
nuevo <- matrix(nrow=length(aux) , ncol = 81)
nuevo[,] <- F
nuevo <- as.data.frame(nuevo)
colnames(nuevo) <- valores
for (i in 1:length(aux)){
  a <- unlist(strsplit(aux[i],split = ","))
  print(paste("va por la: ",i))
  for (j in a){
    nuevo[i,j] <- T 
  }
}
class(nuevo)
# save(nuevo,file="data/nuevo.RData")
load("data/nuevo.RData")

## Actividad 3



periodico$entry <- as.POSIXct(periodico$entry, origin = "1970-01-01")
periodico$exit <- as.POSIXct(periodico$exit, origin = "1970-01-01")
periodico <- mutate(periodico, estadia = exit - entry)



estadia <- periodico$estadia

# 10 visitas con menor tiempo de estadia
estadia <- estadia[order(estadia)]
estadia[1:10]

# 10 visitas con mayor tiempo de estadia
estadia <-estadia[order(estadia,decreasing = T)]
estadia[1:10]








trans <- as(nuevo, "transactions")
summary(trans)
unique(trans)

itemFrequencyPlot(trans, topN = 10, "absolute")


reglas <- apriori(trans, parameter = list(supp = 0.00004, conf = 0.8, target = "rules"))


#in es cualquiera de los 2 , %ain% son los 2
inspect(subset(reglas, subset = lhs %ain% c("politica/articulo1","deporte/articulo1")))

inspect(reglas)





kmedias = kmeans(x = nuevo ,centers = 8)


nuevo$kmedias <- kmedias$cluster

nuevo$kmedias <- NULL

periodico$kmedias <- kmedias$cluster

periodico$ID <- NULL
periodico$X <- NULL
View(periodico)


for (i in 1:8){
  print(paste("la cantidad de usuarios que pertenecen al grupo:",i,": ",
              length(periodico$kmedias[periodico$kmedias == i])))
}

for (i in 1:8){
  print(paste("la proporción de usuarios que pertenecen al grupo:",i,": ",
              length(periodico$kmedias[periodico$kmedias == i])/length(periodico$kmedias)))
}

