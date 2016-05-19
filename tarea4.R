library("arules")
library("dplyr")
library("arulesViz")

setwd("C:/Users/Alex/Documents/R/DM_T4/recomendacion-modelos")

periodico <- read.csv("data/periodico.csv")
periodico$ID <- NULL
periodico$X <- NULL

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

periodico$num.articulos <- rep(0,length(periodico$articles))
for(i in 1:length(periodico$num.articulos)){
  valor <- gsub(pattern = "}",x=periodico$articles[i],replacement = "")
  valor <- gsub(pattern = "\\{",x=valor,replacement = "")
  valor <- gsub(pattern = " ",x=valor,replacement = "")
  periodico$num.articulos[i] <- length(unlist(strsplit(valor,split = ",")))
}

periodico <- mutate(periodico, tiempo = estadia - (20 * num.articulos))
periodico <- periodico[periodico$tiempo >= 0, ]


# delete unnecesay features 
periodico$entry <- NULL
periodico$exit <- NULL
periodico$tiempo <- NULL
periodico$num.articulos <- NULL



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

# 
# # Crear matriz de incidencia
# aux <- periodico$items
# aux <- as.character(aux)
# aux <- gsub(pattern = "}",x=aux,replacement = "")
# aux <- gsub(pattern = "\\{",x=aux,replacement = "")
# aux <- gsub(pattern = " ",x=aux,replacement = "")
# 
# # construyo la matriz de transacciones
# nuevo <- matrix(nrow=length(aux) , ncol = 81)
# nuevo[,] <- F
# nuevo <- as.data.frame(nuevo)
# colnames(nuevo) <- valores
# for (i in 1:length(aux)){
#   a <- unlist(strsplit(aux[i],split = ","))
#   print(paste("va por la: ",i))
#   for (j in a){
#     nuevo[i,j] <- T 
#   }
# }

#save(nuevo,file="data/nuevo.RData")
periodico$articles <- NULL

load("data/nuevo.RData")

## Actividad 3




# 10 visitas con menor tiempo de estadia
periodico <- periodico[order(periodico$estadia), ]
periodico[1:10, ]

# 10 visitas con mayor tiempo de estadia
periodico <- periodico[order(periodico$estadia,decreasing = T), ]
periodico[1:10, ]





## ACTIVIDAD 3 
recomendar <- function(vector){
  sup = 0.00009
  reglas <- apriori(trans, parameter = list(supp =  sup,
                                            conf = 0.6, target = "rules"))
  
  reglas1 <- sort(subset(reglas,subset = lhs %ain% vector),by="confidence")
  
  
  cantidad.reglas <- length(reglas1)
  
  if(cantidad.reglas == 0){
    i <- 0
    while(cantidad.reglas > 0 &  i < 20){
      sup <- sup - 0.00001
      reglas <- apriori(trans, parameter = list(supp =  sup,
                                                conf = 0.6, target = "rules"))
      
      reglas1 <- sort(subset(reglas,subset = lhs %ain% vector),by="confidence")
      
      cantidad.reglas <- length(reglas1)
      
      i <- i + 1 
    }
    if(cantidad.reglas == 0){
      sup = 0.00007
      reglas <- apriori(trans, parameter = list(supp =  sup,
                                                conf = 0.6, target = "rules"))
      
      reglas1 <- sort(subset(reglas,subset = lhs %in% vector),by="confidence")
      
      cantidad.reglas <- length(reglas1)
    }
  }
  
  
  salida <- inspect(reglas1@rhs[1])
  return(salida)
  
}




#in es cualquiera de los 2 , %ain% son los 2
### Ejemplo de la funcion Usando función
vector <- c("politica/articulo1","deporte/articulo1")
recomendar(vector)







#actividad 2


kmedias = kmeans(x = nuevo ,centers = 8)

nuevo$kmedias <- kmedias$cluster

nuevo$kmedias <- NULL

periodico$kmedias <- kmedias$cluster


for (i in 1:8){
  print(paste("la cantidad de usuarios que pertenecen al grupo:",i,": ",
              length(periodico$kmedias[periodico$kmedias == i])))
}

for (i in 1:8){
  proporcion <- length(periodico$kmedias[periodico$kmedias == i])/
    length(periodico$kmedias)
  print(paste("la proporción de usuarios que pertenecen al grupo:",i,": ",
              round(proporcion,digits = 3)))
}

