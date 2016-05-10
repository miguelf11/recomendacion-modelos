library("arules")
library("dplyr")
library("arulesViz")
library("reshape2")

setwd("C:/Users/Alex/Documents/R/DM_T4/recomendacion-modelos")

ejemplo <- read.csv("data/ejemplo.csv")
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




#delete useless feature
ejemplo$X <- NULL


#creando un arreglo para asignar los valores
contenido <- c("deporte","politica","variedades","internacional","nacionales",
               "sucesos","comunidad","negocios","opinion")

contenido <- rep(contenido,each=9) 

articulo <- rep(x=1:9,times=9)

articulo <- paste0("articulo",articulo,sep="")

valores <- paste(contenido,articulo,sep = "/")



periodico$items <- sapply(periodico$articles, asignar)

rm(articulo)
rm(contenido)


length(unique(periodico$X))
length(unique(periodico$ID))

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





aux <- periodico$items
aux <- as.character(aux)
aux <- gsub(pattern = "}",x=aux,replacement = "")
aux <- gsub(pattern = "\\{",x=aux,replacement = "")
aux <- gsub(pattern = " ",x=aux,replacement = "")

aux <- aux

#construyo la matriz de transacciones
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
View(nuevo)


trans <- as(nuevo, "transactions")
summary(trans)
unique(trans)

itemFrequencyPlot(trans, topN = 10, "absolute")


reglas <- apriori(trans, parameter = list(supp = 0.00003, conf = 0.8, target = "rules"))


#in es cualquiera de los 2 , %ain% son los 2
inspect(subset(reglas, subset = lhs %ain% c("politica/articulo1","deporte/articulo1")))

inspect(reglas)











