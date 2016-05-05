library("arules")
library("dplyr")

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
  for(j in 1:length(a)){
    if(j == 1){
      h <- paste(valores[a[1]])
    }else{
      h <- paste(h,valores[a[j]],sep=",")
    }
  }
  return(h)
}




#delete useless feature
ejemplo$X <- NULL
periodico$X <- NULL


#creando un arreglo para asignar los valores
contenido <- c("deporte","politica","variedades","internacional","nacionales",
               "sucesos","comunidad","negocios","opinion")

contenido <- rep(contenido,each=9) 

articulo <- rep(x=1:9,times=9)

articulo <- paste0("articulo",articulo,sep="")

valores <- paste(contenido,articulo,sep = "/")



items <- sapply(periodico$articles, asignar)
periodico$items <- items

