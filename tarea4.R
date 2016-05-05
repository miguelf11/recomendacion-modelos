library(arules)

# Transacciones
datos <- data(Groceries)
# inspect(Groceries) # Terrible
summary(Groceries)
# 1 y 2
unique(Groceries) # el numero de columnas son la cantidad de articulos unicos
dim(Groceries)

# 3 y 4
itemFrequencyPlot(Groceries, topN = 20, "absolute")

# 5

9835*0.0006100661

# 6 169 son items unicos
9835*169

# 7 pendiente

# 8 
50/9835

# 9
reglas <- apriori(Groceries)
# el soporte esta molestando, si el soporte es 0.1 tiene q aparecer 900 veces (10%) de las veces
inspect(reglas)


# Reglas 
reglas <- apriori(Groceries, parameter = list(supp = 0.0006, conf = 0.98, target = "rules"))
# dado un soporte y una confianza todos los algoritmos deberian tener los mismos resultados


# 1
inspect(sort(reglas, by = "support")[1:10])



# 2
inspect(sort(reglas, by = "confidence", decreasing = F)[1:10])


# 3
inspect(subset(reglas, subset = lhs %ain% c("sausage", 
                                            "citrus fruit", 
                                            "root vegetables", 
                                            "other vegetables", 
                                            "whole milk", 
                                            "whipped/sour cream")))
# 4
inspect(subset(reglas, subset = lhs %in% c("sausage", 
                                           "citrus fruit", 
                                           "root vegetables", 
                                           "other vegetables", 
                                           "whole milk", 
                                           "whipped/sour cream")))
# 5
inspect(subset(reglas, subset = lhs %in% c("pip fruit", 
                                           "root vegetables",
                                           "yogurt",
                                           "soda", 
                                           "fruit/vegetable juice")))


# 6 
inspect(subset(reglas, subset = lhs %in% c("whole milk",
                                           "yogurt",
                                           "whipped/sour cream")))

unique(labels(rhs(subset(reglas, subset = lhs %in% c("whole milk",
                                                     "yogurt",
                                                     "whipped/sour cream"))))$elements)











