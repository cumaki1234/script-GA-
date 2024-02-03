#install.packages("dplyr")
#install.packages("arules")
#install.packages("xlsx")
library(xlsx)
library(ggplot2)
library(dplyr)
library(arules) #Librería para utilizar funciones y el algoritmo apriori

fname = "C:/Users/eduar/OneDrive/Desktop/mis cosas/universidad/Inteligencia de negocios/tutorados-spa2324.xls"
#fname=file.choose() #ruta de un archivo tipo transaccional "trans.txt"
#transacciones <- read.transactions(fname, sep = ",")

df_materias = data.frame(read.xlsx(fname,sheetName = "MODELO EVALUATIVO 1",colIndex = 10),
                         read.xlsx(fname,sheetName = "MODELO EVALUATIVO 1",colIndex = 3)
);

transacciones = as(split(df_materias[,"MATERIA"],df_materias[,"ESTUDIANTE"]),"transactions")

inspect(transacciones) #Visualiza las transacciones



#####################################################
#Frequent itemsets
####################################################
itemsets <- apriori(data = transacciones,parameter = list(support = 0.3,minlen = 1,maxlen = 5,target = "frequent itemset"))
summary(itemsets)
#(los itemsets de menor tamaño).
top_5_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)
inspect(top_5_itemsets)

#grafico
as(top_5_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Itemsets más frecuentes", x = "itemsets") +
  theme_bw()


#ITEMS MAS FRECUENTES
inspect(sort(itemsets[size(itemsets) > 1], decreasing = TRUE)[1:2])

reglas1=apriori(transacciones) #Por defecto el soporte es 0.1, confianza mínima de 0.8, máximo 10 items (maxlen).
# Se muestran los top 6 itemsets de mayor a menor soporte
inspect(reglas1)
reglas2 <- apriori(transacciones,parameter = list(support = 0.2,confidence = 0.5,minlen = 3, target = "rules"))
summary(reglas2)
#se han identificado un conjunto de 6 reglas, y su longitud es 3

#se muestran las estadísticas de las principales métricas
inspect(sort(x = reglas2, decreasing = TRUE, by = "confidence")) #Se muestra las reglas ordenadas por confianza




# Para obtener otras métricas de las reglas podemos hacer uso de la siguiente función
metricas <- interestMeasure(reglas2, measure = c("lambda", "fishersExactTest"),transactions = transacciones)
metricas


