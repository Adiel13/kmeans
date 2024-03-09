install.packages("ggplot2")
library("ggplot2")
install.packages("arules")
library(arules)
install.packages("genero")
library(genero)

data = read.csv('C:/Users/kevin/OneDrive/Documentos/data.csv', sep=";")
data[,c(3,4)]
subset(data, Lab. >60)
subset(data, Lab. >60)[,c(3,4)]

data2 <- data[,c(1,2,3,4,8,9,10,11)]
data2$Nota.Final[data2$Nota.Final=="SDE"] <- -1
data2$Exam..Final[data2$Exam..Final=="SDE"] <- -1
data2$nombre1 <-as.data.frame(do.call(rbind, strsplit(data2$Nombre.Estudiante, " ")))["V1"]
data2$nombre2 <-as.data.frame(do.call(rbind, strsplit(data2$Nombre.Estudiante, " ")))["V2"]
data2$genero <- genero(data2$nombre1$V1)

subset(data2, is.na(data2$genero))
data2$genero <- ifelse(is.na(data2$genero), genero(data2$nombre2$V2), data2$genero)

data2[55, "genero"] <- "female"
data2[77, "genero"] <- "male"
data2[113, "genero"] <- "male"
data2[119, "genero"] <- "female"
data2[120, "genero"] <- "male"
data2[122, "genero"] <- "male"
data2[130, "genero"] <- "female"
data2[179, "genero"] <- "female"
data2[185, "genero"] <- "male"
data2[202, "genero"] <- "male"
data2[225, "genero"] <- "male"
data2[250, "genero"] <- "male"
data2[276, "genero"] <- "female"
data2[360, "genero"] <- "female"
data2[363, "genero"] <- "female"
data2[417, "genero"] <- "male"
data2[431, "genero"] <- "male"
data2[473, "genero"] <- "female"
data2[487, "genero"] <- "male"

data3 <- data2[,c(1,2,3,5,6,7,8,11)]
data3$genero <- ifelse(data3$genero == "male", 1, 0)

data3$anio_carne<- substr(data3$Reg..Acad., start =1, stop=4)
subset(data3, anio_carne > 8000)
data3[47, "anio_carne"] <- 1992
data3[91, "anio_carne"] <- 1986
data3[92, "anio_carne"] <- 1996
data3[287, "anio_carne"] <- 1992
data3[391, "anio_carne"] <- 1983
data3[392, "anio_carne"] <- 1992
data3[393, "anio_carne"] <- 1995
data3[394, "anio_carne"] <- 1996

data3$edad <-as.integer(data3$anio)-as.integer(data3$anio_carne)+18

clusterk <- kmeans(data3, centers=4)

data3

ggplot(data3, aes(x = edad, y = Lab. , color = as.factor(clusterk$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(clusterk$centers), aes(x = edad, y = Lab.), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Lab") +
  theme_minimal()

ggplot(data3, aes(x = anio , y = Lab. , color = as.factor(clusterk$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(clusterk$centers), aes(x = anio , y = Lab.), color = "black", size = 4, shape = 17) +
  labs(title = "Año vs Lab") +
  theme_minimal()

ggplot(data3, aes(x = Nota.Final , y = Lab. , color = as.factor(clusterk$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(clusterk$centers), aes(x = Nota.Final , y = Lab.), color = "blue", size = 4, shape = 17) +
  labs(title = "Nota Final vs Lab") +
  theme_minimal()

clusterk <- kmeans(data3, centers=2)

ggplot(data3, aes(x = genero, y = Lab. , color = as.factor(clusterk$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(clusterk$centers), aes(x = Nota.Final , y = Lab.), color = "blue", size = 4, shape = 17) +
  labs(title = "Nota Final vs Lab") +
  theme_minimal() +
  xlim (0,2)

 data4 <- subset(data3, genero == 0)

 clusterk2 <- kmeans(data4, centers=3)
 data4
 ggplot(data4, aes(x = edad, y = Lab. , color = as.factor(clusterk2$cluster))) +
   geom_point() + 
   geom_point(data = as.data.frame(clusterk2$centers), aes(x = edad, y = Lab.), color = "black", size = 4, shape = 17) +
   labs(title = "Edad vs Lab (mujeres)") +
   theme_minimal()

 ggplot(data4, aes(x = edad, y = Nota.Final , color = as.factor(clusterk2$cluster))) +
   geom_point() + 
   geom_point(data = as.data.frame(clusterk2$centers), aes(x = edad, y = Nota.Final), color = "black", size = 4, shape = 17) +
   labs(title = "Edad vs Lab (hombres)") +
   theme_minimal()
 
 
 data5 <- subset(data3, genero == 1)
 clusterk3 <- kmeans(data5, centers=3)
 
 ggplot(data5, aes(x = edad, y = Nota.Final , color = as.factor(clusterk3$cluster))) +
   geom_point() + 
   geom_point(data = as.data.frame(clusterk3$centers), aes(x = edad, y = Nota.Final), color = "black", size = 4, shape = 17) +
   labs(title = "Edad vs Lab (hombres)") +
   theme_minimal()
 
 ggplot(data5, aes(x = edad, y = Lab. , color = as.factor(clusterk3$cluster))) +
   geom_point() + 
   geom_point(data = as.data.frame(clusterk3$centers), aes(x = edad, y = Lab.), color = "black", size = 4, shape = 17) +
   labs(title = "Edad vs Lab (hombres)") +
   theme_minimal()
 
 ggplot(data5, aes(x = Lab., y = edad , color = as.factor(clusterk3$cluster))) +
   geom_point() + 
   geom_point(data = as.data.frame(clusterk3$centers), aes(x = Lab., y = edad), color = "black", size = 4, shape = 17) +
   labs(title = "Edad vs Lab (hombres)") +
   theme_minimal()
 
 
 