datacenso = read.csv('C:/Users/kevin/OneDrive/Documentos/2. Repo/1. MIIC/fpgrowth/PERSONA_BDP.csv', sep=',')

subs <- subset(datacenso, NIVGRADO == 71 | NIVGRADO == 72)
subs[is.na(subs)] <- -1
result_kmeans <- kmeans (subs, centers = 2)

ggplot(subs, aes(x = PCP7, y = PCP32_2D , color = as.factor(result_kmeans$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(result_kmeans$centers), aes(x = PCP7, y = PCP32_2D), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Ac. Economica") +
  theme_minimal()+
  ylim(0,100)

ggplot(subs, aes(x = PCP7, y = PCP30_2D , color = as.factor(result_kmeans$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(result_kmeans$centers), aes(x = PCP7, y = PCP30_2D), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Ac. Economica") +
  theme_minimal()+
  ylim(0,100)




ggplot(subs, aes(x = MUNICIPIO, y = PCP32_2D , color = as.factor(result_kmeans$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(result_kmeans$centers), aes(x = MUNICIPIO, y = PCP32_2D), color = "black", size = 4, shape = 17) +
  labs(title = "Municipio vs Ac. Economica") +
  theme_minimal()+
  ylim(0,100)


subs <- subset(datacenso, NIVGRADO == 61 | NIVGRADO == 62)
subs[is.na(subs)] <- -1

result_kmeans <- kmeans (subs, centers = 2)

ggplot(subs, aes(x = PCP7, y = PCP32_2D , color = as.factor(result_kmeans$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(result_kmeans$centers), aes(x = PCP7, y = PCP32_2D), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Ac. Economica") +
  theme_minimal()+
  ylim(0,100)

subs2 <- subset(subs, PCP32_2D == 86)
subs2[is.na(subs2)] <- -1
result_kmeans2 <- kmeans (subs2, centers = 4)

ggplot(subs2, aes(x = PCP7, y = MUNICIPIO , color = as.factor(result_kmeans2$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(result_kmeans2$centers), aes(x = PCP7, y = MUNICIPIO), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Municipio") +
  theme_minimal()


