#Retrieve the data saved AFTER the profiling practice...... this means data already cleaned

dd <- read.table("merged_data.csv",header=T, sep=",");
dd$edat <- as.numeric(dd$edat)


dd <- dd[dd[, 33] == 1, ]
rownames(dd) <- seq(length = nrow(dd))

#treure columna de morts ja que te 0 variancia (tot 0s)
dd <- subset(dd, select = -morts)
#treure columna de ipus de vehicle ja que te 0 variancia (tot 0s)
dd <- subset(dd, select = -dTVehImp)
#treure columna de si es bici o no ja que te 0 variancia (tot 0s)
dd <- subset(dd, select = -accbici)

names(dd)
dim(dd)
summary(dd)

attach(dd)

#set a list of numerical variables
names(dd)

#dcon <- data.frame (Antiguedad.Trabajo,Plazo,Edad,Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,Importe.solicitado,Precio.del.bien.financiado,Estalvi, RatiFin)
dcon <- data.frame ( dd$cDis, dd$nMes, dd$hora, dd$lesLl, dd$lesGr, dd$vict, dd$vehi, dd$long, dd$lat)
dim(dcon)



# CLUSTERING
# KMEANS RUN, BUT HOW MANY CLASSES?

k1 <- kmeans(dcon,2)
names(dcon)
print(k1)

attributes(k1)

k1$size

k1$withinss

k1$centers

# LETS COMPUTE THE DECOMPOSITION OF INERTIA

Bss <- sum(rowSums(k1$centers^2)*k1$size)
Bss
Wss <- sum(k1$withinss)
Wss
Tss <- k1$totss
Tss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1




# LETS REPEAT THE KMEANS RUN WITH K=5

k2 <- kmeans(dcon,4)
k2$size

Bss <- sum(rowSums(k2$centers^2)*k2$size)
Bss
Wss <- sum(k2$withinss)
Wss

Ib2 <- 100*Bss/(Bss+Wss)
Ib2
Ib1

k2$centers
k1$centers

k1<-k2

plot(k1$centers[,3],k1$centers[,2])

table(k1$cluster, k2$cluster)

# WHY WE HAVE OBTAINED DIFFERENT RESULTS?, AND WHICH RUN IS BETTER?



# NOW TRY K=8

k3 <- kmeans(dcon,8)
k3$size

Bss <- sum(rowSums(k3$centers^2)*k3$size)
Wss <- sum(k3$withinss)

Ib3 <- 100*Bss/(Bss+Wss)
Ib3



# HIERARCHICAL CLUSTERING

d  <- dist(dcon[1:50,])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

d  <- dist(dcon)
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

# BUT WE ONLY NEED WHERE THERE ARE THE LEAPS OF THE HEIGHT

# WHERE ARE THER THE LEAPS? WHERE WILL YOU CUT THE DENDREOGRAM?, HOW MANY CLASSES WILL YOU OBTAIN?

# DECIDIR SI 2 o 3 a partir del cluster dendorgram
nc = 2

c1 <- cutree(h1,nc)

c1[1:20]

nc = 4

c5 <- cutree(h1,nc)

c5[1:20]


table(c1)
table(c5)
table(c1,c5)


length(dcon)
list(c1)
cdg <- aggregate(as.data.frame(dcon),list(c5),mean)
cdg

plot(cdg[,1], cdg[,9])




# LETS SEE THE PARTITION VISUALLY

# dd$cDis, dd$nMes, dd$hora, dd$lesLl, dd$lesGr, dd$vict, dd$vehi, dd$long, dd$lat

plot(hora,edat,col=c1,main="Clustering of bicycle accidents in 2 classes")
legend("topleft",c("class1","class2"),pch=1,col=c(1:2), cex=0.6)

plot(hora,edat,col=c5,main="Clustering of bicycle accidents in 4 classes")
legend("topleft",c("class1","class2","class3","class4"),pch=1,col=c(1:4), cex=0.6)

plot(cDis,edat,col=c5,main="Clustering of credit data in 4 classes")
legend("topright",c("class1","class2","class3","class4"),pch=1,col=c(1:4), cex=0.6)

plot(cBar, lesGr,col=c5,main="Clustering of credit data in 4 classes")
legend("topright",c("class1","class2","class3","class4"),pch=1,col=c(1:4), cex=0.6)


plot(edat, cDis,col=c5,main="Clustering of bicycle accidents in 4 classes")
legend("topright",c("class1","class2","class3","class4"),pch=1,col=c(1:4), cex=0.6)
plot(sexe, mes,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

pairs(dcon[,1:7], col=c1)

#plot(FI[,1],FI[,2],col=c1,main="Clustering of credit data in 3 classes")
#legend("topleft",c("c1","c2","c3"),pch=1,col=c(1:3))



# LETS SEE THE QUALITY OF THE HIERARCHICAL PARTITION



Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))

Ib4 <- 100*Bss/Tss
Ib4


#move to Gower mixed distance to deal 
#simoultaneously with numerical and qualitative data

library(cluster)

#dissimilarity matrix

actives<-c(2:16)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

c2 <- cutree(h1,4)

#class sizes 
table(c2)

#comparing with other partitions
table(c1,c2)


names(dd)
#ratiFin
boxplot(dd[,16]~c2, horizontal=TRUE)

#plazo
boxplot(dd[,4]~c2, horizontal=TRUE)

#gastos
boxplot(dd[,9]~c2, horizontal=TRUE)

pairs(dcon[,1:7], col=c2)

plot(RatiFin,Estalvi,col=c2,main="Clustering of credit data in 3 classes")
legend("topright",levels(c2),pch=1,col=c(1:4), cex=0.6)

cdg <- aggregate(as.data.frame(dcon),list(c2),mean)
cdg

plot(Edad, Gastos, col= c2)
points(cdg[,4],cdg[,5],pch=16,col="orange")
text(cdg[,4],cdg[,5], labels=cdg[,1], pos=2, font=2, cex=0.7, col="orange")

potencials<-c(3,4,6,7,10,11)
pairs(dcon[,potencials],col=c2)

#Profiling plots

