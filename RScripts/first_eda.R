rm(list=ls())
# graphics.off()
data <- read.csv("spotify.csv",header=TRUE)
head(data)
dim(data)
data = data[,-1]
head(data)
B = 10
# alcuni dati sono molto concentrati in 0 --> passo ai logaritmi
use_log = TRUE
if(use_log == TRUE)
{  data$speechiness = log(data$speechiness)
data$liveness = log(data$liveness)}

# EDA
## dimensionality reduction --> PCA
## dimensionality reduction --> Depth Measure

# get rid of labels & make double

which(data$loudness == min(data$loudness))
which(data$loudness == max(data$loudness))

# since we want to understand info about the technical features of the dataset, we omit "popularity" and thee categorical variables
data_reduced = data[,c(-4,-6)]

# rendo tutte le feature tra 0 e 1
data_reduced <- data.frame(lapply(data_reduced, function(x) (x - min(x))/(max(x)-min(x))))


par(mfrow = c(1,1))
boxplot(scale(data_reduced), col = "gold")
# instrumentalness seems to have some issues
plot(data_reduced$instrumentalness)
# is seems that most values of instrumentalness are nea zero:
n = dim(data)[1]
sum(data_reduced$instrumentalness <= 0.05)/n
# almost 90% of the variables have instrumentalness less or equal than 0.05, which means
# that for almost 90% of the song the probability/confidence that the track contains no vocals is less than 5%
range(data_reduced$instrumentalness)
hist(data_reduced$instrumentalness)
# we could use instrumentalness as a categorical variable by setting 1 if it is >= tolerance and 0 otherwise

index_instrumentalness = which(names(data_reduced)=="instrumentalness")
index_instrumentalness
data_reduced = data_reduced[,-index_instrumentalness]
index_acousticness = which(names(data_reduced)=="acousticness")
index_acousticness
data_reduced = data_reduced[,-index_acousticness]

for (i in 1:dim(data_reduced)[2])
{
  data_reduced[,i] = as.double(data_reduced[,i])
}

for (i in 1:dim(data_reduced)[2])
{
  print(typeof(data_reduced[,i]))
}

par(mfrow = c(1,2))
boxplot(data_reduced, las=2, col='gold')
boxplot(scale(data_reduced), las=2, col='gold')
par(mfrow=c(1,1))
# need to rescale data
x11()
plot(as.data.frame(scale(data_reduced)))

x11()
plot(as.data.frame(scale(data_reduced)))

plot(as.matrix(cor(data_reduced)), dimnames=names(data_reduced))
cor(data_reduced)[1,]
which(cor(data_reduced)[1,] < 0.06)

# tolgo tempo, duration e popularity
index_tempo = which(names(data_reduced)=="tempo")
index_tempo
data_reduced = data_reduced[,-index_tempo]
index_duration_ms = which(names(data_reduced)=="duration_ms")
index_duration_ms
data_reduced = data_reduced[,-index_duration_ms]
index_popularity = which(names(data_reduced)=="popularity")
index_popularity
data_reduced = data_reduced[,-index_popularity]


# pca:
pc.data <- princomp((data_reduced), scores=T)
summary(pc.data)
load.data <- pc.data$loadings
x11()
par(mfcol = c(1,3))
for(i in 1:3) barplot(load.data[,i], ylim = c(-1, 1), main=paste("PC",i))
graphics.off()

data_reduced_scaled = scale(data_reduced)
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(as.data.frame(data_reduced_scaled),sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.data$sde^2)/sum(pc.data$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.9, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data_reduced_scaled),labels=1:ncol(data_reduced_scaled),las=2)
data_reduced_scaled = as.data.frame(data_reduced_scaled)
names(data_reduced)

# plot the correlation:
x11()
plot(as.matrix(cor(data_reduced)), dimnames=names(data_reduced))
cor(data_reduced)
plot(data_reduced)

# le prime due componenti spiegano pochissimo, serve togliere alcune variabili

####################### linear models:
# enegy e loudness

data_ = data_reduced
# data_ = data_reduced_scaled 
x1 = data_$loudness
Y = data_$energy

set.seed(123)

n = dim(data)[1]
permutazione <- sample(n)
Y.perm.glob <- Y[permutazione]
layout(matrix(1:2,nrow=2,byrow=FALSE))
plot(x1,Y,main='Y vs x1 (original data)',pch=16)
plot(x1,Y.perm.glob,main='energy vs loudness (permuted data)',pch=16)

T01 <- abs(summary(lm(Y ~ x1))$coefficients[2,3])

T_H01 <- numeric(B)
regr1.H0 <- lm(Y ~ 1)
residui1.H0 <- regr1.H0$residuals
n = dim(data_)[1]
for(perm in 1:B){
  permutazione <- sample(n)
  
  residui.H01.perm <- residui1.H0[permutazione]
  Y.perm.H01 <- regr1.H0$fitted + residui.H01.perm
  T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ x1))$coefficients[2,3])
  
}

k = 1
p_val1 = sum(T_H01>=T01)/B #0.2
p_val1_boncorr = k * sum(T_H01>=T01)/B
p_val1 
p_val1_boncorr 

x11()
par(mfrow = c(1,1))
hist(T_H01)
abline(v=T01, col="green")

summary(lm(Y ~ x1))$r.squared

# energy e loudness dipendono linearmente

# facciamo lo stesso test per danceability e valence

data_ = data_reduced
# data_ = data_reduced_scaled 
x1 = data_$danceability
Y = data_$valence

set.seed(123)

n = dim(data)[1]
permutazione <- sample(n)
Y.perm.glob <- Y[permutazione]
layout(matrix(1:2,nrow=2,byrow=FALSE))
plot(x1,Y,main='Y vs x1 (original data)',pch=16)
plot(x1,Y.perm.glob,main='energy vs loudness (permuted data)',pch=16)

T01 <- abs(summary(lm(Y ~ x1))$coefficients[2,3])

T_H01 <- numeric(B)
regr1.H0 <- lm(Y ~ 1)
residui1.H0 <- regr1.H0$residuals
n = dim(data_)[1]
for(perm in 1:B){
  permutazione <- sample(n)
  
  residui.H01.perm <- residui1.H0[permutazione]
  Y.perm.H01 <- regr1.H0$fitted + residui.H01.perm
  T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ x1))$coefficients[2,3])
  
}

k = 1
p_val1 = sum(T_H01>=T01)/B #0.2
p_val1_boncorr = k * sum(T_H01>=T01)/B
p_val1 
p_val1_boncorr 

x11()
par(mfrow = c(1,1))
hist(T_H01)
abline(v=T01, col="green")

summary(lm(Y ~ x1))$r.squared

# danceability e valence dipendono linearmente

# tolgo valence e loudness


names(data_reduced)
# new_variable = (data_reduced$energy + data_reduced$loudness)/2
# data_reduced$en_loud = new_variable 
index_valence = which(names(data_reduced)=="valence")
index_valence
data_reduced = data_reduced[,-index_valence]
index_energy = which(names(data_reduced)=="energy")
index_energy
data_reduced = data_reduced[,-index_energy]
names(data_reduced)

# pca:
pc.data <- princomp(scale(data_reduced), scores=T)
summary(pc.data)
load.data <- pc.data$loadings
x11()
par(mfcol = c(1,3))
for(i in 1:3) barplot(load.data[,i], ylim = c(-1, 1), main=paste("PC",i))
graphics.off()

data_reduced_scaled = scale(data_reduced)
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(as.data.frame(data_reduced_scaled),sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.data$sde^2)/sum(pc.data$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.9, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data_reduced_scaled),labels=1:ncol(data_reduced_scaled),las=2)
data_reduced_scaled = as.data.frame(data_reduced_scaled)
names(data_reduced)

# meglio ma non molto

# il codice che segue serve per plottare le 2 componenti principali e vedere se le canzoni più popolari
# hanno caratteristiche in comune in termine di componenti principali
x11()
plot(data.frame(pc.data$scores[,1:2]))
pop = data$popularity
sort(pop)
colore = rep(0,500)
hist(pop, breaks = 100)
# i1 = which(pop <= sort(pop)[100])
# i2 = which(pop <= sort(pop)[400] & pop > sort(pop)[100])
# i3 = which(pop > sort(pop)[400])
# i1 = which(pop <= 60)
# i2 = which(pop <= 72 & pop > 60)
# i3 = which(pop > 72)
# colore[i1] = 1
# colore[i2] = 2
# colore[i3] = 3
par(mfrow = c(1,1))
i1 = which(pop <= median(pop))
i2 = which(pop > median(pop))
colore[i1] = 1
colore[i2] = 2
plot(data.frame(pc.data$scores[,1:2]), col = colore, pch = 19)
points(rep(-0.45,length(i1)), pc.data$scores[i1,2], col = 1, pch = 19, cex = 0.5)
points(rep(-0.45,length(i2)), pc.data$scores[i2,2], col = 2, pch = 19, cex = 0.5)
points(pc.data$scores[i1,1], rep(-0.62,length(i1)), col = 1, pch = 19, cex = 0.5)
points(pc.data$scores[i2,1], rep(-0.62,length(i2)), col = 2, pch = 19, cex = 0.5)
x11()
par(mfrow = c(3,1))
plot(data.frame(pc.data$scores[i1,1:2]), col = 1)
plot(data.frame(pc.data$scores[i2,1:2]), col = 2)
plot(data.frame(pc.data$scores[i3,1:2]), col = 3)
par(mfrow = c(1,1))
plot(data.frame(popularity = data$popularity,
                PC1 = pc.data$scores[,1],
                PC2 = pc.data$scores[,2]))

# idee per proseguire:
# togliere le covariate da cui popularity dipende meno
# togliere le covariate che riteniamo meno interessanti per il GAM
# una volta che si ottiene una EDA fatta bene, si usa il bagplot per vedere se ci sono outliers
