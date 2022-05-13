####
rm(list = ls())
graphics.off()
artist_means <- read.table('artist_means.csv', header=TRUE, sep=",")
head(artist_means)
names(artist_means)

# data cleaning:
# 2 è la popularity
# 6 e 8 sono key e mode
artist_means = artist_means[,c(-1,-2,-6,-8,-11,-16,-17,-18)]
names(artist_means)
#artist_means = artist_means[,c(1, 2)]
# normalize
#artist_means <- data.frame(lapply(artist_means, function(x) (x - min(x))/(max(x)-min(x))))

artist_means.dist <- dist(artist_means[,c(-10)], method='euclidean')
# artist_means.dist <- dist(artist_means, method='canberra')
# artist_means.dist <- dist(artist_means, method='manhattan')

artist_means.hclust <- hclust(artist_means.dist, method='single')
artist_means.hclust <- hclust(artist_means.dist, method='average')
artist_means.hclust <- hclust(artist_means.dist, method='complete')
artist_means.hclust <- hclust(artist_means.dist, method='ward.D2')

# plot(artist_means.single, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(artist_means.hclust, main='euclidean-ward.D2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# plot(artist_means.average, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# single distance is too "thick", so the average is spoiled by this. So we use the complete distance
# Indeed the theory of hierarchical clustering says that:
# - SINGLE: if data are already clearly separated, performs well; if points are close to each other, it doesn't work
# - COMPLETE: it tends to aggregate in "spheres", if data are already clearly separated it has a hard time
# - AVERAGE: in-between the two previous algorithms

# library(scorecard)
# library(factoextra)
# fviz_dend(artist_means.complete, k = 3,cex = 0.5,k_colors = c("#00AFBB","#E7B800","#FC4E07"),
#           color_labels_by_k = TRUE, ggtheme = theme_minimal())

rect.hclust(artist_means.hclust, k=2)
rect.hclust(artist_means.hclust, k=3)

# since we want to allow as many groups as possible, we opt for 3 clusters

artist_means_cluster <- cutree(artist_means.hclust, k=3) # euclidean-complete:
x11()
plot(artist_means, col = artist_means_cluster)
# hclust basically separates the authors based on mean duration. It seems that the 
# green cluster seems to group authors who have produced songs with a low value of
# danceability

coph <- cophenetic(artist_means.hclust)
correlation_before_after <- cor(artist_means.dist, coph)
correlation_before_after
# se usiamo ward.2Dotteniamo una divisione migliore e più equilibrata anche usando 3 gruppi, 
# ma correlation_before_after è la minima tra tutti i linkage che possiamo scegliere

# if we want to use 3 groups
groups_temp <- as.numeric(cutree(artist_means.hclust, k = 3))
groups_temp[which(groups_temp == 1)] = "A"
groups_temp[which(groups_temp == 2)] = "B"
groups_temp[which(groups_temp == 3)] = "C"
groups = factor(groups_temp)

# if we want to use 2 groups
groups_temp <- as.numeric(cutree(artist_means.hclust, k = 2))
groups_temp[which(groups_temp == 1)] = "A"
groups_temp[which(groups_temp == 2)] = "B"
groups_2 = factor(groups_temp)

# 2 OPTIONS: 
# 1)WE USE THESE CLUSTERS TO LABEL THE ARTISTS IN THE DATASET
# AND THEN APPLY KNN TO CLASSSIFY A NEW DATUM WHEN WE WANT TO PREDICT THE LM 
# ON A NEW DATUM
# 2)WE USE HCLUST ONLY TO UNDERSTAND THE NUMBER OF GROUPS IN THE DATASET AND
# USE THESE INFO AS THE NUMBER OF CENTERS IN KMEAN

# IMPLEMENTATION OF 1:
# assumption: we performed hclust out of the function (indeed hclust is used to 
# modify the data by attaching a label)
# Before calling cluser_1 i have performed:
# artist_means_temp = artist_means
# groups_temp <- as.numeric(cutree(artist_means.hclust, k = 3))
# groups_temp[which(groups_temp == 1)] = "A"
# groups_temp[which(groups_temp == 2)] = "B"
# groups_temp[which(groups_temp == 3)] = "C"
# groups = factor(groups_temp)

cluster_1 <- function(groups, k=100, train, test){
  knnClust <- knn(train = artist_means, test = test , k = k, cl = groups)
  return(knnClust)
}

# x = seq(min(artist_means[,1]), max(artist_means[,1]), by = 0.1)
# y = seq(min(artist_means[,2]), max(artist_means[,2]), by = 0.01)
# xy = expand.grid(x,y)
# 
# colore = knn(train = artist_means, test = xy , k = 100, cl = groups)
# plot(xy, col = colore, pch = 19)
# points(artist_means[,1],artist_means[,2],pch = 19, col = "black")


# IMPLEMENTATION OF 2:
# so at the end of the day, hclust suggests 3 groups, i.e. 3 centers for kmeans

# result.k <- kmeans(artist_means, centers=3) # the only use of hclust is here
# # to recap
# names(result.k)
# result.k$cluster      # labels of clusters
# result.k$centers      # centers of the clusters
# result.k$totss        # tot. sum of squares
# result.k$withinss     # sum of squares within clusters
# result.k$tot.withinss # sum(sum of squares nei cluster)
# result.k$betweenss    # sum of squares between clusters
# result.k$size         # dimention of the clusters

cluster_2 <- function(x, train) {
  result.k <- kmeans(artist_means, centers=3)
  # compute squared euclidean distance from each sample to each cluster center
  centers = result.k[["centers"]]
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  # dim(tmp) = number of centers * number of features
  max.col(-t(tmp))  # find index of min distance
}

# x needs to be a matrix with N rows and dim(artist_means)[2] columns (INCLUDING THE BASE POP)
# is x is a vector, then seq_len does not work, we need to say
# x = matrix(x, nrow = 1) to convert the vector x into a 1-rows-matrix
# moreover the features in x need to be in the same order as in names(artist_means)
# centers = result.k[["centers"]]

# test:
# for (i in 1:dim(artist_means)[1])
# {print(clusters(artist_means[i,], result.k[["centers"]]))}


# I LIKE THE FIRST IMPLEMENTATION BETTER, SINCE WE USE KNN AND THERE IS NO NEED FOR
# THE NEW DATUM TO HAVE A SPECIFIC TYPE. MOREOEVER IN THE SECOND IMPLEMENTATION,
# HCLUST IS FAIRLY USELESS, SINCE THE NUMBER OF CLUSTERS IS ALWAYS EITHER 2 OR 3...

# new idea: use kmedians instead of kmeans, which imposes the data to have a mean.
# moreover kmedians could be more robust than kmeans, even though i odn't really
# know what "robust" means in this context


# testing on a new point
# x_new = rep(0, length(names(artist_means)))
x_new = c(1,2,3,4,5,6,7,8,9,0)
x_new = c(1,1,1,1,1,1,1,1,1,900000)
cluster_1(groups, train = artist_means, test=x_new)
cluster_2(matrix(x_new, nrow = 1), artist_means)


# linear model

songs <- read.csv('additional_songs.csv', header=TRUE)
names(songs)
songs = songs[,c(-1,-2,-17)]
names(songs)

songs_for_pca = songs[,c(-1,-2,-5,-7,-8,-9,-10,-11)]

pc.data <- princomp(scale(songs_for_pca), scores=T)
summary(pc.data)
load.data <- pc.data$loadings
x11()
par(mfcol = c(1,3))
for(i in 1:3) barplot(load.data[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(as.data.frame(scale(songs_for_pca)),sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.data$sde^2)/sum(pc.data$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.9, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(scale(songs_for_pca)),labels=1:ncol(scale(songs_for_pca)),las=2)

x11()
plot(data.frame(PC1 = pc.data$scores[,1],
                PC2 = pc.data$scores[,2]), pch = 19, 
                col = groups)

# now we need to understand which is the formula that computes the 
# first 2 PCs

# for now, let's fit a GAM with:
# s(durability) + s(loudness) + abse pop + cluster + I(featuring)

songs_complete <- read.csv('additional_songs_clean.csv', header=TRUE)

names(songs_complete)
names(artist_means)

songs_complete = songs_complete[,c(-1,-2,-7,-9,-12,-17,-19)]
names(songs_complete)
n_artists = songs_complete$n_artists
media_followers = (songs_complete$followers_0 + songs_complete$followers_1 + songs_complete$followers_2 +
                       songs_complete$followers_3 + songs_complete$followers_4 + songs_complete$followers_5 +
                       songs_complete$followers_6 + 1)/n_artists

inidici_da_togliere = c(which(names(songs_complete)=="followers_0"),
                       which(names(songs_complete)=="followers_1"),
                       which(names(songs_complete)=="followers_2"),
                       which(names(songs_complete)=="followers_3"),
                       which(names(songs_complete)=="followers_4"),
                       which(names(songs_complete)=="followers_5"),
                       which(names(songs_complete)=="followers_6"),
                       which(names(songs_complete)=="n_artists"))
songs_complete = songs_complete[,-inidici_da_togliere]

groups_songs = cluster_1(groups, k=100, artist_means, songs_complete[,-1])
songs_complete$featuring = ifelse(n_artists > 1, 1, 0)
songs_complete$groups = groups_songs
songs_complete$log_followers = log(media_followers)
songs_complete$n_artists = n_artists

set.seed(123)
split = sample(x=1:dim(songs_complete)[1], size=1500)
test_set = songs_complete[split,]
songs_complete = songs_complete[-split,]
dim(test_set)
dim(songs_complete)

model_gam1=mgcv::gam(popularity ~ s(duration_ms, bs='cr') + s(base_popularity, bs='cr') + s(log_followers,bs='cr') + groups + featuring, data = songs_complete)
summary(model_gam1)
plot(model_gam1)

model_gam2=mgcv::gam(popularity ~ s(duration_ms, bs='cr') + s(base_popularity, bs='cr') + log_followers + groups + featuring, data = songs_complete)
summary(model_gam2)
plot(model_gam2)

anova(model_gam1,model_gam2, test = "F")

model_gam3=mgcv::gam(popularity ~ s(duration_ms, bs='cr') + s(log_followers,bs='cr') + base_popularity + groups + featuring, data = songs_complete)
summary(model_gam3)
plot(model_gam3)

anova(model_gam1,model_gam3, test = "F")
# meglio model_gam3 stando all'anova, ma :
par(mfrow = c(1,1))
plot(model_gam1$residuals,model_gam3$residuals)
cor(model_gam1$residuals,model_gam3$residuals)
# quindi in realtà sono simili

# test assumptions:
hist(model_gam3$residuals)
qqnorm(model_gam3$residuals)
abline(0,1)
shapiro.test(model_gam3$residuals)
gam.check(model_gam3)
# x11()
qq.gam(model_gam3, rep = 100, type="deviance", s.rep = 0, level = 0.9)
# x11()
qq.gam(model_gam3, rep = 0, type="deviance", s.rep = 100, level = 0.9)

# prediction
pred = predict(model_gam3,newdata=test_set)

sqrt(sum((as.numeric(pred) - test_set$popularity)^2))
plot(as.numeric(pred), test_set$popularity, pch = 19, main = "residuals")
abline(0,1, col = "red")
idx = sample(1:1500, 1)
as.numeric(pred[idx]) ; test_set$popularity[idx]

# è necessario capire quali componenti sono da smoothare

plot(songs_complete$duration_ms, songs_complete$popularity, main = "duration")
plot(songs_complete$log_followers, songs_complete$popularity, main = "log-fllowers")
plot(songs_complete$base_popularity, songs_complete$popularity, main = "base pop")

# piuttosto che usare le splines, potrebbe essere meglio usare binning per duration

uneven_breaks <- as.numeric(quantile(songs_complete$duration_ms, c(0.05,0.10,0.25, 0.4,0.5,0.75, 0.85, 0.9,0.95, 0.99)))

model_gam3=mgcv::gam(popularity ~ cut(duration_ms,breaks=uneven_breaks) + 
                       s(log_followers,bs='tp') + 
                       base_popularity +
                       groups + 
                       featuring, 
                     data = songs_complete)
summary(model_gam3)

plot(model_gam3, scheme = 1)

plot(songs_complete$duration_ms, songs_complete$popularity, main = "duration", pch = 19)
abline(v = uneven_breaks, lty = 2, col="red")

plot(songs_complete$log_followers, songs_complete$popularity, pch = 19, col = "gray", ylab = "popularity", xlab = "log(followers)")
# if only one knot, just use the argument knots = 1000 (for example) in the bspline function
degree = 3
model_linear_spline <-
  lm(popularity ~ bs(log_followers, degree = degree), data = songs_complete)

new_data <-data.frame(
  log_followers = seq(range(songs_complete$log_followers)[1], range(songs_complete$log_followers)[2], by = 0.5)
)

preds=predict(model_linear_spline, new_data,se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

plot(songs_complete$log_followers, songs_complete$popularity, pch = 19, col = "gray", ylab = "popularity", xlab = "log(followers)")
lines(new_data$log_followers,preds$fit ,lwd =2, col =" blue")
matlines(new_data$log_followers, se.bands ,lwd =1, col =" blue",lty =3)


model_gam3=mgcv::gam(popularity ~ s(base_popularity, k = 10), 
                     data = songs_complete)
gam.check(model_gam3)
