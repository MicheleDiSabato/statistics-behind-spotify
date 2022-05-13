####
setwd("E:/NPS_project")

rm(list = ls())
graphics.off()
artist_means <- read.table('artist_means.csv', header=TRUE, sep=",")
head(artist_means)
names(artist_means)

artist_means = artist_means[,c(-1,-2,-6,-8,-11,-16,-17,-18)]
names(artist_means)

min_max = list()
for (j in 1:dim(artist_means)[2])
{
  v = artist_means[,j]
  min_max[[j]] = c(min(v), max(v))
}
artist_means <- data.frame(lapply(artist_means, function(x) (x - min(x))/(max(x)-min(x))))

artist_means.dist <- dist(artist_means[,c(-10)], method='euclidean')
# artist_means.dist <- dist(artist_means, method='canberra')
# artist_means.dist <- dist(artist_means, method='manhattan')

artist_means.hclust <- hclust(artist_means.dist, method='single')
artist_means.hclust <- hclust(artist_means.dist, method='average')
artist_means.hclust <- hclust(artist_means.dist, method='complete')
artist_means.hclust <- hclust(artist_means.dist, method='ward.D2')

plot(artist_means.hclust, main='euclidean-ward.D2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

rect.hclust(artist_means.hclust, k=2)
rect.hclust(artist_means.hclust, k=3)
rect.hclust(artist_means.hclust, k=4)

artist_means_cluster <- cutree(artist_means.hclust, k=4) 
x11()
plot(artist_means, col = artist_means_cluster)

coph <- cophenetic(artist_means.hclust)
correlation_before_after <- cor(artist_means.dist, coph)
correlation_before_after

# if we want to use 3 groups
groups_temp <- as.numeric(cutree(artist_means.hclust, k = 3))
groups_temp[which(groups_temp == 1)] = "A"
groups_temp[which(groups_temp == 2)] = "B"
groups_temp[which(groups_temp == 3)] = "C"
groups_3 = factor(groups_temp)

# if we want to use 2 groups
groups_temp <- as.numeric(cutree(artist_means.hclust, k = 2))
groups_temp[which(groups_temp == 1)] = "A"
groups_temp[which(groups_temp == 2)] = "B"
groups_2 = factor(groups_temp)

# if we want 4 groups
groups_temp <- as.numeric(cutree(artist_means.hclust, k = 4))
groups_temp[which(groups_temp == 1)] = "A"
groups_temp[which(groups_temp == 2)] = "B"
groups_temp[which(groups_temp == 3)] = "C"
groups_temp[which(groups_temp == 4)] = "D"
groups_4 = factor(groups_temp)

# if we want 5 groups
groups_temp <- as.numeric(cutree(artist_means.hclust, k = 5))
groups_temp[which(groups_temp == 1)] = "A"
groups_temp[which(groups_temp == 2)] = "B"
groups_temp[which(groups_temp == 3)] = "C"
groups_temp[which(groups_temp == 4)] = "D"
groups_temp[which(groups_temp == 5)] = "E"
groups_5 = factor(groups_temp)

# is there an actual difference among the groups? Are the groups significant?
fit2 <- manova(as.matrix(artist_means) ~ groups_2)
fit3 <- manova(as.matrix(artist_means) ~ groups_3)
fit4 <- manova(as.matrix(artist_means) ~ groups_4)
fit5 <- manova(as.matrix(artist_means) ~ groups_5)

T2 <- -summary.manova(fit2,test="Wilks")$stats[1,2]
T3 <- -summary.manova(fit3,test="Wilks")$stats[1,2]
T4 <- -summary.manova(fit4,test="Wilks")$stats[1,2]
T5 <- -summary.manova(fit5,test="Wilks")$stats[1,2]

p_val2 = p_val3 = p_val4 = p_val5 = 0

execute = FALSE
B <- ifelse(execute, 1000, 1)
T_stat2 <- T_stat3 <- T_stat4 <- T_stat5 <- numeric(B)
n = dim(artist_means)[1]
set.seed(123)
for(perm in 1:B){
  permutation <- sample(1:n)
  groups.perm_2 <- groups_2[permutation]
  groups.perm_3 <- groups_3[permutation]
  groups.perm_4 <- groups_4[permutation]
  groups.perm_5 <- groups_5[permutation]
  fit.perm2 <- manova(as.matrix(artist_means) ~ groups.perm_2)
  T_stat2[perm] <- -summary.manova(fit.perm2,test="Wilks")$stats[1,2]
  fit.perm3 <- manova(as.matrix(artist_means) ~ groups.perm_3)
  T_stat3[perm] <- -summary.manova(fit.perm3,test="Wilks")$stats[1,2]
  fit.perm4 <- manova(as.matrix(artist_means) ~ groups.perm_4)
  T_stat4[perm] <- -summary.manova(fit.perm4,test="Wilks")$stats[1,2]
  fit.perm5 <- manova(as.matrix(artist_means) ~ groups.perm_5)
  T_stat5[perm] <- -summary.manova(fit.perm5,test="Wilks")$stats[1,2]
  print(perm/B*100)
}

(p_val2 <- sum(T_stat2>=T2)/B)
(p_val3 <- sum(T_stat3>=T3)/B)
(p_val4 <- sum(T_stat4>=T4)/B)
(p_val5 <- sum(T_stat5>=T5)/B)

if(execute)
  {
par(mfrow = c(2,3))
plot(ecdf(T_stat2))
abline(v=T2,col=3,lwd=4)
plot(ecdf(T_stat3))
abline(v=T3,col=3,lwd=4)
plot(ecdf(T_stat4))
abline(v=T4,col=3,lwd=4)
plot(ecdf(T_stat5))
abline(v=T5,col=3,lwd=4)
par(mfrow = c(1,1))
}

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
  for(col in 1:length(min_max))
  {
    a = min_max[[col]][1] ; b = min_max[[col]][2] ; l = b-a
    test[,col] = (test[,col] - a)/l
  }
  
  knnClust <- knn(train = train, test = test , k = k, cl = groups)
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

# new idea: use kmedians instead of kmeans, which imposes the data to have a mean.
# moreover kmedians could be more robust than kmeans, even though i odn't really
# know what "robust" means in this context

# linear model

songs_complete <- read.csv('additional_songs_clean.csv', header=TRUE)
# songs_complete$popularity = floor(songs_complete$popularity)
# songs_complete$popularity = as.integer(songs_complete$popularity)
# songs_complete$base_popularity = floor(songs_complete$base_popularity)
# songs_complete$base_popularity = as.integer(songs_complete$base_popularity)

# songs_complete$base_popularity = as.double(songs_complete$base_popularity)
# songs_complete$popularity = as.double(songs_complete$popularity)
names(songs_complete)
names(artist_means)

songs_complete = songs_complete[,c(-1,-2,-7,-9,-12,-17,-19)]
names(songs_complete)
n_artists = songs_complete$n_artists
media_followers = (songs_complete$followers_0 + songs_complete$followers_1 + songs_complete$followers_2 +
                       songs_complete$followers_3 + songs_complete$followers_4 + songs_complete$followers_5 +
                       songs_complete$followers_6 + 1)/n_artists
log_max_followers = log(pmax(songs_complete$followers_0, songs_complete$followers_1, 
                           songs_complete$followers_2,songs_complete$followers_3,
                           songs_complete$followers_4, songs_complete$followers_5,
                           songs_complete$followers_6) +1)

# provare col max 
inidici_da_togliere = c(which(names(songs_complete)=="followers_0"),
                       which(names(songs_complete)=="followers_1"),
                       which(names(songs_complete)=="followers_2"),
                       which(names(songs_complete)=="followers_3"),
                       which(names(songs_complete)=="followers_4"),
                       which(names(songs_complete)=="followers_5"),
                       which(names(songs_complete)=="followers_6"),
                       which(names(songs_complete)=="n_artists"))
songs_complete = songs_complete[,-inidici_da_togliere]

table(groups_2)
table(groups_3)
table(groups_4)
table(groups_5)
# if i INcrease the number of groups, i need to DEcrease k
groups_songs2 = cluster_1(groups_2, k=100, artist_means, songs_complete[,-1])
groups_songs3 = cluster_1(groups_3, k=80, artist_means, songs_complete[,-1])
groups_songs4 = cluster_1(groups_4, k=70, artist_means, songs_complete[,-1])
groups_songs5 = cluster_1(groups_5, k=70, artist_means, songs_complete[,-1])

songs_complete$featuring = ifelse(n_artists > 1, 1, 0)

# now i need to choose a number of groups: i choose 4
songs_complete$groups = groups_songs5
# songs_complete$log_followers = log(media_followers)
songs_complete$log_followers = log_max_followers
songs_complete$n_artists = n_artists

songs_complete$base_popularity_scaled = (songs_complete$base_popularity - mean(songs_complete$base_popularity))/sd(songs_complete$base_popularity)
songs_complete$log_followers_scaled = (songs_complete$log_followers - mean(songs_complete$log_followers))/sd(songs_complete$log_followers)
sd_base_pop = sd(songs_complete$base_popularity)
sd_log_followers = sd(songs_complete$log_followers)
mean_base_pop = mean(songs_complete$base_popularity)
mean_log_followers = mean(songs_complete$log_followers)

data = data.frame(sum = songs_complete$base_popularity_scaled + songs_complete$log_followers_scaled,
                  pop = songs_complete$base_popularity)
data.dist <- dist(data, method='euclidean')
# artist_means.dist <- dist(artist_means, method='canberra')
# artist_means.dist <- dist(artist_means, method='manhattan')

data.hclust <- hclust(data.dist, method='ward.D2')
plot(data.hclust)
data.cluster <- as.numeric(cutree(data.hclust, k=2))
data.cluster[which(data.cluster == 1)] = "group1"
data.cluster[which(data.cluster == 2)] = "group2"
pop_cluster = factor(data.cluster)
songs_complete$pop_cluster = pop_cluster

data_pop = data.frame(base_pop = songs_complete$base_popularity + rnorm(4776, 0, 0.001),
                      pop = songs_complete$popularity + rnorm(4776, 0, 0.001))
# plot(data_pop, pch = 19, ylab = "artist popularity", xlab = "song popularity")
# depthContour(
#   as.matrix(data_pop),
#   depth_params = list(method = 'Tukey'),
#   points = TRUE,
#   colors = colorRampPalette(c('white', 'blue')), # scala cromatica
#   levels = 10, # numero di livelli della scala cromatica
#   pdmedian = T, # non plottare la Tuckey median
#   graph_params = list(cex=.01, pch=1), # parametr del plot dei punti
#   pmean = T # non plottare la media
# )
tm = as.numeric(depthMedian(data_pop,depth_params = list(method='Tukey')))
plot(data_pop, pch = 19, ylab = "artist popularity", xlab = "song popularity")
points(tm[1],tm[2], col ="red", pch = 19)
bagplot_pop <- bagplot(data_pop)
outlying_obs <- bagplot_pop$pxy.outlier
# outlying_obs[,1] = as.integer(outlying_obs[,1])
# outlying_obs[,2] = as.integer(outlying_obs[,2])
ind_outlying_obs <- which(apply(data_pop,1,function(x) all(x %in% outlying_obs)))

# ind_outlying_obs = numeric(dim(outlying_obs)[1])
# for(i in 1:170)
# {
#   found = 0
#   for(j in 1:4776)
#   {
#     if(found == 0)
#     {
#       if(data_pop[j,1] == outlying_obs[i,1] & data_pop[j,2] == outlying_obs[i,2])
#       {
#         found = 1
#         ind_outlying_obs[i] = j
#       }
#     }
#   }
#   
# }

colore = rep("green", dim(data_pop)[1])
colore[ind_outlying_obs] = "red"
plot(data_pop, pch=19, col = colore, main="red: discard, green: keep")
# abline(h=30)
# abline(h=12)
# abline(v=66)
# which(outlying_obs[,1] == 66)
# sort(data_pop$pop[which(data_pop$base_pop == 66)])
# outlying_obs[which(outlying_obs[,1] == 66),]
# 3578 66  47

songs_reduced = songs_complete[-ind_outlying_obs,]
points(songs_reduced$base_popularity, songs_reduced$popularity, col="blue")

par(mfrow=c(1,3))
plot(songs_complete$log_followers, songs_complete$popularity, pch=19)
plot(songs_complete$base_popularity, songs_complete$popularity, pch=19)
plot(songs_complete$loudness, songs_complete$popularity, pch=19)
par(mfrow = c(1,1))
# GAM

set.seed(123)
split = sample(x=1:dim(songs_complete)[1], size=500)
test_set = songs_complete[split,]
test_set$popularity = test_set$popularity + 0.001 # to avoid division by zero
train_set = songs_complete[-split,]
dim(test_set)
dim(train_set)

model_gam1=mgcv::gam(popularity ~ s(loudness, bs='cr') + 
                       s(base_popularity, bs='cr') + 
                       s(log_followers,bs='cr') + 
                       groups + featuring, 
                     data = train_set, family=gaussian)
summary(model_gam1)
par(mfrow = c(1,3))
plot(model_gam1)
par(mfrow=c(1,1))

gam.check(model_gam1)
# residui non normali e sono eteroschedastici inoltre sembrano esserci due gruppi

model_gam2=mgcv::gam(popularity ~ loudness + 
                       s(base_popularity, bs='cr') + 
                       s(log_followers,bs='cr') + 
                       groups + featuring, 
                     data = train_set, family=gaussian)
summary(model_gam2)
gam.check(model_gam2)

model_gam3=mgcv::gam(popularity ~ loudness + 
                       s(base_popularity, bs='cr') +
                       s(log_followers,bs='cr') + 
                       s(I(log_followers*base_popularity), bs='cr') + 
                       groups + featuring, 
                     data = train_set, family=gaussian)
summary(model_gam3)
plot(model_gam3)

data = data.frame(base_pop = train_set$base_popularity, 
                  log_followers = train_set$log_followers)

pc.data <- princomp(scale(data), scores=T)
summary(pc.data)
load.data <- pc.data$loadings
x11()
par(mfcol = c(1,2))
for(i in 1:2) barplot(load.data[,i], ylim = c(-1, 1), main=paste("PC",i))

# plot(train_set$base_popularity, train_set$log_followers, pch = 19)
model_gam4=mgcv::gam(popularity ~ s(duration_ms, bs='cr') + 
                       s(I(base_popularity_scaled - log_followers_scaled), bs='cr') + 
                       s(I(base_popularity_scaled + log_followers_scaled), bs='cr') + 
                       groups + featuring, 
                       data = train_set, family=gaussian)
summary(model_gam4)
plot(model_gam4)

x11() 
plot(train_set$base_popularity_scaled + train_set$log_followers_scaled,train_set$popularity)
# points(train_set[which(train_set$pop_cluster=="group1"),]$base_popularity_scaled + train_set[which(train_set$pop_cluster=="group1"),]$log_followers_scaled,train_set[which(train_set$pop_cluster=="group1"),]$popularity, col ="red")

# model_gam5=mgcv::gam(popularity ~ loudness + 
#                        s(I(base_popularity_scaled - log_followers_scaled), bs='cr') + 
#                        s(I(base_popularity_scaled + log_followers_scaled), bs='cr') + 
#                        groups + featuring + pop_cluster, 
#                        data = train_set, family=gaussian)
# summary(model_gam5)
# plot(model_gam5)
# gam.check(model_gam5)
# 
# ddPlot(x = train_set[which(train_set$pop_cluster=="group1"),c(2,14)],
#        y = train_set[which(train_set$pop_cluster=="group2"),c(2,14)], 
#        depth_params = list(method='Tukey'))

# model_gam2=mgcv::gam(popularity ~ s(duration_ms, bs='cr') + s(base_popularity, bs='cr') + log_followers + groups + featuring, data = songs_complete)
# summary(model_gam2)
# par(mfrow = c(1,2))
# plot(model_gam2)
# 
# anova(model_gam1,model_gam2, test = "F")
# 
# model_gam3=mgcv::gam(popularity ~ s(duration_ms, bs='cr') + s(log_followers,bs='cr') + base_popularity + groups + featuring, data = songs_complete)
# summary(model_gam3)
# plot(model_gam3)
# 
# anova(model_gam1,model_gam3, test = "F")
# # meglio model_gam3 stando all'anova, ma :
# par(mfrow = c(1,1))
# plot(model_gam1$residuals,model_gam3$residuals)
# cor(model_gam1$residuals,model_gam3$residuals)
# # quindi in realtï¿½ sono simili
# 

# # test assumptions:
hist(model_gam4$residuals)
shapiro.test(model_gam4$residuals)
gam.check(model_gam4)
# x11()
qq.gam(model_gam4, rep = 100, type="deviance", s.rep = 0, level = 0.9)
# x11()
qq.gam(model_gam4, rep = 0, type="deviance", s.rep = 100, level = 0.9)

# the assumptions are not met --> permutational tests to understand which variables are relevant:
summary(model_gam4)
names(summary(model_gam4))
summary(model_gam4)$pTerms.chi.sq # wald test stat for each parametric term
summary(model_gam4)$pTerms.table
summary(model_gam4)$p.table

summary(model_gam4)$chi.sq # for smooth terms
summary(model_gam4)$s.table

# dalla documentazione sembra che tutte queste quantità servano per
# testare che la rilevanza delle covariate, scelgo di usare p.table e s.table

x1 = train_set$duration_ms
x2 = train_set$base_popularity_scaled - train_set$log_followers_scaled
x3 = train_set$base_popularity_scaled + train_set$log_followers_scaled
x4 = train_set$groups
x5 = train_set$featuring
y = train_set$popularity
data = data.frame(x1,x2,x3,x4,x5,y)
get_formula = function(i)
{
  if(i==0)
    f = y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + x4 + x5
  if(i==1)
    f = y ~ s(x2, bs='cr') + s(x3, bs='cr') + x4 + x5
  if(i==2)
    f = y ~ s(x1, bs='cr') + s(x3, bs='cr') + x4 + x5
  if(i==3)
    f = y ~ s(x1, bs='cr') + s(x2, bs='cr') + x4 + x5
  if(i==4)
    f = y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + x5
  if(i==5)
    f = y ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + x4
  return(f)
}
set.seed(123)
B = 1000
pval = numeric(5)
for(i in 1:5)
{
  print(i)
  model_gam_reduced = mgcv::gam(get_formula(i), data=data, family = gaussian)
  fitted.obs = model_gam_reduced$fitted.values
  residuals.obs = data$y - fitted.obs
  Tperm = numeric(B)
  if(i<=3)
    T0 = summary(mgcv::gam(get_formula(0), data=data, family = gaussian))$s.table[i,3]
  else
    T0 = summary(mgcv::gam(get_formula(0), data=data, family = gaussian))$pTerms.chi.sq[i-3]
  pb=progress_bar$new(total=B)
  for(b in 1:B)
  {
    residuals.perm = sample(residuals.obs, replace = FALSE)
    y.perm = fitted.obs + residuals.perm
    gam.perm = mgcv::gam(y.perm ~ s(x1, bs='cr') + s(x2, bs='cr') + s(x3, bs='cr') + x4 + x5, data = data, family=gaussian)
    if(i<=3)
      Tperm[b] = summary(gam.perm)$s.table[i,3]
    else
      Tperm[b] = summary(gam.perm)$pTerms.chi.sq[i-3]
    pb$tick()
  }
  pval[i] = sum(Tperm>=T0)/B
}

pval
c(summary(model_gam4)$s.pv, summary(model_gam4)$pTerms.pv)

# 
# prediction
pred = predict(model_gam1,newdata=test_set)

sqrt(sum((as.numeric(pred) - test_set$popularity)^2))

mean(abs(test_set$popularity-as.numeric(pred))/test_set$popularity)*100
# mean(abs(1-as.numeric(pred)/test_set$popularity))*100
mean(abs(test_set$popularity-as.numeric(pred)))

idx = sample(1:500, 1)
as.numeric(pred[idx]) ; test_set$popularity[idx]


# fare robust statistics e depth measures per togliere outliers
# rifare lm robusto e cercare di batterlo con gam

#### GENERALIZED ADDITIVE MIXED MODELS ####
  # FUNZIONI PER IL CALCOLE DEGLI ERRORI SUL TEST SET
mse_func = function(pred, true){
  n = length(pred)
  err = 0.0
  for(i in 1:n){
    err = err + (true[i]-pred[i])^2
  }
  err = err/n
  return(err)
}

mae_func = function(pred, true){
  n = length(pred)
  err = 0.0
  for(i in 1:n){
    err = err + abs(true[i]-pred[i])
  }
  err = err/n
  return(err)
}

mape_func = function(pred, true){ # difficilmente utilizzabile perchÃ¨ alcune popolaritÃ  sono troppo basse
  n = length(pred)                # e.g. min(test_set$popularity) Ã¨ 0.001
  err = 0.0
  for(i in 1:n){
    err = err + abs(true[i]-pred[i])/true[i]
  }
  err = err/n
  return(err)
}
  # PRIMO GAMM --> come model_gam4 + re su groups
gamm_fit1 = mgcv::gam(popularity ~ loudness + 
                        s(I(base_popularity_scaled - log_followers_scaled), bs='cr') +
                        s(I(base_popularity_scaled + log_followers_scaled), bs='cr') +
                        s(groups, bs = 're') + featuring,
                      data = train_set, method = "REML")
summary(gamm_fit1)
par(mfcol = c(1,3))
plot(gamm_fit1)

gamm_pred1 = as.vector(predict(gamm_fit1, test_set))

MSE_gamm1 = mse_func(gamm_pred1, test_set$popularity)
MAE_gamm1 = mae_func(gamm_pred1, test_set$popularity)
MAPE_gamm1 = mape_func(gamm_pred1, test_set$popularity) 

  # SECONDO GAMM --> provo ad aggiungere interazione groups-pop_cluster
gamm_fit2 = mgcv::gam(popularity ~ loudness + 
                        s(I(base_popularity_scaled - log_followers_scaled), bs='cr') +
                        s(I(base_popularity_scaled + log_followers_scaled), bs='cr') +
                        s(groups,pop_cluster, bs = 're') + featuring,
                      data = train_set, method = "REML")
summary(gamm_fit2)
par(mfcol = c(1,3))
plot(gamm_fit2)

gamm_pred2 = as.vector(predict(gamm_fit2, test_set))

MSE_gamm2 = mse_func(gamm_pred2, test_set$popularity)
MAE_gamm2 = mae_func(gamm_pred2, test_set$popularity)
MAPE_gamm2 = mape_func(gamm_pred2, test_set$popularity)  
# come errori Ã¨ il migliore, anche se le differenze sono veramente piccole

  # TERZO GAMM --> provo ad aggiungere interazione groups-pop_cluster
gamm_fit3 = mgcv::gam(popularity ~ loudness + 
                        s(I(base_popularity_scaled - log_followers_scaled), bs='cr') +
                        s(I(base_popularity_scaled + log_followers_scaled), bs='cr') +
                        s(groups, bs = 're') + s(featuring, bs = 're'),
                      data = train_set, method = "REML")
summary(gamm_fit3)
par(mfcol = c(1,4))
plot(gamm_fit3)

gamm_pred3 = as.vector(predict(gamm_fit3, test_set))

MSE_gamm3 = mse_func(gamm_pred3, test_set$popularity)
MAE_gamm3 = mae_func(gamm_pred3, test_set$popularity)
MAPE_gamm3 = mape_func(gamm_pred3, test_set$popularity) 

  # PROVA CON FUNZIONE GAMM
gamm_fit_prova <- mgcv::gamm(popularity ~ loudness + 
                               s(I(base_popularity_scaled - log_followers_scaled), bs='cr') +
                               s(I(base_popularity_scaled + log_followers_scaled), bs='cr'),
                             data = train_set, random = list(groups =~1) )
summary(gamm_fit_prova$lme)
summary(gamm_fit_prova$gam)

gamm_pred_prova = as.vector(predict(gamm_fit_prova$gam, test_set))

MSE_gamm_prova = mse_func(gamm_pred_prova, test_set$popularity)
MAE_gamm_prova = mae_func(gamm_pred_prova, test_set$popularity)
MAPE_gamm_prova = mape_func(gamm_pred_prova, test_set$popularity) 
# praticamente identico a quello ottenuto con funzione gam. Consigliavano di usare funzione gam
# perchÃ¨ ha un optimizer piÃ¹ preciso https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
