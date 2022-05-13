plot_binning = function(data, stringa, layout_pic = c(3,3))
{

  if (stringa == "pop")
    pop = data$popularity
  if (stringa == "base_pop")
    pop = data$base_popularity
  if (stringa != "pop" & stringa !="base_pop")
    stop("Wrong input: either use <pop> or <base_pop>")
  
  # cut_danceability     = cut(data$danceability,breaks = as.numeric(quantile(data$danceability, prob = seq(0,1,0.1))) )
  # cut_energy           = cut(data$energy,breaks = as.numeric(quantile(data$energy, prob = seq(0,1,0.1))) )
  # cut_loudness         = cut(data$loudness,breaks = as.numeric(quantile(data$loudness, prob = seq(0,1,0.1))) )
  # cut_speechiness      = cut(data$speechiness,breaks = as.numeric(quantile(data$speechiness, prob = seq(0,1,0.1))) )
  # cut_acousticness     = cut(data$acousticness,breaks = as.numeric(quantile(data$acousticness, prob = seq(0,1,0.1))) )
  # cut_instrumentalness = cut(data$instrumentalness,breaks = c(min(data$instrumentalness),0.002,max(data$instrumentalness)) )
  # cut_valence          = cut(data$valence,breaks = as.numeric(quantile(data$valence, prob = seq(0,1,0.1))) )
  # cut_tempo            = cut(data$tempo,breaks = as.numeric(quantile(data$tempo, prob = seq(0,1,0.1))) )
  # cut_duration_ms      = cut(data$duration_ms,breaks = as.numeric(quantile(data$duration_ms, prob = seq(0,1,0.1))) )
  # 
  # cuts = rbind(cut_danceability, cut_energy, cut_loudness,
  #              cut_speechiness, cut_acousticness, cut_instrumentalness,
  #              cut_valence, cut_tempo, cut_duration_ms)
  x11()
  par(mfrow=c(layout_pic[1],layout_pic[2]))
  
  n = dim(data)[1]
  p = dim(data)[2] - 1 # number of features different from popularity

  container = matrix(rep(0, n*p), nrow = n, ncol = p)
  for (j in 1:p)
  {
    container[,j] = data[,j+1] # we need to skip popularity
  }

  names = names(data)[-1]
  for (i in 1:p)
  {
    data_ = data.frame(x = container[,i])
    data_$popularity = pop
    if (names[i] == "instrumentalness")
    {
      br = c(min(data_$x), 0.02, max(data_$x))
    }
    if (names[i] != "instrumentalness")
    {
      br = as.numeric(quantile(data_$x, prob=seq(0,1,0.1)))
    }
    m_cut=lm(popularity ~ cut(x, breaks = br), data=data_)
    data.grid=with(data_, seq(range(x)[1],range(x)[2],length.out = 1000))
    preds=predict(m_cut,list(x=data.grid),se=T)
    se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
    with(data_, plot(x ,popularity  ,cex =.5, col =" darkgrey ", xlab = names[i]))
    lines(data.grid,preds$fit ,lwd =2, col =" blue")
    matlines(data.grid ,se.bands ,lwd =1, col =" blue",lty =3)
  }
  
  
}