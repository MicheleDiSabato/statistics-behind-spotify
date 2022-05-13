cross_validate = function(d, length_range = 100, kernel)
{

  # data will have two features: 
  #       popularity
  #       x (e.g. x = energy, danceability, tempo, ...)
  
  n = dim(d)[1] # n should be equal to 500
  p = dim(d)[2] # p should be equal to 2

  bw_range = abs(diff(range(d$x))) / 1:length_range
  errors = rep(0,length_range)
  num_batches = 5
  for (j in 1:length_range) # for each choice of the parameter bw
  {
    for (i in 1:num_batches) # for each batch
    {
      # gap_index = (1:100) + 100 * (i-1)
      gap_size = ceiling(n/num_batches)
      gap_index = sample(1:n, size = gap_size, replace = FALSE)
      m_loc = npreg(popularity ~ x,
                    ckertype = kernel,
                    bws = bw_range[j],
                    data = d[-gap_index,])  
      
      ith_datum = data.frame(x=d[gap_index,]$x)
      preds=predict(m_loc,newdata=ith_datum,se=T)
      errors[j] = errors[j] + sum((preds$fit - d[i,]$popularity)^2)
    }
    # for each element in the vector "bw_range" we compute the weighted L2 norm
    # of the errors. The weight is gap_size/
    errors[j] = sqrt(errors[j])/num_batches
  }
  return(bw_range[which.min(errors)])
}


plot_kernel_estimation = function(data, popularity_choice, kernel, bw_vector = rep(0,9), cv_method = 3, layout_pic = c(3,3))
{
  # INPUTS:
  #   data
  #   popularity_choice: string used to determine which popularity feature to use
  #   kernel: kernel chosed for the local averages
  #   bw_vector: optional input that contains the bandwidth to use for each feature 
  #              (vector with 9 components)
  #   cv_method: integer specifying which method to use to choose bw
  #              (WARNING: to use bw_vector as badwith it is necessary to set cv_method = 2 )
  # 
  # OUTPUT: 
  #   names_bw: vector of 9 strings, used to understand which bw has been chosen for each
  #             of the 9 features
  
  
  
  # if popularity_choice is "popularity", then we use as popularity the feature with the same name,
  # otherwise we use the feature called "base_popularity" (if the dataset contains such feature)
  if (popularity_choice == "popularity")
    popularity = data$popularity
  if (popularity_choice == "base_popularity" & "base_popularity" %in% names(data))
    popularity = data$base_popularity
  if (popularity_choice == "base_popularity" & !("base_popularity" %in% names(data)))
    stop("Wrong input: feature <base_popularity> is not contained in the dataset")
  if(popularity_choice != "popularity" & popularity_choice != "base_popularity") 
    stop("Wrong input: either use <popularity> or <base_popularity>")
  
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
  names_bw = names
  
  for (i in 1:p)
  {
    data_ = data.frame(x = container[,i],
                       popularity = popularity)
    # we propose three ways of selecting the bandwidth
    # the best way to choose the bandwidth should be via CV, but it is computationally expensive
    # 1) CV
    if(cv_method == 1)
    {
      bw = cross_validate(data_, length_range = 100, kernel = kernel)
    }
    # 2) user input
    if(cv_method == 2)
    {
      bw = bw_vector[i]
    }
    # 3) mean of the sorted distance of the first half of the dataset
    if(cv_method == 3)
    {
      dist_matrix = dist(data_$x)
      dist_matrix_upper_tri = dist_matrix[upper.tri(dist_matrix)]
      dist_matrix_upper_tri[which(is.na(dist_matrix_upper_tri))] = 0 # to deal with some NaN
      bw = mean(sort(dist_matrix_upper_tri[1:ceiling(500/2)]))
    }
    names_bw[i] = paste(names_bw[i], toString(bw), sep = ": ")
    m_loc = npreg(popularity ~ x,
                  ckertype = kernel,
                  bws = bw,
                  data = data_)
    # the spacing between the points has been set to half of the bandwidth
    new_data=data.frame(x=with(data_, seq(range(x)[1],range(x)[2],by=bw/3)))
    preds=predict(m_loc,newdata=new_data,se=T)
    se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
    with(
      data_,
      plot(
        x ,
        popularity ,
        xlim = range(new_data$x) ,
        cex = .5,
        col = " darkgrey ",
        main = paste("Kernel: ", kernel, sep=""),
        xlab = names[i]
      )
    )
    lines(new_data$x,preds$fit ,lwd =2, col =" blue")
    matlines(new_data$x,se.bands ,lwd =1, col =" blue",lty =3)
    print(names[i])
  }
  
  # the output is the bandwidth selected for each feature
  return(names_bw)
  
}

