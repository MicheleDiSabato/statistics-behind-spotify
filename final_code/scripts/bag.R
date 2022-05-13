ggbag <- function(data,x,y,polyClr,A,pointClr = "#ffffff") {
  bag <- compute.bagplot(data[,x], data[,y])
  
  hull.loop <- data.frame(x = bag$hull.loop[,1], y = bag$hull.loop[,2])
  hull.bag <- data.frame(x = bag$hull.bag[,1], y = bag$hull.bag[,2])
  pxy.outlier <- data.frame(x = bag$pxy.outlier[,1], y = bag$pxy.outlier[,2])
  
  ggplot() + 
    geom_polygon(data = hull.loop, aes(x,y), color = polyClr, fill = polyClr, alpha = A) +
    geom_polygon(data = hull.bag, aes(x,y), color = polyClr, fill = polyClr, alpha = A) +
    geom_point(data = pxy.outlier, aes(x,y), col = pointClr, pch = 16, cex = 1.5)
}

bag <- function(data,x,y) {
  bag <- compute.bagplot(data[,x], data[,y])
  hull.bag <- data.frame(x = bag$hull.bag[,1], y = bag$hull.bag[,2])
}

loop <- function(data,x,y) {
  bag <- compute.bagplot(data[,x], data[,y])
  hull.loop <- data.frame(x = bag$hull.loop[,1], y = bag$hull.loop[,2])
}