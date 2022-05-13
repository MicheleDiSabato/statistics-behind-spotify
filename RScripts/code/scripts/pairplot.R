scatterplot_helper <- function (data, thm, i, j, colorvec) {
  p <- local({
    i <- i
    j <- j
    ggplot(data, aes(x=data[,j],y=data[,i],color=colorvec)) + geom_point(cex=3) +
      thm + theme(legend.position = "none", axis.line.y = element_line(colour = "#929292", size = 1)) + 
      scale_color_manual(values=c("#000000", "#39c344")) + labs(x = NULL, y = NULL)
  })
}

histogram_helper <- function (data, thm, i) {
  p <- ggplot(data, aes(x=data[,i])) + geom_histogram( bins=25, fill="#39c344") +
    thm +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          axis.line.x = element_blank(), 
          axis.ticks.y = element_blank()) + 
    scale_color_manual(values=c("#000000", "#39c344")) + labs(x = names(data)[i], y = NULL)
}

fc <- function(c) {
  c
}

corr_helper <- function (data, i, j, custom_fonts) {
  correlation <- cor(data[,i], data[,j])
  if (correlation > 0) {
    # 39c344 - green
    clr <- mixcolor(fc(correlation), RGB(1,1,1), RGB(57/255,195/255,68/255))
  } else {
    # c24d38 - red
    clr <- mixcolor(fc(correlation), RGB(1,1,1), RGB(194/255,77/255,56/255))
  }
  color <- round(clr@coords * 255)
  color <- as.hexmode(color)
  color <- paste("#",paste(color,collapse=""),sep="")
  
  if (custom_fonts) {
    p <- ggplot() + 
      annotate("text", x = 4, y = 25, size=8,
               label = paste("Correlation\n",round(correlation, digits=3)), family = "SF Pro Display", fontface="bold") + 
      theme_void() + theme(panel.background = element_rect(fill=color))
  } else {
    p <- ggplot() + 
      annotate("text", x = 4, y = 25, size=8,
               label = paste("Correlation\n",round(correlation, digits=3)), fontface="bold") + 
      theme_void() + theme(panel.background = element_rect(fill=color))
  }
  p <- p
}

pairplot <- function (data, thm, colorvec, custom_fonts = T) {
  n <- ncol(data)
  for (i in 1:n) {
    for (j in 1:n) {
      if (j < i) {
        # Scatterplot
        p <- scatterplot_helper(data,thm,i,j,colorvec)
      } else if (j == i) {
        # Histogram
        p <- histogram_helper(data, thm, i)
      } else if (j > i) {
        # Correlation
        p <- corr_helper(data, i, j, custom_fonts)
      }
      
      if (i==1 & j==1) {
        pfinal <- p
      } else {
        pfinal <- pfinal + p
      }
    }
  }
  pfinal <- pfinal
}