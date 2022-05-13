ddplot <- function(
  x, y,
  colorx = "#e8e7e3",
  colory,
  method = "Tukey"
) {
  pnt.size <- 2
  u <- rbind(x,y)
  if (method %in% c("Tukey", "Mahalanobis", "Projection", "Euclidean")) {
    xcoords <- DepthProc::depth(u, x, method = method)
    xcoords <- xcoords@.Data
    ycoords <- DepthProc::depth(u, y, method = method)
    ycoords <- ycoords@.Data
  }
  if (method == "Liu") {
    xcoords <- deeper::depth_liu(u,x)
    ycoords <- deeper::depth_liu(u,y)
  }
  
  df           <- data.frame(xcoords,ycoords)
  colnames(df) <- c("D.1", "D.2")
  df$dist                    <- "1"
  df[nrow(x)+1:nrow(y),3]    <- "2"
  
  if (!missing(colory)) {
    if (colory == TRUE) {
      return_plot <- ggplot2::ggplot(data=df, aes(x=D.1, y=D.2, color=dist)) +
        ggplot2::geom_point(show.legend = FALSE, cex=pnt.size) +
        ggplot2::geom_abline(slope = 1) +
        ggplot2::scale_color_manual(
          values = c("1" = colorx, "2" = "#d5bd5e")
        )
    } else {
      return_plot <- ggplot2::ggplot(data=df, aes(x=D.1, y=D.2, color=dist)) +
        ggplot2::geom_point(show.legend = FALSE, cex=pnt.size) +
        ggplot2::geom_abline(slope = 1) +
        ggplot2::scale_color_manual(
          values = c("1" = colorx, "2" = colory)
        )
    }
  } else {
    return_plot <- ggplot2::ggplot(data=df, aes(x=D.1, y=D.2)) +
      ggplot2::geom_point(cex=pnt.size) +
      ggplot2::geom_abline(slope = 1)
  }
  return_plot <- return_plot
}