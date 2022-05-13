apply.pca <- function(loads, data) {
  for (i in seq(1,nrow(loads))) {
    new.var <- paste("PC",i,sep="")
    data[,new.var] <- 0
    for (feature in names(loads)) {
      data[,new.var] <- data[,new.var] + loads[i,feature]*data[,feature]
    }
  }
  data <- data
}


has_custom_fonts <- F

plot.pc <- function(pc.loads, pos, colorBar, colorLine, thm) {
  p <- ggplot(data.frame(var = names(pc.loads), lds = t(pc.loads)[,pos]), aes(x=var,y=lds)) + 
    geom_bar(stat="identity", fill=colorBar) +
    thm + theme(panel.grid.major.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank()) + 
    geom_hline(yintercept = 0, color=colorLine, size=1.5) +
    labs(x=NULL,y=NULL) + 
    ylim(min(pc.loads),max(pc.loads))
  if (has_custom_fonts) {
    p <- p + geom_text(aes(label=var, y = -0.75, angle=90), size = 6, family = "SF Pro Display", hjust=0, fontface = "bold")
  } else {
    p <- p + geom_text(aes(label=var, y = -0.75, angle=90), size = 6, hjust=0, fontface = "bold")
  }
  p <- p
}