library(ggplot2)
library(gridExtra)
library(grid)


p1 <- ggplot(iris, aes(x=Sepal.Length, y = Petal.Length, col=Species))+geom_point()
p2 <- ggplot(iris, aes(x=Sepal.Width, y = Petal.Length, col=Species))+geom_point()
p3 <- ggplot(iris, aes(x=Sepal.Width, y = Petal.Width, col=Species))+geom_point()
p4 <- ggplot(iris, aes(x=Sepal.Length, y = Petal.Width, col=Species))+geom_point()


grid.arrange(p1, p2, p3, p4,nrow=2)


p1 <- p1+theme(legend.position = "none")
p2 <- p2+theme(legend.position = "none")
p3 <- p3+theme(legend.position = "none")
p4 <- p4+theme(legend.position = "none")


g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
p1.original = ggplot(iris, aes(x=Sepal.Length, y = Petal.Length, col=Species))+geom_point()
plotLegend = g_legend(p1.original)

lwidth = sum(plotLegend$width)
combinedPlots <- arrangeGrob(p1, p2, p3, p4, nrow=2)

grid.arrange(combinedPlots,
             plotLegend, 
             widths=unit.c(unit(1, "npc") - lwidth, lwidth), nrow=1
             )