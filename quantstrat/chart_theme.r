chart_theme <- mytheme <- function() {
  theme <-list(col=list(bg="#FFFFFF",
                        label.bg="#F0F0F0",
                        grid="#303030",
                        grid2="#F5F5F5",
                        ticks="#999999",
                        labels="#BABABA",
                        line.col="darkorange",
                        dn.col="goldenrod4",
                        up.col="goldenrod",
                        dn.border="#BABABA",
                        up.border="#BABABA"),
               shading=1,
               format.labels=TRUE,
               coarse.time=FALSE,
               rylab=FALSE,
               lylab=TRUE,
               grid.ticks.lwd=1,
               grid.ticks.on="months")
  theme$bbands <- list(col=list(fill="whitesmoke",upper="#D5D5D5",
                                lower="#D5D5D5",ma="#D5D5D5"),
                       lty=list(upper="dashed",lower="dashed",ma="dotted")
                      )
  theme
#  theme$bbands
}
