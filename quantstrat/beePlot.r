
beePlot     <- function(returns,  title="", max=100, min=0, profit="green", loss="blue"){


prices      <- 1 +  returns
gen         <- density(na.omit(prices))
more        <- 1 
less        <- more
x1          <- min(which(gen$x >= more))
x2          <- max(which(gen$x <  max))
x3          <- min(which(gen$x >= min))
x4          <- max(which(gen$x <  less))

# plot and save

#pdf("1.pdf")
plot(gen, xlab=title, ylab="", main="", yaxt="n")
with(gen, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col=loss))
with(gen, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=profit))
#dev.off()

}
