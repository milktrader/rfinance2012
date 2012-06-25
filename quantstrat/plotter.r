plotter <- function(returns, lastprice=1234, title="", max=10000, min=10){


prices      <- lastprice + (lastprice * returns)
gen         <- density(na.omit(prices))
more        <- lastprice
less        <- more
x1          <- min(which(gen$x >= more))
x2          <- max(which(gen$x <  max))
x3          <- min(which(gen$x >= min))
x4          <- max(which(gen$x <  less))

# plot and save

pdf("1.pdf")
plot(gen, xlab=title, ylab="", main="", yaxt="n")
with(gen, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="goldenrod4"))
with(gen, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="goldenrod"))
dev.off()

}
