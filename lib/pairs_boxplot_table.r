# Взято из книги Williams, Data Mining with Rattle, 5.2, visualizing distributions

panel.hist <- function(x, ...)
{
	usr <- par("usr"); on.exit(par(usr))
	par(usr=c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot=FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col="grey90", ...)
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	r <- (cor(x, y, use="complete"))
	txt <- format(c(r, 0.123456789), digits=digits)[1]
	txt <- paste(prefix, txt, sep="")
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt)
}

vars <- c(5, 7, 8, 9, 15, 24)
pairs(weather[vars],
				diag.panel=panel.hist,
				upper.panel=panel.smooth,
				lower.panel=panel.cor)