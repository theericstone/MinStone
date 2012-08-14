

lines <- read.csv("C:/Users/Angela/Dropbox/Machine Learning Project/PACs/lines.csv")

plot.new()
plot.window(xlim=c(0,8), ylim=c(0,5))

for (i in 1:3318) {
segments(lines$x1.2[i], lines$y1[i], lines$x2[i], lines$y2[i], col=as.character(lines$Color[i]))
}


as.character(lines$Color[1])



