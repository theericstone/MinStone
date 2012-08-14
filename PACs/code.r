if (.Platform$OS.type=="unix"){
  try <- read.csv("/Users/ericstone/Dropbox/Machine Learning Project/PACs/individuals.csv", header=TRUE)
} else {
try <- read.csv("C:/Users/Angela/Dropbox/Machine Learning Project/PACs/individuals.csv", header=TRUE)}

library(qgraph)
#qgraph(cor(big5),minimum=0.25,cut=0.4,vsize=2,groups=big5groups,legend=TRUE,borders=FALSE)

#title("Big 5 correlations",line=2.5)

#qgraph( try.cor, minimum=0.25, cut=0.4, vsize=2)

try[is.na(try)] <- 0
rownames(try) <- try$Donor
try <- try[,-1]
View(try)

try.cor <- cor(try)
vsize <- apply(try, 2, sum)
qgraph( try.cor, vsize=(vsize/2000000)  )

try.groups <- data.frame(spacs = c("AMERICAN.BRIDGE.21ST.CENTURY",
                                   "HOUSE.MAJORITY.PAC",
                                   "MAJORITY.PAC",
                                   "NEA.ADVOCACY.FUND",
                                   "PRIORITIES.USA.ACTION",
                                   "WORKERS..VOICE",
                                   "AMERICAN.CROSSROADS",
                                   "CLUB.FOR.GROWTH.ACTION",  
                                   "CONGRESSIONAL.LEADERSHIP.FUND",
                                   "ENDORSE.LIBERTY..INC", 
                                   "MAKE.US.GREAT.AGAIN..INC",
                                   "RESTORE.OUR.FUTURE..INC.",
                                   "TEXAS.CONSERVATIVES.FUND",
                                   "WINNING.OUR.FUTURE",
                                   "YG.ACTION.FUND"),
                         party = c("dem", "dem", "dem", "dem", 
                                   "dem", "dem", "rep", "rep", 
                                   "rep", "rep", "rep", "rep", 
                                   "rep", "rep", "rep"))

qgraph( try.cor , vsize=(vsize/2000000),  
        groups=try.groups$party, 
        color=c("light blue", "maroon"), 
        bg=TRUE, minimum=0.01)
#########################
####### HIVES ###########
#########################
require(HiveR)
labs <- c(rownames(try),names(try))
axis <- c(rep(1,nrow(try)),(as.numeric(try.groups$party)+1))
color <- factor(axis, labels=c("purple","blue","red"))

hive.1 <- adj2HPD(M=try, type="2D", desc="contributors to super PACs")
hive.1$nodes$axis <- as.integer(axis)
hive.1$nodes$color <- as.character(color)

hive.2 <- mineHPD(hive.1, option = "rad <- tot.edge.count")
hive.2$nodes$radius <- c(seq(.5,nrow(try)/2,.5),
                         hive.2$nodes$radius[(nrow(try)+1):nrow(hive.2$nodes)])
hive.2$nodes$size <- c(rep(.1,nrow(try)),
                       hive.2$nodes$radius[(nrow(try)+1):nrow(hive.2$nodes)]/100)
#hive.3 <- mineHPD(hive.2, option = "axis <- source.man.sink")

#hive.2$edges$weight <- rep(.1, nrow(hive.2$edges)) 
hive.2$edges$weight <- max(.1,hive.2$edges$weight*0.000000035)
hive.2$axis.cols <- as.character(c("White","Blue","Red"))

plotHive(hive.2, ch=40, bkgnd="Black")
# for (n in seq(1, length(hive.2$edges$id1), by = 2)) {
#   a <- hive.3$edges$id1[n]
#   b <- hive.3$edges$id2[n]
#   hive.3$edges$id1[n] <- b
#   hive.3$edges$id2[n] <- a
# }

plotHive(hive.3)
#######################
dotify <- function(x, data){
  sapply( 1:length(data[,x]), function(y) {
    if (data[y,x] > 0) {
      paste(rownames(data)[y],"->",names(data[x]),sep=" ") 
    } else { NULL }
  })
}

connections <- unlist(c(sapply(1:ncol(try), function(z) dotify(z, try))))


