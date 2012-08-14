try <- read.csv("/Users/ericstone/Dropbox/Machine Learning Project/PACs/individuals.csv", header=TRUE)
try[is.na(try)] <- 0
rownames(try) <- try$Donor
try <- try[,-1]
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
                                   
                               