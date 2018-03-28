### You need several libraries
#install.packages("circlize")
#install.packages("migest")
library(circlize)
library(migest)
library(dplyr)

#https://www.r-graph-gallery.com/122-a-circular-plot-with-the-circlize-package/
### Make data
m <- data.frame(order = 1:6,
                Logradouro = c("Joao Alfredo", "Aureliano", "Republica", "Lopo", "L Afonso", "J Nabuco"),
                V3 = c(1, 150000, 90000, 180000, 15000, 10000),
                V4 = c(35000, 1, 10000, 12000, 25000, 8000),
                V5 = c(10000, 7000, 1, 40000, 5000, 4000),
                V6 = c(7000, 8000, 175000, 1, 11000, 18000),
                V7 = c(70000, 30000, 22000, 120000, 1, 40000),
                V8 = c(60000, 90000, 110000, 14000, 30000, 1),
                r = c(255,255,255,153,51,51),
                g = c(51, 153, 255, 255, 255, 255),
                b = c(51, 51, 51, 51, 51, 153),
                stringsAsFactors = FALSE)
df1 <- m[, c(1,2, 9:11)]
#m <- m[,-(1:2)]/1e04
options(scipen=999)
m <- m[,-(1:2)]/10000
m <- as.matrix(m[,c(1:6)])
dimnames(m) <- list(orig = df1$Logradouro, dest = df1$Logradouro)
#Sort order of data.frame and matrix for plotting in circos
df1 <- arrange(df1, order)
df1$Logradouro <- factor(df1$Logradouro, levels = df1$Logradouro)
m <- m[levels(df1$Logradouro),levels(df1$Logradouro)]


### Define ranges of circos sectors and their colors (both of the sectors and the links)
df1$xmin <- 0
df1$xmax <- rowSums(m) + colSums(m)
n <- nrow(df1)
df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)

### Plot sectors (outer part)
par(mar=rep(0,4))
circos.clear()

### Basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.15), start.degree = 90, gap.degree =4)

### Sector details
circos.initialize(factors = df1$Logradouro, xlim = cbind(df1$xmin, df1$xmax))

### Plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$Logradouro, track.height=0.1,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #text direction (dd) and adjusmtents (aa)
                         theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                         aa = c(1, 0.5)
                         if(theta < 90 || theta > 270)  aa = c(0, 0.5)
                         
                         #plot Logradouro labels
                         circos.text(x=mean(xlim), y=1.7, labels=name, facing = dd, cex=0.6,  adj = aa)
                         
                         #plot main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2],
                                     col = df1$rcol[i], border=df1$rcol[i])
                         
                         #blank in part of main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.3,
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         
                         #plot axis
                         circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(from=0,to=floor(df1$xmax)[i],by=5),
                                     minor.ticks=1, labels.away.percentage = 0.15)
                       })

### Plot links (inner part)
### Add sum values to df1, marking the x-position of the first links
### out (sum1) and in (sum2). Updated for further links in loop below.
df1$sum1 <- colSums(m)
df1$sum2 <- numeric(n)
df1
### Create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
df2 <- cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long",
               timevar="dest", time=rownames(m),  v.names = "m")
df2 <- arrange(df2,desc(m))

### Keep only the largest flows to avoid clutter
df2 <- subset(df2, m > quantile(m,0.6))
df2
### Plot links
for(k in 1:nrow(df2)){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$Logradouro)
  j<-match(df2$dest[k],df1$Logradouro)
  
  #plot link
  circos.link(sector.index1=df1$Logradouro[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$Logradouro[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              col = df1$lcol[i])
  
  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}










#https://www.r-graph-gallery.com/227-add-several-tracks/
#library
library(circlize)

#Create data
data = data.frame(
  factor = sample(letters[1:8], 1000, replace = TRUE),
  x = rnorm(1000), 
  y = runif(1000)
)

#Initialize the plot.
par(mar = c(1, 1, 1, 1) ) 
circos.initialize(factors = data$factor, x = data$x )


# Build the regions of track #1 
circos.trackPlotRegion(factors = data$factor, y=data$y, panel.fun = function(x, y) {
  circos.axis(labels.cex=0.5, labels.font=1, lwd=0.8)
})
# --> Add a scatterplot on it:
circos.trackPoints(data$factor, data$x, data$y, col = rgb(0.1,0.5,0.8,0.3), pch=20)


# Build the regions of track #2:
circos.trackPlotRegion(factors = data$factor, y=data$y, panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
})
# --> Add a scatterplot on it
circos.trackPoints(data$factor, data$x, data$y, col = rgb(0.9,0.5,0.8,0.3), pch=20, cex=2)



# Add the couche #3 --> don't forget you can custom the height of tracks!
circos.par("track.height" = 0.4)
circos.trackPlotRegion(factors = data$factor, y=data$y, panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
})
circos.trackLines(data$factor, data$x, data$y, col = rgb(0.9,0.5,0.1,0.3), pch=20, cex=2, type="h")
# and continue as long as needed!




#https://www.r-graph-gallery.com/315-hide-first-level-in-circle-packing/
# Libraries

#install.packages("ggraph")
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("viridisLite") 
library(viridis)
library(ggraph)
library(igraph)
library(tidyverse)


# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges=flare$edges
vertices = flare$vertices
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Hide the first level (right)
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE") 

# Second one: add 2 first levels
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = "white", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "white", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE")

# Add the data.tree library
#install.packages("data.tree")
library(data.tree)

# Rebuild the data
edges=flare$edges
vertices = flare$vertices

# Transform it in a 'tree' format
tree <- FromDataFrameNetwork(edges)

# Then I can easily get the level of each node, and add it to the initial data frame:
mylevels=data.frame( name=tree$Get('name'), level=tree$Get("level") )
vertices = vertices %>% left_join(., mylevels, by=c("name"="name"))

# Now we can add label for level1 and 2 only for example:
vertices = vertices %>% mutate(new_label=ifelse(level==2, shortName, NA))
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the graph
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  geom_node_label( aes(label=new_label), size=4) +
  theme_void() + 
  theme(legend.position="FALSE", plot.margin = unit(rep(0,4), "cm"))




#######################
#Create data
name=c("Joao Alfredo","Republica","Republica","Joao Alfredo","Lima","Aureliano","Tv Carmo","Joao Alfredo","Lima","L Afonso","Lopo","Lopo","Lima","Republica","Lopo","Joao Alfredo","Joao Alfredo","Republica","Loureiro","Nabuco","Borges","Republica")
#feature=paste("Modal: ", c("carro","carro","moto","moto","moto","moto","moto","Onibus","Onibus","Onibus","Onibus","Onibus","Onibus","Onibus","taxi","taxi","taxi","taxi","pedestre","pedestre","pedestre","pedestre") , sep="")
feature=paste("Modal:  ", c("carro","carro","carro","carro","carro","carro","carro","carro","carro","carro","carro","carro","carro","carro","carro","carro","pedestre","pedestre","pedestre","pedestre","pedestre","pedestre") , sep="")
dat <- data.frame(name,feature)
dat <- with(dat, table(name, feature))
dat
# Charge the circlize library
library(circlize)

# Make the circular plot
#grid.col = c("Modal: carro" = "red", "Modal: pedestre" = "green")
chordDiagram(as.data.frame(dat), transparency = 0.5)





#####################
# library
library(circlize)

#Create data
factors = letters[1:4]
x1 = runif(100)
y1 = runif(100)

# general parameter of the plot. 
# With canvas.xlim and canvas.ylim we kind of "zoom on a part of the plot:
par(mar = c(1, 2, 0.1, 0.1) )
circos.par("track.height" = 0.7, "canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1), "gap.degree" = 0, "clock.wise" = FALSE)

# Make the usual plot, but with no border
circos.initialize(factors = factors, xlim = c(0, 1)) 
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA ) 

# Finally we plot only the firs sector, so let's change its border to "black" to see it
circos.updatePlotRegion(sector.index = "a", bg.border = "grey" , bg.lwd=0.2)

# Now we can add a plot in this section! You can repeat these steps to add several regions
circos.lines(x1, y1, pch = 16, cex = 0.5, type="h" , col=rgb(0.2,0.5,0.1,0.5) , lwd=3)

# Add axis
circos.axis(h="bottom" , labels.cex=0.4, direction = "inside" )

#clear
circos.clear()










######################################
#http://zuguang.de/blog/html/add_legend_to_circlize.html
library(circlize)

set.seed(123)
col_fun = colorRamp2(c(-1, 0, 1), c("green", "yellow", "red"))
circlize_plot = function() {
  circos.initializeWithIdeogram(plotType = NULL)
  
  bed = generateRandomBed(nr = 300)
  bed = generateRandomBed(nr = 300, nc = 2)
  circos.genomicTrackPlotRegion(bed,
                                panel.fun = function(region, value, ...) {
                                  circos.genomicPoints(region, value, cex = 0.5, pch = 16, col = 2:3, ...)
                                })
  
  bed = generateRandomBed(nr = 500, nc = 2)
  circos.genomicTrackPlotRegion(bed,
                                panel.fun = function(region, value, ...) {
                                  circos.genomicLines(region, value, col = 4:5, ...)
                                })
  
  bed1 = generateRandomBed(nr = 100)
  bed1 = bed1[sample(nrow(bed1), 20), ]
  bed2 = generateRandomBed(nr = 100)
  bed2 = bed2[sample(nrow(bed2), 20), ]
  
  circos.genomicLink(bed1, bed2, col = col_fun(bed1[[4]]))
  
  circos.clear()
}
source("http://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
#install.packages("ComplexHeatmap")
library(ComplexHeatmap)
# discrete
lgd_points = Legend(at = c("label1", "label2"), type = "points", legend_gp = gpar(col = 2:3), 
                    title_position = "topleft", title = "Track1")
# discrete
lgd_lines = Legend(at = c("label3", "label4"), type = "lines", legend_gp = gpar(col = 4:5, lwd = 2), 
                   title_position = "topleft", title = "Track2")
# continuous
lgd_links = Legend(at = c(-1, -0.5, 0, 0.5, 1), col_fun = col_fun, title_position = "topleft",
                   title = "Links")

lgd_list_vertical = packLegend(lgd_points, lgd_lines, lgd_links)
lgd_list_vertical



circlize_plot()
pushViewport(viewport(x = unit(2, "mm"), y = unit(4, "mm"), width = grobWidth(lgd_list_vertical), 
                      height = grobHeight(lgd_list_vertical), just = c("left", "bottom")))
grid.draw(lgd_list_vertical)
upViewport()


install.packages("gridBase")
library(gridBase)
plot.new()
circle_size = unit(1, "snpc") - unit(1, "inches")

pushViewport(viewport(x = 0, y = 0.5, width = circle_size, height = circle_size,
                      just = c("left", "center")))
par(omi = gridOMI(), new = TRUE)
circlize_plot()
upViewport()

pushViewport(viewport(x = circle_size, y = 0.5, width = grobWidth(lgd_list_vertical), 
                      height = grobHeight(lgd_list_vertical), just = c("left", "center")))
grid.draw(lgd_list_vertical)
upViewport()













##############################
# library
library(tidyverse)
library(viridis)

# Create dataset
data=data.frame(
  individual=paste( "Logradouro ", seq(1,60), sep=""),
  group=c( rep('Moinhos', 10), rep('Centro', 30), rep('Tristeza', 14), rep('Bordini', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data = data %>% gather(key = "observation", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
nObsType=nlevels(as.factor(data$observation))
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar*nObsType )
data=rbind(data, to_add)
data=data %>% arrange(group, individual)
data$id=rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data= data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# Make the plot
p = ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="red", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
p
