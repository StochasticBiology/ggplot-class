# some illustrations for ggplot2 class, May 2024

# get ggplot2...
library(ggplot2)

# ... and some associated libraries for useful geoms and data wranglinh
library(ggbeeswarm)
library(ggrepel)
library(metR)
library(dplyr)

# scaling factor for output plots
sf = 2

#####
# create a couple of basic predictor/response datasets
# continuous-continuous variables, with groups and (trivial) labels
ngroup = 5
npoint = 10

df.xy = data.frame(x = rep(1:npoint, ngroup))
df.xy$y = df.xy$x + rnorm(npoint*ngroup)
df.xy$group = as.factor(rep(1:ngroup, each=npoint))
df.xy$label = round(df.xy$y)

# factor-continuous variables
npoint.ab = 100
df.ab = data.frame(y = c(rnorm(npoint.ab), rnorm(npoint.ab, mean=1)),
                   group = as.factor(rep(1:2, each=npoint.ab)))

# basic plot commands, upon which other functions (geoms, styles) will be layered
base.plot.xy = ggplot(data=df.xy, aes(x=x, y=y))
base.plot.ab = ggplot(data=df.ab, aes(x=group, y=y, color=group, fill=group)) 

#####
# collection of geoms for continuous-continuous

png("cont-cont.png", width=800*sf, height=600*sf, res=72*sf)
ggarrange(
base.plot.xy + geom_point(aes(color=group)),
base.plot.xy + geom_line(aes(color=group)),
base.plot.xy + geom_smooth(aes(color=group, fill=group), method="lm"),
base.plot.xy + geom_text(aes(color=group, label=group)),
base.plot.xy + geom_point(aes(color=group)) + geom_text_repel(aes(color=group,label=group)),
base.plot.xy + geom_smooth(aes(color=group, fill=group), method="lm") + geom_point(aes(color=group)) + facet_wrap(~ group),
labels=c("point", "line", "smooth", "text", "point+text_repel", "smooth+point [facet]")
)
dev.off()

# collection of geoms for factor-continuous
png("fact-cont.png", width=800*sf, height=600*sf, res=72*sf)
ggarrange(
base.plot.ab + geom_jitter(width=0.25), 
base.plot.ab + geom_beeswarm(),
base.plot.ab + geom_col(),
base.plot.ab + geom_boxplot(),
base.plot.ab + geom_violin(),
labels=c("jitter", "beeswarm", "col", "boxplot", "violin")
)
dev.off()

# pull a bunch of ideas together: different variable types, facetting, arrangement
png("ggplot-examples.png", width=600*sf, height=400*sf, res=72*sf)
ggarrange(
  ggarrange(base.plot.xy + geom_point() + theme_light(),
          base.plot.ab + geom_boxplot(alpha=0.2) + geom_beeswarm(cex=2) +  theme_light(),
          widths=c(2,1),
          labels=c("A", "B")),
          base.plot.xy + geom_smooth(aes(colour=group, fill=group), method="lm", alpha=0.25) + 
            geom_point(aes(color=group)) + facet_wrap(~ group) +theme_light(),
  nrow=2, labels=c("", "C")
)
dev.off()

#### z axis
# scatter data
df.scatter = data.frame(x=rnorm(1000), y=rnorm(1000))
# continuous data
plotsize = 16
df.3d = data.frame(x = rep(0:(plotsize-1), plotsize), y=rep(0:(plotsize-1), each=plotsize))
df.3d$z = (df.3d$x-plotsize/2)**2+(df.3d$y-plotsize/2)**2+rnorm(plotsize**2)

png("cont-cont-z.png", width=800*sf, height=600*sf, res=72*sf)
ggarrange(
ggplot(df.scatter, aes(x=x, y=y)) + geom_hex(),
ggplot(df.scatter, aes(x=x, y=y)) + geom_bin2d(),
ggplot(df.scatter, aes(x=x, y=y)) + geom_density_2d(), 
ggplot(df.scatter, aes(x=x, y=y)) + geom_density_2d_filled(),
ggplot(df.3d, aes(x=x, y=y, fill=z)) + geom_tile(),
ggplot(df.3d, aes(x=x, y=y, z=z)) + geom_contour(),
labels=c("xy: hex", "xy: bin2d", "xy: density_2d", "xy: density_2d_filled", "xyz: tile", "xyz: contour")
)
dev.off()

#### arrangement
g.1 = base.plot.xy + geom_point(aes(color=group))
g.2 = base.plot.ab + geom_beeswarm()
g.3 = ggplot(df.scatter, aes(x=x, y=y)) + geom_hex()
png("arrangement.png", width=600*sf, height=300*sf, res=72*sf)
ggarrange(g.1, ggarrange(g.2, g.3, labels=c("B", "C"), nrow=2), labels=c("A", ""), widths=c(1,2), nrow=1)
dev.off()

#### colours
df.fill = data.frame(x=rep(0:9, each=10), y=rep(0:9, 10))
df.fill$z = (df.fill$x-5)**2+(df.fill$y-5)**2
base.plot.fill = ggplot(df.fill, aes(x=x, y=y, fill=z)) + geom_tile() 

png("ggplot-colours.png", width=700*sf, height=400*sf, res=72*sf)
ggarrange(
  base.plot.fill,
  base.plot.fill + scale_fill_gradient(low="white", high="red"),
  base.plot.fill + scale_fill_gradient2(low="white", mid="blue", high="red", midpoint=25),
  base.plot.fill + scale_fill_gradientn(colors = c("white", "blue", "red"), 
                                        values = c(0, 0.1, 1),
                                        limits = c(0,50)),
  base.plot.fill + scale_fill_viridis(),
  base.plot.fill + scale_fill_viridis(option="inferno"),
  labels=c("default", "gradient", "gradient2", "gradientn", "viridis", "viridis [inferno]")
)
dev.off()

#######
# more involved examples for some other demos

## random walker
# we'll simulate a random walker for some number of timesteps, then plot its actual trajectory and a density map of its location
set.seed(1)
df.rw = data.frame(x=rep(0,1000), y=rep(0,1000))
# simulate random walker
for(i in 2:1000) {
  df.rw$x[i] = df.rw$x[i-1]+rnorm(1)
  df.rw$y[i] = df.rw$y[i-1]+rnorm(1)
}
# create plot -- layering an x,y path over a hex map of density. the rest is just styling
g.rw = ggplot(df.rw, aes(x=x,y=y)) + 
  geom_hex() + 
  geom_path(color="white", alpha=0.25) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey"))

png("ggplot-rw.png", width=400*sf, height=300*sf, res=72*sf)
print(g.rw)
dev.off()

# this is an example pulling quite a few concepts together
ggplot(df.rw, aes(x=x,y=y)) + 
  geom_hex() + 
  geom_path(color="white", alpha=0.5) +
  scale_fill_viridis() +
  theme_dark() 

g.rw.2 = ggplot(df.rw, aes(x=x,y=y)) + 
  geom_hex() + 
  geom_path(color="white", alpha=0.5) +
  scale_fill_viridis() +
  theme_dark() 
  
png("ggplot-rw2.png", width=400*sf, height=200*sf, res=72*sf)
print(g.rw.2)
dev.off()

## contours with labels
# we'll use the 3D data from above and geom_label_contour from metR
g.3d = ggplot() + 
  geom_tile(data=df.3d, aes(x=x, y=y, fill=z)) + 
  geom_contour(data=df.3d, aes(x=x, y=y, z=z), color="white", alpha=0.5) +
  geom_label_contour(data=df.3d, aes(x=x, y=y, z=z)) +
  theme_minimal()
  
png("ggplot-contour.png", width=400*sf, height=300*sf, res=72*sf)
print(g.3d)
dev.off()

## network structure
# we'll simulate a branching network then plot it with "fruit"
# mainly to demonstrate styling data
set.seed(1)
# the data frame will store start and end coordinates for each branch
df.net = data.frame(x=0,y=0,xend=0,yend=1, layer=0)
# for every layer, go through existing branches and add a new branch from the end
for(layer in 1:6) {
  curr.rows = nrow(df.net)
  for(i in 1:curr.rows) {
    df.net = rbind(df.net, data.frame(x=df.net$xend[i], y=df.net$yend[i],
                                      xend=df.net$xend[i]+rnorm(1), yend=df.net$yend[i]+rnorm(1,mean=1),
                                      layer=layer))
  }
}

# create a new data frame for "fruit" -- randomly positioned points on the end of the final layer of branches
d = 0.05
df.fruit = df.net[df.net$layer == max(df.net$layer),]
df.fruit$xend = df.fruit$xend+rnorm(nrow(df.fruit), sd=d/2)
df.fruit$yend = df.fruit$yend+rnorm(nrow(df.fruit), sd=d/2)

# bare-bones plot of the network
ggplot(df.net, aes(x=x, y=y, xend=xend, yend=yend)) + geom_segment()

# stylised plot. first plot drop shadows for branches and fruit. 
# then three layers of branch and three layers of fruit, with different sizes, colours, and offsets, to mimic shading
ns = 0.4
g.net = ggplot() +  
  geom_segment(data=df.net, aes(x=x+d, y=y-d, xend=xend+d, yend=yend-d), linewidth=3*ns, color="#CCCCCC") +
  geom_point(data=df.fruit, aes(x=xend+d, y=yend-d), color="#CCCCCC", size=7*ns) +
  geom_segment(data=df.net, aes(x=x, y=y, xend=xend, yend=yend), linewidth=3*ns, color="black") +
  geom_segment(data=df.net, aes(x=x-d/2, y=y-d/2, xend=xend, yend=yend), linewidth=1.5*ns, color="#222222") +
  geom_segment(data=df.net, aes(x=x-d/2, y=y-d/2, xend=xend, yend=yend), linewidth=0.2*ns, color="#555555") +
  geom_point(data=df.fruit, aes(x=xend, y=yend), color="red", size=7*ns) +
  geom_point(data=df.fruit, aes(x=xend-d/2, y=yend+d/2), color="white", size=4*ns, alpha=0.2) +
  geom_point(data=df.fruit, aes(x=xend-d/2, y=yend+d/2), color="white", size=2*ns, alpha=0.2) +
  theme_void()

png("ggplot-net.png", width=800*sf, height=600*sf, res=72*sf)
print(g.net)
dev.off()

##### plant segregation
# this is not the best way to code this!
# we build up a dataframe of points storing x,y coordinates for samples drawn from different plant structures
# leaves, stems, etc
# each point also has a colour value drawn from a beta distribution, which gets more bimodal as we get higher up the plant
# this describes "segregation" in plant organelles; https://nph.onlinelibrary.wiley.com/toc/14698137/2024/241/2
set.seed(1)

# helper function for a useful transform
ab = function(x) {
  return(0.1/(x+0.1)**2)
}

# generate stem datapoints
layout.df = data.frame()
npoint = 500
df = data.frame(i=1:npoint, x = 0, y=0, col=0)
for(i in 1:npoint) {
  y = runif(10, min=0, max=1)
  df$y[i] = y
  df$col[i] = rbeta(1, shape1=ab(y), shape2=ab(y))
  df$x[i] = rnorm(1, sd=ifelse(y < 0.8, 0.05, sqrt(y-0.8)))
}
# generate leaf datapoints
npoint=400
leaf.df = data.frame(i=1:npoint, x = 0, y=0, col=0)
for(i in 1:npoint) {
  lsign = 1
  if(runif(1) < 0.5) {lsign = -1}
  leaf.df$x[i] = lsign*rnorm(1, mean=0.75, sd=0.4)
  y = rnorm(1, sd=0.02)
  leaf.df$y[i] = y
  leaf.df$col[i] = rbeta(1, shape1=ab(y), shape2=ab(y))
}
npoint=100
leaf2.df = data.frame(i=1:npoint, x = 0, y=0, col=0)
for(i in 1:npoint) {
  lsign = 1
  if(runif(1) < 0.5) {lsign = -1}
  leaf2.df$x[i] = lsign*rnorm(1, mean=0.5, sd=0.2)
  y = 0.05+abs(leaf2.df$x[i]/20)+rnorm(1, sd=0.01)
  leaf2.df$y[i] = y
  leaf2.df$col[i] = rbeta(1, shape1=ab(y), shape2=ab(y))
}
# generate branch datapoints
npoint=100
stem2.df = data.frame(i=1:npoint, x = 0, y=0, col=0)
for(i in 1:npoint) {
  y = runif(1,min=0.4,max=0.8)
  stem2.df$x[i] = 2*y-0.8+rnorm(1, mean=0, sd=ifelse(y < 0.7, 0.05, 0.05+1.5*y-0.7))
  stem2.df$y[i] = y
  stem2.df$col[i] = rbeta(1, shape1=ab(y), shape2=ab(y))
}
npoint=100
stem3.df = data.frame(i=1:npoint, x = 0, y=0, col=0)
for(i in 1:npoint) {
  y = runif(1,min=0.5,max=0.9)
  stem3.df$x[i] = -2*y+1+rnorm(1, mean=0, sd=ifelse(y < 0.8, 0.05, 0.05+1.5*y-0.8))
  stem3.df$y[i] = y
  stem3.df$col[i] = rbeta(1, shape1=ab(y), shape2=ab(y))
}

# for our purposes the plot is then simple -- just shadows and points, with a colour gradient chosen accordingly
g.plant = ggplot(rbind(df, leaf.df, leaf2.df, stem2.df, stem3.df), aes(x=x,y=y,colour=col)) + 
  geom_point(aes(x=x+0.005,y=y-0.002), alpha=0.2, size=1, fill="#FFFFFF", color="#FFFFFF") +
  #geom_point(aes(x=x,y=y-0.01), alpha=0.1, size=1, color="#FFFFFF") +
  geom_point(size=0.5) + 
  scale_colour_gradient(low = "#007711", high = "white") + theme_void() +
  theme(panel.background = element_rect(fill = 'black', color = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

png("ggplot-plant.png", width=400*sf, height=500*sf, res=72*sf)
print(g.plant)
dev.off()

# pull these together for the example slide
png("ggplot-advanced.png", width=700*sf, height=400*sf, res=72*sf)
ggarrange(g.plant, ggarrange( ggarrange(g.3d, g.net, nrow=1, widths=c(1.4,1)), g.rw, nrow=2), nrow=1, widths=c(1,2))
dev.off()

##### illustrations of issues (early in slides)

base.xy = base.plot.xy + geom_point()
base.ab = base.plot.ab + geom_beeswarm(size=0.5, cex=2)

# match sizes of elements!
bad.1 = base.plot.xy + geom_point() +
  theme(axis.title = element_text(size=4), 
        axis.text = element_text(size=6))

# have information occupy most of the plot!
bad.2 = base.plot.xy + geom_point() +
  ylim(-20,10)

# plot individual datapoints!
bad.3 = base.plot.ab + geom_boxplot() 

# explain approximations!
bad.4 = base.plot.xy + geom_smooth() + geom_point() 
bad.5 = base.plot.ab + geom_violin() + geom_point() 

png("bad-1.png", width=400*sf, height=200*sf, res=72*sf)
ggarrange(base.xy, bad.1)
dev.off()

png("bad-2.png", width=400*sf, height=200*sf, res=72*sf)
ggarrange(base.xy, bad.2)
dev.off()

png("bad-3.png", width=400*sf, height=200*sf, res=72*sf)
ggarrange(base.ab, bad.3)
dev.off()

png("bad-4.png", width=400*sf, height=200*sf, res=72*sf)
ggarrange(base.xy, bad.4)
dev.off()

png("bad-5.png", width=400*sf, height=200*sf, res=72*sf)
ggarrange(base.ab, bad.5)
dev.off()

png("bad-6.png", width=600*sf, height=200*sf, res=72*sf)
ggarrange(
  ggplot(data=df.ab, aes(x=y)) + geom_histogram(binwidth=1),
  ggplot(data=df.ab, aes(x=y)) + geom_histogram(binwidth=0.5),
  ggplot(data=df.ab, aes(x=y)) + geom_histogram(binwidth=0.1),
  nrow=1
)
dev.off()

## example misleading bar plots (earliest slides)
# construct data with awkward properties
df.ex = data.frame(g=as.factor(rep(1:3, each=50)), y=c(rnorm(50,mean=10),
                                            rnorm(25,mean=5),rnorm(25,mean=15),
                                            rnorm(30,mean=10),rnorm(20,mean=40,sd=0.1)))

# bar plot
ex.bars = ggbarplot(df.ex, x = "g", y = "y", 
          add = "mean_se", 
          fill = "black",
          error.plot = "errorbar",
          xlab = "Group", ylab = "Value") +
  stat_compare_means( label="p.signif", 
                      label.y = c(25, 25, 30),
                      comparisons=list(c("1","2"), c("2","3"), c("1","3")))

# plot the actual data
ex.data = ggplot(df.ex, aes(x = g, y = y)) +
  geom_beeswarm(size=0.5) +
  labs(x = "Group", y = "Value") +
  theme_light()

# produce those plots
sf = 2
png("ex-1.png", width=500*sf, height=200*sf, res=72*sf)
ggarrange(ex.bars, ggplot() + geom_blank() + theme_void()) 
dev.off()

png("ex-2.png", width=500*sf, height=200*sf, res=72*sf)
ggarrange(ex.bars, ex.data)
dev.off()
