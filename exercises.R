# set of exercises for ggplot2 class, May 2024

#####
# first example: simple scatter plot
library(ggplot2)
my.df = data.frame(x=1:10, y=1:10+rnorm(10))
ggplot(my.df, aes(x=x, y=y)) + geom_point() + theme_light()

#####
# visual exploration of mtcars data, particularly:
# mpg (efficiency); disp (displacement); cyl (number of cylinders)

# get the data
data(mtcars)

# simple scatter of disp ~ mpg
ggplot(mtcars, aes(x=mpg, y=disp)) + geom_point()
# colour by cyl...
ggplot(mtcars, aes(x=mpg, y=disp, color=cyl)) + geom_point()
# ... but that assumed cyl was continuous, let's make it a factor
ggplot(mtcars, aes(x=mpg, y=disp, color=factor(cyl))) + geom_point()

# produce plots for the slides
png("exercise-cars.png", width=500*sf, height=200*sf, res=72*sf)
ggarrange(ggplot(mtcars, aes(x=mpg, y=disp)) + geom_point(),
          ggplot(mtcars, aes(x=mpg, y=disp, color=factor(cyl))) + geom_point(),
          widths=c(1,1.3)
)
dev.off()

# style the plot for the theme section of the slides
png("exercise-cars2.png", width=250*sf, height=200*sf, res=72*sf)
ggplot(mtcars, aes(x=mpg, y=disp, color=factor(cyl))) + 
  geom_point(size=3) + 
  labs(x="Effiency / mpg", y="Displacement / cu.in.", color="Cylinders") +
  theme_light() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
dev.off()

#####
# visual exploration of plant growth data

# get the data
data(PlantGrowth)
# plot boxplot + point layers
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + geom_point() 

png("exercise-growth.png", width=250*sf, height=200*sf, res=72*sf)
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + geom_point() 
dev.off()

#####
# another example: plant physiology
data(iris)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + 
  geom_point() + 
  geom_smooth(method="lm")
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point() + 
  geom_smooth(method="lm")

ggarrange(
  ggplot(iris, aes(x=Petal.Length, y=Petal.Width, fill=Species, color=Species)) + 
    geom_point() + 
    geom_smooth(method="lm") + 
    theme_light() +
    labs(x = "Petal length", y = "Petal width"),
  ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, fill=Species, color=Species)) + 
    geom_point() + 
    geom_smooth(method="lm") + 
    theme_light() +
    labs(x = "Sepal length", y = "Sepal width")
)
