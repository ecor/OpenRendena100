# TODO: Add comment
# 
# Author: ecor
###############################################################################

library(ggplot2)

gapminderf <- '/home/ecor/Dropbox/R-packages/OpenRendena100/gapminder/gapminder-FiveYearData.csv' 

gapminder <- read.table(gapminderf,header=TRUE,sep=",")

g <- ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country, color=continent)) +
		geom_line()
