## This project is intended to generate a pdf of a boxplot indicating the speed of different-generation pokemons----

# install.packages("ggplot2")

# load the data from pokemon.csv
pokemon <- read.csv(file = "data/pokemon.csv", header = TRUE)

# load the library
library(ggplot2)

# create the boxplot
p <- ggplot(pokemon) + geom_boxplot(aes(as.factor(generation), speed, fill = legendary))

# modify the graph title, axis titles and legend title
p <- p + labs(title = " The Speed of Different-Generation Pokemons", 
              fill = "Legendary", x = "Generation", y = "Speed")

# modify the filling color of boxes and legend texts
p <- p + scale_fill_manual(values = c("#5F9EA0", "#E1B378"), labels = c("False", "True"))

# modify the title and text fonts
p <- p + theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), 
               axis.title.x = element_text(size = 10, face = "bold"),
               axis.title.y = element_text(size = 10, face = "bold"),
               axis.text = element_text(size = 8),
               legend.background = element_rect(color = "black"),
               legend.title = element_text(size = 10, face = "bold"),
               legend.text = element_text(size = 8),
               
               # modify the graph background
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_rect(colour="black"),
               panel.border = element_rect(colour = "black", fill="transparent"))

# modify the x axis labels
p <- p + scale_x_discrete(labels=c("generation 1", "generation 2", "generation 3",
                                   "generation 4", "generation 5", "generation 6"))

# modify the y axis data limit
p <- p + scale_y_continuous(breaks = seq(0, 200, 20))

# export a pdf to the file
pdf(file="output/boxplot_qip.pdf",
    onefile = TRUE, width = 11, height = 8, paper = "A4r")
print(p)
dev.off()