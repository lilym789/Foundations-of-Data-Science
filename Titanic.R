## Exercise 4.1
# Code from my submission for the Data Camp course, "Data Visualization with ggplot2"
# titanic is avaliable in your workspace

# Check out the structure of titanic
str(titanic)

# Use ggplot() for the first instruction
ggplot(titanic, aes(x=factor(Pclass), fill=factor(Sex)))+geom_bar(position="dodge")


# Use ggplot() for the second instruction
ggplot(titanic, aes(x=factor(Pclass), fill=factor(Sex)))+geom_bar(position="dodge") + facet_grid(".~Survived")

# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction

ggplot(titanic, aes(x=factor(Pclass), y=Age, col=factor(Sex)))+geom_jitter(position=posn.j, alpha=0.5, size=3) + facet_grid(".~Survived")
