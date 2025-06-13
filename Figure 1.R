#May 28, 2025
#R code for drawing Figure 1
#written by Elisabeth F. Heuston

# read data
data <- read.table(file = "data for Figure 1.txt", header = T, sep = "\t", na.strings = "NA")
data <- data[,c(1:3, 4, 6, 5)] #reformat for preferred order

scale.value <- 1e6
df_long <- reshape(data, 
                   varying = c("Adipose", "Muscle", "Blood"), 
                   v.names = "Expression", 
                   timevar = "Tissue", 
                   times = c("Adipose", "Muscle", "Blood"), 
                   direction = "long")

# note that each "value" starts at gene start site
df_long$Pos <- df_long$Pos/scale.value
rownames(df_long) <- NULL
df_long$Tissue <- factor(df_long$Tissue, levels = c("Adipose", "Muscle", "Blood"))
df_long$id <- NULL

# Categorize expression values
# CS = completely silenced
# MS = mostly silenced
# VE = variably escaped
# ME = mostly escaped
# CE = completely escaped

df_long$Expression_Category <- cut(df_long$Expression, 
                                   breaks = c(-Inf, 0, 0.25, 0.5, 0.75, Inf), 
                                   labels = c("CS", "MS", "VE", "ME", "CE"), 
                                   right = TRUE)

# Define colors
color_map <- c("CS" = "black", "MS" = "blue", "VE" = "yellow", "ME" = "orange", "CE" = "red")

library(ggplot2)  # Needed for visualization


p <- ggplot(df_long, aes(x = Pos, y = Tissue, fill = Expression_Category)) +
  geom_tile(width = 1000000/scale.value) +
  scale_fill_manual(values = color_map, na.value = "gray") +
  labs(fill = "Expression Level") +
  scale_x_reverse(breaks = seq(0, 156040895/scale.value, by = 10000000/scale.value), labels = scales::comma) + 
   theme_minimal() + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_line(linewidth = .1, color="white")) + 
  theme(axis.text.x=element_text(size=16),axis.title.x=element_text(size=20),axis.text.y=element_text(size=16),axis.title.y=element_text(size=20)) + 
  xlab("Pos (Mb)") +
  coord_flip()

png(filename = "Figure 1.png", width = 400, height = 2000, bg = "white")
plot(p)
dev.off()
