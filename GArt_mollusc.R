# Mollusc
# 
library(mathart)
library(ggforce)
library(Rcpp)
library(tidyverse)
df <- mollusc()
df1 <- df %>% mutate(id = 1)
df2 <- df %>% mutate(id = 2)
df3 <- df %>% mutate(id = 3)

p <- ggplot() +
	geom_point(aes(x, y), df1, size = 0.03, alpha = 0.03) +
	geom_path( aes(x, y), df1, size = 0.03, alpha = 0.03) +
	geom_point(aes(x, z), df2, size = 0.03, alpha = 0.03) +
	geom_path( aes(x, z), df2, size = 0.03, alpha = 0.03) +
	geom_point(aes(y, z), df3, size = 0.03, alpha = 0.03) +
	geom_path( aes(y, z), df3, size = 0.03, alpha = 0.03) +
	facet_wrap(~id, nrow = 2, scales = "free") +
	theme_blankcanvas(margin_cm = 0.5)

ggsave("natalina_cafra01.png", width = 80, height = 80, units = "cm")

