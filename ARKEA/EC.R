library(scales)
library(formatR)

library(readxl)
library(readr)

library(tidyverse)
library(broom)
library(broomExtra)
library(janitor)
library(lubridate)
library(tidylog)
library(eeptools)
library(Rclean)

library(plotly)
library(ggsci)
library(ggThemeAssist)
library(ggthemes)
library(gghighlight)
library(ggthemr)
library(mdthemes) # imagemagick-x11 and gtk+
library(patchwork)
library(ggpubr)
library(ggridges)
library(ggExtra)
library(ggparallel)
library(alluvial)
library(GGally)
library(cdparcoord)
library(RColorBrewer)
library(colorspace)
library(wesanderson)
library(data.table)
library(xlsx) # problemas JDK
library(cowplot)
library(viridis)
library(coronavirus)
library(incidence)
library(earlyR) # libsodium
library(EpiEstim) 
library(distcrete)
library(epitrix)
library(projections)


library(sp)# varios
library(spdep) # gdal  udunits
library(sf)
library(tmap)#fastmap
library(tmaptools)
library(raster)
library(spData)
library(spDataLarge) # v8
library(ggsn)
library(hrbrthemes)
library(gganimate)
library(anytime)
library(reshape2)
library(ggforce)
library(ggfortify)
library(ggedit)

library(geobr)
library(ggmap)
library(maps)
library(mapdata)
library(gridExtra)
library(grid)
library(ggspatial)
library(ggrepel)
library(googleway)
library(leaflet) # for interactive maps
library(mapview) # for interactive maps # tidyverse data visualization package
library(shiny)
library(rgdal)# for web applications
library(cartography)
library(magrittr)
library(pracma) #media movel 
library(zoo) #media movel
library(rgeos)
library(spgwr)
library(lctools) # cluster hots spot, moran 
library(geogrid) 

autor <- read_delim("nextstrain_ncov_global_authors.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

metadata <- read_delim("nextstrain_ncov_global_metadata.tsv",  "\t", escape_double = FALSE, trim_ws = TRUE)

entropy <- read_delim("nextstrain_ncov_global_diversity.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)


library(ggstream)
library(streamgraph)

t <- metadata %>% 
  filter(Country == "Brazil") %>% 
  group_by(`Collection Data`, `PANGO Lineage`) %>% # Admin Division
  count(`PANGO Lineage`) %>% 
  ungroup() %>% 
  rename(data = `Collection Data`, total = n, Linage = `PANGO Lineage`) %>% 
  print()
  

ti <- t  %>% 
  ggplot(aes(x = data, y  = total, fill = Linage, text = Linage)) + 
  geom_stream(type = "proportional", bw =  0.75, extra_span = 0.1, color = "white", alpha = 0.9, size = 0.5) +
  scale_fill_brewer(palette = "RdYlBu") + #"Spectral"
  scale_fill_viridis(discrete = TRUE, option = "B") +
  scale_y_percent() +
  labs(x = "Data", y = "Frequência", fill = "Linhagem",
       title = "Frequência de Linhagem (PANGO) no Brasil",
       subtitle = paste("ARKEA das Archaeas:", today()),
       caption = "https://nextstrain.org/ncov/global?c=location&lang=es") +
  theme_modern_rc(base_size = 12, axis_title_size = 14, ticks = TRUE)
  


ti

ggplotly(ti, tooltip = "text")


t <- metadata %>% 
  filter(Country == "Brazil") %>% 
  group_by(`Collection Data`, `PANGO Lineage`) %>% # Admin Division
  count(`PANGO Lineage`) %>% 
  ungroup() %>% 
  rename(data = `Collection Data`, total = n, Linage = `PANGO Lineage`) %>% 
  print()


# Compute distances and hierarchical clustering
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")





t1 <- metadata %>% 
  filter(Country == "Brazil") %>% 
  group_by(`PANGO Lineage`) %>% # Admin Division
  count(`PANGO Lineage`) %>% 
  ungroup() %>% 
  rename(total = n, Linage = `PANGO Lineage`) %>% 
  column_to_rownames(var = "Linage") %>% 
  print()

library(factoextra)


dd <- dist(scale(t1), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

fviz_dend(hc, cex = 0.5)

fviz_dend(hc, cex = 0.5, 
          main = "Dendrogram - ward.D2",
          xlab = "Objects", ylab = "Distance", sub = "")

fviz_dend(hc, cex = 0.6, horiz = TRUE)


fviz_dend(hc, k = 5,                 # Cut in four groups
          cex = 0.5,                 # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_classic2(),     # Change theme
          horiz = TRUE)





t1 <- metadata %>% 
  filter(Country == "Brazil") %>% 
  group_by(`PANGO Lineage`, `Admin Division`) %>% # Admin Division
  count(`PANGO Lineage`, `Admin Division`) %>% 
  ungroup() %>% 
  rename(total = n, Linage = `PANGO Lineage`, Estado = `Admin Division`) %>% 
  mutate(Estado = ifelse(Estado == "Amazonas BR", "Amazonas", Estado)) %>% 
  spread(Estado, total, fill = 0) %>% 
  column_to_rownames(var = "Linage") %>% 
  print()

library("heatmaply")


mat <- t1
mat[] <- paste("Esta célula é: Linhagem (Pongo) ", rownames(mat))
mat[] <- lapply(colnames(mat), function(colname) {
  paste0(mat[, colname], ", ", colname)
})


heatmaply(
  t1,
  seriate = "mean",
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
    low = "#98F5FF", 
    high = "#FF4500", 
    midpoint = 6, 
    limits = c(0, 13)),
  cellnote = t1,
  custom_hovertext = mat,
  k_col = 4,
  k_row = 4,
  xlab = "Estados da Federação",
  ylab = "Linhagem (Pongo)", 
  main = "Número de linhagens por estados da Federação",
)



est <- st_read("Centroides_Estados.shp", quiet = TRUE) #Estado BR
est <- st_as_sf(est) 

Centroide <- st_centroid(est, of_largest_polygon = FALSE)

Centroide <- as_tibble(Centroide)
