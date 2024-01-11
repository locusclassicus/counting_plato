load("./data/dtm100.Rdata")
load("./data/dtm200.Rdata")
load("./data/dtm300.Rdata")
load("./data/dtm400.Rdata")
library(ape)
library(philentropy)
library(purrr)
library(dplyr)


colors = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
           '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', 
           '#008080', '#e6beff', '#9a6324', '#800000', '#00fa9a', 
           '#808000', '#fa8072', '#000075', '#808080', '#2f4f4f'
)

# list of dtms
dtms <- c("dtm100", "dtm200", "dtm300", "dtm400")

# cosine distance function
dtm_to_cosine <- function(dtm_name, clus.number) {
  dist_mx <- as.dist(1-philentropy::distance(scale(get(dtm_name)), 
                                             method = "cosine",
                                             use.row.names=T))
  # cluster
  hc <- hclust(dist_mx, method = "ward.D2")
  clus = cutree(hc, clus.number)
  filename = paste0("./images/plato_cos", 
                    gsub("dtm", "", dtm_name), 
                    "_", clus.number, ".png")
  # save plot
  png(filename = filename, width = 41, height = 65, 
      units = "cm", res = 150)
  plot(as.phylo(hc), type = "phylo",
       tip.color = colors[1:clus.number][clus],
       main = paste0(gsub("dtm", "", dtm_name), "MFW Cosine Scaled, ", 
                     clus.number, " clusters"), 
     cex = 1.5)
  dev.off()
  
  # clusters
  tbl = tibble(id = paste0(dtm_name, "_", clus.number), 
               dialogue = names(clus),
               cluster = clus)
}

# iterate 
cosine_clusters10 <- map_df(dtms, dtm_to_cosine, 10)
cosine_clusters15 <- map_df(dtms, dtm_to_cosine, 15)
cosine_clusters20 <- map_df(dtms, dtm_to_cosine, 20)


# join
cosine_clusters <- cosine_clusters10 %>% 
  bind_rows(cosine_clusters15) %>% 
  bind_rows(cosine_clusters20)


save(cosine_clusters, file = "./data/cosine_clusters.Rdata")

