library(igraph)
library(ggraph)
library(ggrepel)
library(widyr)
library(stringr)
load("./data/cosine_clusters.Rdata")

# examine clusters
cluster_counts <- cosine_clusters %>% 
  group_by(id) %>% 
  pairwise_count(dialogue, cluster) %>% 
  ungroup()

cluster_counts_total <- cluster_counts %>% 
  group_by(item1, item2) %>% 
  summarise(total = sum(n)) 

# Guthrie 1975 p. 50
early <- c("Apology", "Crito", "Euthyphro", 
           "Laches", "Lysis", "Charmides",
           "HippiasMi", "HippiasMa", "Protagoras",
           "Gorgias", "Ion")
           
middle <- c("Meno", "Phaedo", "Republic",
            "Symposium", "Phaedrus", "Euthydemus", 
            "Menexenus", "Cratylus")
            
late <- c("Parmenides", "Theaetetus", "Sophist", 
          "Statesman","Timaeus", "Critias",
          "Philebus", "Laws")

# методом исключения
dubia <- c("Alcibiades1", "Alcibiades2", "Hipparchus", 
           "Theages", "Cleitophon", "Minos", "Lovers", 
           "Menexenus", "Letters", "Epinomis") 

# Appendix Platonica
spuria <- c("Halcyon", "DeIusto", "DeVirtute",
            "Sisyphus", "Eryxias", "Demodocus2", "Demodocus1",
            "Axiochus")

# an impostor for more fun
xen <- "Xen"

# count coocurrences within clusters
dialogue_labels <- cluster_counts_total %>% 
  mutate(label = str_remove_all(item1, "_.+")) %>% 
  mutate(group = case_when(label %in% early ~ "early",
                           label %in% middle ~ "middle",
                           label %in% late ~ "late",
                           label %in% dubia ~ "dubia",
                           label %in% spuria ~ "spuria",
                           label %in% xen ~ "Xenophon")) %>% 
  select(-label) %>% 
  distinct(item1, group)

# graph
cos_graph <- graph_from_data_frame(cluster_counts_total, 
                                   directed = F)

V(cos_graph)$group <- dialogue_labels$group

ggraph(cos_graph, layout = "stress") + 
  geom_edge_link(aes(edge_alpha = total), color = "grey30") +
  geom_node_point(aes(fill = group), 
                  color = "lightgrey",
                  size = 4,
                  shape = 21) + 
  #geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  geom_label_repel(aes(label = name, x = x, y = y,
                       color = group), 
                   nudge_y = 0.05, 
                   label.size = 1#,
                   #fontface = "bold"
  ) +
  theme_void()



ggsave(filename = "./images/cos_plot.png",
       width = 40, 
       height = 40, 
       units = "cm",
       bg='#ffffff')
