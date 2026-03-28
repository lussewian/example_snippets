library("networkD3")
library(gtools)
library(ggplot2)

crisis_prob_df <- read.csv("data/sankey_data/CONF_transition.csv")



prob_links <- crisis_prob_df %>%
  mutate(source_name = paste0(from_age, "(", from_crisis_cat, ")"),
         target_name = paste0(to_age, "(", to_crisis_cat, ")"),
         rate = (count/from_crisis_count) * 10000) %>%
  filter(!(from_crisis_cat  == 1 & to_crisis_cat == 1))

View(prob_links)

prob_dist <- prob_links %>%
  select(source_name, target_name) %>%
  distinct(source_name, target_name)

prob_dist_nodes <- data.frame(nodes = unique(c(prob_dist$source_name, prob_dist$target_name))) |>
  mutate(idx = row_number() - 1) %>% 
  arrange(nodes)
prob_dist_nodes$nodes <- mixedsort(prob_dist_nodes$nodes)




prob_links <- left_join(prob_links, crisis_dist_nodes, by = c("source_name" = "nodes"))
prob_links <- prob_links %>% rename("source_idx" = idx)
prob_links <- left_join(prob_links, crisis_dist_nodes, by = c("target_name" = "nodes"))
prob_links <- prob_links %>% rename("target_idx" = idx)
prob_links <- dplyr::rename(prob_links, "values" = rate)
prob_links <- dplyr::arrange(prob_links, from_age, from_crisis_cat)

p <- sankeyNetwork(
  Links = prob_links,
  Nodes = prob_dist_nodes,
  Source = "source_idx",
  Target = "target_idx",
  Value = "values",
  NodeID = "nodes",
  sinksRight = TRUE,
  nodeWidth = 12,
  nodePadding = 92,
  fontSize = 12,
  iterations = 0,
  width = 1200,
  height = 800
)

ggsave("sankey_trans_prob.png", p)
