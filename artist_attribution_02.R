arts <- arts %>% 
  group_by(artist) %>% 
  sample_n(100) %>% 
  ungroup()

burdel <- arts %>% 
  group_by(title) %>% 
  unnest_tokens(words, lyrics) %>% 
  count(words, sort = TRUE) %>% 
  mutate(p = n / sum(n)) %>% 
  top_n(300, wt = p) %>% 
  ungroup() %>%
  group_by(words) %>% 
  mutate(
    z = (p - mean(p)) / sd(p)
  ) %>% 
  ungroup()


w_labs <- burdel %>% 
  select(title, words, z) %>% 
  spread(words, z)


rownames(w_labs) <- w_labs$title
w_labs <- select(w_labs, -title)

w_labs_mat <- as.matrix(w_labs)
w_labs_mat[is.na(w_labs_mat)] <- 0

cos_dist <- 1 - as.dist(
  w_labs_mat %*% t(w_labs_mat) / (sqrt(rowSums(w_labs_mat^2) %*% t(rowSums(w_labs_mat^2))))
)

hc_arts <- hclust(cos_dist)


library(dendextend)
library(colorspace)

hc_dend <- as.dendrogram(hc_arts) %>% 
  rotate(1:340) %>% 
  color_branches(17)

labels_colors(hc_dend) <- rainbow_hcl(17)[sort_levels_values(
  as.numeric(
    factor(arts$artist)
  )[order.dendrogram(hc_dend)]
)]

labels(hc_dend) <- arts$artist[order.dendrogram(hc_dend)]


png("dendo.png", width = 600, height = 3000)

hc_dend %>% 
  hang.dendrogram(hang_height = .1) %>% 
  plot(horiz = TRUE)

dev.off()
