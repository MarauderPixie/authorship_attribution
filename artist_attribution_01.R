art_token <- arts %>% 
  group_by(artist) %>% 
  unnest_tokens(lyr_token, lyrics) %>% 
  count(lyr_token, sort = TRUE) %>% 
  mutate(p = n / sum(n)) %>% 
  top_n(300, wt = p) %>% 
  # grouped or ungrouped normalization? 
  # -> ungroup seems to make more sense on first thougt
  ungroup() %>% 
  mutate(
    z_p = (p - mean(p)) / sd(p)
  )


# kmeans is not the right method here:
clusters <- kmeans(art_token$z_p, 5, iter.max = 20, nstart = 5)

art_token$cluster <- clusters$cluster

# mb a little hclustering?
w_labs <- art_token
w_labs <- w_labs %>% 
  select(artist, lyr_token, z_p) %>% 
  spread(lyr_token, z_p)

rownames(w_labs) <- w_labs$artist
w_labs <- select(w_labs, -artist)

mh_dist <- dist(w_labs, method = "manhattan")
hclusters <- hclust(mh_dist)
 
# clusters  <- kmeans(pre_clust$z, 5)
# clustered <- pre_clust
# clustered$cluster <- clusters$cluster


# cosine dist maybe?
w_labs_mat <- as.matrix(w_labs)
w_labs_mat[is.na(w_labs_mat)] <- 0


cos_dist <- 1 - as.dist(
  w_labs_mat %*% t(w_labs_mat) / (sqrt(rowSums(w_labs_mat^2) %*% t(rowSums(w_labs_mat^2))))
)
