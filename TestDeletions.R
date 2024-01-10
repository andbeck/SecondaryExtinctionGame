g <- erdos.renyi.game(n, p.or.m = 0.4, directed = TRUE) %>% 
  set_vertex_attr("name", value = LETTERS[1:n])
par(mfrow = c(1,1))
plot(g, layout = layout_as_tree(g))
which(degree(g, mode = "out")==0)
degree(g, mode = "in")


# Isolated = which(degree(my$g)==0)
# my$g = delete.vertices(my$g, Isolated)
# #my$g = G[-Isolated,]
# 
# NoRes <- which(degree(my$g, mode = "in")==0)
# my$g = delete.vertices(my$g, NoRes)
# #my$g = G[-NoRes,]