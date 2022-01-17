# system1_func.R

genre.margin = as.data.frame(read.csv('margin.csv', header = T))

system1 = function(sel_genre, n = 10){
  return(genre.margin[1:n, sel_genre])
}
