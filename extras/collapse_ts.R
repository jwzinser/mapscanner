library(tidyverse)
suppressMessages(library(rlang))
suppressMessages(library(xts))

df <- tibble(
  id = rep(1:10, 10) %>% 
    sort,
  visit = rep(1:10, 10),
  value = rnorm(100)
)

vals_int <- c(1, 2, 3)

tmp <- sapply(vals_int,
              function(val_i) abs(df$value - val_i))


collapse_ts <- function(
  x,
  halflife = NULL
) {
  if(is.null(halflife)) {
    sapply(1:length(x), 
           function(l) sum(x[1:l]/l))
  } else {
    sapply(1:length(x), 
           function(l) sum(x[1:l]*get_w_with_geomTruncDecay(l, halflife)))
  }
}

get_w_with_geomTruncDecay <- function(
  T,
  halflife
) {
  #median_p <- function(p) (1-(1-p)^halflife) / (1-(1-p)^T) - 0.5
  p <- 1-2^(-1/halflife)
  k <- T:1
  w <- p * (1-p)^{k-1} / (1-(1-p)^T)
  w
}

get_label_prom <- function(
  x, 
  ventana_predict
) {
  lag.xts(rollmean(x, 
                   k = ventana_predict, 
                   na.pad = TRUE, 
                   align = 'left'), 
          k = -1)
}

nc <- paste("value", "_decay", sep="")
dm <- df %>% group_by(id) %>%  mutate(!!nc :=  collapse_ts(UQ(parse_quosure("value")), halflife = 6))

collapse_ts(c(1,2,3,4,5,6,7,8,9),3)
