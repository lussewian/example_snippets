#' Title: Co occurrence matrix
#' Author: Wian Lusse
#' 
#' Creates a co occurrence matrix from a table where all variables are binary

# Function to check all columns are binary
is_binary <- function(cols, tbl){
  sapply(cols, function(col){
    x <- tbl[[col]]
    all(x[!is.na(x)] %in% c(0,1))
  })
}

create_co_occur <- function(tbl, cols){
  stopifnot(all(is_binary(cols, tbl)))
  stopifnot(length(tbl) > 0)
  stopifnot(cols %in% names(tbl))
  
  co_occur <- matrix(NA, nrow = length(cols), ncol = length(cols),
                     dimnames = list(cols, cols))
  
  for(col_x in cols) {
    for(col_y in cols) {
      rows_x <- tbl[[col_x]] == 1
      
      if(sum(rows_x) == 0){
        co_occur[col_x, col_y] <- NA
      } else {
        co_occur[col_x, col_y] <- sum(tbl[[col_y]][rows_x] == 1)/sum(rows_x) 
      }
    }
  }
  
  return(co_occur)
}

#' Input
#' tbl = name of data frame
#' cols = list of column names
coo <- create_co_occur(tbl, c_cols)

coo_long <- as.data.frame(coo) %>%
  rownames_to_column("rows") %>%
  tidyr::pivot_longer(
    cols = -rows,
    names_to = "col",
    values_to = "value"
  )
 
View(coo)
write.csv(coo, file.path(output_folder, out_file))







