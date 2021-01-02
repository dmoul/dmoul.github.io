# string-collapse-function.R

string_collapse <- function(vec, n = 10) {
  # INPUT: Vector of strings
  # OUTPUT: Single string, converted to title case
  # PRECONDITIONS: glue package loaded
  
  if(length(vec) <= n) {
    glue_collapse(str_to_title(sort(vec)), sep = ", ", last = " and ")
  } else {
    len <- length(vec)
    glue("{glue_collapse(str_to_title(head(sort(vec), n=n)), sep = ', ')} and {len - n} more")
  }
}

# test
# vec <- c("hellevoetsluis", "middelburg", "texel", "rotterdam", "amsterdam", "den helder", "vlissingen")
# n <- 4
# string_collapse(vec, 3)

