#' function to generate random codes for students
#' @export
generate_access_codes <- function(n, length = 4) {
  sapply(1:n, function(i) {
    sample(1:36, length, replace = TRUE) %>% 
      sapply(function(x) if (x>26) x-27 else LETTERS[x]) %>% 
      paste(collapse = "")
  })
}