library(tidyverse)
for (i in 1:10) {
  print(i)
}

i <- 1
while (i <= 10) {
  print(1)
  i <- i+1
}
p <- read_csv("/Users/juneburke/Downloads/papers.csv", col_names = TRUE)
c <- read_csv("/Users/juneburke/Downloads/creators.csv", col_names = TRUE)
p <- p |>
  separate_wider_delim(cols = Author,
                       delim = ";",
                       names = c("First Author", "A2", "A3", "A4"),
                       too_few = "align_start", too_many = "drop") |>
  mutate(A2 = str_trim(`A2`, "both"),
         A3 = str_trim(`A3`, "both"),
         A4 = str_trim(`A4`, "both"))
c <- c |>
  distinct()
head(c)
#joins
inner <-inner_join(c, p, by = c("fullName" = "First Author"))
left <- left_join(c,p, by = c("fullName" = "First Author"))
right <- right_join(p,c, by = c("First Author" = "fullName"))
find_pubs <- tibble(fullName = c("Abbott, David H"))
inner2 <- inner_join(find_pubs,p, by = c("fullName" = "First Author"))

install.packages("fuzzyjoin")
library(fuzzyjoin)
find_pubs2 <- tibble(partialName = c("^Abbott"))
inner_fuzzy <- regex_inner_join(p,find_pubs2, by = c("First Author" = "partialName"))
find_pubs3 <- tibble(partialName = c("^Mea", "ony$"))
inner_fuzzy2 <- regex_inner_join(p, find_pubs3, by = c("First Author" =
                                                       "partialName"))
#custom functions
my_print_reps <- function(x, reps = 2){
  for (i in 1:reps) {
    print(x)
  }
  for (i in 1:nrow(x)){ 
    print(x[i, ])
  }
  return(NULL) 
}
my_filter <- function(x, condition, variable){ 
  library(tidyverse)
  x <- x |> filter(rowid %in% condition) #non-functioning function
  return(x) }
df <- data.frame(rowid = 1:5, value = c("a","b", "c", "d", "e"))
my_filter(df, condition = c(3), variable = "rowid")




