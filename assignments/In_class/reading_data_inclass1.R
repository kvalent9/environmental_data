here(catrate.csv)
here()
read.csv(here("data", "catrate.csv"))
read.csv(here("data", "delomys.csv"))
read.csv(here("data", "rope.csv"))
here("catrate.csv")
dat_catrate <- data.frame(read.csv(here("data", "catrate.csv")))
dat_catrate
hist(dat_catrate$cat.rate, xlab = "Cat Rate", main= "Kaitlyn's Pond Catrate histogram")
create_and_print_vec = function(n, min = 1, max = 10)
{
  vec_3 <- sample(min:max, n, replace= TRUE)
  for(i in 1:n)
  {
    print(paste0("The element at index ", i, " is ", vec_3[[i]]))
  }}
create_and_print_vec(5)
for (i in 1:10)
{
  print(paste0("This is a loop iteration: ", i))
}


