class(4)
class(4.0)
n= 125
vec_1 = sample(12, n, replace = TRUE)
vec_1
head(vec_1)
vec_2 <- vec_1 == 3
vec_1[vec_2]
length(vec_1)
sum(vec_1 == 3)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
n = 10
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
for (i in 1:10)
{
  print(i)
}
for (i in 1:10)
{
  print(paste0("This is a loop iteration: ", i))
}
n = 5
for (i in 1:10)
{
  print(n)
}
n = 5
for(i in 1:n)
{
  print(paste0("This is a loop iteration: ", i))
}
n= 17
vec_1 <- sample(10, n, replace = TRUE)  
for(i in 1:n)
{
  print(paste0("The element of vec_1 at index ", i, " is: ", vec_1[[i]]))
  }
create_and_print_vec = function(n, min = 1, max = 10)
{
vec_3 <- sample(min:max, n, replace= TRUE)
  for(i in 1:n)
{
  print(paste0("The element at index ", i, " is ", vec_3[[i]]))
  }}
create_and_print_vec(5)

