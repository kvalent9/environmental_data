1+1
my_vec <- c(1:6)
mat_1 <- matrix(my_vec, nrow = 3)
mat_1[3]
mat_1[3,1]
mat_2 <- matrix(my_vec, nrow = 2, ncol = 3)
mat_3 <- matrix(my_vec, nrow= 3)
mat_4 <- matrix(my_vec, nrow=5, ncol=2)

my_list_1 <- list(5.2, "five point two", c(0:5))
names(my_list_1) <- c("two", "one", "three")
my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"
