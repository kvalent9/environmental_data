my_vec = rep(1:3, 5)
my_vec
my_bool_vec <- my_vec == 3
my_bool_vec
data.frame(my_vec, my_bool_vec)
my_bool_vec[3]
my_bool_vec[my_vec[3]]
my_bool_vec["TRUE"]
subset(my_bool_vec, "TRUE")
my_bool_vec[which(my_vec==TRUE)]
my_vec[my_bool_vec]
my_list_1<- list(5.2 <- "two", "five point two" <- "one", 0:5 <- "three")
my_list_1[[3]]
my_list_1$"one"
names(5.2 <- "two")
names(my_list_1) <- c("two", "one", "three")
my_list_1$"one"
