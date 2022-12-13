require(here)
read.csv(here("data", "ginkgo_data_2022.csv"))

ginkgo_dat <- data.frame(read.csv(here("data", "ginkgo_data_2022.csv")))

plot(x = ginkgo_dat$max_depth, y = ginkgo_dat$max_width, main = "Scatterplot of Max Leaf Width and Depth", xlab = "Depth (mm)", ylab = "Width (mm)", cex = 0.9)
