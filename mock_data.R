mock_data <- tibble(
  x = c(-2,-1,0,1,2),
  y = c(0,1,2,3,4)
)

x <- c(-2,-1,0,1,2)
y <- c(0,1,2,3,4)

ggplot(mock_data, aes(x, y)) +
  geom_point() +
  geom_line()
