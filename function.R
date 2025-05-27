# funkcja liczy parametr y na podstawie zbioru danych x
# x - zbiór danych w postaci wektora liczbowego 
# funkcja sprawdza, czy zmienna jest typu numerycznego 

new_f <- function(x){
  
  if (!is.numeric(x)) { stop("Obiekt wejściowy x nie jest typu numeric") }
  
  srednia <- mean(x)
  
  y <- srednia / 10 + sqrt(srednia)
  
  return(y)
  
}

# test 1 funkcji 

dane <- 1:10

new_f(x = dane)

# test 2 - sprawdzamy, czy odporna

new_f(x = letters)

letters |> str()

ggplot(dane, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue") +
  labs(
    title = "Wykres x od y",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

