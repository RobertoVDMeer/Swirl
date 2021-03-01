add2 <- function(x, y) {
  x + y 
}

above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

# Lexical Scoping
"
cube <- make.power(3)
square <- make.power(2)
cube(3)
square(3)
Check the free vars
ls(environment(cube))
get('n', environment(cube))
"
make.power <- function(n) {
  pow <- function(x) {
      x^n
  }
  pow
}