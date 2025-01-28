
####### AHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH



# --- 1.3 A quick R session

5 - 8

5 -

xx <- 1:10

xx

yy <- rnorm(n = 20, mean = 50, sd = 15)
yy
mean(yy)

dat <- rnorm(1000)
hist(dat)

q()

# ============================================================

# --- Exercise 1.4.1

  cat("aaa bbb")
  cat("aaa bbb \n")
  cat("aaa \n bbb \n")
  cat("aaa \nbbb \n")
  cat("aaa \t\t bbb \n")
  cat("aaa\b\b\bbbb \n")
  cat("aaa \n\a bbb \a\n")
  cat("1\a\n"); cat("2\a\n")

  cat("1\a\n"); Sys.sleep(2); cat("2\a\n")

# --- 1.8 A simple function

  sleep
  x <- sleep$extra
  xbar <- mean(x)
  s <- sd(x)
  n <- nrow(sleep)
  t.perc <- qt(0.975,n-1)
  left.boundary <- xbar - (s/sqrt(n))*t.perc
  right.boundary <- xbar + (s/sqrt(n))*t.perc
  cat ("[", left.boundary, ";", right.boundary, "]\n")

###########################################################################

CHAPTER 7: Sections 7.1 & 7.2

# --- pmatch()

  pmatch(c("", "ab", "ab"), c("abc", "ab"), dup = FALSE)
  pmatch(c("", "ab", "ab"), c("abc", "ab"), dup = TRUE)

  seq(from = 1, to = 100, length.out=10)
  seq(f = 1, t = 100, len=10)

  loess(Petal.Width ~ Sepal.Length, data=iris, degree = 2, drop.square = FALSE)
  loess(Petal.Width ~ Sepal.Length, data=iris, degree = 2)
  loess(Petal.Width ~ Sepal.Length, data=iris, d = 2)

# == Function with no arguments

  my.func <- function()
  { print (1+2) 
  }

  my.func()
  my.func(3)

# == Function with a single argument
  my.func <- function(x)
  { print (x+2) 
  }

  my.func()
  my.func(x=5)
  my.func(5)

# == Default arguments

  my.func <- function(x=10)
  { print (x+2) 
  }
 
  my.func()
  my.func(x=5)
  my.func(5)

# == Multiple arguments

  my.func <- function(x=10, y, zz)
  { print (x+y+zz) 
  }

  my.func(x=5, y=10, zz=2)
  my.func(x=5, y=10, z=2)  # partial matching
  my.func(5, 10, 2)
  my.func(y=10, zz=2)
  my.func(10, 2)

  my.func <- function(x=10, y=5, z1=3, z2=4)
  { print (x+y+z1) 
  }

  my.func (z1=8, z2=1)
  my.func (z=8, z2=1)  # duplicate matching

# == missing()

  my.func <- function(x=10, y, zz)
  { output <- missing(y)
    print (output)
    if (missing(y)) print (x+zz)
    else print (x+y+zz) 
  }

  my.func (x=3, y=5, z=2)
  my.func (z=2)

# == stop()

  my.func <- function(x=10, y, zz)
  {
    if (missing(y)) stop ("y must be specified for the computation")
    else print (x+y+zz) 
    cat ("---------------------------\n")
  }

  my.func (x=3, y=5, z=2)
  my.func (z=2)

# == warning()

  my.func <- function(x=10, y, zz)
  {
    if (missing(y)) warning ("y must be specified for the computation")
    else print (x+y+zz) 
    cat ("---------------------------\n")
  }

  my.func (x=3, y=5, z=2)
  my.func (z=2)

# == R function warnings()

  my.func <- function(a=5)
  {  
    for (i in 1:40) if (a==0) warning ("cannot divide by zero")
    else print (100/a) 
  }

  my.func ()
  my.func (a=0)

# == Returning output

  my.func <- function(a=5) a+2
  my.func()

  my.func <- function(a=5)
  {  
    number <- (a+3)^2
    number/a
  }
  
  my.func ()

  my.func <- function(a=5)
  {  
    number <- (a+3)^2
    list (number, number/a)
  }
  
  my.func ()

  my.func <- function(a=5)
  {  
    number <- (a+3)^2
    list (numerator=number, ratio=number/a)
  }
  
  my.func ()

# == return()

  my.func <- function(a=5)
  {  
    number <- (a+3)^2
    if (number < 10) return(number)
    list (numerator=number, ratio=number/a)
  }

  my.func ()
  my.func (a=0.1)

  a <- 5 + 7
  a

  a <- 5 +
       7
  a

  a <- 5
       + 7
  a

###########################################################################

# --- 1.14 Built-in data sets

  data()

# --- 1.15 .First() and .Last()

  .First <- function()
  {
     cat ("Welcome to your session.\n")
  }

