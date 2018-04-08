def factorial(a: Int, b: Int): Int = {
  if (a == 0) b else factorial(a - 1, b * a)
}

factorial(5, 1)
