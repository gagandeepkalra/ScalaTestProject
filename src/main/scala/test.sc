val arr = new Array[Int](10)

arr(0) = 1
arr(1) = 5
(2 to 9).foreach(i => {
  arr(i) = 2 * arr(i - 1) - arr(i - 2) + 3
})

arr