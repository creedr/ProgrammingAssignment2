# create a 10x10 matrix
mat <- makeCacheMatrix(matrix(rnorm(100), c(10,10)))
stopifnot(!is.null(mat$get()))
print("get() Works!")

mat$set(matrix(rnorm(10000), c(100,100)))
stopifnot(dim(mat$get()) == c(100,100))
print("set() Works!")

stopifnot(is.null(mat$getinverse()))
print("inverse is correctly null")

# calculating and caching inverse
cacheSolve(mat)
stopifnot(!is.null(mat$getinverse()))
print("cacheSolve seems to work!")
print("If the next print occurs immediately, then caching is working")

for(i in 1:10000) {
    cacheSolve(mat)
}
stopifnot(!is.null(mat$getinverse()))
print("Called cacheSolve() 10,000 times")