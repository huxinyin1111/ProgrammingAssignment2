# 创建一个特殊的 "matrix" 对象，可以缓存其逆矩阵
makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

# 计算特殊 "matrix" 的平均值，使用上述函数创建
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(is.null(m)) {
    message("getting cached data")
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
  }
  return(m)
}

# 创建一个特殊的 "matrix" 对象，可以缓存其逆矩阵
makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# 计算特殊 "matrix" 的逆矩阵
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(is.null(m)) {
    message("Calculating inverse...")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
  }
  return(m)
}