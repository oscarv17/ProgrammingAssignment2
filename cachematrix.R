#ProgrammingAssignment2

## This function will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      setMat <- function(m,k,j){ #Set the matrix
          x <<- matrix(m,k,j)
          i <<- NULL
      }
      getMat <- function() x 
      setInverse <- function(inv) i <<- inv #set the inverse of the matrix
      getInverse <- function() i # get the inverse
      list(setMat = setMat,getMat = getMat,setInverse = setInverse,getInverse = getInverse) #list with functionsS
}


## This function computes the inverse of the special "matrix" from the function
## makeCacheMatrix 

cacheSolve <- function(x, ...) {
     i <- x$getInverse() #get the inversen of the matrix
     if(!is.null(i)){ #checks if is not null
         message("Cache inverse")
         return(i)
     }
     mat <- x$getMat() #get de data 
     i <- solve(mat) #compute the inverse 
     x$setInverse(i) #cache the inverse
     i #returns
}
