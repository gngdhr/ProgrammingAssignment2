## Author: Gangadhar Nittala
## https://class.coursera.org/rprog-030
## R-Programming, Assignment# 2

## makeCacheMatrix - Creates a 'Cached Matrix'
## Is applicable for square matrices only
## @Param mat - matrix that is to be cached. Needs to be square matrix only
## @Return
##    list(setter, getter, set-inverse, get-inverse) : On Success
##    NULL : On failure i.e. when the matrix is not a square matrix
makeCacheMatrix <- function(mat = matrix()) {
  #Matrix has to be a smatre matrix to be able to create an invertible
  if(nrow(mat) != ncol(mat)) {
    message("Inverse can be created for square matrices only. Return NULL.")
    return (NULL)
  }
  #The 'cache' for the inverse of the matrix
  mat_inv <- NULL
  #setter function, that will set the passed vector to x
  set <- function(y) {
    #'Lift' the values to the parent scope
    mat <<- y
    mat_inv <<- NULL
  }
  get <- function() mat
  #This setter will also 'lift' the passed inv to mat_inv 
  # -  since getinv returns the 'cache'
  setinv <- function(inv) mat_inv <<- inv
  getinv <- function() mat_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve - returns the inverse of a non-singular matrix or NULL if not possible
## @Param matx - a 'type' of cacheMatrix i.e. a list created by makeCacheMatrix
## @Return
##    NULL - if the matrix is singular i.e. determinant of the matrix is zero
##    matrix - inverse of the matrix either from the 'cache' or compute it
cacheSolve <- function(matx, ...) {
  #check if the matrix is singular or not, by checking the determinant
  #of the enclosed/wrapped matrix
  matx_det<-det(matx$get())
  if(matx_det == 0) { 
    message("Matrix is singular and hence can't create inverse. Return NULL.") 
    return (NULL)
  }
  
  cached_matx_inv <- matx$getinv()
  if(!is.null(cached_matx_inv)) {
    message("Getting cached inverse of matrix..")
    return(cached_matx_inv)
  }
  #not in cache, so compute the inverse
  mat_internal <- matx$get()
  mat_inv <- solve(mat_internal)
  matx$setinv(mat_inv)
  mat_inv
}


#-- Test Data --
#This matrix is singular and will fail
m0 <- matrix(1:9,nrow=3)
m01 <- makeCacheMatrix(m0)
m011 <- cacheSolve(m01)
m011

#This matrix is non-singular and can create inverse
m1=rbind(c(1,1,2),c(1,2,10),c(12,1,1))
m11 <- makeCacheMatrix(m1)
m111 <- cacheSolve(m11)
m111
m112 <- cacheSolve(m11) #retrieve from cache
m112
print(m111==m112)

#This matrix is non-square and hence we can't create inverse
m5<-matrix(c(1:10),nrow=2)
m51<-makeCacheMatrix(m5)
m51
