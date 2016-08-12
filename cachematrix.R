## makeCacheMatrix creates a special matrix object that can return its inverse
## pass a predefined square matrix eitehr as an argument of the function or
## as an argument of the $set() nexted function

#### Usage:
#### var<-makeCacheMatrix(#matrix_object)
#### OR
#### var<-makeCacheMatrix(matrix(#data,nrow,ncol))

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set=function(y){
    x<<-y
    inv<<-NULL
  }
  get=function()x
  setInv=function(inverse) inv<<-inverse
  getInv=function()inv
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## cacheSolve returns either:
## 1.) the cached inverted matrix, if it exists
## 2.) a new inverted matrix if no cache exists
## If returning new inverted matrix, it will cache the results

#### Usage:
#### cacheSolve(var) 
#### "var" references the object set for makeCacheMatrix function

cacheSolve <- function(x, ...) {
  mat=x$getInv()
  if(!is.null(mat)){
    message("getting inverted matrix from cache")
    return(mat)
  }
  data=x$get()
  inv=solve(data,...)
  x$setInv(inv)
  return(inv)
}
