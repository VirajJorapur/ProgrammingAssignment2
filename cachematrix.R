## makeCacheMatrix creats a special matrix which returns a list
## this list is a list of functions which can be used to
## set the matrix value, get the matrix, 
## set the inverse of the matrix, get the inverse of the matrix

## cacheSolve will check whether the inverse is already calculated
## if it is, then it will return the cached value
## else, it will calculate the inverse and store it for further use

## this function will return a list containing all the functions mentioned

makeCacheMatrix <- function(x = matrix()) 
{
  inverse<-NULL
  set<-function(y)
  {
    x<<-y
    inverse<<-NULL
  }
  get<- function()
  {
    x
  }
  setinverse<-function(given_inverse)
  {
    inverse<<-given_inverse
  }
  getinverse<-function()
  {
    inverse
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## this function checks if the inverse is calculated
## if it is not, it'll calculate it and then store it

cacheSolve <- function(x, ...) 
{
  inverse<-x$getinverse()
  if(!is.null(inverse))
  {
    print("Getting Cached data")
    return(inverse)
  }
  invertible_matrix<-x$get()
  inverse<-solve(invertible_matrix)
  x$setinverse(inverse)
  inverse
}
