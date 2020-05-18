## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  #Inicializar la variable Inversa
  inversa <- NULL
  
  #Crear metodo asignacion de matriz
  set <- function(matriz) {
    x <<- matriz
    inversa <<- NULL
  }
  
  #Crear el metodo de obtencion de matriz
  get <- function() x
  
  #Asignar la inversa de la matriz
  setinversa <- function(inverse) inversa <<- inverse
  
  #Obtener la inversa de la matriz
  getinversa <- function() inversa
  
  #Retornar el listado de metodos
  list(set = set,
       get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

## Crear la inversa de la matriz a partir de la funcion "makeCacheMatrix"
## Despues de calculada la matriz a travez de la funcion anterior, se realiza el proseso de obtencion y impresion de la matriz inversa 
## por medio de la funcion "cacheSolve"




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #Obtener la inversa de la matriz x que ingresa como parametro
  inversa <- x$getinversa()
  
  #Validacion de la variable inversa
  if (!is.null(inversa)) {
    message("Obtener Datos makeCacheMatrix")
    return(inversa)
  }
  
  #Obtener la matriz
  datos <- x$get()
  
  #Calcular la matriz
  inversa <- solve(datos, ...)
  
  #Asiganar la inversa de la matriz
  x$setinversa(inversa)
  
  #Retornar el valor de la matriz
  inversa
}




