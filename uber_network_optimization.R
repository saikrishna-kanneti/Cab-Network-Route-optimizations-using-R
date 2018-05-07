################################################################################
##
##Sai krishna kanneti
## Uber Network optimization using lpsolve
## April 27 2018
################################################################################


create_passenger = function(id){
  
  initial.position = sample(50, 2, replace = TRUE)
  final.destination = sample(50, 2, replace = TRUE)
  
  return(list('number' = id, 'initial' = initial.position,
              'final' = final.destination))
}

create_car = function(id){
  
  initial.position = sample(50, 2, replace = TRUE)
  
  return(list('number' = id, 'position' = initial.position))
}

distance = function(x,y){
  sum(abs(x-y))
}

distance.matrix = function(cars, passengers){
  
  d.matrix = matrix(0, nrow = length(cars), ncol = length(cars))
  
  for (i in 1:length(cars)){
    for (j in 1:length(passengers)){
      d.matrix[i,j] = distance(cars[[i]]$position, passengers[[j]]$initial)
    }
  }
  return(d.matrix)
}


library(lpSolve)
library(ggplot2)

set.seed(20)

passengers = lapply(seq(1:10), create_passenger)
cars = lapply(seq(1:10), create_car)

d.matrix = distance.matrix(cars, passengers)
opt.allocation = lp.assign(d.matrix)

passengers.points = sapply(passengers, function(x) x$initial)
cars.points = sapply(cars, function(x) x$position)

points = t(cbind(passengers.points, cars.points))
assignments = apply(opt.allocation$solution, 1, which.max) #checking the assignment for each car

df1 = data.frame('x.axis' = points[,1],
                 'y.axis' = points[,2],
                 'id' = c(rep('Passenger',10), rep('Car',10)))

# df.assign = data.frame('x' = cars.points[1,],
#                        'y' = cars.points[2,],
#                        'xend' = passengers.points[1,assignments],
#                        'yend' = passengers.points[2,assignments])

df.assign1 = data.frame('x' = cars.points[1,],
                        'y' = cars.points[2,],
                        'xend' = passengers.points[1,assignments],
                        'yend' = cars.points[2,])

df.assign2 = data.frame('x' = passengers.points[1,assignments],
                        'y' = cars.points[2,],
                        'xend' = passengers.points[1,assignments],
                        'yend' = passengers.points[2,assignments])

x11()
ggplot(df1, aes(x.axis,y.axis)) + geom_point(aes(color = id, group = id), size = 3) + # car and passengers
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = df.assign1) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = df.assign2,
               arrow = arrow(length = unit(0.02, "npc"), type = 'closed')) +
  scale_x_continuous(minor_breaks = seq(1, 50, 1)) +
  scale_y_continuous(minor_breaks = seq(1, 50, 1)) +
  ggtitle('Optimal Allocation')





