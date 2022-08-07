###################################
## TRI HEX GAME (tentative title)
####

# HEXFLEX
# TRIGON

## CONTENTS
'
 12 orange stones
 12 green stones
 12 blue stones
 37 space hex game board (tentative size)
 opaque drawstring bag
'

## OBJECTIVE
'
 Create groups of three stones of the same color in the orientation 
 unique to that color. Each group of three is worth one point.
'
# any group of 3+ continuous green stones
# any group of 4+ continuous orange stones
# any group of 5+ continuous blue stones

## GAMEPLAY
'
 All stones are placed in the drawstring bag.
 Each player takes turns randomly drawing a stone from the bag and placing it 
 on the hex board.
'

## BASIC MOTION
'
 orange stones push blue stones one space in a linear path.
 blue stones push green stones one space in a linear path.
 green stones push orange stones one space in a linear path.
'

## COMBOS
# If a orange stone pushes a blue stone into a space adjacent to a green stone,
#   the green stone is also pushed one space by the blue stone. If the pushed
#   green stone is adjacent to a orange stone, the orange stone is subsequently
#   pushed one space by the green stone. As so on and so forth. The reaction 
#   will continue until no other stones can be pushed.
#     X X X X X         X X G X X       X X X X X 
#     X B X G X         X B X X X       O X B X G 
#     X X X X X   -->   X X B X X   OR  X X X X X 
#     X X B X X         X X X X X       X X B X X 
#     X X X X X         X X O X X       X X X X X 

## RESTRICTIONS
'
 N/A for now
'

## EDGE EFFECTS
'
 N/A for now
'

## GAME PLAY METRICS
'
 05/30/22: initial observations
    - orange configuration is more likely to occur by random placement, no pushing or replacement
 06/03/2022: change game mechanics
    - no illegal positions
    - no pushing off board
    - no Newton cradle pushing effect
    - objective change to groups of 3+, 4+, and 5+ for green, orange, blue, respectively in any orientation
'
#############################################################################
## GAME BUILDING
##

require(pracma)
require(sp)
require(zoo)


## CREATE BOARD
#
#     O O O O
#    O O O O O
#   O O O O O O
#  O O O O O O O
#   O O O O O O
#    O O O O O
#     O O O O
#
'
 37 spaces, hexagons in the shape of a hexagon
 
'

createBoard <- function(size=13,x0=4,y0=11){
  
  Hexagon <- function(x, y, unitcell = 1){
    ratio <- sqrt(3)/2
    polygon(x = c(x, x+unitcell/2, x+unitcell+(unitcell/2), 
                  x+unitcell*2, x+unitcell+(unitcell/2), x+unitcell/2),
            y = c(y, y+ratio, y+ratio, y, y-ratio, y-ratio))
  }
  
  nrow <- ncol <- size
  ratio <- sqrt(3)/2
  
  # x0 <- 4
  # y0 <- 11
  
  xint <- ratio*sqrt(3)
  yint <- ratio
  
  x_offset <- 3
  x_multiplier <- c(0,1,2,3,2,3,2,3,2,3,2,1,0)
  y_multiplier <- seq(0,12)
  num_in_row <- c(1,2,3,4,3,4,3,4,3,4,3,2,1)
  x_center <- c()
  y_center <- c()
  
  # empty plot window
  plot(0, 0, type = "n", axes = FALSE, xlim=c(0, ncol),
       ylim=c(0, nrow), xlab="", ylab= "", asp=1)
  
  # plot hexes, compute centerpoints
  for(i in seq_along(num_in_row)){
    offset_multiplier <- seq(0,num_in_row[i]-1)
    for(j in seq(num_in_row[i])){
      update_x <- x0-xint*x_multiplier[i]+x_offset*offset_multiplier[j]
      update_y <- y0-yint*y_multiplier[i]
      Hexagon(update_x,update_y)
      
      x_center <- append(x_center, update_x+1, after=length(x_center))
      y_center <- append(y_center, update_y, after=length(y_center))
    }
  }
  
  # list relationships between hexes
  '
    Blue - linear-linear-linear
    Green - three nearest neighbors
    orange - linear-linear-offset
  '
  
  linear_rows <- list(c(7,4,2,1),
                      c(14,11,8,5,3),
                      c(21,18,15,12,9,6),
                      c(28,25,22,19,16,13,10),
                      c(32,29,26,23,20,17),
                      c(35,33,30,27,24),
                      c(37,36,34,31),
                      c(28,32,35,37),
                      c(21,25,29,33,36),
                      c(14,18,22,26,30,34),
                      c(7,11,15,19,23,27,31),
                      c(4,8,12,16,20,24),
                      c(2,5,9,13,17),
                      c(1,3,6,10),
                      c(7,14,21,28),
                      c(4,11,18,25,32),
                      c(2,8,15,22,29,35),
                      c(1,5,12,19,26,33,37),
                      c(3,9,16,23,30,36),
                      c(6,13,20,27,34),
                      c(10,17,24,31))
  
  neighbors <- list(c(3,5,2),
                    c(1,5,8,4),
                    c(6,9,5,1),
                    c(2,8,11,7),
                    c(1,3,9,12,8,2),
                    c(10,13,9,3),
                    c(4,11,14),
                    c(2,5,12,15,11,4),
                    c(3,6,13,16,12,5),
                    c(17,13,6),
                    c(4,8,15,18,14,7),
                    c(5,9,16,19,15,8),
                    c(6,10,17,20,16,9),
                    c(7,11,18,21),
                    c(8,12,19,22,18,11),
                    c(9,13,20,23,19,12),
                    c(24,20,13,10),
                    c(11,15,22,25,21,14),
                    c(12,16,23,26,22,15),
                    c(13,17,24,27,23,16),
                    c(14,18,25,28),
                    c(15,19,26,29,25,18),
                    c(16,20,27,30,26,19),
                    c(31,27,20,17),
                    c(18,22,29,32,28,21),
                    c(19,23,30,33,29,22),
                    c(20,24,31,34,30,23),
                    c(21,25,32),
                    c(22,26,33,35,32,25),
                    c(23,27,34,36,33,26),
                    c(34,27,24),
                    c(28,25,29,35),
                    c(26,30,36,37,35,29),
                    c(36,30,27,31),
                    c(32,29,33,37),
                    c(37,33,30,34),
                    c(35,33,36))
  
  return(list("ID"=seq(1,length(x_center)),
              "X"=x_center,
              "Y"=y_center,
              "VAL"=rep(0,length(x_center)),
              "turn"=0,
              "linear_rows"=linear_rows,
              "neighbors"=neighbors))
}

refreshBoard <- function(size=13,x0=4,y0=11){
  Hexagon <- function(x, y, unitcell = 1){
    ratio <- sqrt(3)/2
    polygon(x = c(x, x+unitcell/2, x+unitcell+(unitcell/2), 
                  x+unitcell*2, x+unitcell+(unitcell/2), x+unitcell/2),
            y = c(y, y+ratio, y+ratio, y, y-ratio, y-ratio))
  }
  
  nrow <- ncol <- size
  ratio <- sqrt(3)/2
  
  # x0 <- 4
  # y0 <- 11
  
  xint <- ratio*sqrt(3)
  yint <- ratio
  
  x_offset <- 3
  x_multiplier <- c(0,1,2,3,2,3,2,3,2,3,2,1,0)
  y_multiplier <- seq(0,12)
  num_in_row <- c(1,2,3,4,3,4,3,4,3,4,3,2,1)
  x_center <- c()
  y_center <- c()
  
  # empty plot window
  plot(0, 0, type = "n", axes = FALSE, xlim=c(0, ncol),
       ylim=c(0, nrow), xlab="", ylab= "", asp=1)
  
  for(i in seq_along(num_in_row)){
    offset_multiplier <- seq(0,num_in_row[i]-1)
    for(j in seq(num_in_row[i])){
      update_x <- x0-xint*x_multiplier[i]+x_offset*offset_multiplier[j]
      update_y <- y0-yint*y_multiplier[i]
      Hexagon(update_x,update_y)
    }
  }
  
}

updateBoard <- function(centers,selection,placeRandom){
  # reset hexes according to 0 value
  x_reset <- centers$X[which(centers$VAL==0)]
  y_reset <- centers$Y[which(centers$VAL==0)]
  points(x_reset,y_reset,col="white",pch=19,cex=4,lwd=2)
  
  # color hexes according to placed stones
  # account for multiple placements (if multiple pushes)
  for(n in seq_along(placeRandom)){
    x_cell <- centers$X[which(centers$ID==placeRandom[n])]
    y_cell <- centers$Y[which(centers$ID==placeRandom[n])]
    points(x_cell,y_cell,col=pastellize(selection$stone[n],p=0.4),pch=19,cex=4,lwd=2)  
  }
  
}

pastellize <- function(x, p){
  
  # x is a colour
  # p is a number in [0,1]
  # p = 1 will give no pastellization
  
  # convert hex or letter names to rgb
  if (is.character(x)) x <- col2rgb(x)/255
  
  # convert vector to rgb
  if (is.numeric(x)) x <- matrix(x, nr=3)
  
  col <- rgb2hsv(x, maxColorValue=1)
  col[2,1] <- col[2,1]*p
  col <- hsv2rgb(col)
  
  # return in convenient format for plots
  rgb(col[1], col[2], col[3])
}

hsv2rgb <- function(x){  
  # convert an hsv colour to rgb  
  # input:  a 3 x 1 matrix (same as output of rgb2hsv() function)  
  # output: vector of length 3 with values in [0,1]    
  
  # recover h, s, v values  
  h <- x[1,1]  
  s <- x[2,1]  
  v <- x[3,1]    
  
  # follow the algorithm from Wikipedia  
  C <- s*v   
  
  # in R, h takes values in [0,1] rather than [0, 360], so dividing by  
  # 60 degrees is the same as multiplying by six  
  hdash <- h*6  
  X <- C * (1 - abs(hdash %% 2 -1))
  
  if (0 <= hdash & hdash <=1) RGB1 <- c(C, X, 0)  
  if (1 <= hdash & hdash <=2) RGB1 <- c(X, C, 0)  
  if (2 <= hdash & hdash <=3) RGB1 <- c(0, C, X)  
  if (3 <= hdash & hdash <=4) RGB1 <- c(0, X, C)  
  if (4 <= hdash & hdash <=5) RGB1 <- c(X, 0, C)  
  if (5 <= hdash & hdash <=6) RGB1 <- c(C, 0, X)    
  
  # the output is a vector of length 3. This is the most convenient  
  # format for using as the col argument in an R plotting function  
  RGB1 + (v-C)
}

## DRAW STONES
initializeBag <- function(num_stones=12){
  bag_init = c(rep("orange",num_stones),rep("blue",num_stones),rep("green",num_stones))
  return(list("available"=bag_init,"memory"=c()))
}

drawFromBag <- function(bag){
  updated_bag <- bag
  
  if(isempty(bag$available)){ stop("Bag is empty") }
  drawn_stone <- sample(bag$available,1)
  updated_bag$memory <- c(bag$memory,drawn_stone)
  updated_bag$stone <- drawn_stone
  
  to_remove <- which(bag$available==drawn_stone)[1]
  updated_bag$available <- bag$available[-to_remove]
  
  return(updated_bag)
}

addToBag <- function(placeStone,bag){
  updated_bag <- bag
  
}


## PLACE STONES
# random for now

availablePositions <- function(centers){
  open <- centers$ID[which(centers$VAL==0)]
  return(open)
}

legalPositions <- function(centers){
  '
  Identify illegal orientations
  Outputs: yes legal, no not legal & locations
  '
  
  # check blue - linear - VAL=2
  check_blue <- c()
  for(i in seq_along(centers$linear_rows)){
    hex_in_row <- centers_new$ID[centers_new$linear_rows[[i]]]
    tmp <- centers$VAL[centers$linear_rows[[i]]]
    triplets <- c()
    if(any(tmp==2)){
      id <- as.integer(tmp==2)
      trip <- rollsum(id,3)
      if(any(trip==3)){
        id_trip <- which(trip==3)
        for(j in seq_along(id_trip)){
          triplets <- append(triplets,list(hex_in_row[seq(id_trip[j],id_trip[j]+2)]),after=length(triplets))
        }
      }
    }
    
    if(!is.null(triplets)){
      check_blue <- append(check_blue,triplets,after=length(check_blue))
    }
  }
  
  # check green - neighbors - VAL=3
  check_green <- c()
  green_on_board <- centers$ID[which(centers$VAL==3)]
  if(any(green_on_board>0)){
    for(i in seq_along(green_on_board)){
      group_green <- c()
      hex_in_group <- centers$neighbors[[green_on_board[i]]]
      id_gr <- as.integer(centers$VAL[hex_in_group]==3)
      if(any(id_gr==1)){
        duo <- rollsum(id_gr,2)
        if(any(duo==2)){
          id_duo <- which(duo==2)
          for(j in seq_along(id_duo)){
            group_green <- append(group_green,list(c(green_on_board[i],hex_in_group[seq(id_duo[j],id_duo[j]+1)])),after=length(group_green))
          }
        }
      }
      
      if(!is.null(group_green)){
        check_green <- append(check_green,group_green,after=length(check_green))
      }
    }
    'remove duplicates'
    len <- length(check_green)
    rm_id <- c()
    for(i in seq(len-1)){
      a <- check_green[[i]]
      for(j in seq(i+1,len)){
        b <- check_green[[j]]
        tmp <- intersect(a,b)
        if(length(tmp)==3){
          rm_id <- append(rm_id,j,after=length(rm_id))
        }
      }
    }
    if(!is.null(rm_id)){
      check_green <- check_green[- unique(rm_id)]
    }
  }
  
  # check orange - linear+offset - VAL=1
  check_orange <- c()
  for(i in seq_along(centers$neighbors)){
    group_orange <- c()
    hex_neighbors <- centers$neighbors[[i]]
    if(length(hex_neighbors)==6){ 'for fully surrounded hexes'
      tmp_hex <- c(hex_neighbors[5:6],hex_neighbors)
      id_yel <- as.integer(centers$VAL[tmp_hex]==1)
      if(any(id_yel==1)){
        yel <- rollsum(id_yel,3)
        if(any(yel==3)){
          yel_id <- which(yel==3)
          for(j in seq_along(yel_id)){
            group_orange <- append(group_orange,list(tmp_hex[seq(yel_id[j],yel_id[j]+2)]),after=length(group_orange))
          }
        }
      }
    } else { 'for all other hexes'
      id_yel <- as.integer(centers$VAL[hex_neighbors]==1)
      if(any(id_yel==1)){
        yel <- rollsum(id_yel,3)
        if(any(yel==3)){
          yel_id <- which(yel==3)
          for(j in seq_along(yel_id)){
            group_orange <- append(group_orange,list(hex_neighbors[seq(yel_id[j],yel_id[j]+2)]),after=length(group_orange))
          }
        }
      }
    }
    
    
    if(!is.null(group_orange)){
      check_orange <- append(check_orange,group_orange,after=length(check_orange))
    }
  }
  
  'determine yes/no'
  if(is.null(check_blue) & is.null(check_green) & is.null(check_orange)){
    outcome <- "LEGAL"
  } else {
    outcome <- "ILLEGAL"
  }
  
  'return values'
  return(list("outcome" = outcome,
              "blue"=check_blue,
              "green"=check_green,
              "orange"=check_orange))
}

selectPosition <- function(available_positions){
  '
   Random for now.
   Consider situation where player must select between two potential wins.
  '
  placeRandom <- sample(available_positions,1)
  return(placeRandom)
}

updatePositions <- function(centers,selection,placement){
  update_centers <- centers
  #update_centers$turn <- centers$turn + 1
  
  switch(selection$stone,
         orange = update_centers$VAL[placement] <- 1,
         blue = update_centers$VAL[placement] <- 2,
         green = update_centers$VAL[placement] <- 3)
  
  return(update_centers)
}

pushStones <- function(centers=centers_new_tmp,selection=bag_update_tmp,placement=placeStone,push=NULL){
  '
  inputs: centers_new_tmp, bag_update_tmp,placeStone
  logic:
    - check neighbors of placed stone
    - evaluate push rule until reaction complete
    - update centers
  '
  # push <- NULL
  updateBoard(centers,selection,placement)
  neighbors <- centers$neighbors[[placement]]
  
  input <- switch(selection$stone,
                  "green" = 1,
                  "orange" = 2,
                  "blue" = 3)
  
  # evaluate push mechanic
  if(any(centers$VAL[neighbors]==input)){
    nearest_id <- which(centers$VAL[neighbors]==input) # which neighbor is reactive
    for(i in seq_along(nearest_id)){
      next_nearest <- centers$neighbors[[neighbors[nearest_id[i]]]] # check its neighbors
      row_group <- c()
      for(j in seq_along(centers$linear_rows)){ # identify which linear row placement and reactive stones belong to
        tmp <- sum(as.integer(c(placement,neighbors[nearest_id[i]]) %in% centers$linear_rows[[j]]))
        if(tmp == 2){ row_group <- centers$linear_rows[[j]] }
      }
      a <- which(row_group==placement)
      b <- which(row_group==neighbors[nearest_id[i]])
      next_nearest_space <- row_group[b+(b-a)] # identify next nearest available space in row
      if(!is.na(next_nearest_space) && centers$VAL[next_nearest_space]==0){ # if empty, record push reaction
        push <- append(push, list(c(neighbors[nearest_id[i]],next_nearest_space)),after=length(push))
      }
    }
  }
  
  # update board following initial push(es)
  if(is.null(push)){ 
    selection$stone <- selection$memory[length(selection$memory)]
    return(list(centers,selection,placement)) } # if no push reaction, return original board
  else{
    update_centers <- centers
    update_selection <- selection
    push_space <- c()
    for(i in seq_along(push)){
      push_space <- c(push_space, push[[i]][2])
      pushed_stone_id <- update_centers$VAL[push[[i]][1]]
      pushed_stone <- switch(as.character(pushed_stone_id),
                             "1" = "orange",
                             "2" = "blue",
                             "3" = "green")
      update_selection$stone[i] <- pushed_stone
      
      update_centers$VAL[push[[i]][1]] <- 0 # resets reactive stone placement
      update_centers$VAL[push[[i]][2]] <- pushed_stone_id # assigns reactive stone new placement
    }
    
    # option 2: update board and run recursion after evaluating all initial pushes
    updateBoard(centers=update_centers,selection=update_selection,placeRandom=push_space)
    pushStones(centers=update_centers,selection=update_selection,placement=push[[i]][2])
  }
}

#############################################
## GAMEPLAY

centers <- createBoard()
bag <- initializeBag()
turn <- 0
#illegal_count <- 0
for(game in seq(36)){
  if(turn==0){
    selection <- drawFromBag(bag)
    open_positions <- availablePositions(centers)
    placeStone <- selectPosition(open_positions)
    updateBoard(centers,selection,placeStone)
    centers_new <- updatePositions(centers,selection,placeStone)
    bag_update <- selection
    print(paste(selection$stone,"stone placed at:",placeStone))
    turn <- turn + 1
  } else {
    bag_update_tmp <- drawFromBag(bag_update)
    open_positions <- availablePositions(centers_new)
    
    placeStone <- selectPosition(open_positions)
    centers_new_tmp <- updatePositions(centers_new,bag_update_tmp,placeStone)
    
    # push stones - recursive function
    print(paste(bag_update_tmp$stone,"stone placed at:",placeStone))
    reaction <- pushStones()
    centers_new <- reaction[[1]]
    bag_update <- reaction[[2]]
    
    # centers_new <- centers_new_tmp
    # bag_update <- bag_update_tmp
    # updateBoard(centers_new,bag_update,placeStone)
    turn <- turn + 1
    
    #illegal_positions <- legalPositions(centers_new_tmp)
    #status <- illegal_positions$outcome
    # if(status == "LEGAL"){
    # --> push stones, update positions, eval win, remove stones, update bag, update board
    # } else {
    # illegal_count <- illegal_count + 1
    # }
  }
}

#print(illegal_count)
# print(bag_update)
# print(centers_new)


#############################################
## OLD CODE

# VERTICES
# x <- seq(0,ncol,3)
# y <- seq(0,nrow,sqrt(3))
# x0 <- x+1.5
# y0 <- y+sqrt(3)/2
# 
# XY <- meshgrid(x,y)
# XY0 <- meshgrid(x0,y0)
# 
# x1 <- x0 - 1
# XY1 <- meshgrid(x1,y0)
# 
# x2 <- x - 1
# XY2 <- meshgrid(x2,y)
# 
# plot(XY$X,XY$Y,xlim=c(-1,ncol+3),ylim=c(-1,nrow+3))
# points(XY0$X,XY0$Y,col='red')
# points(XY1$X,XY1$Y,col='cyan')
# points(XY2$X,XY2$Y,col='green')