# #################################################################################################################################

# min_enclosing_box.py
generate_table <- function(){
  # """generate candidates of hull polygon edges and the the other 6 points
  #
  #   Returns:
  #       lines: (24, 2)
  #       points: (24, 6)
  #   """
  skip = list(c(1,3), c(2,4), c(6,8), c(5,7))     # impossible hull edge
  line = list()
  points = list()
  
  all_except_two <- function(o1, o2){
    a = list()
    for (i in 1:8){
      if (i != o1 & i != o2){
        a[[i]] <- i # NOT SURE IF THIS IS CORRECT
      }
    }
    return (a)
  }
  
  for (i in 1:7){
    for (j in (i+1):8){
      
      if (!(list(c(i, j)) %in% skip)){ # DOM DOM DOM !!! CHECK THIS IS CORRECT
        line[[length(line) + 1]] <- c(i, j)
        points[[length(points) + 1]] <- all_except_two(i, j)
        
      }
    }
    # print(line)
    # browser() 
  }
  return(list(line, points))
}

generate_table_out = generate_table()
LINES <- generate_table_out[[1]]
POINTS <- generate_table_out[[2]]

# LINES = np.array(LINES).astype(np.int)
# POINTS = np.array(POINTS).astype(np.int)


#################################################################################################################################
# oriented_iou_loss.py  
enclosing_box <- function(corners1, corners2, enclosing_type ="smallest"){ 
  # browser()
  if(enclosing_type == "aligned"){
    return(enclosing_box_aligned(corners1, corners2))
  }
  
  if(enclosing_type == "pca"){
    return (enclosing_box_pca(corners1, corners2))
  }
  if(enclosing_type == "smallest")
    #browser()
    Enclosed_out <- smallest_bounding_box(torch_cat(list(corners1, corners2), dim=-2))
  return (Enclosed_out)
}

#################################################################################################################################
# min_enclosing_box.py
gather_lines_points <- function(corners){
  # """get hull edge candidates and the rest points using the index
  # 
  #   Args:
  #       corners (torch.Tensor): (..., 8, 2)
  #   
  #   Return: 
  #       lines (torch.Tensor): (..., 24, 2, 2)
  #       points (torch.Tensor): (..., 24, 6, 2)
  #       idx_lines (torch.Tensor): Long (..., 24, 2, 2)
  #       idx_points (torch.Tensor): Long (..., 24, 6, 2)
  #   """
  dim_lngth = length(dim(corners))
  # browser()
  idx_lines = torch_tensor(unlist(LINES))$view(c(length(LINES),2))$to(dtype = torch_long())$unsqueeze(-1)$to(device=device)       # (24, 2, 1)
  idx_points = torch_tensor(unlist(POINTS))$view(c(length(POINTS),6))$to(dtype = torch_long())$unsqueeze(-1)$to(device=device)     # (24, 6, 1)
  idx_lines = idx_lines$'repeat'(c(1,1,2))                                  # (24, 2, 2)
  idx_points = idx_points$'repeat'(c(1,1,2))                                   # (24, 6, 2)
  if (dim_lngth > 2){
    # browser()
    for (i in 1:(dim_lngth-2)){
      idx_lines = torch_unsqueeze(idx_lines, 1)
      idx_points = torch_unsqueeze(idx_points, 1)
    } # FOR LOOP
    #browser()
    idx_points = idx_points$'repeat'(c(corners$size()[1:2], 1, 1, 1))  # DOM DOM DOM * URL -->         # (..., 24, 2, 2) for * see https://stackoverflow.com/questions/5239856/asterisk-in-function-call
    idx_lines = idx_lines$'repeat'(c(corners$size()[1:2], 1, 1, 1))    # DOM DOM DOM *  URL -->          # (..., 24, 6, 2)  
  } # IF LOOP
  
  corners_ext = corners$unsqueeze(-3)$'repeat'( c(rep(1,(dim_lngth-2)), 24, 1, 1)) # DOM DOM DOM * URL -->       # (..., 24, 8, 2)
  # browser()
  lines = torch_gather(corners_ext, dim=-2, index=idx_lines)                  # (..., 24, 2, 2)
  points = torch_gather(corners_ext, dim=-2, index=idx_points)                # (..., 24, 6, 2)
  return (list(lines, points, idx_lines, idx_points))
}


#################################################################################################################################
# min_enclosing_box.py
point_line_distance_range <- function(lines, points){
  # """calculate the maximal distance between the points in the direction perpendicular to the line
  #   methode: point-line-distance
  # 
  #   Args:
  #       lines (torch.Tensor): (..., 24, 2, 2)
  #       points (torch.Tensor): (..., 24, 6, 2)
  #   
  #   Return:
  #       torch.Tensor: (..., 24)
  #   """
  x1 = lines[.., 1, 1]$unsqueeze(-1)  # DOM DOM DOM MAYBE unsqueeze       # (..., 24, 1)
  y1 = lines[.., 1, 2]$unsqueeze(-1)       # (..., 24, 1)
  x2 = lines[.., 2, 1]$unsqueeze(-1)       # (..., 24, 1)
  y2 = lines[.., 2, 2] $unsqueeze(-1)      # (..., 24, 1)
  x = points[.., 1]            # (..., 24, 6)
  y = points[.., 2]            # (..., 24, 6)
  
  den = (y2-y1)*x - (x2-x1)*y + x2*y1 - y2*x1
  # den$register_hook(max_hook1)
  
  # NOTE: the backward pass of torch.sqrt(x) generates NaN if x==0
  
  num = torch_sqrt( (y2-y1)$square() + (x2-x1)$square() + 1e-14 )
  # num$register_hook(max_hook1)
  
  d = den/num         # (..., 24, 6)
  #d$register_hook(max_hook1)
  
  d_max = d$max(dim=-1)[[1]] # torch_max(d, dim =-1)[[1]]       # (..., 24)
  #d_max$register_hook(max_hook1)
  
  d_min = d$min(dim=-1)[[1]]  # torch_min(d, dim=-1)[[1]]      # (..., 24)
  #d_min$register_hook(max_hook1)
  
  d1 = d_max - d_min  
  #d1$register_hook(max_hook1)
  
  # suppose points on different side
  d_abs = d$abs()
  #d_abs$register_hook(max_hook1)
  
  # d2 = torch_max(d_abs, dim=-1)[[1]]      # or, all points are on the same side
  d2 = d_abs$max(dim=-1)[[1]]
  #d2$register_hook(max_hook1)
  
  output = torch_max(d1, other = d2)
  #output$register_hook(max_hook1)
  
  # NOTE: if x1 = x2 and y1 = y2, this will return 0
  return (output)
}


#################################################################################################################################
# min_enclosing_box.py 
point_line_projection_range <- function(lines, points){
  # """calculate the maximal distance between the points in the direction parallel to the line
  #   methode: point-line projection
  # 
  #   Args:
  #       lines (torch.Tensor): (..., 24, 2, 2)
  #       points (torch.Tensor): (..., 24, 6, 2)
  #   
  #   Return:
  #       torch.Tensor: (..., 24)
  #  """
  # browser()
  x1 = lines[.., 1, 1]$unsqueeze(-1)  # DOM DOM DOM MAYBE unsqueeze     # (..., 24, 1)
  y1 = lines[.., 1, 2]$unsqueeze(-1)       # (..., 24, 1)
  x2 = lines[.., 2, 1]$unsqueeze(-1)       # (..., 24, 1)
  y2 = lines[.., 2, 2]$unsqueeze(-1)       # (..., 24, 1)
  k = (y2 - y1)/(x2 - x1 + 1e-8)      # (..., 24, 1)
  vec = torch_cat(c(torch_ones_like(k, dtype=k$dtype), k), dim=-1)  # (..., 24, 2) torch_float()
  vec = vec$unsqueeze(-2)             # (..., 24, 1, 2)  
  points_ext = torch_cat(c(lines, points), dim=-2)         # (..., 24, 8), consider all 8 points
  den = torch_sum(points_ext * vec, dim=-1)               # (..., 24, 8) 
  proj = den / torch_norm(vec, dim=-1, keepdim=FALSE)     # (..., 24, 8)
  
  proj_max = proj$max(dim=-1)[[1]]  #torch_max(proj, dim =-1)[[1]]       # (..., 24)
  #proj_max$register_hook(max_hook2)
  
  proj_min = proj$min(dim=-1)[[1]]  #torch_min(proj, dim =-1)[[1]]      # (..., 24)
  #proj_min$register_hook(max_hook2)
  
  Output <- proj_max - proj_min
  #Output$register_hook(max_hook2)
  
  return (Output) #
}

#################################################################################################################################
# min_enclosing_box.py
smallest_bounding_box <- function(corners, verbose=FALSE){
  # """return width and length of the smallest bouding box which encloses two boxes.
  # 
  #   Args:
  #       lines (torch.Tensor): (..., 24, 2, 2)
  #       verbose (bool, optional): If True, return area and index. Defaults to False.
  # 
  #   Returns:
  #       (torch.Tensor): width (..., 24)
  #       (torch.Tensor): height (..., 24)
  #       (torch.Tensor): area (..., )
  #       (torch.Tensor): index of candiatae (..., )
  #   """
  gather_lines_points_out <- gather_lines_points(corners)
  lines <- gather_lines_points_out[[1]] 
  points <- gather_lines_points_out[[2]]  
  idx_lines <- gather_lines_points_out[[3]]  
  idx_points <- gather_lines_points_out[[4]] 
  # browser()
  proj = point_line_projection_range(lines, points)   # (..., 24)
  dist = point_line_distance_range(lines, points)     # (..., 24)
  
  area = proj * dist
  
  # remove area with 0 when the two points of the line have the same coordinates
  zero_mask = (area == 0)$to(dtype = corners$dtype) #type(corners.dtype)
  
  fake = torch_ones_like(zero_mask, dtype=corners$dtype)* 1e8 * zero_mask
  area_cl <- torch_clone(area)    # IN-PLACE OPERATION FIX
  area_cl <- area_cl$add(fake)       # $add_(fake) add large value to zero_mask
  area_min_out <- torch_min(area_cl, dim=-1, keepdim=TRUE)     # (..., 1)
  area_min <- area_min_out[[1]]$squeeze(-1)$to(dtype = torch_float())
  idx  <- area_min_out[[2]]
  
  w = torch_gather(proj, dim=-1, index=idx)$squeeze(-1)$to(dtype = torch_float())
  h = torch_gather(dist, dim=-1, index=idx)$squeeze(-1)$to(dtype = torch_float())          # (..., 1)
  # w = w$squeeze(-1)$to(dtype = torch_float())
  # h = h$squeeze(-1)$to(dtype = torch_float())
  # area_min = area_min$squeeze(-1)$to(dtype = torch_float())
  if(verbose == TRUE){
    return (list(w, h, area_min, idx$squeeze(-1)))
  }
  else{
    return (list(w, h)) 
  }
}



#################################################################################################################################
# oriented_iou_loss.py  
enclosing_box <- function(corners1, corners2, enclosing_type ="smallest"){ 
  # browser()
  if(enclosing_type == "aligned"){
    return(enclosing_box_aligned(corners1, corners2))
  }
  
  if(enclosing_type == "pca"){
    return (enclosing_box_pca(corners1, corners2))
  }
  if(enclosing_type == "smallest")
    #browser()
    Enclosed_out <- smallest_bounding_box(torch_cat(list(corners1, corners2), dim=-2))
  return (Enclosed_out)
}

#################################################################################################################################
# box_intersection_2d.py
calculate_area <- function(idx_sorted, vertices){
  # """calculate area of intersection
  # 
  #   Args:
  #       idx_sorted (torch.Tensor): (B, N, 9)
  #       vertices (torch.Tensor): (B, N, 24, 2)
  #   
  #   return:
  #       area: (B, N), area of intersection
  #       selected: (B, N, 9, 2), vertices of polygon with zero padding 
  #   """
  
  
  
  idx_ext = idx_sorted$unsqueeze(-1)$'repeat'(c(1,1,1,2))
  
  # ERROR CATCHING
  # print(idx_ext$min())
  # print(idx_ext$max())
  # print(dim(vertices))
  # print(dim(idx_ext))
  # print(min(as.vector(as.array(idx_ext$to(device= "cpu")))))
  # print(max(as.vector(as.array(idx_ext$to(device= "cpu")))))
  #browser()
  if(min(as.vector(as.array(idx_ext$to(device= "cpu")))) == 0){browser()}
  # browser()
  selected = torch_gather(vertices, 3, idx_ext)
  
  
  # t = torch_tensor(matrix(c(1,2,3,4,5,6,7,8), ncol = 2, byrow = TRUE))
  # TT <- torch_tensor(matrix(c(1,1,2,1,2,2,1,1), ncol = 2, byrow=TRUE), dtype = torch_int64())
  # TTT <- torch_gather(t, 2, TT)
  
  
  
  D_s <- dim(selected)
  total = selected[, , 1:(D_s[3]-1), 1]*selected[, , 2:D_s[3], 2] - selected[, , 1:(D_s[3]-1), 2]*selected[, , 2:D_s[3], 1] # head( 1:4, -1)
  
  total = torch_sum(total, dim=3)
  area = torch_abs(total) / 2
  return(list(area, selected))
}

#################################################################################################################################
# box_intersection_2d.py
sort_indices <- function(vertices, mask){
  # """[summary]
  # 
  #   Args:
  #       vertices (torch.Tensor): float (B, N, 24, 2)
  #       mask (torch.Tensor): bool (B, N, 24)
  # 
  #   Returns:
  #       sorted_index: bool (B, N, 9)
  #   
  #   Note:
  #       why 9? the polygon has maximal 8 vertices. +1 to duplicate the first element.
  #       the index should have following structure:
  #           (A, B, C, ... , A, X, X, X) 
  #       and X indicates the index of arbitary elements in the last 16 (intersections not corners) with 
  #       value 0 and mask False. (cause they have zero value and zero gradient)
  #   """
  
  num_valid = torch_sum(mask$to(dtype = torch_int()), dim=3)$to(dtype = torch_int())    # (B, N)
  mean = (torch_sum(vertices * mask$to(dtype = torch_float())$unsqueeze(-1), dim=3, keepdim=TRUE) / num_valid$unsqueeze(-1)$unsqueeze(-1))
  vertices_normalized = (vertices - mean)      # normalization makes sorting easier
  # browser() # DOM DOM DOM !!! YOU NEED TO MAKE EVERYTHING GPU BEFORE YOU CAN APPLY THIS 
  
  sorted_vertices <-contrib_sort_vertices(vertices_normalized, mask, num_valid)$to(dtype = torch_long())
  
  # sorted_vertices <- contrib_sort_vertices(vertices_normalized1, mask1, num_valid1)$to(dtype = torch_long())
  return (sorted_vertices)
}

#################################################################################################################################

build_vertices <- function(corners1, corners2, 
                           c1_in_2, c2_in_1, 
                           inters, mask_inter){
  
  # """find vertices of intersection area
  # 
  #   Args:
  #       corners1 (torch.Tensor): (B, N, 4, 2)
  #       corners2 (torch.Tensor): (B, N, 4, 2)
  #       c1_in_2 (torch.Tensor): Bool, (B, N, 4)
  #       c2_in_1 (torch.Tensor): Bool, (B, N, 4)
  #       inters (torch.Tensor): (B, N, 4, 4, 2)
  #       mask_inter (torch.Tensor): (B, N, 4, 4)
  #   
  #   Returns:
  #       vertices (torch.Tensor): (B, N, 24, 2) vertices of intersection area. only some elements are valid
  #       mask (torch.Tensor): (B, N, 24) indicates valid elements in vertices
  #   """
  # NOTE: inter has elements equals zero and has zeros gradient (masked by multiplying with 0). 
  # can be used as trick
  
  B = corners1$size(1)
  N = corners1$size(2)
  #
  vertices = torch_cat(c(corners1, corners2, inters$view(c(B, N, -1, 2))), dim=3) # (B, N, 4+4+16, 2)
  mask = torch_cat(c(c1_in_2, c2_in_1, mask_inter$view(c(B, N,-1))), dim=3) # Bool (B, N, 4+4+16)
  # browser()
  return (list(vertices, mask))
}

#################################################################################################################################
# box_intersection_2d.py
box1_in_box2 <- function(corners1, corners2){
  # """check if corners of box1 lie in box2
  #   Convention: if a corner is exactly on the edge of the other box, it's also a valid point
  # 
  #   Args:
  #       corners1 (torch.Tensor): (B, N, 4, 2)
  #       corners2 (torch.Tensor): (B, N, 4, 2)
  # 
  #   Returns:
  #       c1_in_2: (B, N, 4) Bool
  #   """
  
  a = corners2[, , 1, ]$unsqueeze(3)  # (B, N, 1, 2)
  b = corners2[, , 2, ]$unsqueeze(3)  # (B, N, 1, 2)
  d = corners2[, , 4, ]$unsqueeze(3)  # (B, N, 1, 2)
  ab = b - a                  # (B, N, 1, 2)
  am = corners1 - a           # (B, N, 4, 2)
  ad = d - a                  # (B, N, 1, 2)
  p_ab = torch_sum(ab * am, dim=-1)       # (B, N, 4)
  norm_ab = torch_sum(ab * ab, dim=-1)    # (B, N, 1)
  p_ad = torch_sum(ad * am, dim=-1)       # (B, N, 4)
  norm_ad = torch_sum(ad * ad, dim=-1)    # (B, N, 1)
  # NOTE: the expression looks ugly but is stable if the two boxes are exactly the same
  # also stable with different scale of bboxes
  # browser()
  cond1 = ((p_ab / norm_ab) > - EPSILON) * ((p_ab / norm_ab) < (1 + EPSILON))   # (B, N, 4)
  cond2 = ((p_ad / norm_ad )> - EPSILON) * ((p_ad / norm_ad) < (1 + EPSILON))   # (B, N, 4)
  
  return (cond1*cond2)
}

#################################################################################################################################
box_in_box_th <- function(corners1, corners2){
  # """check if corners of two boxes lie in each other
  # 
  #   Args:
  #       corners1 (torch.Tensor): (B, N, 4, 2)
  #       corners2 (torch.Tensor): (B, N, 4, 2)
  # 
  #   Returns:
  #       c1_in_2: (B, N, 4) Bool. i-th corner of box1 in box2
  #       c2_in_1: (B, N, 4) Bool. i-th corner of box2 in box1
  #   """
  c1_in_2 = box1_in_box2(corners1, corners2)
  c2_in_1 = box1_in_box2(corners2, corners1)
  return(list(c1_in_2, c2_in_1))
}

#################################################################################################################################
# box_intersection_2d.py
box_intersection_th <- function(corners1, corners2){
  # """find intersection points of rectangles
  #   Convention: if two edges are collinear, there is no intersection point
  # 
  #   Args:
  #       corners1 (torch.Tensor): B, N, 4, 2
  #       corners2 (torch.Tensor): B, N, 4, 2
  # 
  #   Returns:
  #       intersectons (torch.Tensor): B, N, 4, 4, 2
  #       mask (torch.Tensor) : B, N, 4, 4; bool
  #   """
  # build edges from corners
  #browser()
  line1 = torch_cat(c(corners1, corners1[, , c(2, 3, 4, 1),]), dim=4) # B, N, 4, 4: Batch, Box, edge, point
  line2 = torch_cat(c(corners2, corners2[, , c(2, 3, 4, 1),]), dim=4)
  # duplicate data to pair each edges from the boxes
  # (B, N, 4, 4) -> (B, N, 4, 4, 4) : Batch, Box, edge1, edge2, point
  #browser()
  line1_ext = line1$unsqueeze(4)$'repeat'(c(1,1,1,4,1))
  line2_ext = line2$unsqueeze(3)$'repeat'(c(1,1,4,1,1))
  
  x1 = line1_ext[.., 1]
  y1 = line1_ext[.., 2]
  x2 = line1_ext[.., 3]
  y2 = line1_ext[.., 4]
  x3 = line2_ext[.., 1]
  y3 = line2_ext[.., 2]
  x4 = line2_ext[.., 3]
  y4 = line2_ext[.., 4]
  # math: https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
  num = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)     
  den_t = (x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)
  
  # THERE MAY BE A POTENTIAL PROBLEM HERE
  
  t = den_t / num
  t[num == .0] = -1.
  # tt<- torch_where(num == 0.0, t, torch_tensor(-1.0, device=device)$to(dtype = torch_float()))
  #browser()
  mask_t = (t > 0) * (t < 1)                # intersection on line segment 1
  den_u = (x1-x2)*(y1-y3) - (y1-y2)*(x1-x3)
  u = -den_u / num
  #browser()
  u[num == .0] = -1.
  #uu <- torch_where(num == 0.0, u, torch_tensor(-1.0, device=device)$to(dtype = torch_float()))
  mask_u = (u > 0) * (u < 1)                # intersection on line segment 2
  mask = mask_t * mask_u 
  t = den_t / (num + EPSILON)                 # overwrite with EPSILON. otherwise numerically unstable
  intersections = torch_stack(c(x1 + t*(x2-x1), y1 + t*(y2-y1)), dim=-1)
  intersections = intersections * mask$to(dtype = torch_float())$unsqueeze(-1)
  return(list(intersections, mask))
}

#################################################################################################################################
# box_intersection_2d.py
oriented_box_intersection_2d <- function(corners1, corners2){
  # """calculate intersection area of 2d rectangles 
  # 
  #   Args:
  #       corners1 (torch.Tensor): (B, N, 4, 2)
  #       corners2 (torch.Tensor): (B, N, 4, 2)
  # 
  #   Returns:
  #       area: (B, N), area of intersection
  #       selected: (B, N, 9, 2), vertices of polygon with zero padding 
  #   """
  
  box_intersection_th_out <- box_intersection_th(corners1, corners2)
  
  inters <- box_intersection_th_out[[1]]
  mask_inter <- box_intersection_th_out[[2]]
  box_in_box_th_out = box_in_box_th(corners1, corners2)
  c12 <- box_in_box_th_out[[1]]
  c21 <- box_in_box_th_out[[2]]
  
  # vertices, mask = build_vertices(corners1, corners2, c12, c21, inters, mask_inter)
  build_vertices_out <- build_vertices(corners1, corners2, c12, c21, inters, mask_inter)
  vertices <- build_vertices_out[[1]]
  mask <- build_vertices_out[[2]]
  
  sorted_indices = sort_indices(vertices, mask)
  #browser()
  # if(as.array(sorted_indices$min()$to(device="cpu")) == 0){browser()}
  # browser()
  sorted_indices = sorted_indices + 1L # WORK AROUND
  Output <- calculate_area(sorted_indices, vertices)
  
  return(Output)
}

#################################################################################################################################
# oriented_iou_loss.py
box2corners_th <- function(box){
  # """convert box coordinate to corners
  # 
  #   Args:
  #       box (torch.Tensor): (B, N, 5) with x, y, w, l, alpha
  # 
  #   Returns:
  #       torch.Tensor: (B, N, 4, 2) corners
  #   """
  # browser()
  B = box$size(1)
  x = box[, , 1]$unsqueeze(3)
  y = box[, , 2]$unsqueeze(3)
  w = box[, , 3]$unsqueeze(3)
  l = box[, , 4]$unsqueeze(3)
  alpha = box[, , 5]$unsqueeze(3) # (B, N, 1)
  x4 = torch_tensor(c(0.5, -0.5, -0.5, 0.5))$unsqueeze(1)$unsqueeze(1)$to(device = device) # (1,1,4)
  x4 = x4 * w     # (B, N, 4)
  y4 = torch_tensor(c(0.5, 0.5, -0.5, -0.5))$unsqueeze(1)$unsqueeze(1)$to(device = device)# to(box.device) # (1,1,4) 
  y4 = y4 * l     # (B, N, 4)
  corners = torch_stack(list(x4, y4), dim=-1)     # (B, N, 4, 2)
  sin = torch_sin(alpha)
  cos = torch_cos(alpha)
  row1 = torch_cat(list(cos, sin), dim=-1)
  row2 = torch_cat(list(-sin, cos), dim=-1)       # (B, N, 2)
  rot_T = torch_stack(list(row1, row2), dim=-2)   # (B, N, 2, 2)
  
  rotated = torch_bmm(corners$view(c(-1,4,2)), rot_T$view(c(-1,2,2)))
  rotated = rotated$view(c(B,-1,4,2))          # (B*N, 4, 2) -> (B, N, 4, 2)
  
  # THERE MAY BE A POTENTIAL PROBLEM HERE
  
  rotated[.., 1]$add_(x)
  rotated[.., 2]$add_(y)
  
  # rotated[.., 1] <- rotated[.., 1]$add_(x)
  # rotated[,,, 2] <- rotated[,,, 2]$add_(y) 
  
  # rotated_Cl = torch_clone(rotated)
  # rotated_Cl[.., 1] <- rotated_Cl[.., 1]$add(x) #$add_(x)
  # rotated_Cll = torch_clone(rotated_Cl)
  # rotated_Cll[,,, 2] <- rotated_Cll[,,, 2]$add(y) #$add_(x)
  
  # rotated[.., 1] <- rotated[.., 1] + x # $add_(x)
  # rotated[,,, 2] <- rotated[,,, 2] + y #  $add_(y) 
  return (rotated)
}

#################################################################################################################################
# oriented_iou_loss.py  
cal_iou <- function(box1, box2){
  
  # """calculate iou
  # 
  #   Args:
  #       box1 (torch.Tensor): (B, N, 5)
  #       box2 (torch.Tensor): (B, N, 5)
  #   
  #   Returns:
  #       iou (torch.Tensor): (B, N)
  #       corners1 (torch.Tensor): (B, N, 4, 2)
  #       corners1 (torch.Tensor): (B, N, 4, 2)
  #       union (torch.Tensor): (B, N) area1 + area2 - inter_area
  #   """
  
  corners1 = box2corners_th(box1)
  corners2 = box2corners_th(box2)
  
  inter_area = oriented_box_intersection_2d(corners1, corners2)        #(B, N)
  # browser()
  area1 = box1[, , 3] * box1[, , 4]
  area2 = box2[, , 3] * box2[, , 4]
  union = area1 + area2 - inter_area[[1]]
  iou = inter_area[[1]] / union
  return(list(iou, corners1, corners2, union))
}

#################################################################################################################################


cal_iou_3d <- function(box3d1, box3d2, verbose=TRUE){
  # """calculated 3d iou. assume the 3d bounding boxes are only rotated around z axis
  #
  #   Args:
  #       box3d1 (torch.Tensor): (B, N, 3+3+1),  (x,y,z,w,l,h,alpha)
  #       box3d2 (torch.Tensor): (B, N, 3+3+1),  (x,y,z,w,l,h,alpha)
  #   """
  
  # GET 2D Box
  box1 = box3d1[.., c(1,2,4,5,7)]$to(device=device)    # 2d box  x,y,w,l, alpha
  box2 = box3d2[.., c(1,2,4,5,7)]$to(device=device)    # 2d box  x,y,w,l, alpha
  
  # OVERLAP IN THE Z DIRECTION
  zmin1 = box3d1[.., 3]  - box3d1[.., 6] * 0.5
  zmax1 = box3d1[.., 3] + box3d1[.., 6]  * 0.5
  
  zmin2 = box3d2[.., 3] - box3d2[.., 6] * 0.5
  zmax2 = box3d2[.., 3] + box3d2[.., 6] * 0.5
  
  z_overlap = (torch_min(zmax1, other=zmax2) - torch_max(zmin1, other=zmin2))$clamp_min(0.)
  
  # CALCULATES IoU FOR 2D Box, also outputs corners 
  cal_iou_Out <- cal_iou(box1, box2)        # (B, N)
  iou_2d <- cal_iou_Out[[1]]
  corners1 <- cal_iou_Out[[2]]
  corners2 <- cal_iou_Out[[3]]
  union <- cal_iou_Out[[4]]
  intersection_3d = iou_2d * union * z_overlap # NOTE THAT iou_2d = intersection2D/union SO Intersection3D is intersection2D*z_overlap
  v1 = box3d1[.., 4] * box3d1[.., 5] * box3d1[.., 6] # w,l,h
  v2 = box3d2[.., 4] * box3d2[.., 5] * box3d2[.., 6] #  w,l,h
  union3d = v1 + v2 - intersection_3d
  IoU3D <- intersection_3d/union3d
  if(verbose == TRUE){
    z_range = (torch_max(zmax1, other=zmax2) - torch_min(zmin1, other=zmin2))$clamp_min(0.)
    return (list(IoU3D, corners1, corners2, z_range, union3d))
  }else{
    return (IoU3D)
  }
  
}

###############################################################################################################################################################

cal_complete_iou_3d <- function(box3d1, box3d2, enclosing_type)
{
  #  """calculated 3d GIoU loss. assume the 3d bounding boxes are only rotated around z axis
  #
  #   Args:
  #       box3d1 (torch.Tensor): (B, N, 3+3+1),  (x,y,z,w,l,h,alpha)
  #       box3d2 (torch.Tensor): (B, N, 3+3+1),  (x,y,z,w,l,h,alpha)
  #       enclosing_type (str, optional): type of enclosing box. Defaults to "smallest".
  #
  #   Returns:
  #       (torch.Tensor): (B, N) 3d GIoU loss
  #       (torch.Tensor): (B, N) 3d IoU
  # """
  # browser()
  
  # NORMALISING x,y,z,w,l,h
  box3d1_Norm <- torch_clone(box3d1)
  box3d2_Norm <- torch_clone(box3d2)
  
  box3d1_Norm[,,1:6] = torch_sigmoid(box3d1[,,1:6])
  box3d2_Norm[,,1:6] = torch_sigmoid(box3d2[,,1:6])
  
  cal_iou_3d_Out <- cal_iou_3d(box3d1_Norm, box3d2_Norm, verbose=TRUE) # cal_iou for 2d
  iou3d <- cal_iou_3d_Out[[1]]
  S = 1. - iou3d
  corners1 <- cal_iou_3d_Out[[2]]
  corners2 <- cal_iou_3d_Out[[3]]
  z_range <- cal_iou_3d_Out[[4]]  # DOM DOM DOM !!! MAKE SURE THAT THE Z RANGE IS CORRECT ...
  union3d <- cal_iou_3d_Out[[5]]
  
  enclosing_box_out <- enclosing_box(corners1, corners2, enclosing_type)
  w <- enclosing_box_out[[1]] # return width of the smallest bounding box which encloses two 2D boxes.
  l <- enclosing_box_out[[2]] # return length of the smallest bounding box which encloses two 2D boxes.
  
  # # THIS IS ADOPTED FROM CIoU code
  # w <- torch_exp(w)
  # l <- torch_exp(w)
  # z_range <- torch_exp(z_range)
  
  x_offset = box3d1_Norm[..,1] - box3d2_Norm[.., 1]
  y_offset = box3d1_Norm[..,2] - box3d2_Norm[.., 2]
  z_offset = box3d1_Norm[..,3] - box3d2_Norm[.., 3]
  d2 = x_offset*x_offset + y_offset*y_offset + z_offset*z_offset
  c2 = w*w + l*l + z_range*z_range
  D = torch_pow(d2,2)/torch_pow(c2,2) # SQUARING USING ZHENG 2021 formula 
  
  # D_a <- as.array(D_Norm$to(device = "cpu"))
  # browser()
  # DOM DOM DOM !!! D NEEDS TO BE NORMALISED
  
  # RATIO MEASURE
  V = (4 / (pi ** 2)) * torch_pow((torch_atan(w / l) - torch_atan(w / l)), 2)
  
  with_no_grad(
    S_TrueFalse <- (iou3d >= 0.5)$to(dtype= torch_float()) # 
  )
  
  alpha = S_TrueFalse*V/(1-iou3d- V)
  
  # if(iou3d >= 0.5){
  #   S =S$to(dtype= torch_float())
  #   alpha = S*V/(1-iou- V)
  # }else{
  #   alpha = 0
  # }
  
  cIoU = S + D + alpha*V # (v_c - union3d)/v_c
  return (list(cIoU, iou3d))
}