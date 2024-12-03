pacman::p_load(
  # For Gower Distance
  StatMatch,
  # For Mixed Integer Programme
  ompr, ompr.roi, 
  ROI.plugin.gurobi,
  # For Data Manipulation
  tidyverse
  )

#' get_dist: Get Distance Matrix
#' @param attributes Number of attributes.
#' @param items Number of items.
#' @param seed Random seed.
#' @return A `matrix` object with distances between `items`.
#' @export
get_dist <- function(attributes, items, seed=1) {
  A <- attributes
  N <- items
  set.seed(seed)
  x <- replicate(n=A, expr=runif(N))
  d <- gower.dist(x)
  return(d)
}

#' get_sizes: Get Minimum and Maximum Group Sizes for Each Group
#' @param groups Number of groups.
#' @param items Number of items.
#' @param equal Logical for whether the minimum and maximum group sizes are equal across all groups.
#' @return List with components
#' `a` Vector of minimum sizes for each group. If `equal`, then all elements are equal.
#' `b` Vector of maximum sizes for each group. If `equal`, then all elements are equal.
#' @export
get_sizes <- function(groups, items, equal=TRUE) {
  A <- attributes
  G <- groups
  N <- items
  if (equal) {
    n_g <- rep(N/G, G)
    a <- rep(max(1,floor(n_g[1])-sample(0:ceiling(0.05*n_g[1]), 1)), G)
    b <- rep(max(1,ceiling(n_g[1])+sample(0:ceiling(0.05*n_g[1]), 1)), G)
  } else {
    n_g <- c(0,sample(x=1:(N-G), size=(G-1), replace=TRUE))
    n_g <- sort(n_g)
    n_g <- diff(n_g)
    n_g <- n_g+1
    n_g <- c(n_g, N-sum(n_g))
    a <- sapply(X=n_g, FUN=function(x){max(1,floor(x)-sample(0:ceiling(0.05*x), 1))})
    b <- sapply(X=n_g, FUN=function(x){max(1,ceiling(x)+sample(0:ceiling(0.05*x), 1))})
  }
  output <- list(a=a, b=b)
  return(output)
}

#' mdgp_standard: Build Standard Model for the Maximally Diverse Grouping Problem
#' @param dist Distance matrix.
#' @param groups Number of groups.
#' @param items Number of items.
#' @param max Maximum group sizes for each group.
#' @param min Minimum group sizes for each group.
#' @return MIP Model
#' @export
mdgp_standard <- function(dist, groups, items, max, min) {
  G <- groups
  N <- items
  a <- min
  b <- max
  d <- dist
  model <- MIPModel() %>%
    add_variable(x[i,g], i=1:N, g=1:G, type="binary") %>%
    add_variable(z[i,j,g], i=1:(N-1), j=(i+1):N, g=1:G, type="binary") %>%
    set_objective(sum_over(d[i,j]*z[i,j,g], i=1:(N-1), j=(i+1):N, g=1:G), "max") %>%
    add_constraint((z[i,j,g]<=x[i,g]), i=1:(N-1), j=(i+1):N, g=1:G) %>%
    add_constraint((z[i,j,g]<=x[j,g]), i=1:(N-1), j=(i+1):N, g=1:G) %>%
    add_constraint((z[i,j,g]>=x[i,g]+x[j,g]-1), i=1:(N-1), j=(i+1):N, g=1:G) %>%
    add_constraint(sum_over(x[i,g], i=1:N)>=a[g], g=1:G) %>%
    add_constraint(sum_over(x[i,g], i=1:N)<=b[g], g=1:G) %>%
    add_constraint(sum_over(x[i,g], g=1:G)==1, i=1:N)
  return(model)
}

#' mdgp_index: Build Index Model for the Maximally Diverse Grouping Problem
#' @param dist Distance matrix.
#' @param groups Number of groups.
#' @param items Number of items.
#' @param max Maximum group sizes for each group.
#' @param min Minimum group sizes for each group.
#' @return MIP Model
#' @export
mdgp_index <- function(dist, groups, items, max, min) {
  G <- groups
  N <- items
  a <- min[1]
  b <- max[1]
  d <- dist
  model <- MIPModel() %>%
    add_variable(x[i,j], i=1:(N-1), j=(i+1):N, type="binary") %>%
    add_variable(y[i], i=2:N, type="binary") %>%
    set_objective(sum_over(d[i,j]*x[i,j], i=1:(N-1), j=(i+1):N), "max") %>%
    add_constraint((x[i,j]+x[j,k]-x[i,k])<=1, i=1:(N-2), j=(i+1):(N-1), k=(j+1):N) %>%
    add_constraint((x[i,j]+x[i,k]-x[j,k])<=1, i=1:(N-2), j=(i+1):(N-1), k=(j+1):N) %>%
    add_constraint((x[i,k]+x[j,k]-x[i,j])<=1, i=1:(N-2), j=(i+1):(N-1), k=(j+1):N) %>%
    add_constraint((sum_over(x[i,j], j=(i+1):N))>=a-1, i=1) %>%
    add_constraint((sum_over(x[i,j], j=(i+1):N)+sum_over(x[j,i], j=1:(i-1)))>=a-1, i=2:(N-1)) %>%
    add_constraint((sum_over(x[j,i], j=1:(i-1)))>=a-1, i=N) %>%
    add_constraint((sum_over(x[i,j], j=(i+1):N))<=b-1, i=1) %>%
    add_constraint((sum_over(x[i,j], j=(i+1):N)+sum_over(x[j,i], j=1:(i-1)))<=b-1, i=2:(N-1)) %>%
    add_constraint((sum_over(x[j,i], j=1:(i-1)))<=b-1, i=N) %>%
    add_constraint((x[i,j]+y[j])<=1, i=1:(N-1), j=(i+1):N) %>%
    add_constraint((sum_over(x[i,j], i=1:(j-1))+y[j])>=1, j=2:N) %>%
    add_constraint((sum_over(y[i], i=2:N))==G-1)
  return(model)
}

#' mdgp_bigm: Build Big-M Model for the Maximally Diverse Grouping Problem
#' @param dist Distance matrix.
#' @param groups Number of groups.
#' @param items Number of items.
#' @param max Maximum group sizes for each group.
#' @param min Minimum group sizes for each group.
#' @return MIP Model
#' @export
mdgp_bigm <- function(dist, groups, items, max, min) {
  G <- groups
  N <- items
  a <- min
  b <- max
  d <- dist
  model <- MIPModel() %>%
    add_variable(x[i,j], i=1:(N-1), j=(i+1):N, type="binary") %>%
    add_variable(y[i,g], i=1:N, g=1:G, type="binary") %>%
    set_objective(sum_over(d[i,j]*x[i,j], i=1:(N-1), j=(i+1):N), "max") %>%
    add_constraint((x[i,j]+x[j,k]-x[i,k])<=1, i=1:(N-2), j=(i+1):(N-1), k=(j+1):N) %>%
    add_constraint((x[i,j]+x[i,k]-x[j,k])<=1, i=1:(N-2), j=(i+1):(N-1), k=(j+1):N) %>%
    add_constraint((x[i,k]+x[j,k]-x[i,j])<=1, i=1:(N-2), j=(i+1):(N-1), k=(j+1):N) %>%
    add_constraint((sum_over(x[i,j], j=(i+1):N))>=(a[g]-1-(a[g]-1)*(1-y[i,g])), i=1:(N-1), g=1:G) %>%
    # add_constraint((sum_over(x[i,j], i=1:(j-1)))>=(a[g]-1-(a[g]-1)*(1-y[j,g])), j=N, g=1:G) %>% # Changed to the one below
    add_constraint(0>=(a[g]-1-(a[g]-1)*(1-y[i,g])), i=N, g=1:G) %>%
    add_constraint((sum_over(x[i,j], j=(i+1):N))<=(b[g]-1+(max(b)-b[g]+1)*(1-y[i,g])), i=1:(N-1), g=1:G) %>%
    # add_constraint((sum_over(x[i,j], i=1:(j-1)))<=(b[g]-1+(max(b)-b[g]+1)*(1-y[j,g])), j=N, g=1:G) %>% # Redundant as b[g] at least 1
    add_constraint((sum_over(y[j,g], g=1:G))==1, j=1) %>%
    add_constraint((x[i,j]+sum_over(y[j,g], g=1:G))<=1, i=1:(N-1), j=(i+1):N) %>%
    add_constraint((sum_over(x[i,j], i=1:(j-1))+sum_over(y[j,g], g=1:G))>=1, j=2:N) %>%
    add_constraint((sum_over(y[i,g], i=1:N))==1, g=1:G)
  return(model)
}

#' get_sims: Get Simulations for a Given Maximally Diverse Grouping Problem Model
#' @param attributes Number of attributes.
#' @param groups Number of groups.
#' @param items Number of items.
#' @param name Model name (i.e., "standard", "index" or "bigm").
#' @param equal Logical for whether the minimum and maximum group sizes are equal across all groups.
#' @param n_sum Number of simulations to run.
#' @param TimeLimit Time limit for each simulation.
#' @param Verbose Logical for whether to return gurobi verbose output.
#' @return List with components
#' `optval` Vector of optimal values.
#' `time` Vector of run times.
#' `gap` Vector of gaps.
#' `status` Vector of statuses (i.e., "OPTIMAL", "TIME_LIMIT" or "<NA>").
#' @export
get_sims <- function(attributes, groups, items, name, equal=TRUE, n_sim=30, 
                     TimeLimit=1800, verbose=TRUE) {
  A <- attributes
  G <- groups
  N <- items
  mdgp <- paste0("mdgp_", name)
  cat("====================")
  cat("\nModel:", mdgp, "\n")
  cat("====================\n")
  mdgp <- get(mdgp, envir=parent.frame())
  output <- lapply(X=1:n_sim,
                   FUN=function(x){
                     d <- get_dist(attributes=A, items=N, seed=x)
                     sizes <- get_sizes(groups=G, items=N, equal=equal)
                     a <- sizes$a
                     b <- sizes$b
                     model <- mdgp(dist=d, groups=G, items=N, max=b, min=a)
                     result <- tryCatch(
                       {solve_model(model, with_ROI(solver="gurobi", verbose=verbose, control=list(TimeLimit=TimeLimit)))},
                       error = function(e) {NULL}
                     )
                     cat("\nCompleted Iteration ", x, "of ", n_sim, "\n\n")
                     result
                   })
  optval <- unlist(lapply(X=1:n_sim, FUN=function(x){if (!is.null(output[[x]])) {output[[x]]$objective_value} else {NA}}))
  time <- unlist(lapply(X=1:n_sim, FUN=function(x){if (!is.null(output[[x]])) {output[[x]]$additional_solver_output$ROI$message$runtime} else {NA}}))
  gap <-   unlist(lapply(X=1:n_sim, FUN=function(x){if (!is.null(output[[x]])) {output[[x]]$additional_solver_output$ROI$message$mipgap} else {NA}}))
  status <- unlist(lapply(X=1:n_sim, FUN=function(x){if (!is.null(output[[x]])) {output[[x]]$additional_solver_output$ROI$message$status} else {NA}}))
  return(list(optval=optval,
              time=time,
              gap=gap,
              status=status))
}