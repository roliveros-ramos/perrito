#' Assign seats
#'
#' @param x A matrix or data.frame. Votes for each partie are in columns, rows are
#' jurisdictions.
#' @param nseats A vector with the number of seats per jurisdiction.
#' @param method The method used for seats allocation. Available methods are
#' "dhondt", "webster", "danish", "imperiali", "hill-huntington", "dean",
#' "mod-saint-lague", "equal-proportions", "adams", "hare", "droop",
#' "hangenbach-bischoff", "imperial", "mod-imperial", "quotas-remainders"
#'
#' @return A matrix the same dimension of 'x' with the seats allocated to each partie
#' to every jurisdiction.
#' @export
#'
#' @examples
assign_seats = function(x, nseats, method, thr=0) {

  if(!is.character(method) | length(method)>1)
    stop("only one method must be provided.")
  ha = c("dhondt", "webster", "danish", "imperiali", "hill-huntington", "dean",
         "mod-saint-lague", "equal-proportions", "adams")
  lr = c("hare", "droop", "hangenbach-bischoff", "imperial",
         "mod-imperial", "quotas-remainders")

  isHa = method %in% ha
  isLr = method %in% lr

  if(!isHa & !isLr) stop("method not recognised.")

  if(isHa) seats_aloc = electoral::seats_ha
  if(isLr) seats_aloc = electoral::seats_lr

  n = nrow(x)
  nseats = as.numeric(nseats)
  out = array(dim=dim(x))
  parties = colnames(x)
  if(is.null(parties)) parties = paste0("P", seq_len(ncol(x)))

  tx = which(colSums(x)/sum(x, na.rm=TRUE) < thr)
  x[, tx] = 0 # remove voters for parties below thr.

  for(i in seq_len(n)) {
    out[i,] = seats_aloc(parties, x[i,], n_seats=nseats[i], method=method)
  }
  dimnames(out) = dimnames(x)
  return(out)

}


#' Split seats between jurisdictions
#'
#' @param voters A vector with the number of voters per jurisdiction.
#' @param nseats The total number of seats to split.
#' @param fixed The fixed number of seats for each jurisdiction. By default, zero.
#' @param method The method used to split seats, by default, "Hare".
#'
#' @return A vector with the number of seats for each jurisdiction.
#' @export
#'
#' @examples
split_seats = function(voters, nseats, fixed=0, method="hare") {

  n_districts = length(voters)
  if(fixed*n_districts > nseats) stop("Too many fixed seats.")

  fixed_seats = rep(fixed, length=n_districts)

  nseats = nseats - sum(fixed_seats)

  parties = names(voters)
  if(is.null(parties)) parties = paste0("D", seq_along(voters))

  prop_seats = electoral::seats_lr(parties=parties, votes=voters,
                                   n_seats=nseats, method = method)

  seats = fixed_seats + prop_seats
  names(seats) = names(voters)

  return(seats)

}
