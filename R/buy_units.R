#' Buy Units
#'
#' Calculates the cost of a purchase during the purchase units phase
#'
#' @param infantry Number of infantry purchased, infantry cost three
#' @param artillery Number of artillery purchased, artillery cost four
#' @param tanks Number of tanks purchased, tanks cost five
#' @param fighters Number of fighters purchased, fighters cost ten
#' @param bombers Number of bombers purchased, bombers cost twelve
#' @param aaguns Number of anti aircraft guns purchased, anti aircraft guns cost five
#' @param complexes Number of industrial complexes purchased, industrial complexes cost fifteen
#' @param submarines Number of submarines purchased, submarines cost six
#' @param destroyers Number of destroyers purchased, destroyers cost eight
#' @param carriers Number of aircraft carriers purchased, aircraft carriers cost fourteen
#' @param cruisers Number of cruisers purchased, cruisers cost twelve
#' @param battleships Number of battleships purchased, battleships cost twenty
#'
#' @return Numerical cost of purchase
#' @export
#'
#' @examples buy_units(infantry = 3, artillery = 1, tanks = 1, complexes = 1, submarines = 2)
buy_units <- function(infantry = 0, artillery = 0, tanks = 0, fighters = 0, bombers = 0, aaguns = 0, complexes = 0, submarines = 0, destroyers = 0, carriers = 0, cruisers = 0, battleships = 0){
  cost = 3*infantry + 4*artillery + 5*tanks + 10*fighters + 12*bombers + 5*aaguns + 15*complexes + 6*submarines + 8*destroyers + 14*carriers + 12*cruisers + 20*battleships
  return(cost)
}
