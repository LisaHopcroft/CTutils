#' Given weight and height, returns BMI
#'
#' @param weight Weight, in kg.
#' @param height Height, in cm.
#'
#' @return Calculated BMI
#' @export
#'
#' @examples
#' calculate_BMI( 52, 155 )
calculate_BMI = function( weight, height ) {
  return ( weight/((height/100)*(height/100)) )
}
