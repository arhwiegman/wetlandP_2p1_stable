# calculate flow volume from water level and flow velocity using area velocity relationships


fn_circle_segment_area <- function(
  # https://www.mathopenref.com/segmentareaht.html
  r = 2,   # r is the radius of the circle of which the segment is a part.
  h = 0.75 # h is the height of the segment (e.g. height from bottom of pipe center to water level)
){
  r^2*acos((r-h)/r)-(r-h)*sqrt(2*r*h-h^2)
}
fn_circle_segment_area()

  
  