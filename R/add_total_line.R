add_total_line = function( d, id ) {

  d_new = d[1,]
  d_new[,id] = "Total"

  d       = d %>% dplyr::mutate_if( is.factor, as.character )
  v_tosum = d %>% dplyr::select_if( is.numeric ) %>% colnames

  for ( v in v_tosum ) {
    d_new[,v] = sum( d[,v] )
  }

  return( d %>% dplyr::bind_rows( d_new ) )
}
