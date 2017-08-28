#' Has a list of color palettes used
#' 
#' uses color palettes of interest
#' 
#' 
#' @author Shahab Asgharzadeh
#' @param n n is the name of the color palette that one needs
colorPalette = function(n) {
  switch(n,
         "largePalette" = c('firebrick2', 	'limegreen', 	'gold', 	'deepskyblue3', 	'chocolate1', 	'darkorchid', 	
                            'turquoise', 	'magenta2', 	'olivedrab1', 	'rosybrown1', 	'turquoise4', 	'plum2', 	'sienna', 	
                            'lemonchiffon', 	'darkred', 	'red4', 	'darkseagreen1', 	'gold4', 	'yellow4', 	'navy', 	'gray50', 	
                            'gray100', 	'black', 	'gray0'),
         "kmPalette" = c('red', 'blue', 'darkgreen', 'black', 'orange', 'yellow4', 'brown', 'purple', 'grey', 'turquoise'),
         "kmPalette2" = c('darkgreen', 'blue', 'red', 'black', 'orange', 'yellow4', 'brown', 'purple', 'grey', 'turquoise'),
         'cbbPalette' = c( GetColorHex('darkgreen'), GetColorHex('blue'), GetColorHex('red'), GetColorHex('black'),
                           GetColorHex('saddlebrown'), GetColorHex('hotpink4'), GetColorHex('yellow4'),
                           GetColorHex('purple'), GetColorHex('steelblue'), GetColorHex('palegreen'),
                           GetColorHex('violetred'), GetColorHex('peachpuff'), GetColorHex('lemonchiffon')),
         'kmPalette3' = c('orangered4', 'dodgerblue', 'seagreen2', 'purple', 'darkgreen', 'blue', 'red', 'black', 'grey', 'violetred'),
         'Pallette1' = c('grey', 'black'),
         'Pallette2' = c('grey', 'black'),
         'Pallette3' = c('black', 'grey', 'white'),
         'Pallette4' = c('green', 'blue', 'red'),
         'Pallette5' = c('firebrick3', 'blueviolet', 'white')
  )
  # To use for fills, for line and point colors, add the following to the ggplot function
  # For List of Colors, can change to red//oragne or whatever to get a list to use.
  # colors()[grep("red",colors())]  
  
  #scale_colour_manual(values=cbbPallette)
  #scale_fill_manual(values=cbbPallette)
}

