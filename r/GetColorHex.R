GetColorHex <-
function(color){ c <- col2rgb(color)
                                #sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3]), this is if you also need RGB info
                                sprintf("#%02X%02X%02X", c[1],c[2],c[3])
}
