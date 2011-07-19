    # unpack image array, 10 x 5 x 3 -> r g b --
img = np.arange( 10*5*3 ).reshape(( 10,5,3 ))
print "img.shape:", img.shape
r,g,b = img.transpose( 2,0,1 )  # 3 10 5
print "r.shape:", r.shape

    # pack 10 x 5 r g b -> 10 x 5 x 3 again --
rgb = np.array(( r, g, b )).transpose( 1,2,0 )  # 10 5 3 again
print "rgb.shape:", rgb.shape
assert (rgb == img).all()

    # rgb 0 .. 255 <-> float 0 .. 1 --
imgfloat = img.astype(np.float32) / 255.
img8 = (imgfloat * 255).round().astype(np.uint8)  
assert (img == img8).all()
