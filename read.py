import scipy.misc
import scipy.misc.pilutil
import numpy
from Tkinter import *
import PIL.Image
import PIL.ImageTk

# matrix.size = 691200 = 720 x 960print 
matrix = scipy.misc.imread('photo.jpeg', flatten=True)
#matrix = scipy.fliplr(matrix) # flip left<->right (horizontally)
#matrix = scipy.flipud(matrix) # flip up<->down (vertically)
matrix = numpy.rot90(matrix, 3) # rotates 270
# matrix[y][x]

#for x in xrange(0, 50):
#  matrix[x] = numpy.array(x * 4)

#shifted = shift_canvas_down(matrix, 10)
#matrix = matrix - shifted
#matrix = 255 - matrix
#for y in xrange(0, 960):
#  for x in xrange(250, 300):
#    #matrix[y][x] *= (1 if (y + 6) % 12 > 10 else 0)
#    #matrix[y][x] *= (1 if (y + 6) % 12 <= 7 else 0)
#    matrix[y][x] += (0 if (y + 6) % 12 <= 6 else 20)
#matrix = 255 - matrix

#matrix = scipy.transpose(matrix)
#scipy.misc.pilutil.imshow(matrix)
#scipy.misc.pilutil.imsave('filename.png', matrix)

#data = numpy.array(numpy.random.random((400,500)) * 100, dtype=int)
width = matrix.shape[1]
height = matrix.shape[0]
root = Tk()

im = PIL.Image.fromstring('L', (width, height), matrix.astype('b').tostring())
im_resized = None
photo = PIL.ImageTk.PhotoImage(image=im)

frame = Frame(root, bd=2, relief=SUNKEN)
frame.grid_rowconfigure(0, weight=1)
frame.grid_columnconfigure(0, weight=1)

xscrollbar = Scrollbar(frame, orient=HORIZONTAL)
xscrollbar.grid(row=1, column=0, sticky=E+W)

yscrollbar = Scrollbar(frame)
yscrollbar.grid(row=0, column=1, sticky=N+S)

canvas = Canvas(frame, width=300, height=300, \
  scrollregion = (0, 0, im.size[0], im.size[1]))
image_num = canvas.create_image(0,0, image=photo, anchor=NW)
zoom = 1

def update_image():
  global canvas, image_num, photo, im_resized, zoom, scroll_x, scroll_y, yscrollbar
  canvas.delete(image_num)

  iw, ih = im.size
  cw, ch = (int)(iw / zoom), (int)(ih / zoom)
  _x, _y = (int)(scroll_x * iw), (int)(scroll_y * ih)
  canvas_w = (int)(canvas.config()['width'][4])
  canvas_h = (int)(canvas.config()['height'][4])
  crop_window = (_x, _y, _x + (canvas_w / zoom), _y + (canvas_h / zoom))
  im_resized = im.crop(crop_window).resize((canvas_w, canvas_h))

  photo = PIL.ImageTk.PhotoImage(image=im_resized)
  image_num = canvas.create_image(0, 0, image=photo, anchor=NW)
  xscrollbar.set(scroll_x, scroll_x + ((float)(canvas_w) / im.size[0] / zoom))
  yscrollbar.set(scroll_y, scroll_y + ((float)(canvas_h) / im.size[1] / zoom))

scroll_x = 0.0 # position between 0 and 1 where left of scroll bar is
scroll_y = 0.0 # position between 0 and 1 where top of scroll bar is
def xview(arg1, arg2=None, arg3=None):
  global canvas, scroll_x, scroll_y
  if arg1 == 'moveto':
    scroll_x = float(arg2)
    update_image()
def yview(arg1, arg2=None, arg3=None):
  global canvas, scroll_x, scroll_y
  if arg1 == 'moveto':
    scroll_y = float(arg2)
    update_image()

canvas.grid(row=0, column=0, sticky=N+S+E+W)
canvas.config(yscrollcommand=yscrollbar.set)
canvas.config(xscrollcommand=xscrollbar.set)

xscrollbar.config(command=xview)
yscrollbar.config(command=yview)

frame.pack(side=LEFT)

def scale_changed(multiplier): # 0 to 100
  # im_resized needs to be global for some reason,
  # maybe because otherwise it's incorrectly garbage collected?
  global im, im_resized, image_num, canvas, photo, zoom
  multiplier = int(multiplier)
  zoom = multiplier
  update_image()

frame2 = Frame(root).pack(side=LEFT)
Scale(frame2, orient=HORIZONTAL, command=scale_changed, from_=1, to=2).pack()
Label(frame2, text='here').pack()
Button(frame2, text='left').pack()
Button(frame2, text='right').pack()

root.update()
root.mainloop()
