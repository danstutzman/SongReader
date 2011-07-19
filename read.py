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
#im = im.resize((width / 2, height / 2), PIL.Image.NEAREST)
photo = PIL.ImageTk.PhotoImage(image=im)

frame = Frame(root, bd=2, relief=SUNKEN)
frame.grid_rowconfigure(0, weight=1)
frame.grid_columnconfigure(0, weight=1)

xscrollbar = Scrollbar(frame, orient=HORIZONTAL)
xscrollbar.grid(row=1, column=0, sticky=E+W)

yscrollbar = Scrollbar(frame)
yscrollbar.grid(row=0, column=1, sticky=N+S)

#canvas = Canvas(frame, bd=0,
#                xscrollcommand=xscrollbar.set,
#                yscrollcommand=yscrollbar.set)
canvas = Canvas(frame, width=im.size[0], height=im.size[1], \
  scrollregion = (0, 0, im.size[0], im.size[1]))
canvas.create_image(0,0, image=photo, anchor=NW)

canvas.grid(row=0, column=0, sticky=N+S+E+W)
canvas.config(yscrollcommand=yscrollbar.set)
canvas.config(xscrollcommand=xscrollbar.set)

xscrollbar.config(command=canvas.xview)
yscrollbar.config(command=canvas.yview)

frame.pack(side=LEFT)

def scale_changed(new_value):
  print new_value

frame2 = Frame(root).pack(side=LEFT)
Scale(frame2, orient=HORIZONTAL, command=scale_changed).pack()
Label(frame2, text='here').pack()
Button(frame2, text='left').pack()
Button(frame2, text='right').pack()

root.update()
root.mainloop()
