import scipy.misc
import scipy.misc.pilutil
import numpy
from Tkinter import *
import PIL.Image
import PIL.ImageTk
import yaml
import os.path
from annotated_image import AnnotatedImage

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
root = Tk()

size = (matrix.shape[1], matrix.shape[0])
image = PIL.Image.fromstring('L', size, matrix.astype('b').tostring())


settings = {}
if os.path.exists('settings.yaml'):
  with open('settings.yaml', 'r') as file:
    settings = yaml.load(file)
    if not settings:
      settings = {}
big_image_and_chart = Frame(root)
big_image = AnnotatedImage(big_image_and_chart, image, 500, 500, settings)
big_image.pack(side=TOP)
chart = Canvas(big_image_and_chart, width=500, height=100)
chart.pack()
big_image_and_chart.pack(side=LEFT)

def scale_changed(multiplier):
  big_image.zoom = int(multiplier)
  big_image.update_canvas()

frame3 = Frame(root).pack(side=LEFT)
Label(frame3, text='Zoom').pack()

scale = Scale(frame3, orient=HORIZONTAL, command=scale_changed, from_=1, to=5)
scale.pack()
scale.set(big_image.zoom)

def scan():
  pass # unimplemented

button = Button(frame3, text='Scan', command=scan)
button.pack(pady=30)

Label(frame3, text='Note Cam').pack()
note_cam = Canvas(frame3, width=100, height=100)
note_cam.pack()
crop_x = int(big_image.points[0].world_x)
crop_y = int(big_image.points[0].world_y)
note_image = image.crop((crop_x - 10, crop_y - 10, crop_x + 10, crop_y + 10))
note_image = note_image.resize((100, 100))
note_photo = PIL.ImageTk.PhotoImage(image=note_image)
cam_image_num = note_cam.create_image(0, 0, image=note_photo, anchor=NW)

world_x0 = big_image.points[0].world_x
world_y0 = big_image.points[0].world_y
world_x1 = big_image.points[1].world_x
world_y1 = big_image.points[1].world_y
for chart_x in xrange(0, 500):
  progress = chart_x / 500.0
  world_x = int((world_x0 * progress) + (world_x1 * (1 - progress)))
  world_y = int((world_y0 * progress) + (world_y1 * (1 - progress)))
  darkness = matrix[world_y][world_x] * 100 / 255
  chart.create_line((chart_x, darkness, chart_x, 100))

root.update()
root.mainloop()

with open('settings.yaml', 'w') as file:
  yaml.dump(big_image.get_all_settings(), file)
