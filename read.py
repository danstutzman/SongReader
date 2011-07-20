import scipy.misc
import scipy.misc.pilutil
import scipy.stats
import numpy
from Tkinter import *
import PIL.Image
import PIL.ImageTk
import yaml
import os.path
import sys
import numpy.fft
from annotated_image import AnnotatedImage

# matrix.size = 691200 = 720 x 960print 
matrix = scipy.misc.imread('photo.jpeg', flatten=True)
#matrix = scipy.fliplr(matrix) # flip left<->right (horizontally)
#matrix = scipy.flipud(matrix) # flip up<->down (vertically)
matrix = numpy.rot90(matrix, 3) # rotates 270
# matrix[y][x]

# binarize
#matrix_median = scipy.stats.scoreatpercentile(matrix.flat, 30)
#for y in xrange(matrix.shape[0]):
#  for x in xrange(matrix.shape[1]):
#    matrix[y][x] = (255 if matrix[y][x] >= matrix_median else 0)

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

def wavelen_and_first_peak(array):
  fft_num_buckets = 2048
  array = array * numpy.hamming(len(array))
  fft = numpy.fft.fft(array, fft_num_buckets)

  max = None
  best_bucket = None
  for x in range(20, fft_num_buckets / 2):
    if max == None or abs(fft[x]) > max:
      max = abs(fft[x])
      best_bucket = x
  print 'best_bucket', best_bucket

  num_cycles = numpy.fft.fftfreq(fft_num_buckets)[best_bucket] * len(array)
  wave_length = len(array) / num_cycles
  print 'wave_lenth', wave_length

  phase = (numpy.angle(fft[best_bucket]) / numpy.pi / 2) + 0.5
  phase += 0.75
  if phase >= 1.0:
    phase -= 1.0
  x = ((-phase - 0.75) * wave_length)

  return x, wave_length

def update_chart():
  global chart_photo

  world_x0 = big_image.points[0].world_x
  world_y0 = big_image.points[0].world_y
  world_x1 = big_image.points[1].world_x
  world_y1 = big_image.points[1].world_y
  chart_matrix = numpy.zeros((500, 100, 3))
  histogram = numpy.zeros(100)
  matrix_min = 0 #scipy.stats.scoreatpercentile(matrix.flat, 0)
  matrix_max = 255 #scipy.stats.scoreatpercentile(matrix.flat, 100)
  darknesses = numpy.zeros(500)
  for chart_x in xrange(0, 500):
    progress = chart_x / 500.0
    world_x = int((world_x0 * progress) + (world_x1 * (1 - progress)))
    world_y = int((world_y0 * progress) + (world_y1 * (1 - progress)))
    darkness = (matrix[world_y][world_x] - matrix_min) / \
      float(matrix_max - matrix_min) * 99
    if darkness < 0:
      darkness = 0
    if darkness > 99:
      darkness = 99
    chart_matrix[chart_x][darkness] = (0, 0, 255)
    darknesses[chart_x] = darkness
    histogram[darkness] += 1

  chart_x, wave_length = wavelen_and_first_peak(darknesses)
  while chart_x < 500:
    if chart_x >= 0:
      for y in xrange(0, 100):
        chart_matrix[int(chart_x)][y] = (0, 255, 0)
    chart_x += wave_length
  
  fft = numpy.fft.fft(darknesses, 512)
  for i in xrange(len(fft)):
    fft[i] = fft[i] / 2 + \
      fft[i - 1] / 4 if i > 0 else 0 + \
      fft[i + 1] / 4 if i < 1023 else 0
  fft = fft / 4000
  last_height = 0
  for chart_x in xrange(0, 500):
    height = fft[chart_x] * 100 * 10
    if height < 0:
      height = 0
    if height > 99:
      height = 99
    for h in xrange(height, last_height):
      chart_matrix[chart_x][h] = (155, 155, 155)
    for h in xrange(last_height, height):
      chart_matrix[chart_x][h] = (155, 155, 155)
    last_height = height
  
  # smooth histogram
  #for i in xrange(3):
  #  for chart_x in xrange(0, 100):
  #    histogram[chart_x] = histogram[chart_x] / 2 + \
  #      (histogram[chart_x - 1] / 4 if chart_x > 0 else 0) + \
  #      (histogram[chart_x + 1] / 4 if chart_x < 99 else 0)
  histogram = histogram / histogram.max() * 99
  
  # draw histogram
  for chart_x in xrange(0, 100):
    chart_matrix[chart_x][99 - histogram[chart_x]] = (255, 0, 0)
  # draw percentiles
  for percentile in [5, 50, 95]:
    percentile_as_x = \
      int(scipy.stats.scoreatpercentile(matrix.flat, percentile) / 255.0 * 100.0)
    for y in xrange(0, 10):
      chart_matrix[percentile_as_x][y] = (255, 0, 0)
  
  chart_matrix = numpy.rot90(chart_matrix, 3) # rotates 270
  chart_matrix = numpy.fliplr(chart_matrix)
  chart_image = \
    PIL.Image.fromstring('RGB', (500, 100), chart_matrix.astype('b').tostring())
  chart_photo = PIL.ImageTk.PhotoImage(image=chart_image)
  chart.create_image(0, 0, image=chart_photo, anchor=NW)
  
update_chart()
big_image.callback = update_chart

root.update()
root.mainloop()
  
with open('settings.yaml', 'w') as file:
  yaml.dump(big_image.get_all_settings(), file)
