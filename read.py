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
note_cam = Canvas(frame3, width=100, height=400)
note_cam.pack()
crop_x = int(big_image.points[0].world_x)
crop_y = int(big_image.points[0].world_y)
note_image = image.crop((crop_x - 10, crop_y - 10, crop_x + 10, crop_y + 10))
note_image = note_image.resize((100, 100))
note_photo = PIL.ImageTk.PhotoImage(image=note_image)
cam_image_num = note_cam.create_image(0, 0, image=note_photo, anchor=NW)

def wavelen_and_first_peak(array):
  fft_num_buckets = 512
  array = array * numpy.hamming(len(array))
  fft = numpy.fft.fft(array, fft_num_buckets)

  max = None
  best_bucket = None
  for x in range(20, fft_num_buckets / 2):
    if max == None or abs(fft[x]) > max:
      max = abs(fft[x])
      best_bucket = x
  #print 'best_bucket', best_bucket

  num_cycles = numpy.fft.fftfreq(fft_num_buckets)[best_bucket] * len(array)
  wave_length = len(array) / num_cycles
  #print 'wave_length', wave_length

  phase = (numpy.angle(fft[best_bucket]) / numpy.pi / 2) + 0.5
  phase += 0.75
  if phase >= 1.0:
    phase -= 1.0
  x = ((-phase - 0.75) * wave_length)

  return x, wave_length

def threshold_array(array, cutoff):
  vertical_slice4 = numpy.copy(array)
  vertical_slice4 -= cutoff
  vertical_slice4 += abs(vertical_slice4)
  vertical_slice4 += 100
  vertical_slice4 = 255 - vertical_slice4
  vertical_slice4 -= (254 - cutoff)
  vertical_slice4 += abs(vertical_slice4)
  vertical_slice4 *= 255 / vertical_slice4.max()
  return vertical_slice4

def update_chart():
  global chart_photo, note_photo
  global cam_image_num

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
  big_image.draw_tick_marks(chart_x, wave_length)
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

  point0 = int(big_image.points[0].world_x), int(big_image.points[0].world_y)
  point1 = int(big_image.points[1].world_x), int(big_image.points[1].world_y)
  if point0[0] > point1[0]: # order them left to right
    point0, point1 = point1, point0
  cam_matrix = []
  cam2_matrix = [] # black except for where the lines are predicted to be
  wavelens = []
  centered_xs = []
  best_center_xs = []
  slice3_threshold = 220
  for world_x in xrange(point0[0], point1[0]):
    progress = (world_x - point0[0]) / float(point1[0] - point0[0])
    world_y = int((point0[1] * progress) + (point1[1] * (1 - progress)))
    #vertical_slice = matrix[world_x, (world_y - 100):(world_y + 100)]
    vertical_slice = matrix[(world_y-50):(world_y+50),world_x]
    vertical_slice = numpy.copy(vertical_slice)
    vertical_slice2 = numpy.zeros(len(vertical_slice))
    #vertical_slice3 = \
    #vertical_slice3 = \
    #  -1 * matrix[(world_y-49):(world_y+51),world_x-1] + \
    #  -2 * matrix[(world_y-49):(world_y+51),world_x  ] + \
    #  -1 * matrix[(world_y-49):(world_y+51),world_x+1] + \
    #   1 * matrix[(world_y-51):(world_y+49),world_x-1] + \
    #   2 * matrix[(world_y-51):(world_y+49),world_x  ] + \
    #   1 * matrix[(world_y-51):(world_y+49),world_x+1]
    #vertical_slice3 = \
    #   0 * matrix[(world_y-49):(world_y+51),world_x-1] + \
    #   2 * matrix[(world_y-49):(world_y+51),world_x  ] + \
    #   0 * matrix[(world_y-49):(world_y+51),world_x+1] + \
    #   0 * matrix[(world_y-50):(world_y+50),world_x-1] + \
    #   4 * matrix[(world_y-50):(world_y+50),world_x  ] + \
    #   0 * matrix[(world_y-50):(world_y+50),world_x+1] + \
    #   0 * matrix[(world_y-51):(world_y+49),world_x-1] + \
    #   2 * matrix[(world_y-51):(world_y+49),world_x  ] + \
    #   0 * matrix[(world_y-51):(world_y+49),world_x+1]
    #vertical_slice3 = numpy.copy(vertical_slice)
    vertical_slice3 = (numpy.roll(vertical_slice, 1) + \
      numpy.roll(vertical_slice, 0) +
      numpy.roll(vertical_slice, -1)) / 3
    #vertical_slice3 *= -1
    #vertical_slice3 += abs(vertical_slice3)
    #vertical_slice3 /= 8
    vertical_slice3 -= 80
    vertical_slice3 *= -1
    vertical_slice3 += abs(vertical_slice3)
    vertical_slice3 *= 2
    #vertical_slice3 += 128
    #vertical_slice4 = vertical_slice3
    #vertical_slice4 = vertical_slice + vertical_slice3 * 0.2

    vertical_slice3 = \
      numpy.roll(vertical_slice3, -4) + \
      numpy.roll(vertical_slice3, -3) + \
      numpy.roll(vertical_slice3, -2) + \
      numpy.roll(vertical_slice3, -1) + \
      numpy.roll(vertical_slice3,  0) + \
      numpy.roll(vertical_slice3,  1) + \
      numpy.roll(vertical_slice3,  2) + \
      numpy.roll(vertical_slice3,  3) + \
      numpy.roll(vertical_slice3,  4)
    #vertical_slice3 = 200 - vertical_slice3
    #vertical_slice3 += abs(vertical_slice3)
    #vertical_slice3 = 200 - vertical_slice3
    #vertical_slice3 = threshold_array(vertical_slice3, 100)

    vertical_slice5 = 130 - vertical_slice
    vertical_slice5 += abs(vertical_slice5)
    vertical_slice5 /= 1

    #vertical_slice4 = \
    #  threshold_array(vertical_slice, 105) * \
    #  threshold_array(vertical_slice3, 100) 
    vertical_slice4 = \
      vertical_slice5 * \
      threshold_array(vertical_slice3, 100) 
    vertical_slice4 /= 255
    #vertical_slice4 = vertical_slice3
    #vertical_slice4 = threshold_array(vertical_slice, 120)
    #vertical_slice4 = vertical_slice5

    x, wavelen = wavelen_and_first_peak(vertical_slice)

    slice3_x = x - (wavelen / 2)
    total_slice3 = 0
    while slice3_x < len(vertical_slice):
      if int(slice3_x) >= 0:
        total_slice3 += vertical_slice3[int(slice3_x)]
      slice3_x += wavelen

    dark_x = x - (wavelen / 2)
    dark_xs = []
    corresponding_darkness = []
    while dark_x < len(vertical_slice):
      if int(dark_x) >= 0:
        if wavelen < 20 and total_slice3 < slice3_threshold:
          #vertical_slice2[int(dark_x)] = 250 if vertical_slice3[int(dark_x)] > 130 or vertical_slice3[int(dark_x) - 1] > 130 else 50
          pass #vertical_slice2[int(dark_x)] = \
            #250 if vertical_slice[int(dark_x)] > 130 else 50
          #vertical_slice2[int(dark_x)] = \
          #  255 - vertical_slice3[int(dark_x)]
        dark_xs.append(int(dark_x))
        #darkness = 255 - vertical_slice[int(dark_x)]
        #corresponding_darkness.append(darkness)
        edge_score = 255 - vertical_slice[int(dark_x)]
        #if int(dark_x) > 0:
        #  edge_score -= vertical_slice[int(dark_x) - 1] / 2
        #if int(dark_x) < len(vertical_slice) - 1:
        #  edge_score -= vertical_slice[int(dark_x) + 1] / 2
        corresponding_darkness.append(edge_score)
      dark_x += wavelen

    # 0 corresponds to the first predicted_x being the center,
    #           -2 -1 | (0) 1 2
    # so -2 corresponds to only one line being visible in the window
    #   -4 -3 (-2) -1 |  0
    # 2 or higher means all 5 staff lines are visible, unless it's too high
    #                 |  0 1 (2) 3 4
    best_center_position = None
    second_best_center_position = None
    max_score = None
    second_best_score = None
    for center_position in xrange(2, len(dark_xs) - 2):
      five_darknesses = \
        corresponding_darkness[(center_position - 2):(center_position + 3)]
      total_score = five_darknesses[0] + five_darknesses[1] + \
        five_darknesses[2] + five_darknesses[3] + five_darknesses[4]
      if best_center_position == None or total_score > max_score:
        second_best_center_position = best_center_position
        second_best_score = max_score
        best_center_position = center_position
        max_score = total_score
    if best_center_position != None:
      best_center_x = dark_xs[best_center_position]
      #if second_best_score and max_score - second_best_score > 20:
      if wavelen < 20 and total_slice3 < slice3_threshold:
        vertical_slice2[best_center_x] = 120

    centered_x = x
    while centered_x < 50.0:
      centered_x += wavelen
    centered_xs.append(int(centered_x))

    wavelens.append(wavelen)
    while x < 0:
      x += wavelen
    while x < len(vertical_slice):
      #vertical_slice[x] = 0
      x += wavelen
    #vertical_slice[int(centered_x)] = 200

    #cam_matrix.append(vertical_slice)
    #cam_matrix.append(vertical_slice2)
    #cam_matrix.append(vertical_slice3)
    cam_matrix.append(vertical_slice4)
    #print centered_xs

#  median_wavelen = int(scipy.stats.scoreatpercentile(wavelens, 50))
#  print 'median_wavelen', median_wavelen
#  dampened_average = numpy.array(centered_xs).mean()
#  adjustment = 0 # always a multiple of median_wavelen
#  new_xs = []
#  for x in centered_xs:
#    old_x = x
#    while x > dampened_average + (median_wavelen * 2 / 3):
#      x -= median_wavelen
#    while x < dampened_average - (median_wavelen * 2 / 3):
#      x += median_wavelen
#    print old_x, x, dampened_average
#    dampened_average = (dampened_average * 1/2) + (x * 1/2)
#    new_xs.append(int(dampened_average))
#
#  new_xs = numpy.array(new_xs)
#  y = 0
#  for x in new_xs:
#    if x > 0 and x < 100:
#      pass #cam_matrix[y][x] = 255
#    y += 1

  cam_matrix = numpy.array(cam_matrix)
  size = (cam_matrix.shape[1], cam_matrix.shape[0])
  cam_matrix = scipy.flipud(cam_matrix)
  cam_image = PIL.Image.fromstring('L', size, cam_matrix.astype('b').tostring())
  #note_image = cam_image.resize((100, 100))
  note_photo = PIL.ImageTk.PhotoImage(image=cam_image)
  note_cam.delete(cam_image_num)
  cam_image_num = note_cam.create_image(0, 0, image=note_photo, anchor=NW)

update_chart()
big_image.callback = update_chart

root.update()
root.mainloop()
  
with open('settings.yaml', 'w') as file:
  yaml.dump(big_image.get_all_settings(), file)
