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

def make_white_pure_white(array):
  array = numpy.copy(array)
  array = 130 - array
  array += abs(array)
  array /= 2
  array = 255 - array
  return array

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
    chart_matrix[chart_x][darkness] = (128, 128, 255)
    darknesses[chart_x] = darkness
    histogram[darkness] += 1

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
  centered_ys = []
  best_center_ys = []
  trains = [] # as in: trains on train racks
  for world_x in xrange(point0[0], point1[0]):
    progress = (world_x - point0[0]) / float(point1[0] - point0[0])
    world_y = int((point1[1] * progress) + (point0[1] * (1 - progress)))
    vertical_slice = matrix[(world_y-50):(world_y+50),world_x]

    binary = numpy.array([1 if y > 130 else 0 for y in vertical_slice])
    
    pure_white = make_white_pure_white(vertical_slice)
    blurred = (numpy.roll(pure_white, 1) + \
      numpy.roll(pure_white, 0) +
      numpy.roll(pure_white, -1)) / 3
    binary_non_staff = numpy.array([1 if y >= 200 else 0 for y in blurred])

    blurred_binary_non_staff = \
      numpy.roll(binary_non_staff, -4) + \
      numpy.roll(binary_non_staff, -3) + \
      numpy.roll(binary_non_staff, -2) + \
      numpy.roll(binary_non_staff, -1) + \
      numpy.roll(binary_non_staff,  0) + \
      numpy.roll(binary_non_staff,  1) + \
      numpy.roll(binary_non_staff,  2) + \
      numpy.roll(binary_non_staff,  3) + \
      numpy.roll(binary_non_staff,  4)
    augmented_binary_non_staff = \
      numpy.array([1 if y == 9 else 0 for y in blurred_binary_non_staff])

    partially_erased_pure_white = \
      255 - ((255 - pure_white) * augmented_binary_non_staff)
    vertical_slice = partially_erased_pure_white

    binary_partially_erased = \
      numpy.array([1 if y > 240 else 0 for y in partially_erased_pure_white])

    y, wavelen = wavelen_and_first_peak(vertical_slice)

    dark_y = y - (wavelen / 2)
    dark_ys = []
    corresponding_darkness = []
    while dark_y < len(vertical_slice):
      if int(dark_y) >= 0:
        #if wavelen < 12:
        #  vertical_slice[int(dark_y)] = 120
        dark_ys.append(int(dark_y))
        #darkness = 255 - vertical_slice[int(dark_y)]
        #corresponding_darkness.append(darkness)
        edge_score = 255 - vertical_slice[int(dark_y)]
        #edge_score = -binary_partially_erased[int(dark_y)]
        #if int(dark_y) > 0:
        #  edge_score -= vertical_slice[int(dark_y) - 1] / 2
        #if int(dark_y) < len(vertical_slice) - 1:
        #  edge_score -= vertical_slice[int(dark_y) + 1] / 2
        corresponding_darkness.append(edge_score)
      dark_y += wavelen

    # 0 corresponds to the first predicted_y being the center,
    #           -2 -1 | (0) 1 2
    # so -2 corresponds to only one line being visible in the window
    #   -4 -3 (-2) -1 |  0
    # 2 or higher means all 5 staff lines are visible, unless it's too high
    #                 |  0 1 (2) 3 4
    best_center_positions = []
    max_score = None
    for center_position in xrange(2, len(dark_ys) - 2):
      five_darknesses = \
        corresponding_darkness[(center_position - 2):(center_position + 3)]
      total_score = five_darknesses[0] + five_darknesses[1] + \
        five_darknesses[2] + five_darknesses[3] + five_darknesses[4]
      if best_center_positions == [] or total_score >= max_score:
        if best_center_positions == [] or total_score > max_score:
          best_center_positions = [center_position]
          max_score = total_score
        else:
          best_center_positions.append(center_position)

    for best_center_position in best_center_positions:
      best_center_y = dark_ys[best_center_position]
      #vertical_slice[int(best_center_y)] = 0

      closest_train = None
      min_distance = None
      for possible_train in trains:
        if min_distance == None or \
            abs(possible_train['last_y'] - best_center_y) < min_distance:
          closest_train = possible_train
          min_distance = abs(possible_train['last_y'] - best_center_y)
      if closest_train:
        if abs(closest_train['last_y'] - best_center_y) > 4:
          closest_train = None
      if closest_train == None:
        closest_train = {
          'length':0,
          'last_y':best_center_y,
          'sequence':[-1] * (point1[0] - point0[0])
        }
        trains.append(closest_train)
      closest_train['last_y'] = \
        closest_train['last_y'] * 0.75 + best_center_y * 0.25
      closest_train['sequence'][world_x - point0[0]] = best_center_y
      closest_train['length'] += 1

    cam_matrix.append(vertical_slice)

  longest_train = None
  max_length = 0
  for train in trains:
    if train['length'] > max_length:
      max_length = train['length']
      longest_train = train
  last_train_y = None
  last_train_x = None
  for x in xrange(len(longest_train['sequence'])):
    y = longest_train['sequence'][x]
    if y > -1:
      if last_train_x and last_train_y:
        for x2 in xrange(last_train_x, x):
          progress = (x2 - last_train_x) / float(x - last_train_x)
          y2 = int(progress * y + (1 - progress) * last_train_y)
          cam_matrix[x2][y2] = 0 
      for bold in [-1, 0, 1]:
        cam_matrix[x][y + bold] = 0 
      last_train_x = x
      last_train_y = y

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
