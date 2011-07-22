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
import scipy.ndimage.interpolation
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
  #for chart_x in xrange(0, 500):
  #  progress = chart_x / 500.0
  #  world_x = int((world_x0 * progress) + (world_x1 * (1 - progress)))
  #  world_y = int((world_y0 * progress) + (world_y1 * (1 - progress)))
  #  darkness = (matrix[world_y][world_x] - matrix_min) / \
  #    float(matrix_max - matrix_min) * 99
  #  if darkness < 0:
  #    darkness = 0
  #  if darkness > 99:
  #    darkness = 99
  #  chart_matrix[chart_x][darkness] = (128, 128, 255)
  #  darknesses[chart_x] = darkness
  #  histogram[darkness] += 1

  point0 = int(big_image.points[0].world_x), int(big_image.points[0].world_y)
  point1 = int(big_image.points[1].world_x), int(big_image.points[1].world_y)
  if point0[0] > point1[0]: # order them left to right
    point0, point1 = point1, point0
  cam_matrix = []
  cam_matrix_annotated = []
  wavelens = []
  centered_ys = []
  best_center_ys = []
  trains = [] # as in: trains on train racks
  binary_non_staff_matrix = []
  partially_erased_pure_white_matrix = []
  augmented_binary_non_staff_matrix = []
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
    vertical_slice_annotated = numpy.copy(vertical_slice)

    y, wavelen = wavelen_and_first_peak(partially_erased_pure_white)

    dark_y = y - (wavelen / 2)
    dark_ys = []
    corresponding_darkness = []
    while dark_y < len(vertical_slice):
      if int(dark_y) >= 0:
        #if wavelen < 16:
        #vertical_slice_annotated[int(dark_y)] = 160
        dark_ys.append(int(dark_y))
        #darkness = 255 - vertical_slice[int(dark_y)]
        #corresponding_darkness.append(darkness)
        edge_score = 255 - partially_erased_pure_white[int(dark_y)]
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
      #vertical_slice_annotated[int(best_center_y)] = 0

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
          'center_ys':[-1] * (point1[0] - point0[0]),
          'widths':[-1] * (point1[0] - point0[0])
        }
        trains.append(closest_train)
      closest_train['last_y'] = \
        closest_train['last_y'] * 0.75 + best_center_y * 0.25
      closest_train['center_ys'][world_x - point0[0]] = best_center_y
      closest_train['length'] += 1
      closest_train['widths'][world_x - point0[0]] = wavelen * 2

    cam_matrix_annotated.append(vertical_slice_annotated)
    cam_matrix.append(vertical_slice)
    #cam_matrix.append(binary_non_staff * 255)
    binary_non_staff_matrix.append(binary_non_staff)
    partially_erased_pure_white_matrix.append(partially_erased_pure_white)
    augmented_binary_non_staff_matrix.append(augmented_binary_non_staff)

  min_lightness = 255
  max_lightness = 0
  for y in xrange(100):
    lightness = cam_matrix[0][y]
    if lightness < min_lightness:
      min_lightness = lightness
    if lightness > max_lightness:
      max_lightness = lightness
  for y in xrange(100):
    adjusted_lightness = \
      (cam_matrix[0][y] - min_lightness) * 99 / (max_lightness - min_lightness)
    if adjusted_lightness < 1:
      adjusted_lightness = 1
    if adjusted_lightness > 99:
      adjusted_lightness = 99
    chart_matrix[y * 5][adjusted_lightness] = (255, 0, 0)
  for y in xrange(100):
    groove = (y - 33) / 7
    if groove >= 0 and groove < 5:
      adjusted_lightness = abs(y - ((groove * 7) + 33 + 4)) * (100 / 4)
    else:
      adjusted_lightness = 50
    if adjusted_lightness < 1:
      adjusted_lightness = 1
    if adjusted_lightness > 99:
      adjusted_lightness = 99
    chart_matrix[y * 5][adjusted_lightness] = (0, 255, 0)

  blur_matrix = []
  best_skew = None
  best_i = None
  max_range = None
  width = len(partially_erased_pure_white_matrix)
  #width = 100
  skew = width * -0.1
  i = 0
  while skew <= width * 0.1:
    sum_slice = numpy.zeros(100)
    denominator = numpy.zeros(100)
    for x in xrange(width):
      progress = x / float(width)
      #slice = numpy.roll(cam_matrix[x],
      slice = numpy.roll(partially_erased_pure_white_matrix[x] * \
        augmented_binary_non_staff_matrix[x],
        -int(progress * skew))
      denominator += numpy.roll(augmented_binary_non_staff_matrix[x], -int(progress * skew))
      sum_slice += slice
    sum_slice /= denominator
    blur_matrix.append(sum_slice)

    the_range = sum_slice.max() - sum_slice.min()
    if max_range == None or the_range > max_range:
      max_range = the_range
      best_skew = skew
      best_i = i

    skew += 0.2
    i += 1

  best_sum_slice = blur_matrix[best_i]
  first_peak, wavelen = wavelen_and_first_peak(best_sum_slice)

  blur_matrix = numpy.array(blur_matrix)
  blur_matrix = 255 - blur_matrix 
  blur_matrix *= (255 / blur_matrix.max())
  blur_matrix = 255 - blur_matrix 

  dark_y = first_peak - (wavelen / 2)
  dark_ys = []
  corresponding_darkness = []
  while dark_y < len(vertical_slice):
    if int(dark_y) >= 0:
      dark_ys.append(int(dark_y))
      edge_score = 255 - best_sum_slice[int(dark_y)]
      corresponding_darkness.append(edge_score)
    dark_y += wavelen

  # 0 corresponds to the first predicted_y being the center,
  #           -2 -1 | (0) 1 2
  # so -2 corresponds to only one line being visible in the window
  #   -4 -3 (-2) -1 |  0
  # 2 or higher means all 5 staff lines are visible, unless it's too high
  #                 |  0 1 (2) 3 4
  best_center_position = None
  max_score = None
  for center_position in xrange(2, len(dark_ys) - 2):
    five_darknesses = \
      corresponding_darkness[(center_position - 2):(center_position + 3)]
    total_score = five_darknesses[0] + five_darknesses[1] + \
      five_darknesses[2] + five_darknesses[3] + five_darknesses[4]
    if best_center_position == None or total_score > max_score:
      best_center_position = center_position
      max_score = total_score
  best_center_y = dark_ys[best_center_position]
  best_first_y = best_center_y - wavelen * 2

  for x in xrange(width):
    y = best_first_y
    y += x / float(width) * best_skew
    for i in xrange(5):
      cam_matrix_annotated[x][y] = 180
      y += wavelen

  chart_matrix = numpy.rot90(chart_matrix, 3) # rotates 270
  chart_matrix = numpy.fliplr(chart_matrix)
  chart_image = \
    PIL.Image.fromstring('RGB', (500, 100), chart_matrix.astype('b').tostring())
  chart_photo = PIL.ImageTk.PhotoImage(image=chart_image)
  chart.create_image(0, 0, image=chart_photo, anchor=NW)

  cam_matrix_copy = numpy.copy(cam_matrix)
  center_track = []
  longest_train = None
  max_length = 0
  for train in trains:
    if train['length'] > max_length:
      max_length = train['length']
      longest_train = train
  last_train_y = None
  last_train_x = None
  for x in xrange(len(longest_train['center_ys'])):
    y = longest_train['center_ys'][x]
    if y > -1:
      if last_train_x and last_train_y:
        for x2 in xrange(last_train_x, x):
          progress = (x2 - last_train_x) / float(x - last_train_x)
          y2 = int(progress * y + (1 - progress) * last_train_y)
          #cam_matrix_annotated[x2][y2] = 0 
          center_track.append(y2)
      else:
        pass #for bold in [-1, 0, 1]:
        #  cam_matrix_annotated[x][y + bold] = 0 
      #cam_matrix[x][y - longest_train['widths'][x]] = 0
      #cam_matrix[x][y + longest_train['widths'][x]] = 0
      last_train_x = x
      last_train_y = y

  #straight_matrix = []
  #for x in xrange(len(cam_matrix)):
  #  slice = cam_matrix[x]
  #  mapping = lambda point: (point[0] * 0.5,)
  #  new_slice = scipy.ndimage.interpolation.geometric_transform(\
  #    slice, mapping, order=0, prefilter=False)
  #  straight_matrix.append(new_slice)
  #cam_matrix = straight_matrix # just to display it

  object2segment_lists = []
  recent_object_indexes = []
  for x in xrange(len(binary_non_staff_matrix)):
    slice = binary_non_staff_matrix[x]

    segments = []
    open_segment = None
    previous_pixel = 1 # 1 means white (off), 0 means black (on)
    for y in xrange(len(slice)):
      if slice[y] == 0:
        if previous_pixel == 1:
          open_segment = y
      else:
        if previous_pixel == 0:
          segments.append((x, open_segment, y - 1))
          open_segment = None
      previous_pixel = slice[y]

    matched_object_indexes = {}
    for segment in segments:
      matching_object = None
      for possible_object_index in recent_object_indexes:
        possible_object = object2segment_lists[possible_object_index]
        last_segments = possible_object[-1]
        for last_segment in last_segments:
          if segment[1] < last_segment[2] and \
             segment[2] > last_segment[1]:
            matching_object = possible_object
            matched_object_indexes[possible_object_index] = True
            break
        if matching_object:
          break # break out of both loops
      if matching_object:
        matching_object[-1].append(segment)
      else:
        object2segment_lists.append([[segment]])
        matched_object_indexes[len(object2segment_lists) - 1] = True
    recent_object_indexes = matched_object_indexes.keys()

  object2bounds = []
  for segment_lists in object2segment_lists:
    min_x, max_x, min_y, max_y = None, None, None, None
    for segments in segment_lists:
      for segment in segments:
        x, y0, y1 = segment
        if min_x == None:
          min_x = x
        max_x = x
        if min_y == None or y0 < min_y:
          min_y = y0
        if max_y == None or y1 > max_y:
          max_y = y1
    object2bounds.append((min_x, max_x, min_y, max_y))
    if 10 < (max_x - min_x) < 40 and \
       5 < (max_y - min_y) < 10:
      for x in xrange(min_x, max_x + 1):
        cam_matrix_annotated[x][min_y] = 168 \
          if cam_matrix[x][max_y] > 0 else 128
        cam_matrix_annotated[x][max_y] = 168 \
          if cam_matrix[x][max_y] > 0 else 128
      for y in xrange(min_y, max_y + 1):
        cam_matrix_annotated[min_x][y] = 168 \
          if cam_matrix[min_x][y] > 0 else 128
        cam_matrix_annotated[max_x][y] = 168 \
          if cam_matrix[max_x][y] > 0 else 128
      mid_x = (max_x + min_x) / 2
      mid_y = (max_y + min_y) / 2
      staff_y = (mid_y - best_center_y) / wavelen
      notes = ['hi b', 'hi a', 'tenor g', 'tenor f', 'tenor e', \
        'tenor d', 'tenor c', 'tenor b', 'tenor a', 'bass g', ' ', \
        'soprano g', 'soprano a', 'hi f', 'hi e', 'hi d', 'hi c']
      note = notes[int(round(staff_y * 2))]
      print mid_x, staff_y, note
      cam_matrix_annotated[mid_x][best_center_y + (staff_y * wavelen)] = 255

  #augmented_binary_non_staff_matrix = \
  #  numpy.array(augmented_binary_non_staff_matrix)
  #cam_matrix = numpy.array(augmented_binary_non_staff_matrix * 255)
  #cam_matrix = numpy.array(partially_erased_pure_white_matrix)
  #cam_matrix = numpy.array(blur_matrix)
  cam_matrix = numpy.array(cam_matrix_annotated)
  #cam_matrix = numpy.array(cam_matrix)
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
