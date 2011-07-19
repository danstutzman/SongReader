from zoomable_image import ZoomableImage

class Annotation:
  radius = 5
  def __init__(self, canvas, parent, world_x, world_y):
    self.canvas = canvas
    self.parent = parent
    self.world_x = world_x
    self.world_y = world_y
    self.num_on_canvas = self.canvas.create_rectangle(\
      -self.radius, -self.radius, self.radius, self.radius,
      fill='yellow')
    self.last_canvas_x = 0
    self.last_canvas_y = 0
    self.move_to(world_x, world_y)
  def canvas_x(self):
    return (self.world_x - (self.parent.image_w * self.parent.scroll_x)) * \
      self.parent.zoom
  def canvas_y(self):
    return (self.world_y - (self.parent.image_h * self.parent.scroll_y)) * \
      self.parent.zoom
  def move_to(self, world_x, world_y):
    self.world_x, self.world_y = world_x, world_y
    self.canvas.move(self.num_on_canvas,
      self.canvas_x() - self.last_canvas_x,
      self.canvas_y() - self.last_canvas_y)
    self.last_canvas_x, self.last_canvas_y = self.canvas_x(), self.canvas_y()
  def update_canvas(self):
    # have to move the point if zoom or scrolling changed
    self.move_to(self.world_x, self.world_y)
    # have to keep adding it to the top
    self.canvas.tag_raise(self.num_on_canvas)

class AnnotatedImage(ZoomableImage):
  points = []
  dragged_point = None
  def __init__(self, root, pil_image, canvas_w, canvas_h, all_settings):
    ZoomableImage.__init__(self,
      root, pil_image, canvas_w, canvas_h, all_settings)

    settings = all_settings.get('AnnotatedImage', {})
    default_points = [{'x':50, 'y':50}, {'x':70, 'y':50}]
    for point_dict in settings.get('points', default_points):
      annotation = Annotation( \
        self.canvas, self, point_dict['x'], point_dict['y'])
      self.points.append(annotation)

    self.canvas.bind('<1>', self.drag_start)
    self.canvas.bind('<B1-Motion>', self.drag_continue)
    self.canvas.bind('<ButtonRelease-1>', self.drag_continue)

  def update_canvas(self):
    ZoomableImage.update_canvas(self)
    for point in self.points:
      point.update_canvas()

  def drag_start(self, event):
    min_distance = None
    closest_point = None
    for point in self.points:
      distance = abs(point.canvas_x() - event.x) + \
                 abs(point.canvas_y() - event.y)
      if min_distance == None or distance < min_distance:
        min_distance = distance
        closest_point = point
    if min_distance and min_distance < point.radius * 4:
      self.dragged_point = closest_point
    else:
      self.dragged_point = None

    self.drag_continue(event)
  def drag_continue(self, event):
    if self.dragged_point:
      self.dragged_point.move_to(event.x, event.y)
      self.dragged_point.update_canvas()    

  def get_all_settings(self):
    all_settings = ZoomableImage.get_all_settings(self)
    points_list = []
    for point in self.points:
      points_list.append({'x': point.world_x, 'y': point.world_y})
    all_settings['AnnotatedImage'] = {'points': points_list}
    return all_settings
