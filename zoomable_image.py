import scipy.misc
import scipy.misc.pilutil
import numpy
from Tkinter import *
import PIL.Image
import PIL.ImageTk

class ZoomableImage(Frame):
  def __init__(self, root, pil_image, canvas_w, canvas_h, all_settings):
    Frame.__init__(self, root, bd=2, relief=SUNKEN)
    self.pil_image = pil_image
    self.canvas_w = canvas_w
    self.canvas_h = canvas_h
    settings = all_settings.get('ZoomableImage', {})

    self.zoom = settings.get('zoom', 1)
    self.image_w, self.image_h = pil_image.size
    self.image_num = None

    self.grid_rowconfigure(0, weight=1)
    self.grid_columnconfigure(0, weight=1)
    self.xscrollbar = Scrollbar(self, orient=HORIZONTAL)
    self.xscrollbar.grid(row=1, column=0, sticky=E+W)
    self.yscrollbar = Scrollbar(self)
    self.yscrollbar.grid(row=0, column=1, sticky=N+S)
    self.canvas = Canvas(self,
      width=self.canvas_w, height=self.canvas_h,
      scrollregion=(0, 0, self.image_w, self.image_h))
    self.scroll_x = settings.get('scroll_x', 0.0)
    self.scroll_y = settings.get('scroll_y', 0.0)

    self.canvas.grid(row=0, column=0, sticky=N+S+E+W)
    self.canvas.config(yscrollcommand=self.yscrollbar.set)
    self.canvas.config(xscrollcommand=self.xscrollbar.set)
    
    self.xscrollbar.config(command=self.xview)
    self.yscrollbar.config(command=self.yview)

  def scroll_w(self):
    return (float)(self.canvas_w) / self.image_w / self.zoom
  def scroll_h(self):
    return (float)(self.canvas_h) / self.image_h / self.zoom

  def update_canvas(self):
    if self.image_num:
      self.canvas.delete(self.image_num)
  
    if self.scroll_x < 0.0:
      self.scroll_x = 0.0
    if self.scroll_x > 1.0 - self.scroll_w():
      self.scroll_x = 1.0 - self.scroll_w()
    if self.scroll_y < 0.0:
      self.scroll_y = 0.0
    if self.scroll_y > 1.0 - self.scroll_h():
      self.scroll_y = 1.0 - self.scroll_h()

    source_x = (int)(self.scroll_x * self.image_w)
    source_y = (int)(self.scroll_y * self.image_h)
    crop_window = (source_x, source_y,
      source_x + (self.canvas_w / self.zoom),
      source_y + (self.canvas_h / self.zoom))
    im_resized = \
      self.pil_image.crop(crop_window).resize((self.canvas_w, self.canvas_h))
  
    self.photo = PIL.ImageTk.PhotoImage(image=im_resized)
    self.image_num = self.canvas.create_image(0, 0, image=self.photo, anchor=NW)
    self.xscrollbar.set(self.scroll_x, self.scroll_x + self.scroll_w())
    self.yscrollbar.set(self.scroll_y, self.scroll_y + self.scroll_h())

  def xview(self, arg1, arg2=None, arg3=None):
    if arg1 == 'moveto':
      self.scroll_x = float(arg2)
      self.update_canvas()
    elif arg1 == 'scroll':
      if arg3 == 'pages':
        self.scroll_x += int(arg2) * self.scroll_w()
      elif arg3 == 'units':
        self.scroll_x += float(arg2) * 50 / self.zoom / self.image_w
      self.update_canvas()
    else:
      print ('unknown command to xview', arg1, arg2, arg3)
  def yview(self, arg1, arg2=None, arg3=None):
    if arg1 == 'moveto':
      self.scroll_y = float(arg2)
      self.update_canvas()
    elif arg1 == 'scroll':
      if arg3 == 'pages':
        self.scroll_y += int(arg2) * self.scroll_h()
      elif arg3 == 'units':
        self.scroll_y += float(arg2) * 50 / self.zoom / self.image_h
      self.update_canvas()
    else:
      print ('unknown command to yview', arg1, arg2, arg3)

  def get_all_settings(self):
    return { 'ZoomableImage': {
      'zoom': self.zoom,
      'scroll_x': self.scroll_x,
      'scroll_y': self.scroll_y,
    } }
