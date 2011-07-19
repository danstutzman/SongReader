import Tkinter
from PIL import Image, ImageTk
import numpy

data = numpy.array(numpy.random.random((400,500)) * 100, dtype=int)
root = Tkinter.Tk()
frame = Tkinter.Frame(root, width=500, height=400)
frame.pack()
canvas = Tkinter.Canvas(frame, width=500,height=400)
canvas.place(x=-2,y=-2)
im=Image.fromstring('L', (data.shape[1], data.shape[0]), data.astype('b').tostring())
photo = ImageTk.PhotoImage(image=im)
canvas.create_image(0,0,image=photo,anchor=Tkinter.NW)
root.update()
#data=numpy.roll(data,-1,1)
root.mainloop()
