import numpy as np
import matplotlib.pyplot as plt

t = np.arange(256)
st = np.sin((t + 20) / 256.0 * 2 * np.pi * 4.0)
sp = np.fft.fft(st)
#plt.plot(freq, sp.real, freq, sp.imag)
#plt.plot(st)
#plt.show()

max = None
best_bucket = None
for x in range(128):
  if max == None or abs(sp[x]) > max:
    max = abs(sp[x])
    best_bucket = x
print 'best_bucket', best_bucket 
num_cycles = np.fft.fftfreq(256)[best_bucket] * 256
print 'num cycles', num_cycles
wave_length = 256 / num_cycles
print 'wave len', wave_length

phase = (np.angle(sp[best_bucket]) / np.pi / 2) + 0.5
phase += 0.75
if phase >= 1.0:
  phase -= 1.0
print 'phase', phase

x = ((-phase - 0.75) * wave_length)
while x < 255:
  st[int(x)] += 1
  x += wave_length
plt.plot(st)
plt.show()

#sp = np.fft.fft(st)
#freq = np.fft.fftfreq(t.shape[-1])
#plt.plot(freq, sp.real, freq, sp.imag)
#plt.show()
