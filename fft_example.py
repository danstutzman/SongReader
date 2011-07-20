import numpy as np
import matplotlib.pyplot as plt

def wavelen_and_first_peak(array):
  fft_num_buckets = 2048
  fft = np.fft.fft(array, fft_num_buckets)

  max = None
  best_bucket = None
  for x in range(fft_num_buckets / 2):
    if max == None or abs(fft[x]) > max:
      max = abs(fft[x])
      best_bucket = x

  num_cycles = np.fft.fftfreq(fft_num_buckets)[best_bucket] * len(array)
  wave_length = len(array) / num_cycles

  phase = (np.angle(fft[best_bucket]) / np.pi / 2) + 0.5
  phase += 0.75
  if phase >= 1.0:
    phase -= 1.0
  x = ((-phase - 0.75) * wave_length)

  return x, wave_length

t = np.arange(150)
st = np.sin((t - 160) / 220.0 * 2 * np.pi * 4.0)

x, wave_length = wavelen_and_first_peak(st)
while x < 150:
  if x >= 0:
    st[int(x)] += 0.5 
  x += wave_length
plt.plot(st)
plt.show()

#sp = np.fft.fft(st)
#freq = np.fft.fftfreq(t.shape[-1])
#plt.plot(freq, sp.real, freq, sp.imag)
#plt.show()
