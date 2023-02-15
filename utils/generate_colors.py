import cv2
import numpy as np

colors = np.array([[[8 + 32 * i, 48, 224]] for i in range(7)], np.uint8)
out_colors = cv2.cvtColor(colors, cv2.COLOR_HSV2RGB)
hex_colors = ['#' + ''.join(hex(n)[2:] for n in c) for c in out_colors.squeeze()]

for i, c in enumerate(hex_colors):
    print(f'(fainter-{i} "{c}")')
