import datetime
import matplotlib
matplotlib.use('svg')
import cv2
from matplotlib import pyplot as plt
import numpy as np
import sys

white = [255, 255, 255]


def plot(data, name):
    datax = [a[0] for a in data]
    datay = [a[1] for a in data]
    plt.xlim(0, 21)
    fig, ax = plt.subplots(1, 1)
    ax.plot(datax, datay)
    plt.savefig(f"{name}.svg")
    print('file written')


def get_datetime(hours):
    begin = datetime.datetime(year=2020, month=1, day=1)
    return begin + datetime.timedelta(hours=hours)

def show(img, cmap='gray'):
    plt.imshow(img, cmap=cmap)
    # plt.show()


def get_min_max_white(img_slice):
    mx = len(img_slice)
    mn = 0
    middle = len(img_slice)//2
    for a in range(middle):
        lower = middle - a
        if not np.array_equal(img_slice[lower], white):
            mn = lower + 1
            break
    for a in range(middle):
        upper = middle + a
        if not np.array_equal(img_slice[upper], white):
            mx = upper - 1
            break
    return mx, mn


def find_horizontal_lines(img):
    white_score_thresh = 0.55
    white = 255 * 3 * len(img[:, 0])
    highest = 0
    lowest = None
    for i, line in enumerate(img):
        white_score = np.sum(line)/white
        if white_score < white_score_thresh:
            if lowest is None:
                lowest = i
            highest = i
    return lowest, highest


def get_line_length(line):
    start = None
    end = None
    for i, a in enumerate(line):
        if not np.array_equal(a, white):
            start = i
            break
    for i, a in enumerate(line[start:]):
        if np.array_equal(a, white):
            end = i - 1 + start
            break
    return start, end

# long map(long x, long in_min, long in_max, long out_min, long out_max) {
#   return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
# }


def map_coord(x, in_min, in_max, out_min, out_max):
    return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min


def digitize(img_path):
    img = cv2.imread(img_path)
    # show(img)
    width = len(img[0, :])
    height = len(img[:, 0])
    max_height, min_height = get_min_max_white(img[:, 10])
    max_width, min_width = get_min_max_white(img[max_height - 2, :])
    cropped_img = img[min_height:max_height, min_width:max_width]
    # show(cropped_img)

    imgray = cv2.cvtColor(cropped_img, cv2.COLOR_BGR2GRAY)
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3, 3))
    # show(imgray)

    ret, thresh = cv2.threshold(imgray, 127, 255, cv2.THRESH_BINARY_INV)
    # show(thresh)

    erosion = cv2.erode(thresh, kernel, iterations=2)
    # show(erosion)

    nonzero = cv2.findNonZero(erosion)

    line_dict = {a[0][0]: [] for a in nonzero}
    for a in nonzero:
        line_dict[a[0][0]].append(a[0][1])

    line = []
    for a, b in line_dict.items():
        line.append([a, int(np.round(np.mean(b)))])
    line.sort(key=lambda x: x[0])
    e = 2
    for p in line:
        cropped_img[p[1]-e:p[1]+e, p[0]-e: p[0]+e] = [29, 255, 0]

    top_axis, bottom_axis = find_horizontal_lines(cropped_img)
    start, end = get_line_length(cropped_img[bottom_axis, :])
    # print(bottom_axis)
    # print(top_axis)
    for a in range(end - start):
        cropped_img[bottom_axis, a+start] = [29, 255, 0]
        cropped_img[top_axis, a+start] = [29, 255, 0]

    cropped_axis_img = cropped_img[top_axis:bottom_axis, line[0][0]:line[-1][0]]
    # show(cropped_axis_img)
    mapped_line = []
    for cood in line:
        mapped_line.append([map_coord(cood[0], start, end, 0, 24),
                            map_coord(cood[1], bottom_axis, top_axis, 0, 21)])
    # show(cropped_img)
    with open(f'{img_path}.csv', 'w') as f:
        for l in mapped_line:
            d = get_datetime(l[0])
            f.write(f'{d}\t{l[1]}\n')
    # plot(mapped_line, img_path)

if __name__ == "__main__":
    images = sys.argv[1:]
    for i in images:
        digitize(i)