import sys
import numpy as np
import cv2
import datetime
import matplotlib
matplotlib.use('svg')

white = [255, 255, 255]


def plot(data, name):
    from matplotlib import pyplot as plt
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


def show(name, img):
    cv2.namedWindow(name, cv2.WINDOW_NORMAL)
    cv2.resizeWindow(name, 500, 500)
    cv2.imshow(name, img)
    cv2.waitKey(0)
    cv2.destroyAllWindows()


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


def find_horizontal_lines(invert_img):
    nonzero_score_thresh = 0.80
    highest = 0
    lowest = None
    for i, line in enumerate(invert_img):
        nonzero = cv2.findNonZero(line)
        if nonzero is None:
            nonzero = []
        nonzero_score = len(nonzero)/len(line)
        if nonzero_score > nonzero_score_thresh:
            if lowest is None:
                lowest = i
            highest = i
    return lowest, highest


def get_longest_line_length(line):
    mid = len(line)//2
    start = None
    end = None
    for i, a in enumerate(line[:mid]):
        if np.array_equal(line[mid-i], white):
            start = mid - i + 1
            break
    for i, a in enumerate(line[mid:]):
        if np.array_equal(a, white):
            end = i - 1 + mid
            break
    return start, end

# long map(long x, long in_min, long in_max, long out_min, long out_max) {
#   return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
# }


def map_coord(x, in_min, in_max, out_min, out_max):
    return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min


def crop_image(img):
    max_height, min_height = get_min_max_white(img[:, 10])
    max_width, min_width = get_min_max_white(img[max_height - 2, :])
    return img[min_height:max_height, min_width:max_width]


def get_raw_data(imgray):
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (3, 3))

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
        line.append([a, int(np.round(np.median(b)))])
    line.sort(key=lambda x: x[0])
    return line


def debug_img(cropped_img, line, top_axis, bottom_axis, name):
    # paint line
    e = 2
    for p in line:
        cropped_img[p[1]-e:p[1]+e, p[0]-e: p[0]+e] = [29, 255, 0]

    # paint axis
    start, end = get_longest_line_length(cropped_img[bottom_axis, :])
    for a in range(end - start):
        cropped_img[bottom_axis, a+start] = [29, 255, 0]
        cropped_img[top_axis, a+start] = [29, 255, 0]
    show(name, cropped_img)


def digitize(img_path, debug=False):
    img = cv2.imread(img_path)
    # show(img)
    # width = len(img[0, :])
    # height = len(img[:, 0])
    cropped_img = crop_image(img)
    # show(cropped_img)

    imgray = cv2.cvtColor(cropped_img, cv2.COLOR_BGR2GRAY)
    # show(imgray)
    line = get_raw_data(imgray)

    iminvert = cv2.bitwise_not(imgray)
    top_axis, bottom_axis = find_horizontal_lines(iminvert)
    start, end = get_longest_line_length(cropped_img[bottom_axis, :])
    print(start, end)
    print(bottom_axis)
    print(top_axis)

    mapped_line = []
    for cood in line:
        mapped_line.append([map_coord(cood[0], start, end, 0, 24),
                            map_coord(cood[1], bottom_axis, top_axis, 0, 21)])
    # show(cropped_img)

    with open(f'{img_path}.csv', 'w') as f:
        for l in mapped_line:
            d = get_datetime(l[0])
            f.write(f'{d}\t{l[1]}\n')

    plot(mapped_line, img_path)
    if debug:
        debug_img(cropped_img, line, top_axis, bottom_axis, img_path)
        # show('iminvert', iminvert)


if __name__ == "__main__":
    images = sys.argv[1:]
    debug = '--debug' in sys.argv
    for i in images:
        if i == '--debug':
            continue
        digitize(i, debug=debug)
