const white = 250;
const black = 5;
const imgs = [
  "Image-1.jpg",
  "IMG_2354.jpeg",
  "IMG_8041.PNG",
  "IMG_8073.PNG",
  "img.png",
  "Jessie_02_24_19.PNG",
  "Jessie_02_27_19.PNG",
  "IMG_2225.png",
  "IMG_2356.jpeg",
  "IMG_8042.PNG",
  "IMG_8074.PNG",
  "Jessie_02_22_19.PNG",
  "Jessie_02_25_19.PNG",
  "IMG_2239.png",
  "IMG_8040.PNG",
  "IMG_8043.PNG",
  "IMG_8095.PNG",
  "Jessie_02_23_19.PNG",
  "Jessie_02_26_19.PNG"
];

const median = arr => {
  const mid = Math.floor(arr.length / 2),
    nums = [...arr].sort((a, b) => a - b);
  return arr.length % 2 !== 0 ? nums[mid] : (nums[mid - 1] + nums[mid]) / 2;
};

function greyImg(img) {
  return img.grey({ algorithm: "green" });
}

function isBlack(px) {
  return px < black;
}

function isWhite(px) {
  return px > white;
}

function getCrop(img) {
  let maxC = 0;
  let minC = null;
  img.getColumn(1).forEach((px, i) => {
    if (isBlack(px)) {
      minC = minC || i;
      maxC = i;
    }
  });
  let maxR = null;
  img.getRow(maxC - 2).forEach((px, i) => {
    if (isBlack(px)) {
      maxR = i;
    }
  });
  return img.crop({ y: minC, height: maxC - minC, width: maxR });
}

function isLine(pxs) {
  return (
    pxs
      .map(px => {
        return !isBlack(px);
      })
      .reduce((p, c) => p + c) >
    pxs.length * 0.7
  );
}
function getGraphDimentions(img) {
  let minY = null;
  let maxY = 0;
  img.getColumn(Math.floor(img.width / 2)).forEach((px, i) => {
    if (!isBlack(px) && isLine(img.getRow(i))) {
      minY = minY || i;
      maxY = i;
    }
  });
  let minX = null;
  let maxX = 0;
  img.getRow(maxY).forEach((px, i) => {
    if (!isBlack(px)) {
      minX = minX || i;
      maxX = i;
    }
  });
  return { maxY, minY, maxX, minX };
}

function getRawData(img) {
  img = img
    .mask()
    .rgba8()
    .grey();
  img = img.erode({ iterations: 2 });
  data = [];
  for (let index = 0; index < img.width; index++) {
    const column = img
      .getColumn(index)
      .map((v, i) => (v > 10 ? i : -1))
      .filter(a => a > 0);
    let med = median(column);
    if (med) {
      data.push({ x: index, y: med });
    }
  }
  // console.log(data);
  // print(img);
  return data;
}

function map_coord(x, in_min, in_max, out_min, out_max) {
  return ((x - in_min) * (out_max - out_min)) / (in_max - in_min) + out_min;
}

function getGraphData(rawData, dems) {
  return rawData.map(d => {
    return {
      y: 21 - map_coord(d.y, dems.minY, dems.maxY, 0, 21),
      x: map_coord(d.x, dems.minX, dems.maxX, 0, 24)
    };
  });
}

function print(img, id = "None") {
  let el = document.createElement("img");
  el.id = id;
  el.width = 400;
  document.body.append(el);
  el.src = img.toDataURL();
}

function chart(graphData, id = "None") {
  let el = document.createElement("div");
  el.id = id;
  el.width = 400;
  document.body.append(el);
  el.src = img.toDataURL();
}

async function process(i) {
  let image = await IJS.Image.load(`./screenshots/${i}`);
  console.log(i);
  let grey = greyImg(image.rgba8()).invert();
  let crop = getCrop(grey);
  let rawData = getRawData(crop);
  let graphData = getGraphData(rawData, getGraphDimentions(crop));
  new Chartist.Line(
    ".ct-chart",
    { series: [graphData] },
    {
      axisX: {
        type: Chartist.AutoScaleAxis,
        onlyInteger: true
      },
      axisY: {
        type: Chartist.AutoScaleAxis,
        onlyInteger: true
      }
    }
  );
  // crop.paintPoints(data, { color: [29, 255, 0] });
  print(crop);
}
// imgs.forEach(process);
process("img.png");

// var data = {
//   // A labels array that can contain any sort of values
//   labels: ["Mon", "Tue", "Wed", "Thu", "Fri"],
//   // Our series array that contains series objects or in this case series data arrays
//   series: [[5, 2, 4, 2, 0]]
// };

// Create a new line chart object where as first parameter we pass in a selector
// that is resolving to our chart container element. The Second parameter
// is the actual data object.
