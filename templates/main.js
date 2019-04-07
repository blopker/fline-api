const fileInput = document.getElementById("image-uploads");
fileInput.addEventListener("change", e => {
  process_fileapi(e.target.files[0]);
});

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
function process_local(i) {
  process(`./screenshots/${i}`);
}
// process_local("img.png");
// imgs.forEach(process_local);
