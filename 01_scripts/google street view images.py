#please enter your API_key for Street View Static API in line 44
import urllib.request
import pandas as pd
import os
from os import listdir
from io import BytesIO
import tarfile
import tempfile
import sys
from matplotlib import gridspec
from matplotlib import pyplot as plt
import numpy as np
from PIL import Image
import cv2 as cv
from tqdm import tqdm
import IPython
from sklearn.metrics import confusion_matrix
from tabulate import tabulate
import tensorflow as tf
print(tf.__version__)

def download(url, path, n):   
  try:
    conn = urllib.request.urlopen(url)
    f = open(path, 'wb') 
    f.write(conn.read()) 
    f.close()
    print(n,' success!')
  except:
    print(n, 'error:', url)
    
input_path = "E:/GSD/2022 Summer/RA/location-analysis/"
csv = input_path + "02_data/streetview-sites.csv"
location = pd.read_csv(csv)
print(location)

img_path=input_path+'google street view images'
if not os.path.exists(img_path):
  os.makedirs(img_path, exist_ok=False)
  print('New dir google street view images has been created.')
else:
  print("dir google street view images exist")

API_key = ""#your key here
for i in range(0,len(location)): 
  lng = str(location.iloc[i,1])
  lat = str(location.iloc[i,2]) 
  path = img_path +"/"+ str(i) +"_" +lat + "_" + lng + "_-5.JPG"  
  url = "https://maps.googleapis.com/maps/api/streetview?size=800x450&location="+lat+","+lng+"&pitch=-5&fov=110&radius=100&return_error_code=true&key="+API_key  
  download(url, path, i) 
  
all_files = [f for f in listdir(img_path)]
### Get only jpg files
jpg_files = list(filter(lambda x: x[-4:] == ('.JPG') or x[-4:] == ('.jpg'), all_files))
jpg_files.sort()

if len(jpg_files) == 0:
  print ('No JPG files found!')
print("JPG Files in the folder:",len(jpg_files))
print (jpg_files)


#original_code: 'https://github.com/lexfridman/mit-deep-learning/blob/master/tutorial_driving_scene_segmentation/tutorial_driving_scene_segmentation.ipynb'

seg_path=img_path+'/segmentation'
if not os.path.exists(seg_path):
  print('Making new dir segmentation.')
  os.makedirs(seg_path, exist_ok=False)
else:
  print('Segmentation dir already exist.')
os.chdir(seg_path)

# Comment this out if you want to see Deprecation warnings
import warnings
warnings.simplefilter("ignore", DeprecationWarning)

################

class DeepLabModel(object):
    """Class to load deeplab model and run inference."""
    FROZEN_GRAPH_NAME = 'frozen_inference_graph'
 
    def __init__(self, tarball_path):
        """Creates and loads pretrained deeplab model."""
        self.graph = tf.Graph()
        graph_def = None
 
        # Extract frozen graph from tar archive.
        tar_file = tarfile.open(tarball_path)
        for tar_info in tar_file.getmembers():
            if self.FROZEN_GRAPH_NAME in os.path.basename(tar_info.name):
                file_handle = tar_file.extractfile(tar_info)
                graph_def = tf.compat.v1.GraphDef.FromString(file_handle.read())
                # graph_def = tf.GraphDef.FromString(file_handle.read())
                break
        tar_file.close()
 
        if graph_def is None:
            raise RuntimeError('Cannot find inference graph in tar archive.')
 
        with self.graph.as_default():
            tf.import_graph_def(graph_def, name='')
        self.sess = tf.compat.v1.Session(graph=self.graph)
        # self.sess = tf.Session(graph=self.graph)
 
    def run(self, image, INPUT_TENSOR_NAME = 'ImageTensor:0', OUTPUT_TENSOR_NAME = 'SemanticPredictions:0'):
        """Runs inference on a single image.
 
        Args:
            image: A PIL.Image object, raw input image.
            INPUT_TENSOR_NAME: The name of input tensor, default to ImageTensor.
            OUTPUT_TENSOR_NAME: The name of output tensor, default to SemanticPredictions.
 
        Returns:
            resized_image: RGB image resized from original input image.
            seg_map: Segmentation map of `resized_image`.
        """
        width, height = image.size
        target_size = (2049,1025)  # size of Cityscapes images
        resized_image = image.convert('RGB').resize(target_size, Image.ANTIALIAS)
        batch_seg_map = self.sess.run(
            OUTPUT_TENSOR_NAME,
            feed_dict={INPUT_TENSOR_NAME: [np.asarray(resized_image)]})
        seg_map = batch_seg_map[0]  # expected batch size = 1
        if len(seg_map.shape) == 2:
            seg_map = np.expand_dims(seg_map,-1)  # need an extra dimension for cv.resize
        seg_map = cv.resize(seg_map, (width,height), interpolation=cv.INTER_NEAREST)
        return seg_map

def create_label_colormap():
    """Creates a label colormap used in Cityscapes segmentation benchmark.
 
    Returns:
        A Colormap for visualizing segmentation results.
    """
    colormap = np.array([
        [128,  64, 128],
        [244,  35, 232],
        [ 70,  70,  70],
        [102, 102, 156],
        [190, 153, 153],
        [153, 153, 153],
        [250, 170,  30],
        [220, 220,   0],
        [107, 142,  35],
        [152, 251, 152],
        [ 70, 130, 180],
        [220,  20,  60],
        [255,   0,   0],
        [  0,   0, 142],
        [  0,   0,  70],
        [  0,  60, 100],
        [  0,  80, 100],
        [  0,   0, 230],
        [119,  11,  32],
        [  0,   0,   0]], dtype=np.uint8)
    return colormap
 
 
def label_to_color_image(label):
    """Adds color defined by the dataset colormap to the label.
 
    Args:
        label: A 2D array with integer type, storing the segmentation label.
 
    Returns:
        result: A 2D array with floating type. The element of the array
            is the color indexed by the corresponding element in the input label
            to the PASCAL color map.
 
    Raises:
        ValueError: If label is not of rank 2 or its value is larger than color
            map maximum entry.
    """
    if label.ndim != 2:
        raise ValueError('Expect 2-D input label')
 
    colormap = create_label_colormap()
 
    if np.max(label) >= len(colormap):
        raise ValueError('label value too large.')
 
    return colormap[label]

################

 
def vis_segmentation(image, seg_map):
    """Visualizes input image, segmentation map and overlay view."""
    plt.figure(figsize=(20, 20))
 
    seg_image = label_to_color_image(seg_map).astype(np.uint8)
    plt.imshow(seg_image)
    plt.axis('off')
    # save segmentation images
    plt.savefig(str(image_id)+'_seg.jpg',bbox_inches='tight')
    
    plt.imshow(image)
    plt.imshow(seg_image, alpha=0.7)
    plt.axis('off')
    # save overlay images
    plt.savefig(str(image_id)+'_seg_overlay.jpg',bbox_inches='tight')
    
    plt.close()
 
LABEL_NAMES = np.asarray([
    'road', 'sidewalk', 'building', 'wall', 'fence', 'pole', 'traffic light',
    'traffic sign', 'vegetation', 'terrain', 'sky', 'person', 'rider', 'car', 'truck',
    'bus', 'train', 'motorcycle', 'bicycle', 'void'])
 
FULL_LABEL_MAP = np.arange(len(LABEL_NAMES)).reshape(len(LABEL_NAMES), 1)
FULL_COLOR_MAP = label_to_color_image(FULL_LABEL_MAP)

################

# MODEL_NAME = 'mobilenetv2_coco_cityscapes_trainfine' 
#faster: pre-trained model based on MobileNetV2 network backbone
MODEL_NAME = 'xception65_cityscapes_trainfine' 
#more accurate: pre-trained model based on Xception65 network backbone
 
_DOWNLOAD_URL_PREFIX = 'http://download.tensorflow.org/models/'
_MODEL_URLS = {
    'mobilenetv2_coco_cityscapes_trainfine':
        'deeplabv3_mnv2_cityscapes_train_2018_02_05.tar.gz',
    'xception65_cityscapes_trainfine':
        'deeplabv3_cityscapes_train_2018_02_06.tar.gz',
}
_TARBALL_NAME = 'deeplab_model.tar.gz'
try:
  tk = 'https://drive.google.com/uc?export=download&id=1NKfMlQSrECECSKIwFW_cQha0eNTGglnt'
  tkfile = urllib.request.urlopen(tk)
except:
  sys.exit()
model_dir = tempfile.mkdtemp()
tf.io.gfile.makedirs(model_dir)
 
download_path = os.path.join(model_dir, _TARBALL_NAME)
print('downloading model, this might take a while...')
urllib.request.urlretrieve(_DOWNLOAD_URL_PREFIX + _MODEL_URLS[MODEL_NAME], download_path)
print('download completed! loading DeepLab model...')
 
MODEL = DeepLabModel(download_path)
print('model loaded successfully!')

##############

extracted_features = []

for jpg in jpg_files:

  SAMPLE_IMAGE = img_path+"/"+jpg
  image_id = jpg.split('_')[0]
  latitude = float(jpg.split('_')[1])
  longitude = float(jpg.split('_')[2])

  print('running deeplab on '+image_id)
 
  def run_visualization(SAMPLE_IMAGE):
      """Inferences DeepLab model and visualizes result."""
      original_im = Image.open(SAMPLE_IMAGE)
      global seg_map
      seg_map = MODEL.run(original_im)
      vis_segmentation(original_im, seg_map)
 
  run_visualization(SAMPLE_IMAGE)
 
  #print (seg_map)
  total_pixels = seg_map.shape[0]*seg_map.shape[1]
  labels_array = list(np.unique(seg_map, return_counts=True)[0])
  frequency_array = list(np.unique(seg_map, return_counts=True)[1])
  label_indicies = []
  label_frequencies = []
 
  c = 0
  for label in LABEL_NAMES:
    if c in labels_array:
      label_indicies.append(c)
      label_frequencies.append(frequency_array[labels_array.index(c)])
    else:
      label_indicies.append(c)
      label_frequencies.append(0)
    c = c+1
 
  #print(label_indicies)
  #print(label_frequencies)
 
  road_score = (label_frequencies[0]/total_pixels)*100
  sidewalk_score = (label_frequencies[1]/total_pixels)*100
  building_score = (label_frequencies[2]/total_pixels)*100
  wall_score = (label_frequencies[3]/total_pixels)*100
  fence_score = (label_frequencies[4]/total_pixels)*100
  pole_score = (label_frequencies[5]/total_pixels)*100
  traffic_light_score = (label_frequencies[6]/total_pixels)*100
  traffic_sign_score = (label_frequencies[7]/total_pixels)*100
  vegetation_score = (label_frequencies[8]/total_pixels)*100
  terrain_score = (label_frequencies[9]/total_pixels)*100
  sky_score = (label_frequencies[10]/total_pixels)*100
  person_score = (label_frequencies[11]/total_pixels)*100
  rider_score = (label_frequencies[12]/total_pixels)*100
  car_score = (label_frequencies[13]/total_pixels)*100
  truck_score = (label_frequencies[14]/total_pixels)*100
  bus_score = (label_frequencies[15]/total_pixels)*100
  train_score = (label_frequencies[16]/total_pixels)*100
  motorcycle_score = (label_frequencies[17]/total_pixels)*100
  bicycle_score = (label_frequencies[18]/total_pixels)*100
  void_score = (label_frequencies[19]/total_pixels)*100
 
  built_score = building_score + wall_score
  paved_score = road_score + sidewalk_score
  auto_score = car_score + bus_score + truck_score + motorcycle_score
  human_score = person_score + rider_score
  nature_score = terrain_score + vegetation_score
 
 
  extracted_features.append([image_id,built_score,paved_score,auto_score,sky_score,nature_score,human_score,latitude,longitude,road_score,sidewalk_score,building_score,wall_score,fence_score,pole_score,traffic_light_score,traffic_sign_score,vegetation_score,terrain_score,sky_score,person_score,rider_score,car_score,truck_score,bus_score,train_score,motorcycle_score,bicycle_score,void_score])

image_features = pd.DataFrame(extracted_features)

image_features.columns = ['image_id','built_score','paved_score','auto_score','sky_score','nature_score','human_score','latitude', 'longitude','road_score','sidewalk_score','building_score','wall_score','fence_score','pole_score','traffic_light_score','traffic_sign_score','vegetation_score','terrain_score','sky_score','person_score','rider_score','car_score','truck_score','bus_score','train_score','motorcycle_score','bicycle_score','void_score']
image_features = image_features.set_index('image_id')

os.chdir(img_path)

image_features.to_csv('google street view_image_features.csv')

print('Feature Extraction Completed Successfully!')
print('image_features.csv created')
