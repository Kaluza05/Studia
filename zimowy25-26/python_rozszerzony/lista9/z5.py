import matplotlib.pyplot as plt
import numpy as np
from PIL import Image
import os

def is_edge1(image,pos, threshold = 100, k = 1)-> bool:
    """
    return whether cell at pos is an edge or not
    """
    #average of neighbouring colors and distance between the avg and color
    m,n,_ = image.shape
    x,y = pos
    x_min = max(0,     x - k)
    x_max = min(m - 1, x + k)
    y_min = max(0,     y - k)
    y_max = min(n - 1, y + k)
    neighbours = image[x_min : x_max + 1, y_min : y_max + 1]

    diff = abs(neighbours - image[x,y])
    diff = diff.max(axis = -1)
    mse = np.mean(diff)
    #print(mse) if mse > threshold else _
    return mse > threshold

def is_edge2(image,pos, threshold = 100, k = 1)-> bool:
    """
    return whether cell at pos is an edge or not
    """
    #average of neighbouring colors and distance between the avg and color
    m,n,_ = image.shape
    x,y = pos
    x_min = max(0,     x - k)
    x_max = min(m - 1, x + k)
    y_min = max(0,     y - k)
    y_max = min(n - 1, y + k)
    neighbours = image[x_min : x_max + 1, y_min : y_max + 1]

    diff = abs(neighbours - image[x,y])
    diff = diff.mean(axis = -1)
    #print(diff,np.any(diff, where= lambda x : sum(x) > threshold) )
    return np.any(diff > threshold)

def find_edges(image: list[list[tuple[int,int,int]]], edge_func)-> list[list[bool]]:
    """
    returns a matrix of 0's and 1's whether the cell is an edge or not
    image is passed as a (M,N,3) matrix representing image of size MxN with RGB colors
    """
    m,n,_ = image.shape
    return [[edge_func(image,(r,c), threshold = 150, k = 2) for c in range(n)] for r in range(m)]

def main(image):
    """
    shows the image before and with only edges
    """

    edge_mask = np.array(find_edges(image, is_edge1),dtype = bool)
    edge_image = np.copy(image)
    edge_image[~edge_mask]  = [0,0,0]
    edge_image[edge_mask] = [255,255,255]

    edge_mask1 = np.array(find_edges(image, is_edge2),dtype = bool)
    edge_image1 = np.copy(image)
    edge_image1[~edge_mask1]  = [0,0,0]
    edge_image1[edge_mask1] = [255,255,255]
    
    plt.figure(figsize=(10, 5))

    # Original
    plt.subplot(1, 3, 1)
    plt.title("Original")
    plt.imshow(image)
    plt.axis("off")

    # Edge Image
    plt.subplot(1, 3, 2)
    plt.title("Edges")
    plt.imshow(edge_image)
    plt.axis("off")
    
    plt.subplot(1, 3, 3)
    plt.title("Edges")
    plt.imshow(edge_image1)
    plt.axis("off")
    plt.show()



SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
IMAGE1 = 'biedra.png'
IMAGE2 = 'times_square.jpg'
IMAGE3 = 'jablko.jpg'
IMAGE_PATH = SCRIPT_DIR +'\\' + IMAGE2


img = Image.open(IMAGE_PATH).convert("RGB")
img_arr = np.array(img)

main(img_arr)
