# function to compute landscape metrics
import pylandstats as pls
import numpy as np
import pandas as pd

def compute_ls_metrics(name,img_type,tif):
    
    # read tif
    tif_ls = pls.Landscape(tif)

    # compute landscapte metircs
    number_of_patches = pls.Landscape.number_of_patches(tif_ls,class_val=1)
    area_mn = pls.Landscape.area_mn(tif_ls,class_val=1)
    patch_density = pls.Landscape.patch_density(tif_ls,class_val=1)
    largest_patch_index = pls.Landscape.largest_patch_index(tif_ls,class_val=1)
    landscape_shape_index = pls.Landscape.landscape_shape_index(tif_ls,class_val=1)
    effective_mesh_size = pls.Landscape.effective_mesh_size(tif_ls)
    euclidean_nearest_neighbor = pls.Landscape.euclidean_nearest_neighbor(tif_ls,class_val=1)

    stats = {'City':[name],
             'Img_type':[img_type],
             'number_of_patches':[number_of_patches],
             'area_mn':[area_mn],
             'patch_density':[patch_density],
             'largest_patch_index':[largest_patch_index],
             'landscape_shape_index':[landscape_shape_index],
             'effective_mesh_size':[effective_mesh_size],
             'euclidean_nearest_neighbor':[euclidean_nearest_neighbor]}


    # store metrics to df
    patch_metrics_df = pd.DataFrame(stats)
    
    # print out the process
    print(f'Landscapte metircs of {name}_{img_type} is computed.')
    
    return patch_metrics_df