# https://machinelearningmastery.com/pytorch-tutorial-develop-deep-learning-models/
# https://colab.research.google.com/github/rpi-techfundamentals/website_spring_2020/blob/master/content/notebooks/20-deep-learning1/06-regression-bh-pytorch.ipynb#scrollTo=xD9PhAU7hoqT
#!pip install torchvision
import numpy as np
import pandas as pd
import pyreadr
import scipy
#Define the model 
import torch
import torch.nn as nn
from torch.autograd import Variable

def predict_cnn(prediction_set_aggregated_dt, policy)
    X_pred = prediction_set_aggregated_df[['rain_30', 'rain_60','rain_90', 't_max_30', 't_max_60', 't_max_90', 't_min_30', 't_min_60', 't_min_90', 'Y_prev',
    'Y_corn_lt_avg', 'day_sow', 'day_v5', 'lai_v5', 'whc', 'oc_20cm_v5', 'sw_dep_v5', 'n_0_60cm_v5', 'surfaceom_wt_v5', 'sand_40cm', 'clay_40cm']]
    X_pred=X_pred.values

    X = Variable(torch.FloatTensor(X_pred)) 
    
    #Load the cnn model
    path = '/home/germanm2/n_policy_box/Data/files_rds/cnn_models/'+ policy + '.pth'
    net = torch.load(path)

    y_pred = net(X) #This outputs the value for regression
    y_pred=y_pred.data[:,0].numpy()

    prediction_set_aggregated_df['eonr_pred'] = y_pred
    return(prediction_set_aggregated_dt)