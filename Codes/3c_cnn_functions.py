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
import torch.nn.functional as F
from sklearn.utils import shuffle
from torch.autograd import Variable

class Net(torch.nn.Module):
    def __init__(self, cols, size_hidden, n_output):
        super(Net, self).__init__()
        self.hidden = torch.nn.Linear(cols, size_hidden)   # hidden layer
        self.predict = torch.nn.Linear(size_hidden, n_output)   # output layer

    def forward(self, x):
        x = F.relu(self.hidden(x))      # activation function for hidden layer
        x = self.predict(x)             # linear output
        return x
    
def build_cnn(TrainSet_eonr2_df, policy):
    y_train = TrainSet_eonr2_df['eonr']
    X_train = TrainSet_eonr2_df.drop('eonr', axis=1)
    #Define training hyperprameters.
    batch_size = 50
    num_epochs = 200
    learning_rate = 0.01
    size_hidden= 100

    #Calculate some other hyperparameters based on data.  
    batch_no = len(X_train) // batch_size  #batches
    cols=X_train.shape[1] #Number of columns in input matrix
    n_output=1
    #Create the model
    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    # Assume that we are on a CUDA machine, then this should print a CUDA device:
    print("Executing the model on :",device)
    
    net = Net(cols, size_hidden, n_output)
    #Adam is a specific flavor of gradient decent which is typically better
    optimizer = torch.optim.Adam(net.parameters(), lr=learning_rate)
    #optimizer = torch.optim.SGD(net.parameters(), lr=0.2)
    criterion = torch.nn.MSELoss(size_average=False)  # this is for regression mean squared loss
    X_train=X_train.values
    y_train=y_train.values
    running_loss = 0.0
    for epoch in range(num_epochs):
        #Shuffle just mixes up the dataset between epocs
        X_train, y_train = shuffle(X_train, y_train)
        # Mini batch learning
        for i in range(batch_no):
            start = i * batch_size
            end = start + batch_size
            inputs = Variable(torch.FloatTensor(X_train[start:end]))
            labels = Variable(torch.FloatTensor(y_train[start:end]))
            # zero the parameter gradients
            optimizer.zero_grad()

            # forward + backward + optimize
            outputs = net(inputs)
            #print("outputs",outputs)
            #print("outputs",outputs,outputs.shape,"labels",labels, labels.shape)
            loss = criterion(outputs, torch.unsqueeze(labels,dim=1))
            loss.backward()
            optimizer.step()

            # print statistics
            running_loss += loss.item()

        print('Epoch {}'.format(epoch+1), "loss: ",running_loss)
        running_loss = 0.0
        path = '/home/germanm2/n_policy_box/Data/files_rds/cnn_models/'+ policy + '.pth'
        torch.save(net.state_dict(), path)
        
def predict_cnn(prediction_set_aggregated_dt, policy):
    #Initialize the eonr model:
    cols=21 #Number of columns in input matrix        
    n_output=1        
    size_hidden= 100  
    
    #Load the cnn model
    path = '/home/germanm2/n_policy_box/Data/files_rds/cnn_models/'+ policy + '.pth'
    # net = torch.load(path)
    net.load_state_dict(torch.load(path))
    net.eval()
    
    #Get X data ready
    X_pred=prediction_set_aggregated_dt.values
    X = Variable(torch.FloatTensor(X_pred)) 
  
    #Make predictions
    y_pred = net(X) #This outputs the value for regression

    y_pred=y_pred.data[:,0].numpy()

    prediction_set_aggregated_dt['eonr_pred'] = y_pred
    return(prediction_set_aggregated_dt)