import torch
import numpy as np
from torch import nn,optim
from torch.utils.data import Dataset, DataLoader
import pyreadr
import torch.nn.functional as F
import matplotlib.pyplot as plt 
from torch.autograd import Variable

# # Define the class to get our dataset.
class Data(Dataset):
    def __init__(self, TrainSet_eonr2_df, pred_vars):
        self.x=torch.FloatTensor(TrainSet_eonr2_df[pred_vars].values)
        self.y=torch.FloatTensor(TrainSet_eonr2_df['eonr'].values)
        self.y=self.y.view(-1,1)
        self.len=self.x.shape[0]
    def __getitem__(self,index):         
        return self.x[index],self.y[index]
    def __len__(self):
        return self.len
    
# Create the class for model (with Dropout)
class Net(nn.Module):
    
    # Constructor
    def __init__(self, cols, p=0):
        super(Net, self).__init__()
        #self.drop = nn.Dropout(p=p)
        self.hidden1 = torch.nn.Linear(cols, 30)   # hidden layer
        self.hidden2 = torch.nn.Linear(30, 5)   # hidden layer
        self.hidden3 = torch.nn.Linear(5, 5)   # hidden layer
        self.hidden4 = torch.nn.Linear(5, 5)   # hidden layer
        self.predict = torch.nn.Linear(5, 1)   # output layer

    def forward(self, x):
        x = F.relu(self.hidden1(x))      # activation function for hidden layer
        x = F.relu(self.hidden2(x))      # activation function for hidden layer
        x = F.relu(self.hidden3(x))      # activation function for hidden layer
        x = F.relu(self.hidden4(x))      # activation function for hidden layer
        x = self.predict(x)             # linear output
        return x

def train(data_set, model,criterion, optimizer, learning_rate, epochs=5, plot_epoch = False):
    for epoch in range(epochs):
         #all the samples are used for training because they fit in memory
        optimizer.zero_grad()
        yhat = model(data_set.x)
        loss=criterion(yhat,data_set.y)
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()    
    
def build_cnn(TrainSet_eonr2_df, policy, pred_vars):
    data_set=Data(TrainSet_eonr2_df, pred_vars)
    
    cols = len(pred_vars)

    # Create our model 
    model = Net(cols, p=0.1)
    model.train()

    torch.manual_seed(0)
    learning_rate = 0.01
    criterion = torch.nn.MSELoss(reduction='mean')  # this is for regression mean squared loss 
    # criterion = torch.nn.L1Loss() # this is for regression mean absolute loss 

    optimizer=torch.optim.Adam(model.parameters(), lr=learning_rate)

    train(data_set, model,criterion, optimizer, learning_rate, epochs=1000, plot_epoch = False)

    path = '/home/germanm2/n_policy_box/Data/files_rds/cnn_models/'+ policy + '.pth'
    torch.save(model.state_dict(), path)

def predict_cnn(prediction_set_aggregated_df, policy, pred_vars):
    #Load the saved model
    #policy = 'ratio_5'
    cols = len(pred_vars)
    model_load = Net(cols)
    path = '/home/germanm2/n_policy_box/Data/files_rds/cnn_models/'+ policy + '.pth'
    model_load.load_state_dict(torch.load(path))
    model_load.eval()
    model_load.state_dict()
    X_pred = prediction_set_aggregated_df[pred_vars]
    X_pred=X_pred.values
    X_pred
    X = Variable(torch.FloatTensor(X_pred)) 
    y_pred = model_load(X) #This outputs the value for regression
    y_pred=y_pred.data[:,0].numpy()
    y_pred
    prediction_set_aggregated_df['eonr_pred'] = y_pred
    return(prediction_set_aggregated_df)