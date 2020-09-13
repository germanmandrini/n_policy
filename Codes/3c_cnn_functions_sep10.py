import torch
import numpy as np
from torch import nn,optim
from torch.utils.data import Dataset, DataLoader
import pyreadr
import torch.nn.functional as F
import matplotlib.pyplot as plt 
from torch.autograd import Variable
import pandas as pd

# Define the class to get our dataset.
class Data(Dataset):
    def __init__(self, Dataset, x, y):
        self.x=torch.FloatTensor(Dataset[x].values)
        self.y=torch.FloatTensor(Dataset[y].values)
        self.y=self.y.view(-1,1)
        self.len=self.x.shape[0]
    def __getitem__(self,index):         
        return self.x[index],self.y[index]
    def __len__(self):
        return self.len
    
def normalize(df,pred_vars, calculate_parameters = False ): #only normalizes the pred_vars and eonr
    result = df.copy()
    if calculate_parameters:
        frame = { 'mean': df.mean(), 'std': df.std()} 
        parameters = pd.DataFrame(frame) 

        parameters.reset_index(inplace=True) # Resets the index, makes factor a column  
        parameters = parameters[parameters['index'].isin(pred_vars+['eonr'])]
        parameters.to_pickle("/home/germanm2/n_policy_box/Data/files_rds/normalization_parameters.pkl")
    else:
        parameters = pd.read_pickle("/home/germanm2/n_policy_box/Data/files_rds/normalization_parameters.pkl")
    parameters_names = parameters['index']
    parameters_names_both = parameters_names[parameters_names.isin(list(df))]    
    for feature_name in parameters_names_both:
        #print(feature_name)
        mean_value = parameters.loc[(parameters['index'] == feature_name), 'mean']
        std_value = parameters.loc[(parameters['index'] == feature_name), 'std']
        result[feature_name] = (df[feature_name] - mean_value.values)/std_value.values
    return result    

def normalize_back(df): #only normalizes_back the pred_vars and eonr available at df
    parameters = pd.read_pickle("/home/germanm2/n_policy_box/Data/files_rds/normalization_parameters.pkl")
    result = df.copy()
    parameters_names = parameters['index']
    parameters_names_both = parameters_names[parameters_names.isin(list(df))]    
    for feature_name in parameters_names_both:
        mean_value = parameters.loc[(parameters['index'] == feature_name), 'mean']
        std_value = parameters.loc[(parameters['index'] == feature_name), 'std']
        result[feature_name] = (df[feature_name]  * std_value.values) + mean_value.values
    return result

# Create the class for model (with Dropout)
class Net(nn.Module):
    # Constructor
    def __init__(self, cols, p=0.1):
        super(Net, self).__init__()
        self.drop = nn.Dropout(p=p)
        self.hidden1 = torch.nn.Linear(cols, 30)   # hidden layer
        self.hidden2 = torch.nn.Linear(30, 5)   # hidden layer
        #self.hidden3 = torch.nn.Linear(5, 5)   # hidden layer
        #self.hidden4 = torch.nn.Linear(5, 5)   # hidden layer
        self.predict = torch.nn.Linear(5, 1)   # output layer

    def forward(self, x):
        x = F.relu(self.drop(self.hidden1(x)))      # activation function for hidden layer
        x = F.relu(self.drop(self.hidden2(x)))      # activation function for hidden layer
        #x = F.relu(self.drop(self.hidden3(x)))      # activation function for hidden layer
        #x = F.relu(self.drop(self.hidden4(x)))      # activation function for hidden layer
        x = self.predict(x)             # linear output
        return x

def train(training_set2, model, criterion, optimizer, learning_rate, epochs=5, plot_epoch = False):
    for epoch in range(epochs):
         #all the samples are used for training because they fit in memory
        optimizer.zero_grad()
        yhat = model(training_set2.x)
        loss=criterion(yhat,training_set2.y)
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
    
def build_cnn(TrainSet_eonr2_df, policy, pred_vars):
    training_set = normalize(TrainSet_eonr2_df, pred_vars, calculate_parameters = False)
    training_set2=Data(training_set, x = pred_vars, y = 'eonr')
    
    cols = len(pred_vars)

    # Create our model 
    model = Net(cols, p=0.3)
    model.train()

    torch.manual_seed(0)
    learning_rate = 0.01
    criterion = torch.nn.MSELoss(reduction='mean')  # this is for regression mean squared loss 
    # criterion = torch.nn.L1Loss() # this is for regression mean absolute loss 

    optimizer=torch.optim.Adam(model.parameters(), lr=learning_rate)

    train(training_set2, model, criterion, optimizer, learning_rate, epochs=1000, plot_epoch = False)
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

    prediction_set = normalize(prediction_set_aggregated_df, pred_vars, calculate_parameters = False)
    prediction_set_tensor = Variable(torch.FloatTensor(prediction_set[pred_vars].values)) #we don't use the data class, because there is no eonr column

    y_pred = model_load(prediction_set_tensor) #This outputs the value for regression
    y_pred=y_pred.data[:,0].numpy()

    prediction_set['eonr'] = y_pred #needed to have that name for the normalization function
    prediction_set2 = normalize_back(prediction_set)
    # prediction_set2 = prediction_set2.rename(columns={"eonr": "eonr_pred_cnn"}) 

    prediction_set_aggregated_df['eonr_pred']= prediction_set2['eonr']
    return prediction_set_aggregated_df