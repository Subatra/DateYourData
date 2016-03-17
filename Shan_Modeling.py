## The csv file saved in R environment is imported in python environment for further processing


# In[1]:

# Load Libraries

import pandas as pd
import numpy as np
get_ipython().magic(u'matplotlib inline')


# In[2]:

cd "E:\SS\AV\OnlineHack\Online Date Your Data\files"


# In[3]:

#Load data:
train = pd.read_csv('TrainD.csv')
test = pd.read_csv('TestD.csv')


# In[4]:

train.shape, test.shape


# In[5]:

train.dtypes


# In[6]:

import pandas as pd
import numpy as np
from sklearn.ensemble import GradientBoostingClassifier
from sklearn import cross_validation, metrics
from sklearn.grid_search import GridSearchCV

import matplotlib.pylab as plt
get_ipython().magic(u'matplotlib inline')
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 12, 4


# In[7]:

target='Is_Shortlisted'
Internship_ID = 'Internship_ID'
Student_ID = 'Student_ID'


# In[8]:

train['Is_Shortlisted'].value_counts()


# In[9]:

def modelfit(alg, dtrain, dtest, predictors, performCV=True, printFeatureImportance=True, cv_folds=5):
    #Fit the algorithm on the data
    alg.fit(dtrain[predictors], dtrain['Is_Shortlisted'])
        
    #Predict training set:
    dtrain_predictions = alg.predict(dtrain[predictors])
    dtrain_predprob = alg.predict_proba(dtrain[predictors])[:,1]
    
    #Perform cross-validation:
    if performCV:
        cv_score = cross_validation.cross_val_score(alg, dtrain[predictors], dtrain['Is_Shortlisted'], cv=cv_folds, scoring='roc_auc')
    
    #Print model report:
    print "\nModel Report"
    print "Accuracy : %.4g" % metrics.accuracy_score(dtrain['Is_Shortlisted'].values, dtrain_predictions)
    print "AUC Score (Train): %f" % metrics.roc_auc_score(dtrain['Is_Shortlisted'], dtrain_predprob)
    
    if performCV:
        print "CV Score : Mean - %.7g | Std - %.7g | Min - %.7g | Max - %.7g" % (np.mean(cv_score),np.std(cv_score),np.min(cv_score),np.max(cv_score))
                
    #Print Feature Importance:
    if printFeatureImportance:
        feat_imp = pd.Series(alg.feature_importances_, predictors).sort_values(ascending=False)
        feat_imp.plot(kind='bar', title='Feature Importances')
        plt.ylabel('Feature Importance Score')


# ### Baseline Model
# Since here the criteria is AUC, simply predicting the most prominent class would give an AUC of 0.5 always. 
# Another way of getting a baseline model is to use the algorithm without tuning, i.e. with default parameters.

# In[10]:

#Choose all predictors except target & IDcols
predictors = [x for x in train.columns if x not in [target, Internship_ID,Student_ID]]
gbm0 = GradientBoostingClassifier(random_state=10)
modelfit(gbm0, train, test, predictors)


# In[ ]:

pd.Series(gbm0.feature_importances_, predictors).sort_values(ascending=False)[1:30]


# In[ ]:

#Taking important features as predictors


# In[12]:

predictors1= ['Stip_range','Num_Applicant','No_of_openings','Internship_Duration.Months.','Is_SR_No','Diff_Intdl_StrD',
              'Minimum_Duration','Performance_10th','data4.Internship_categoryPart.time','Num_Exp_Row','Duration_Match',
              'hometown_Intern_LocMatch','data4.Internship_Typeregular','data4.Stipend_Typeunpaid','Is_IP_MK','Inf_hometown',
              'Is_IP_CW','Institute_Category','data4.Stipend_Typeperformance','data4.Internship_Typevirtual',
              'data4.Experience_Typeinternship','Inst_Intern_LocMatch','Is_Prof','Performance_12th','Is_IntrnLoc_JABD',
              'data4.Stipend_Typefixed','Workex_Intern_LocMatch','Is_IP_AD','Performance_UG','Is_PlIJCE',
              'data4.Internship_categoryFull.Time','data4.Stipend_Typevariable','Is_IntrnLoc_IIGB','Is_InstLoc_IIIF',
              'Is_IP_BD','NoCross_Deadline','Is_PlNo_Pref','Is_PlIHJB','Is_StrMarketing','Is_IntrnLoc_IIDB',
              'Is_IP_WD','Is_IntrnLoc_IIBD','Is_IntrnLoc_JEJJ','Is_IP_SD','Is_InstLoc_IIDB','Is_StrCommerce','Exp_tenure',
              'Is_Part_Time','St_EMatch','Dif_Yog_IntD','data4.Experience_Typeacademic_project','data4.Current_year2',
              'data4.Experience_TypeNo_Exp','Expected_Stipend','Is_Prof_Marketing','Is_MTech','Is_PlIIDB']


# In[13]:

#Choose important predictors and excepting target & IDcols
predictors = predictors1
param_test1 = {'n_estimators':range(20,81,10)}
gsearch1 = GridSearchCV(estimator = GradientBoostingClassifier(learning_rate=0.1, min_samples_split=500,
                                  min_samples_leaf=50,max_depth=8,max_features='sqrt', subsample=0.8,random_state=10), 
                       param_grid = param_test1, scoring='roc_auc',n_jobs=4,iid=False, cv=5)
gsearch1.fit(train[predictors],train[target])


# In[14]:

gsearch1.grid_scores_, gsearch1.best_params_, gsearch1.best_score_


# In[15]:

#Grid seach on subsample and max_features
predictors = predictors1
param_test2 = {'max_depth':range(2,7,2), 'min_samples_split':range(100,400,100)}
gsearch2 = GridSearchCV(estimator = GradientBoostingClassifier(learning_rate=0.1, n_estimators=70,
                                                max_features='sqrt', subsample=0.8, random_state=10), 
                       param_grid = param_test2, scoring='roc_auc',n_jobs=4,iid=False, cv=5)
gsearch2.fit(train[predictors],train[target])


# In[16]:

gsearch2.grid_scores_, gsearch2.best_params_, gsearch2.best_score_


# In[17]:

#Grid seach on subsample and max_features
predictors = predictors1
param_test3 = {'min_samples_split':range(50,200,50), 'min_samples_leaf':range(30,71,10)}
gsearch3 = GridSearchCV(estimator = GradientBoostingClassifier(learning_rate=0.1, n_estimators=70,max_depth=4,
                                                    max_features='sqrt', subsample=0.8, random_state=10), 
                       param_grid = param_test3, scoring='roc_auc',n_jobs=4,iid=False, cv=5)
gsearch3.fit(train[predictors],train[target])


# In[18]:

gsearch3.grid_scores_,gsearch3.best_estimator_, gsearch3.best_score_


# In[19]:

modelfit(gsearch3.best_estimator_, train, test, predictors)


# In[20]:

#Tune max_features:

#Grid seach on subsample and max_features
param_test4 = {'max_features':range(5,20,2)}
gsearch4 = GridSearchCV(estimator = GradientBoostingClassifier(learning_rate=0.1, n_estimators=70,max_depth=4, 
                            min_samples_split=150, min_samples_leaf=70, subsample=0.8, random_state=10),
                       param_grid = param_test4, scoring='roc_auc',n_jobs=4,iid=False, cv=5)
gsearch4.fit(train[predictors],train[target])


# In[21]:

gsearch4.grid_scores_, gsearch4.best_params_, gsearch4.best_score_


# In[22]:

### Step3- Tune Subsample and Lower Learning Rate
#Grid seach on subsample and max_features
param_test5 = {'subsample':[0.7,0.75,0.8,0.85,0.9]}
gsearch5 = GridSearchCV(estimator = GradientBoostingClassifier(learning_rate=0.1, n_estimators=70,max_depth=4, 
                            min_samples_split=150, min_samples_leaf=70, random_state=10, max_features=7),
                       param_grid = param_test5, scoring='roc_auc',n_jobs=4,iid=False, cv=5)
gsearch5.fit(train[predictors],train[target])


# In[23]:

gsearch5.grid_scores_, gsearch5.best_params_, gsearch5.best_score_


# In[ ]:

#  With all tuned lets try reducing the learning rate and proportionally increasing the number of estimators to get
#  more robust results:
#Choose all predictors except target & IDcols
predictors = predictors1
gbm_tuned_1 = GradientBoostingClassifier(learning_rate=0.05, n_estimators=120,max_depth=4, min_samples_split=150, 
                                         min_samples_leaf=70, subsample=0.7, random_state=10, max_features=5)
modelfit(gbm_tuned_1, train, test, predictors)


# In[24]:

est = GradientBoostingClassifier(learning_rate=0.05, n_estimators=120,max_depth=4, min_samples_split=150, 
                                         min_samples_leaf=70, subsample=0.7, random_state=10, max_features=5)


# In[25]:

est.fit(train[predictors],train[target])


# In[34]:

# predict probabilities
prob = est.predict_proba(test[predictors])[:,1]


# In[37]:

test1=test
test1['Is_Shortlisted']=prob[:]
test1.to_csv('E:/SS/AV/OnlineHack/Online Date Your Data/files/DYD_SEC1.csv',             columns=['Internship_ID','Student_ID','Is_Shortlisted'],index=False)




