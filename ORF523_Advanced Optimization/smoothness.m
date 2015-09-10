%%%%%%%%%%%%%%%%%%%%%%%%%%logistic regression%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
normv=zeros(1,numtrain);%define a vector of norms of all the 60000 matrice
for i=1:numtrain
    normv(1,i)=norm(images(:,i)* images(:,i)');
end
smo=sum(normv)/4;%this is the beta in "beta smooth"
