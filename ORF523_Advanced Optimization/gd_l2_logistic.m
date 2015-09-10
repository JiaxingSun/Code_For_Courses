imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=50;%this is the number of cycles
st=0.00007;%this is the step of the algorithm
lam=1;%this is the penalization factor
beta=zeros(dim,numcyc);%define beta
beta(:,1)=-0.1;%this is the initial value of beta
%%%%%%%%%%%%%%%%%%initialize the labels into binomial labels%%%%%%%%%%%%%%%
labelsbi=zeros(numtrain,10);
for j=1:10
    labelsbi(:,j)=2*(labels(:,1)==(j-1))-1;
end
%%%%%%%%%%%%%%%%%%%%%%%%start the minimization%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for i=2:numcyc
    part1=images*((-1)*labelsbi(:,1).*(exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1))))./(1+exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1)))))));
    part2=2*lam*beta(:,(i-1));
    beta(:,i)=beta(:,(i-1))-st*(part1+part2);
end
plot(beta(:,[1,(numcyc-9):(numcyc)])),title('Estimated coefficients, step=0.00007','FontSize',15);
%%%%%%%%%%%%%%%%%%%%%%%%prediction and error rate%%%%%%%%%%%%%%%%%%%%%%%%%%
pr=zeros(numtrain,10);%define the vector for binomial prediction(1 or -1)
prob=zeros(numtrain,10);%this is the probability of being +1
erate=zeros(1,numcyc);%define the vector of error rate, dim=number of cycles
for k=1:numcyc
prob(:,1)=1./(exp(((-1)*ones(numtrain,1).*(beta(:,k)'*images)'))+1);
pr=2*(prob>0.5)-1;%this uses logic calculation to judge whether p>0.5
erate(1,k)=1-sum(pr(:,1)==labelsbi(:,1))/numtrain;
end%calculate the vector of error rate
%%%%%%%%%%%%%%%%%%%%%%%convergence characteristics%%%%%%%%%%%%%%%%%%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*beta(:,k)'*beta(:,k);
end%calculate the vector of objective function
 plot(erate),title('Prediction error rates, step=0.00007','FontSize',15),xlabel('number of iterates','FontSize',10);
 plot(loglossl2),title('Evolution of loss function','FontSize',15),xlabel('number of iterates','FontSize',10);