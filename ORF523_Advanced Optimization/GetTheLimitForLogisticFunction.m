imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=500;%this is the number of cycles
lam=1;%this is the penalization factor
st=0.0001;%this is the step of the algorithm
beta=zeros(dim,numcyc);%define beta
minim=1400;%this is the minum of the function estimated through 1000iterates
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
%%%%%%%%%%%%%%%%%%%%%%%convergence characteristics%%%%%%%%%%%%%%%%%%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*beta(:,k)'*beta(:,k);
end%calculate the vector of objective function
plot(loglossl2),title('Evolution of loss function','FontSize',15),xlabel('number of iterates','FontSize',10);
lim=beta(:,numcyc);