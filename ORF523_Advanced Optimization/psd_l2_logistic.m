imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=100;%this is the number of cycles
st=0.00014;%this is the step of the algorithm
lam=1;%this is the penalization factor
beta=zeros(dim,numcyc);%define constrained beta
betaun=zeros(dim,numcyc);%define unconstrained beta
betapre=zeros(dim,numcyc);%define the unconstrained beta after each pivot
rad=10;%define the radius of the constraining euclidean ball
beta(:,1)=-0.1;%this is the initial value of constrained beta
betaun(:,1)=beta(:,1);%this is the initial value of unconstrained beta
%%%%%%%%%%%%%%%%%%initialize the labels into binomial label%%%%%%%%%%%%%%%%
labelsbi=zeros(numtrain,10);
for j=1:10
    labelsbi(:,j)=2*(labels(:,1)==(j-1))-1;
end
%%%%%%%%%%%%%%%%%%%start the constrained minimization%%%%%%%%%%%%%%%%%%%%%%
for i=2:numcyc
    part1=images*((-1)*labelsbi(:,1).*(exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1))))./(1+exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1)))))));
    part2=2*lam*beta(:,(i-1));
    betapre(:,i)=beta(:,(i-1))-st*(part1+part2);%this returns the unconstrained beta after each pivot
    beta(:,i)=betapre(:,i)+(sum(betapre(:,i).^2)>rad^2)*(rad*(betapre(:,i)/sqrt(betapre(:,i)'*betapre(:,i)))-betapre(:,i));%finds the nearest point within the ball
end
%%%%%%%%%%%%%%%%%%%%%%%convergence characteristics%%%%%%%%%%%%%%%%%%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*beta(:,k)'*beta(:,k);
end%calculate the vector of objective function
plot(loglossl2);
loglossl2