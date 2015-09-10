imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=50;%this is the number of cycles
lam=1;%this is the penalization factor
Lip=sum(sqrt(sum(images.^2)))+lam;%yita is the lipschitz factor
rad=15;%define the radius of the constraining euclidean ball
st=rad./(Lip*sqrt(1:numcyc));%this is the step of the algorithm
beta=zeros(dim,numcyc);%define constrained beta
betaun=zeros(dim,numcyc);%define unconstrained beta
betapre=zeros(dim,numcyc);%define the unconstrained beta after each pivot
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
    part2=lam*beta(:,(i-1))/sqrt(beta(:,(i-1))'*beta(:,(i-1)));
    betapre(:,i)=beta(:,(i-1))-st(1,(i-1))*(part1+part2);%this returns the unconstrained beta after each pivot
    beta(:,i)=betapre(:,i)+(sum(betapre(:,i).^2)>rad^2)*(rad*(betapre(:,i)/sqrt(betapre(:,i)'*betapre(:,i)))-betapre(:,i));%finds the nearest point within the ball
end
%%%%%%%%%%%%%%%%%%%analyze the convergence rate%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*sqrt(beta(:,k)'*beta(:,k));
end%calculate the vector of objective function
plot(loglossl2);
comp=rad*Lip./sqrt(1:numcyc);%this is the comparison series
loglossl2av=zeros(1,numcyc);%define the vector of objective function, with each element as average of previous items
for k=1:(numcyc-1)
    loglossl2av(1,k)=loglossl2*[ones(1,k),zeros(1,(numcyc-k))]'/k;
end%calculate the vector of objective function
loglossl2av(1,numcyc)=mean(loglossl2);
mini=1919;%this is the result of 10000 steps
plot(1:numcyc,loglossl2av-mini,1:numcyc,comp),legend('Empirical','Theoretical');