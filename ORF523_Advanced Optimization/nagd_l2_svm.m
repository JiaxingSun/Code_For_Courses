imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=100;%this is the number of cycles
lam=1;%this is the penalization factor
beta=zeros(dim,numcyc);%define constrained beta
betaun=zeros(dim,numcyc);%define unconstrained beta
betapre=zeros(dim,numcyc);%define the y series
betapre2=zeros(dim,numcyc);
bt=20000;%define the smoonthness parameter
alpha=18;%define the strongly convexness parameter
Q=bt/alpha;
rad=10;%define the radius of the constraining euclidean ball
beta(:,1)=-0.1;%this is the initial value of constrained beta
betapre2(:,1)=beta(:,1);%Initialize betapre(:,1) the same as beta(:,1)
betapre(:,1)=beta(:,1);%Initialize betapre(:,1) the same as beta(:,1)
%%%%%%%%%%%%%%%%%%initialize the labels into binomial label%%%%%%%%%%%%%%%%
labelsbi=zeros(numtrain,10);
for j=1:10
    labelsbi(:,j)=2*(labels(:,1)==(j-1))-1;
end
%%%%%%%%%%%%%%%%%%%start the constrained minimization%%%%%%%%%%%%%%%%%%%%%%
for i=2:numcyc
    part1=images*((-1)*labelsbi(:,1).*(exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1))))./(1+exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1)))))));
    part2=2*lam*beta(:,(i-1));
    betapre2(:,i)=beta(:,(i-1))-(1/bt)*(part1+part2);%this returns the y series after each pivot
    betapre(:,i)=betapre2(:,i)+(sum(betapre2(:,i).^2)>rad^2)*(rad*(betapre2(:,i)/sqrt(betapre2(:,i)'*betapre2(:,i)))-betapre2(:,i));%finds the nearest point within the ball
    beta(:,i)=betapre(:,i)+(sqrt(Q)-1)/(sqrt(Q)+1)*(betapre(:,i)-betapre(:,(i-1)));
end
%%%%%%%%%%%%%%%%%%%%%%%convergence characteristics%%%%%%%%%%%%%%%%%%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*beta(:,k)'*beta(:,k);
end%calculate the vector of objective function
plot(loglossl2)
plot(1:numcyc,losspsd,1:numcyc,losscgd,1:numcyc,lossnagd)