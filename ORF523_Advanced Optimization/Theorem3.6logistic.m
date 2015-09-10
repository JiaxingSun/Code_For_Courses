imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=100;%this is the number of cycles
lam=1;%this is the penalization factor
rad=15;%define the radius of the constraining euclidean ball
Lip=sum(sqrt(sum(images.^2)))+2*lam*rad;%yita is the lipschitz factor
smo=0.25*sum(sum(images.^2))+2*lam;%calculate the smooth factor
alp=2;
Q=smo/alp;
st=1/smo;%this is the step of the algorithm
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
    part2=2*lam*beta(:,(i-1));
    betapre(:,i)=beta(:,(i-1))-st*(part1+part2);%this returns the unconstrained beta after each pivot
    beta(:,i)=betapre(:,i)+(sum(betapre(:,i).^2)>rad^2)*(rad*(betapre(:,i)/sqrt(betapre(:,i)'*betapre(:,i)))-betapre(:,i));%finds the nearest point within the ball
end
%%%%%%%%%%%%%%%%%%%analyze the convergence rate%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*beta(:,k)'*beta(:,k);
end%calculate the vector of objective function
plot(loglossl2),title('Evolution of loss function','FontSize',15),xlabel('number of iterates','FontSize',10);
%%%%%%%%%%%%%%%%%%%compare with the theoretical upper bound%%%%%%%%%%%%%%%%
compl=sum((beta(:,2:numcyc)-lim*ones(1,numcyc-1)).^2);
compr=exp(-[(1:(numcyc-1))]/Q)*sum((beta(:,1)-lim).^2);
plot(1:numcyc-1,compl,1:numcyc-1,compr),legend('Empirical','Theoretical'),title('Comparison of convergence rate','FontSize',15),xlabel('number of iterates','FontSize',10);
