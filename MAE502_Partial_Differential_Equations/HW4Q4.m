numcyc=400;%number of cycles
num=100;%number of grid points
st=1/num;%size of step
epsi=0.5;%equation parameter
xs=[1:st:2];%x series
ys=zeros(num+1,1);%y series
ysper=zeros(num+1,1);%y series, perturbation
cone=4/(exp(-0.5/epsi)-0.5*exp(-2/epsi));
czero=-4-2*exp(-2/epsi)/(exp(-0.5/epsi)-0.5*exp(-2/epsi));
for i=1:(num+1)
    ysper(i,1)=cone/(1+(i-1)*st)*exp(-0.5/epsi*(1+(i-1)*st)^2)+2*(1+(i-1)*st)+czero;
end
ys(1,1)=2;%boundary value
ys(num+1,1)=0;%boundary value
for k=1:numcyc
for i=2:num
    ys(i,1)=ys(i+1,1)*0.5+0.5*ys(i-1,1)+(st+(i-1)*st*st)/(4*epsi)*(ys(i+1,1)-ys(i-1,1))-(st^2+(i-1)*st^3)/epsi;
end
end
plot(xs,ys,xs,ysper),xlabel('x'),ylabel('y'),title('Comparison'),grid on,legend('Numerical','Perturbation')