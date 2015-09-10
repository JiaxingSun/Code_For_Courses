%%%%%%%%%%Problem 2(e)%%%%%%%%%%
f1 = @(x)(exp(-x^2)); 
f2 = @(x)(sin(abs(x+pi/4)));%define the two functions
true1 = erf(1)*sqrt(pi)
true2 = -cos(1)*sqrt(2)+2 %calculate the true values of the two integrals
%%%%%%Gauss Quadrature
tot = 300;
error1 = zeros(tot,1);
error2 = zeros(tot,1);
for i = 1:tot
    error1(i) = abs(legendre_integrate(f1,i)-true1);
    error2(i) = abs(legendre_integrate(f2,i)-true2);%calculate the errors
end
plot(log(error1))
hold on;
plot(log(error2),'r')
ylabel('log(error)')
xlabel('number of quadrature nodes')

%%%%%Romberg Quadrature
tot = 8;
error1 = zeros(tot,1);
error2 = zeros(tot,1);
No1 = zeros(tot,1);
No2 = zeros(tot,1);
for i = 1:tot
    error1(i) = abs(myromberg(f1,-1,1,1e-50,i)-true1);
    No1(i) = 2^i;
    error2(i) = abs(myromberg(f2,-1,1,1e-50,i)-true2);%calculate the errors
    No2(i) = 2^i;
end

plot(No1,log(error1),'bo')
plot(No2,log(error2),'ro')
ylabel('log(error)')
xlabel('number of quadrature nodes')
title('Error Comparison')
legend('Gauss-f1','Gauss-f2','Romberg-f1','Romberg-f2')
hold off;

