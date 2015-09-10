ers = zeros(1,50);
for N = 1:50


x = linspace(-1,1,N+1);
y = cos(4*x);
[coef, ma] = interpolate(x,y);%solve for the coefficients

%find the largest absolute error
xseries = linspace(-1,1,1000);
yseries = cos(4*xseries);
numys = zeros(1,length(xseries));
for i = 1:length(x)
    numys = numys+coef(i)*xseries.^(i-1);
end
[max_err,max_i] = max(abs(numys-yseries))
ers(N) = max_err;
end
plot(ers);
grid on;
ylabel('maximum absolute error');
xlabel('N');