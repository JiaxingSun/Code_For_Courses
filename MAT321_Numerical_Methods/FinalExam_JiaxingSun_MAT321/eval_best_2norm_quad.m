function [px] = eval_best_2norm_quad(fhandle,n,y,a,b)
%n is the order of the polynomial(highest power of polynomial is n)

%generate the coefficients of Legendre polynomial
coef = zeros(n+1,n+1);
coef(1,1) = 1;
coef(2,1) = 0;
coef(2,2) = 1;
for i = 3:(n+1)
    coef(i,1) = (-1)*(i-2)/(i-1)*coef(i-2,1);
    coef(i,i) = (2*i-3)/(i-1)*coef(i-1,i-1);
    for j = 2:(i-1)
        coef(i,j) = (2*i-3)/(i-1)*coef(i-1,j-1)-(i-2)/(i-1)*coef(i-2,j);
    end
end
coef2 = zeros(1,n+1);%rising order


for i = 1:n+1
    fun = @(x)(polyval(fliplr(coef(i,:)),(a+b-2*x)/(a-b)));
    fh2 = @(x)(fhandle(x).*fun(x));
    coef2(i) = integral(fh2,a,b,'AbsTol',1e-12,'ArrayValued',true)/integral(@(x)(fun(x).*fun(x)),a,b,'AbsTol',1e-12,'ArrayValued',true);
end

px = 0;
for i = 1:n+1
px = px + coef2(i)*polyval(fliplr(coef(i,:)),(a+b-2*y)/(a-b));
end
