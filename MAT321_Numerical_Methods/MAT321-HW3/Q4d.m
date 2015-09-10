rng(1);
n = 100;
Q = orth(rand(n));
D = diag([1 linspace(1.5,11,n-1)]);
A = Q*D*Q';
x_exact = Q(:,1);
lambda_exact = D(1,1);
x0 = x_exact + rand(n,1)/10;
lambda0 = lambda_exact + rand/10;
Nmax = 20;
lambda = zeros(1,Nmax);
lambda(1) = lambda0;
x = zeros(n,Nmax);
x(:,1) = x0;
for i = 1:(Nmax-1)
J = [A-lambda(i)*eye(n),-x(:,i);2*x(:,i)',0];
RHS = [x(:,i);lambda(i)]-J\[(A-lambda(i)*eye(n))*x(:,i);x(:,i)'*x(:,i)-1];
x(:,(i+1)) = RHS(1:n);
lambda(i+1) = RHS(n+1);
end
Er_lambda = abs(lambda-lambda_exact);
Er_x = sqrt(sum((x-x_exact*ones(1,Nmax)).^2));
[vecinv,valinv,Er_inverse] = inversepower_shift(A,lambda0,20);
plot(Er_inverse,'-o');
xlabel('No. of iterations');
ylabel('Error in eigenvalue');
hold on;
plot(Er_lambda,'r-o');
hold off;


