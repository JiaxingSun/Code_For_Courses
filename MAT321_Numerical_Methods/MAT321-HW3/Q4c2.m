rng(1);
n = 100;
Q = orth(rand(n));
D = diag([1 linspace(1.5,11,n-1)]);
A = Q*D*Q';
x_exact = Q(:,1);
lambda_exact = D(1,1);
x0 = x_exact + rand(n,1)/10;
lambda0 = lambda_exact + rand/10;
Nmax = 30;
lambda = zeros(1,Nmax);
lambda(1) = lambda0;
x = zeros(n,Nmax);
x(:,1) = x0;
for i = 1:(Nmax-1)

invJ = [2*x(:,i)'*((A-lambda(i)*eye(n))\eye(n))*x(:,i)*((A-lambda(i)*eye(n))\eye(n))-2*((A-lambda(i)*eye(n))\eye(n))*x(:,i)*x(:,i)'*((A-lambda(i)*eye(n))\eye(n)),((A-lambda(i)*eye(n))\eye(n))*x(:,i);-2*x(:,i)'*((A-lambda(i)*eye(n))\eye(n)),1]/(2*x(:,i)'*((A-lambda(i)*eye(n))\eye(n))*x(:,i));
RHS = [x(:,i);lambda(i)]-invJ*[(A-lambda(i)*eye(n))*x(:,i);x(:,i)'*x(:,i)-1];
x(:,(i+1)) = RHS(1:n);
lambda(i+1) = RHS(n+1);
end
Er_lambda = abs(lambda-lambda_exact);
Er_x = sqrt(sum((x-x_exact*ones(1,Nmax)).^2));
plot(Er_lambda,'-o');
xlabel('No. of iterations');
ylabel('Error in eigenvalue in solving block-wise Jacobian');
hold on;
plot(Er_x,'r-o');
hold off;
