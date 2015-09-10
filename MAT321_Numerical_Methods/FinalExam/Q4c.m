%%%%%%%%this file is for Question 4(c) and 4(d)%%%%%%%%%%%%%

fhandle = @(x)(exp(-x));
lower = -2;
upper = 0.5;
totnum = 10;

%Q4(c) Calculate the 2-norm 
err_2norm = zeros(1,(totnum+1));
err_supnorm = zeros(1,(totnum+1));
for n = 3:(3+totnum)
    fh = @(x)((eval_best_2norm_quad(fhandle,n,x,lower,upper)-fhandle(x)).^2);
    err_2norm(n-2) = integral(fh,lower,upper,'AbsTol',1e-12,'ArrayValued',true);
end
%Q4(d) Calculate the sup-norm
for n = 3:(3+totnum)
    fh2 = @(x)(abs(eval_best_2norm_quad(fhandle,n,x,lower,upper)-fhandle(x)));
    err_supnorm(n-2) = max(fh2(linspace(lower,upper,12345)));
end

plot(3:3+totnum,err_2norm);
hold on;
plot(3:3+totnum,err_supnorm,'r');
hold off;
legend('2-norm','sup-norm');
xlabel('n');
ylabel('error');
title('Using built-in function for Integration');